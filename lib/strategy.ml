(* A textbook symmetric market maker with inventory skew.

   reservation = mid - skew * (inventory / order size)
   bid = reservation - half_spread, rounded down to a tick
   ask = reservation + half_spread, rounded up to a tick

   Quotes are post-only: they are clamped so they never cross the book.
   Both simulators call exactly this function. *)

type config = {
  half_spread_cbps : int; (* half spread, hundredths of a basis point of mid *)
  skew_cbps : int; (* reservation shift per one order-size of inventory *)
  order_notional_usd : int; (* fixed order size, in whole quote-currency units *)
  max_inventory_orders : int; (* inventory cap, in order sizes *)
  requote_ticks : int; (* replace a quote only if it should move this far *)
}

let default =
  {
    half_spread_cbps = 300;
    skew_cbps = 100;
    order_notional_usd = 1000;
    max_inventory_orders = 5;
    requote_ticks = 1;
  }

(* Per-symbol parameters in instrument units, fixed for a whole run. *)
type params = {
  cfg : config;
  tick : int;
  size : int; (* qty units *)
  max_inv : int; (* qty units *)
  qty_min : int;
}

type quote = { price : int; qty : int }

let floor_div a b = if a >= 0 then a / b else -((-a + b - 1) / b)
let ceil_div a b = -floor_div (-a) b

(* Order size from the notional and the first mid we see, rounded down to
   the quantity increment. *)
let make_params (cfg : config) (inst : Feed.instrument) ~mid2 : params =
  let pscale = Fixed.pow10 inst.price_decimals and qscale = Fixed.pow10 inst.qty_decimals in
  (* qty = notional / mid = notional * pscale * 2 / mid2, in qty units *)
  let raw = Fixed.muldiv (cfg.order_notional_usd * pscale * 2) qscale mid2 in
  let inc = max 1 inst.qty_increment in
  let size = max inst.qty_min (raw / inc * inc) in
  { cfg; tick = max 1 inst.price_increment; size; max_inv = size * cfg.max_inventory_orders; qty_min = inst.qty_min }

(* Desired (bid, ask) given the touch and current inventory (qty units). *)
let desired (p : params) ~best_bid ~best_ask ~inventory : quote option * quote option =
  let mid2 = best_bid + best_ask in
  (* All in half price units (x2) to keep the mid exact. *)
  let hs2 = mid2 * p.cfg.half_spread_cbps / 1_000_000 in
  let skew2 = floor_div (mid2 * p.cfg.skew_cbps / 1_000_000 * inventory) p.size in
  let res2 = mid2 - skew2 in
  let bid = floor_div (res2 - hs2) (2 * p.tick) * p.tick in
  let ask = ceil_div (res2 + hs2) (2 * p.tick) * p.tick in
  let bid = min bid (best_ask - p.tick) in
  let ask = max ask (best_bid + p.tick) in
  let bid_qty = min p.size (p.max_inv - inventory) in
  let ask_qty = min p.size (p.max_inv + inventory) in
  let mk price qty = if qty >= p.qty_min && qty > 0 && price > 0 then Some { price; qty } else None in
  (mk bid bid_qty, mk ask ask_qty)

(* Should a live quote at [current] be replaced by [want]? *)
let should_replace (p : params) ~(current : quote option) ~(want : quote option) : bool =
  match (current, want) with
  | None, None -> false
  | None, Some _ | Some _, None -> true
  | Some c, Some w -> abs (c.price - w.price) >= p.cfg.requote_ticks * p.tick
