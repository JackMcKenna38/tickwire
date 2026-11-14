(* Position, cash, fees and P&L bookkeeping shared by both simulators.

   Units: inventory in qty units; cash, fees and P&L in price*qty units
   (i.e. quote currency scaled by 10^(price_decimals + qty_decimals)).
   Mid prices are carried doubled ([mid2] = best bid + best ask) so they
   stay exact integers. *)

type fill = {
  ts : int;
  side : Book.side; (* Bid = we bought, Ask = we sold *)
  price : int;
  qty : int;
  mid2 : int; (* doubled mid at the moment of the fill *)
  fee : int;
  inv_after : int;
}

type sample = { s_ts : int; gross : int; net : int; inv : int }

type t = {
  fee_ppm : int;
  mutable inv : int;
  mutable cash : int;
  mutable fees : int;
  mutable basis : int; (* cost (long) or proceeds (short) of the open position *)
  mutable realised : int; (* gross realised P&L, average-cost method *)
  mutable fills : fill list; (* newest first *)
  mutable n_fills : int;
  mutable orders_placed : int;
  mutable orders_filled : int; (* orders that received at least one fill *)
  mutable quoted_qty : int;
  mutable filled_qty : int;
  mutable rejects : int;
  mutable samples : sample list; (* newest first *)
  mutable next_sample : int;
  mutable peak : int;
  mutable max_drawdown : int;
  mutable last_mid2 : int;
  mutable max_abs_inv : int;
}

let create ~fee_ppm =
  {
    fee_ppm;
    inv = 0;
    cash = 0;
    fees = 0;
    basis = 0;
    realised = 0;
    fills = [];
    n_fills = 0;
    orders_placed = 0;
    orders_filled = 0;
    quoted_qty = 0;
    filled_qty = 0;
    rejects = 0;
    samples = [];
    next_sample = min_int;
    peak = 0;
    max_drawdown = 0;
    last_mid2 = 0;
    max_abs_inv = 0;
  }

let floor_half x = if x >= 0 then x / 2 else -((-x + 1) / 2)

(* Gross marked-to-mid P&L: cash plus inventory valued at the mid. *)
let gross_mtm t ~mid2 = t.cash + floor_half (t.inv * mid2)
let net_mtm t ~mid2 = gross_mtm t ~mid2 - t.fees

let apply_fill t ~ts ~(side : Book.side) ~price ~qty ~mid2 =
  let notional = price * qty in
  let fee = Fees.fee ~ppm:t.fee_ppm notional in
  (match side with
  | Book.Bid ->
      t.cash <- t.cash - notional;
      if t.inv >= 0 then t.basis <- t.basis + notional
      else
        let short = -t.inv in
        let close = min qty short in
        let b = Fixed.muldiv t.basis close short in
        t.realised <- t.realised + b - (price * close);
        t.basis <- t.basis - b + (price * (qty - close));
        ()
  | Book.Ask ->
      t.cash <- t.cash + notional;
      if t.inv <= 0 then t.basis <- t.basis + notional
      else
        let close = min qty t.inv in
        let b = Fixed.muldiv t.basis close t.inv in
        t.realised <- t.realised + (price * close) - b;
        t.basis <- t.basis - b + (price * (qty - close)));
  t.inv <- (match side with Book.Bid -> t.inv + qty | Book.Ask -> t.inv - qty);
  if abs t.inv > t.max_abs_inv then t.max_abs_inv <- abs t.inv;
  t.fees <- t.fees + fee;
  t.filled_qty <- t.filled_qty + qty;
  t.n_fills <- t.n_fills + 1;
  t.fills <- { ts; side; price; qty; mid2; fee; inv_after = t.inv } :: t.fills

let push_sample t ~ts ~mid2 =
  let gross = gross_mtm t ~mid2 in
  let net = gross - t.fees in
  t.samples <- { s_ts = ts; gross; net; inv = t.inv } :: t.samples;
  if net > t.peak then t.peak <- net;
  if t.peak - net > t.max_drawdown then t.max_drawdown <- t.peak - net

(* Called on every book event; records one equity sample per second. *)
let on_mid t ~ts ~mid2 =
  t.last_mid2 <- mid2;
  if ts >= t.next_sample then (
    push_sample t ~ts ~mid2;
    t.next_sample <- ((ts / Timeutil.ns_per_s) + 1) * Timeutil.ns_per_s)

let finish t ~ts = if t.last_mid2 > 0 then push_sample t ~ts ~mid2:t.last_mid2
