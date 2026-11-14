(* Runs the naive and the honest simulation side by side over a stream of
   feed events, and turns the outcome into comparable metrics. *)

type config = { strat : Strategy.config; honest : Honest.config; fee_ppm : int }

let default_config =
  { strat = Strategy.default; honest = Honest.default_config; fee_ppm = 4000 }

(* growable int array *)
module Ivec = struct
  type t = { mutable a : int array; mutable n : int }

  let create () = { a = Array.make 1024 0; n = 0 }

  let push v x =
    if v.n = Array.length v.a then (
      let b = Array.make (2 * v.n) 0 in
      Array.blit v.a 0 b 0 v.n;
      v.a <- b);
    v.a.(v.n) <- x;
    v.n <- v.n + 1
end

type markout = {
  horizon_ns : int;
  n : int; (* fills old enough to be measured *)
  mid_move_cbps : int; (* average signed mid move after the fill; negative = adverse *)
  edge_cbps : int; (* average signed (mid after - fill price); what the fill was worth *)
}

type model_result = {
  model : string;
  fills : Account.fill array; (* chronological *)
  samples : Account.sample array; (* chronological, one per second *)
  n_fills : int;
  orders_placed : int;
  orders_filled : int;
  rejects : int;
  quoted_qty : int;
  filled_qty : int;
  gross : int; (* marked-to-mid P&L before fees *)
  fees : int;
  net : int;
  realised : int; (* gross, average cost *)
  unrealised : int;
  final_inv : int;
  max_abs_inv : int;
  max_drawdown : int; (* on net marked-to-mid equity *)
  markouts : markout list;
}

type sym_result = {
  symbol : string;
  inst : Feed.instrument;
  params : Strategy.params;
  first_ts : int;
  last_ts : int;
  final_mid2 : int;
  trades_book_first : int; (* trades whose level had already left the book *)
  trades_trade_first : int;
  naive : model_result;
  honest : model_result;
}

type result = { cfg : config; syms : sym_result list }

let horizons = [ Timeutil.ns_per_s; 10 * Timeutil.ns_per_s; 60 * Timeutil.ns_per_s ]

type sym_state = {
  name : string;
  mutable s_inst : Feed.instrument option;
  mutable sims : (Strategy.params * Naive.t * Honest.Fast.t) option;
  mid_ts : Ivec.t;
  mid_v : Ivec.t;
  mutable s_first : int;
  mutable s_last : int;
  mutable s_bb : int;
  mutable s_ba : int;
  mutable book_first : int;
  mutable trade_first : int;
}

type t = { config : config; mutable states : sym_state list }

let create config = { config; states = [] }

let state t name =
  match List.find_opt (fun s -> s.name = name) t.states with
  | Some s -> s
  | None ->
      let s =
        {
          name;
          s_inst = None;
          sims = None;
          mid_ts = Ivec.create ();
          mid_v = Ivec.create ();
          s_first = 0;
          s_last = 0;
          s_bb = 0;
          s_ba = 0;
          book_first = 0;
          trade_first = 0;
        }
      in
      t.states <- s :: t.states;
      s

let on_event t (e : Feed.event) =
  match e with
  | Feed.Instrument { symbol; inst; _ } -> (state t symbol).s_inst <- Some inst
  | Feed.Book { symbol; ts; book; _ } -> (
      let s = state t symbol in
      s.s_last <- ts;
      match (Book.best_bid book, Book.best_ask book) with
      | Some (bb, _), Some (ba, _) -> (
          let mid2 = bb + ba in
          s.s_bb <- bb;
          s.s_ba <- ba;
          (match (s.sims, s.s_inst) with
          | None, Some inst ->
              let p = Strategy.make_params t.config.strat inst ~mid2 in
              s.s_first <- ts;
              s.sims <-
                Some
                  ( p,
                    Naive.create p ~fee_ppm:t.config.fee_ppm,
                    Honest.Fast.create p t.config.honest ~fee_ppm:t.config.fee_ppm )
          | _ -> ());
          match s.sims with
          | Some (_, naive, honest) ->
              if s.mid_v.n = 0 || s.mid_v.a.(s.mid_v.n - 1) <> mid2 then (
                Ivec.push s.mid_ts ts;
                Ivec.push s.mid_v mid2);
              Naive.on_book naive ~ts book;
              Honest.Fast.on_book honest ~ts book
          | None -> ())
      | _ -> ())
  | Feed.Trade { ts; trade } -> (
      let s = state t trade.t_symbol in
      s.s_last <- ts;
      (* Which arrived first, this trade or the book update showing it?  If
         the level it printed at is already gone, the book was first. *)
      (if s.s_bb > 0 then
         let gone =
           match trade.taker with Feed.Sell -> trade.price > s.s_bb | Feed.Buy -> trade.price < s.s_ba
         in
         if gone then s.book_first <- s.book_first + 1 else s.trade_first <- s.trade_first + 1);
      match s.sims with
      | Some (_, naive, honest) ->
          Naive.on_trade naive ~ts ~price:trade.price;
          Honest.Fast.on_trade honest ~ts ~taker:trade.taker ~price:trade.price ~qty:trade.qty
      | None -> ())
  | Feed.Resync { symbol; ts; _ } -> (
      let s = state t symbol in
      match s.sims with
      | Some (_, naive, honest) ->
          Naive.on_gap naive;
          Honest.Fast.on_gap honest ~ts
      | None -> ())

(* Doubled mid in force at time [ts]: the last change at or before it. *)
let mid_at (s : sym_state) ts =
  let lo = ref 0 and hi = ref (s.mid_ts.n - 1) in
  while !lo < !hi do
    let m = (!lo + !hi + 1) / 2 in
    if s.mid_ts.a.(m) <= ts then lo := m else hi := m - 1
  done;
  s.mid_v.a.(!lo)

let markouts (s : sym_state) (fills : Account.fill array) =
  List.map
    (fun h ->
      let n = ref 0 and move = ref 0 and edge = ref 0 in
      Array.iter
        (fun (f : Account.fill) ->
          if f.ts + h <= s.s_last && s.mid_ts.n > 0 then (
            let m = mid_at s (f.ts + h) in
            let sign = match f.side with Book.Bid -> 1 | Book.Ask -> -1 in
            incr n;
            move := !move + (sign * (m - f.mid2) * 1_000_000 / f.mid2);
            edge := !edge + (sign * (m - (2 * f.price)) * 1_000_000 / f.mid2)))
        fills;
      {
        horizon_ns = h;
        n = !n;
        mid_move_cbps = (if !n = 0 then 0 else !move / !n);
        edge_cbps = (if !n = 0 then 0 else !edge / !n);
      })
    horizons

let model_result (s : sym_state) model (a : Account.t) : model_result =
  let fills = Array.of_list (List.rev a.fills) in
  let gross = Account.gross_mtm a ~mid2:a.last_mid2 in
  {
    model;
    fills;
    samples = Array.of_list (List.rev a.samples);
    n_fills = a.n_fills;
    orders_placed = a.orders_placed;
    orders_filled = a.orders_filled;
    rejects = a.rejects;
    quoted_qty = a.quoted_qty;
    filled_qty = a.filled_qty;
    gross;
    fees = a.fees;
    net = gross - a.fees;
    realised = a.realised;
    unrealised = gross - a.realised;
    final_inv = a.inv;
    max_abs_inv = a.max_abs_inv;
    max_drawdown = a.max_drawdown;
    markouts = markouts s fills;
  }

let finish (t : t) : result =
  let states = List.sort (fun a b -> compare a.name b.name) t.states in
  let syms =
    List.filter_map
      (fun s ->
        match (s.sims, s.s_inst) with
        | Some (p, naive, honest), Some inst ->
            Naive.finish naive ~ts:s.s_last;
            Honest.Fast.finish honest ~ts:s.s_last;
            Some
              {
                symbol = s.name;
                inst;
                params = p;
                first_ts = s.s_first;
                last_ts = s.s_last;
                final_mid2 = (if s.mid_v.n > 0 then s.mid_v.a.(s.mid_v.n - 1) else 0);
                trades_book_first = s.book_first;
                trades_trade_first = s.trade_first;
                naive = model_result s "naive" naive.acct;
                honest = model_result s "honest" honest.acct;
              }
        | _ -> None)
      states
  in
  { cfg = t.config; syms }

(* ---- money formatting (exact) ---- *)

let cash_decimals (i : Feed.instrument) = i.price_decimals + i.qty_decimals

(* Cash units of a symbol -> micro-dollars, so symbols can be added. *)
let to_micro (i : Feed.instrument) v = Fixed.rescale ~from_decimals:(cash_decimals i) ~to_decimals:6 v
let usd_of_micro v = Fixed.to_string ~decimals:2 (Fixed.rescale ~from_decimals:6 ~to_decimals:2 v)
let usd i v = usd_of_micro (to_micro i v)
let cbps_string v = Fixed.to_string ~decimals:2 v

(* ---- canonical text form: the deterministic output that gets hashed ---- *)

let dump (r : result) : string =
  let b = Buffer.create 65536 in
  let c = r.cfg in
  Printf.bprintf b "tickwire-sim-v1\n";
  Printf.bprintf b
    "config half_spread_cbps=%d skew_cbps=%d order_notional_usd=%d max_inventory_orders=%d requote_ticks=%d latency_ns=%d cancel_model=%s ttl_ns=%d maker_fee_ppm=%d\n"
    c.strat.half_spread_cbps c.strat.skew_cbps c.strat.order_notional_usd c.strat.max_inventory_orders
    c.strat.requote_ticks c.honest.latency_ns
    (Fill.cancel_model_to_string c.honest.cancel_model)
    c.honest.ttl_ns c.fee_ppm;
  List.iter
    (fun s ->
      Printf.bprintf b "symbol %s first_ts=%d last_ts=%d tick=%d size=%d max_inv=%d final_mid2=%d\n" s.symbol
        s.first_ts s.last_ts s.params.tick s.params.size s.params.max_inv s.final_mid2;
      List.iter
        (fun m ->
          Printf.bprintf b
            "model %s fills=%d orders=%d orders_filled=%d rejects=%d quoted=%d filled=%d gross=%d fees=%d net=%d realised=%d unrealised=%d inv=%d max_abs_inv=%d max_drawdown=%d\n"
            m.model m.n_fills m.orders_placed m.orders_filled m.rejects m.quoted_qty m.filled_qty m.gross
            m.fees m.net m.realised m.unrealised m.final_inv m.max_abs_inv m.max_drawdown;
          List.iter
            (fun k ->
              Printf.bprintf b "markout %d n=%d mid_move_cbps=%d edge_cbps=%d\n" k.horizon_ns k.n
                k.mid_move_cbps k.edge_cbps)
            m.markouts;
          Array.iter
            (fun (f : Account.fill) ->
              Printf.bprintf b "F %d %s %d %d %d %d %d\n" f.ts (Book.side_to_string f.side) f.price f.qty
                f.mid2 f.fee f.inv_after)
            m.fills;
          Array.iter
            (fun (x : Account.sample) -> Printf.bprintf b "S %d %d %d %d\n" x.s_ts x.gross x.net x.inv)
            m.samples)
        [ s.naive; s.honest ])
    r.syms;
  Buffer.contents b

let hash (r : result) = Digest.to_hex (Digest.string (dump r))

(* Run both simulations over recorded logs. *)
let run_files ?start ?(log = fun (_ : string) -> ()) (cfg : config) (files : string list) :
    Replay.result * result =
  let t = create cfg in
  let rr = Replay.run ~log ~on_event:(on_event t) ?start files in
  (rr, finish t)
