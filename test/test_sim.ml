open Tickwire

let bid = Book.Bid
let ask = Book.Ask
let sec = Timeutil.ns_per_s
let ms = Timeutil.ns_per_ms

let fill_t =
  Alcotest.testable
    (fun f (x : Fill.fill) ->
      Format.fprintf f "{%s px=%d qty=%d done=%b}" (Book.side_to_string x.f_side) x.f_price x.f_qty x.f_done)
    ( = )

let view_t =
  Alcotest.testable
    (fun f (v : Fill.view) -> Format.fprintf f "{px=%d rem=%d ahead=%d}" v.v_price v.v_remaining v.v_ahead)
    ( = )

let fills = Alcotest.list fill_t
let view = Alcotest.option view_t
let v p r a = Some { Fill.v_price = p; v_remaining = r; v_ahead = a }
let f side p q d = { Fill.f_side = side; f_price = p; f_qty = q; f_done = d }

(* ------------------------------------------------------------------ *)
(* Hand-built scenarios for every fill rule, run against BOTH models.  *)

module Rules (F : Fill.S) = struct
  let mk ?(model = Fill.Prorata) () = F.create ~cancel_model:model ~ttl_ns:sec

  (* a book lookup with one level per side *)
  let shown ?(b = None) ?(a = None) () side _price = match side with Book.Bid -> b | Book.Ask -> a

  let r1_join_back () =
    let t = mk () in
    F.place t bid ~price:100 ~qty:10 ~displayed:(Some 50);
    Alcotest.check view "joins behind the displayed 50" (v 100 10 50) (F.view t bid)

  let r2_queue_then_fill () =
    let t = mk () in
    F.place t bid ~price:100 ~qty:10 ~displayed:(Some 50);
    Alcotest.check fills "trade smaller than queue: no fill" []
      (F.on_trade t ~ts:0 ~taker:Feed.Sell ~price:100 ~qty:30);
    Alcotest.check view "queue shrank by the trade" (v 100 10 20) (F.view t bid);
    Alcotest.check fills "queue exhausted, partial fill of 4"
      [ f bid 100 4 false ]
      (F.on_trade t ~ts:1 ~taker:Feed.Sell ~price:100 ~qty:24);
    Alcotest.check view "6 left, at the front" (v 100 6 0) (F.view t bid);
    Alcotest.check fills "rest fills, capped at remaining"
      [ f bid 100 6 true ]
      (F.on_trade t ~ts:2 ~taker:Feed.Sell ~price:100 ~qty:500);
    Alcotest.check view "order gone" None (F.view t bid)

  let r2_wrong_side_or_price () =
    let t = mk () in
    F.place t bid ~price:100 ~qty:10 ~displayed:(Some 0);
    Alcotest.check fills "buy-taker trades never fill a bid" []
      (F.on_trade t ~ts:0 ~taker:Feed.Buy ~price:100 ~qty:99);
    Alcotest.check fills "trade above our bid does nothing" []
      (F.on_trade t ~ts:0 ~taker:Feed.Sell ~price:101 ~qty:99);
    Alcotest.check view "untouched" (v 100 10 0) (F.view t bid)

  let r3_trade_through () =
    let t = mk () in
    F.place t bid ~price:100 ~qty:10 ~displayed:(Some 1_000_000);
    Alcotest.check fills "a print below our bid fills us in full at our price"
      [ f bid 100 10 true ]
      (F.on_trade t ~ts:0 ~taker:Feed.Sell ~price:99 ~qty:1);
    let t = mk () in
    F.place t ask ~price:200 ~qty:7 ~displayed:(Some 1_000_000);
    Alcotest.check fills "mirror for asks"
      [ f ask 200 7 true ]
      (F.on_trade t ~ts:0 ~taker:Feed.Buy ~price:201 ~qty:1)

  let ask_side_mirror () =
    let t = mk () in
    F.place t ask ~price:200 ~qty:10 ~displayed:(Some 5);
    Alcotest.check fills "sell-taker trades never fill an ask" []
      (F.on_trade t ~ts:0 ~taker:Feed.Sell ~price:200 ~qty:99);
    Alcotest.check fills "queue of 5 then 3 of ours"
      [ f ask 200 3 false ]
      (F.on_trade t ~ts:0 ~taker:Feed.Buy ~price:200 ~qty:8)

  let r4_cancel_prorata () =
    let t = mk ~model:Fill.Prorata () in
    F.place t bid ~price:100 ~qty:10 ~displayed:(Some 100);
    (* 40 more joins behind: displayed 140, ahead still 100 *)
    F.on_book t ~ts:0 (shown ~b:(Some 140) ());
    Alcotest.check view "R6: joins are behind us" (v 100 10 100) (F.view t bid);
    (* 70 cancels with no trade: ahead loses 70 * 100/140 = 50 *)
    F.on_book t ~ts:1 (shown ~b:(Some 70) ());
    Alcotest.check view "pro-rata share of cancels" (v 100 10 50) (F.view t bid)

  let r4_cancel_pessimistic () =
    let t = mk ~model:Fill.Pessimistic () in
    F.place t bid ~price:100 ~qty:10 ~displayed:(Some 100);
    F.on_book t ~ts:0 (shown ~b:(Some 140) ());
    F.on_book t ~ts:1 (shown ~b:(Some 120) ());
    Alcotest.check view "cancels assumed behind us" (v 100 10 100) (F.view t bid);
    F.on_book t ~ts:2 (shown ~b:(Some 60) ());
    Alcotest.check view "R5: ahead capped by what is displayed" (v 100 10 60) (F.view t bid)

  let r4_rounding_against_us () =
    let t = mk () in
    F.place t bid ~price:100 ~qty:10 ~displayed:(Some 3);
    F.on_book t ~ts:0 (shown ~b:(Some 2) ());
    (* cancel 1 of 3 with 3 ahead: exactly 1 *)
    Alcotest.check view "exact" (v 100 10 2) (F.view t bid);
    let t = mk () in
    F.place t bid ~price:100 ~qty:10 ~displayed:(Some 2);
    F.on_book t ~ts:0 (shown ~b:(Some 5) ());
    F.on_book t ~ts:1 (shown ~b:(Some 4) ());
    (* cancel 1 of 5 with 2 ahead: 0.4 -> rounds to 0 *)
    Alcotest.check view "fraction rounds to no improvement" (v 100 10 2) (F.view t bid)

  let r7_trade_then_book () =
    let t = mk () in
    F.place t bid ~price:100 ~qty:10 ~displayed:(Some 100);
    ignore (F.on_trade t ~ts:0 ~taker:Feed.Sell ~price:100 ~qty:30);
    Alcotest.check view "trade took 30 from the front" (v 100 10 70) (F.view t bid);
    (* the book now shows the same 30 gone: must not count again *)
    F.on_book t ~ts:(10 * ms) (shown ~b:(Some 70) ());
    Alcotest.check view "drop explained by the trade" (v 100 10 70) (F.view t bid);
    (* a further unexplained drop is a cancel: 35 * 70/70 *)
    F.on_book t ~ts:(20 * ms) (shown ~b:(Some 35) ());
    Alcotest.check view "then a real cancel" (v 100 10 35) (F.view t bid)

  let r7_book_then_trade () =
    let t = mk () in
    F.place t bid ~price:100 ~qty:10 ~displayed:(Some 100);
    (* book shows 30 gone first; provisionally a cancel: 30 * 100/100 = 30 *)
    F.on_book t ~ts:0 (shown ~b:(Some 70) ());
    Alcotest.check view "provisional cancel" (v 100 10 70) (F.view t bid);
    (* the trade that caused it arrives: must not shrink the queue twice *)
    Alcotest.check fills "no fill" [] (F.on_trade t ~ts:(10 * ms) ~taker:Feed.Sell ~price:100 ~qty:30);
    Alcotest.check view "counted once" (v 100 10 70) (F.view t bid)

  let r7_book_then_trade_partial_queue () =
    let t = mk () in
    F.place t bid ~price:100 ~qty:10 ~displayed:(Some 50);
    F.on_book t ~ts:0 (shown ~b:(Some 100) ());
    (* 50 ahead, 50 behind.  Book drops 40: provisional pro-rata cancel takes 20 from ahead. *)
    F.on_book t ~ts:1 (shown ~b:(Some 60) ());
    Alcotest.check view "provisional" (v 100 10 30) (F.view t bid);
    (* It was really a 40 trade: front of queue, so ahead = 50 - 40 = 10. *)
    ignore (F.on_trade t ~ts:2 ~taker:Feed.Sell ~price:100 ~qty:40);
    Alcotest.check view "reclassified as a trade" (v 100 10 10) (F.view t bid)

  let r7_credit_expires () =
    let t = mk () in
    F.place t bid ~price:100 ~qty:10 ~displayed:(Some 100);
    ignore (F.on_trade t ~ts:0 ~taker:Feed.Sell ~price:100 ~qty:30);
    (* the book never reflected it within the ttl; a much later drop is a cancel *)
    F.on_book t ~ts:(5 * sec) (shown ~b:(Some 50) ());
    (* cancel 50 of 100 with 70 ahead -> 35 *)
    Alcotest.check view "stale credit ignored" (v 100 10 35) (F.view t bid)

  let r1_blind_then_visible () =
    let t = mk () in
    F.place t bid ~price:100 ~qty:10 ~displayed:None;
    Alcotest.check view "unknown queue starts at 0" (v 100 10 0) (F.view t bid);
    F.on_book t ~ts:0 (shown ~b:(Some 80) ());
    Alcotest.check view "first sight: everything displayed is ahead" (v 100 10 80) (F.view t bid);
    F.on_book t ~ts:1 (shown ~b:None ());
    F.on_book t ~ts:2 (shown ~b:(Some 200) ());
    Alcotest.check view "out of view and back: ahead kept" (v 100 10 80) (F.view t bid);
    F.on_book t ~ts:3 (shown ~b:None ());
    F.on_book t ~ts:4 (shown ~b:(Some 30) ());
    Alcotest.check view "back with less displayed: capped" (v 100 10 30) (F.view t bid)

  let r8_gap () =
    let t = mk () in
    F.place t bid ~price:100 ~qty:10 ~displayed:(Some 100);
    F.on_gap t;
    F.on_book t ~ts:0 (shown ~b:(Some 40) ());
    Alcotest.check view "after a gap the drop is not a pro-rata cancel, just a cap" (v 100 10 40)
      (F.view t bid)

  let replace_and_cancel () =
    let t = mk () in
    F.place t bid ~price:100 ~qty:10 ~displayed:(Some 5);
    F.place t bid ~price:101 ~qty:4 ~displayed:(Some 9);
    Alcotest.check view "replace loses queue position" (v 101 4 9) (F.view t bid);
    F.cancel t bid;
    Alcotest.check view "cancelled" None (F.view t bid);
    Alcotest.check fills "a cancelled order cannot fill" []
      (F.on_trade t ~ts:0 ~taker:Feed.Sell ~price:1 ~qty:100)

  let tests name =
    let c n fn = Alcotest.test_case n `Quick fn in
    ( "fill-rules-" ^ name,
      [
        c "R1 join the back of the queue" r1_join_back;
        c "R2 queue shrinks from trades, then partial fills" r2_queue_then_fill;
        c "R2 only the right taker side and price" r2_wrong_side_or_price;
        c "R3 trade through fills in full" r3_trade_through;
        c "ask side mirrors bid side" ask_side_mirror;
        c "R4/R6 pro-rata cancellations" r4_cancel_prorata;
        c "R4/R5 pessimistic cancellations" r4_cancel_pessimistic;
        c "R4 rounding is against us" r4_rounding_against_us;
        c "R7 trade then book is counted once" r7_trade_then_book;
        c "R7 book then trade is counted once" r7_book_then_trade;
        c "R7 book then trade, partial queue" r7_book_then_trade_partial_queue;
        c "R7 credit expires" r7_credit_expires;
        c "R1 beyond visible depth" r1_blind_then_visible;
        c "R8 data gap" r8_gap;
        c "replace and cancel" replace_and_cancel;
      ] )
end

module Rules_ref = Rules (Fill.Ref)
module Rules_opt = Rules (Fill.Opt)

(* ------------------------------------------------------------------ *)
(* Reference vs optimised on random event streams.                     *)

type ev =
  | Place of Book.side * int * int * int option
  | Cancel of Book.side
  | Level of Book.side * int * int option (* displayed qty at a price changes *)
  | Trade of Feed.taker * int * int
  | Gap
  | Tick of int (* time passes *)

let gen_ev =
  QCheck.Gen.(
    let side = map (fun b -> if b then Book.Bid else Book.Ask) bool in
    let price = int_range 98 102 in
    let qty = int_range 1 60 in
    let disp = oneof_weighted [ (1, return None); (6, map Option.some (int_range 0 120)) ] in
    oneof_weighted
      [
        (2, map3 (fun s p (q, d) -> Place (s, p, q, d)) side price (pair qty disp));
        (1, map (fun s -> Cancel s) side);
        (8, map3 (fun s p d -> Level (s, p, d)) side price disp);
        (6, map3 (fun b p q -> Trade ((if b then Feed.Buy else Feed.Sell), p, q)) bool price qty);
        (1, return Gap);
        (3, map (fun n -> Tick (n * 100 * ms)) (int_range 1 8));
      ])

let show_ev = function
  | Place (s, p, q, d) ->
      Printf.sprintf "Place(%s,%d,%d,%s)" (Book.side_to_string s) p q
        (match d with None -> "?" | Some d -> string_of_int d)
  | Cancel s -> "Cancel " ^ Book.side_to_string s
  | Level (s, p, d) ->
      Printf.sprintf "Level(%s,%d,%s)" (Book.side_to_string s) p
        (match d with None -> "?" | Some d -> string_of_int d)
  | Trade (t, p, q) -> Printf.sprintf "Trade(%s,%d,%d)" (if t = Feed.Buy then "buy" else "sell") p q
  | Gap -> "Gap"
  | Tick n -> Printf.sprintf "Tick %dms" (n / ms)

let arb_evs = QCheck.make ~print:(fun l -> String.concat "; " (List.map show_ev l)) QCheck.Gen.(list_size (int_range 0 120) gen_ev)

(* Drive one model; return everything observable after every event. *)
module Drive (F : Fill.S) = struct
  let run model evs =
    let t = F.create ~cancel_model:model ~ttl_ns:sec in
    let levels = Hashtbl.create 16 in
    let lookup side price = match Hashtbl.find_opt levels (side, price) with Some d -> d | None -> Some 0 in
    let now = ref 0 in
    List.map
      (fun e ->
        let fl =
          match e with
          | Place (s, p, q, d) ->
              F.place t s ~price:p ~qty:q ~displayed:d;
              []
          | Cancel s ->
              F.cancel t s;
              []
          | Level (s, p, d) ->
              Hashtbl.replace levels (s, p) d;
              F.on_book t ~ts:!now lookup;
              []
          | Trade (tk, p, q) -> F.on_trade t ~ts:!now ~taker:tk ~price:p ~qty:q
          | Gap ->
              F.on_gap t;
              []
          | Tick n ->
              now := !now + n;
              []
        in
        (fl, F.view t Book.Bid, F.view t Book.Ask))
      evs
end

module Drive_ref = Drive (Fill.Ref)
module Drive_opt = Drive (Fill.Opt)

let prop_models_agree model name =
  QCheck.Test.make ~name ~count:3000 arb_evs (fun evs -> Drive_ref.run model evs = Drive_opt.run model evs)

(* Invariants of the model itself, checked on the optimised version. *)
let prop_invariants =
  QCheck.Test.make ~name:"fill model: ahead >= 0, fills never exceed order size" ~count:2000 arb_evs
    (fun evs ->
      let placed = ref 0 and filled = ref 0 in
      List.iter (function Place (_, _, q, _) -> placed := !placed + q | _ -> ()) evs;
      List.for_all
        (fun (fl, vb, va) ->
          List.iter (fun (x : Fill.fill) -> filled := !filled + x.f_qty) fl;
          let ok = function None -> true | Some (v : Fill.view) -> v.v_ahead >= 0 && v.v_remaining > 0 in
          ok vb && ok va && List.for_all (fun (x : Fill.fill) -> x.f_qty > 0) fl)
        (Drive_opt.run Fill.Prorata evs)
      && !filled <= !placed)

(* ------------------------------------------------------------------ *)
(* Whole honest simulator: reference fill model vs optimised.          *)

let inst = { Feed.price_decimals = 1; qty_decimals = 8; price_increment = 1; qty_increment = 1; qty_min = 10_000 }

let strat_cfg =
  { Strategy.half_spread_cbps = 100; skew_cbps = 50; order_notional_usd = 1000; max_inventory_orders = 3; requote_ticks = 1 }

let params mid2 = Strategy.make_params strat_cfg inst ~mid2

type mev = MBook of int * (int * int) list * (int * int) list | MTrade of int * Feed.taker * int * int | MGap of int

(* Random market: a mid that walks, 5 levels a side, trades near the touch. *)
let gen_market =
  QCheck.Gen.(
    let step =
      map3
        (fun dt (move, kind) (a, b, c) -> (dt, move, kind, a, b, c))
        (int_range 1 400)
        (pair (int_range (-3) 3) (int_range 0 9))
        (triple (int_range 1 9) (int_range 1 3_000_000) (int_range 0 4))
    in
    map
      (fun steps ->
        let ts = ref 0 and mid = ref 500_000 in
        List.map
          (fun (dt, move, kind, spread, q, off) ->
            ts := !ts + (dt * ms);
            if kind < 6 then (
              mid := max 400_000 (!mid + move);
              let bids = List.init 5 (fun i -> (!mid - spread - i, q + (i * 1000))) in
              let asks = List.init 5 (fun i -> (!mid + spread + i, q + (i * 777))) in
              MBook (!ts, bids, asks))
            else if kind < 9 then
              if kind = 6 then MTrade (!ts, Feed.Sell, !mid - spread - off, q)
              else if kind = 7 then MTrade (!ts, Feed.Buy, !mid + spread + off, q)
              else MTrade (!ts, (if off mod 2 = 0 then Feed.Sell else Feed.Buy), !mid + off - 2, q)
            else MGap !ts)
          steps)
      (list_size (int_range 0 300) step))

let book_of bids asks =
  let b = Book.create ~depth:5 in
  List.iter (fun (p, q) -> Book.set b Book.Bid p q) bids;
  List.iter (fun (p, q) -> Book.set b Book.Ask p q) asks;
  b

module Run_honest (H : sig
  type t

  val create : Strategy.params -> Honest.config -> fee_ppm:int -> t
  val on_book : t -> ts:int -> Book.t -> unit
  val on_trade : t -> ts:int -> taker:Feed.taker -> price:int -> qty:int -> unit
  val on_gap : t -> ts:int -> unit
  val finish : t -> ts:int -> unit
  val acct : t -> Account.t
end) =
struct
  let run cfg evs =
    (* a tight quote (about 2-3 ticks from mid) so orders sit on, inside and
       just behind the generated levels *)
    let tight = { strat_cfg with half_spread_cbps = 5; skew_cbps = 3 } in
    let h = H.create (Strategy.make_params tight inst ~mid2:1_000_000) cfg ~fee_ppm:4000 in
    let last = ref 0 in
    List.iter
      (function
        | MBook (ts, b, a) ->
            last := ts;
            H.on_book h ~ts (book_of b a)
        | MTrade (ts, tk, p, q) ->
            last := ts;
            H.on_trade h ~ts ~taker:tk ~price:p ~qty:q
        | MGap ts ->
            last := ts;
            H.on_gap h ~ts)
      evs;
    H.finish h ~ts:(!last + (10 * sec));
    let a = H.acct h in
    (a.Account.fills, a.inv, a.cash, a.fees, a.realised, a.orders_placed, a.orders_filled, a.rejects, a.samples)
end

module RH_fast = Run_honest (struct
  include Honest.Fast

  let acct (t : t) = t.acct
end)

module RH_ref = Run_honest (struct
  include Honest.Reference

  let acct (t : t) = t.acct
end)

let prop_honest_agree model name =
  let cfg = { Honest.default_config with cancel_model = model } in
  QCheck.Test.make ~name ~count:400 (QCheck.make gen_market) (fun evs -> RH_fast.run cfg evs = RH_ref.run cfg evs)

let prop_honest_fills_something =
  (* guards against the agreement test passing vacuously *)
  QCheck.Test.make ~name:"honest simulator: random markets do produce fills" ~count:1
    (QCheck.make QCheck.Gen.(list_size (return 40) gen_market))
    (fun runs ->
      let total =
        List.fold_left
          (fun acc evs ->
            let fl, _, _, _, _, _, _, _, _ = RH_fast.run Honest.default_config evs in
            acc + List.length fl)
          0 runs
      in
      if total < 40 then QCheck.Test.fail_reportf "only %d fills across 40 random markets" total else true)

(* ------------------------------------------------------------------ *)
(* Strategy                                                            *)

let q = Alcotest.(option (pair int int))
let qo (x : Strategy.quote option) = Option.map (fun (x : Strategy.quote) -> (x.price, x.qty)) x

let test_strategy () =
  (* mid 50000.0 (price units 500000), tick 0.1, 1 bp half spread = 5.0 = 50 units *)
  let p = params 1_000_000 in
  Alcotest.(check int) "size = $1000 / 50000 = 0.02" 2_000_000 p.size;
  Alcotest.(check int) "max inventory = 3 orders" 6_000_000 p.max_inv;
  let b, a = Strategy.desired p ~best_bid:499_900 ~best_ask:500_100 ~inventory:0 in
  Alcotest.check q "flat: bid one half-spread below mid" (Some (499_950, 2_000_000)) (qo b);
  Alcotest.check q "flat: ask one half-spread above mid" (Some (500_050, 2_000_000)) (qo a);
  (* long one order: reservation shifts down by 0.5 bp = 25 units *)
  let b, a = Strategy.desired p ~best_bid:499_900 ~best_ask:500_100 ~inventory:2_000_000 in
  Alcotest.check q "long: bid skewed down" (Some (499_925, 2_000_000)) (qo b);
  Alcotest.check q "long: ask skewed down" (Some (500_025, 2_000_000)) (qo a);
  let b, a = Strategy.desired p ~best_bid:499_900 ~best_ask:500_100 ~inventory:(-2_000_000) in
  Alcotest.check q "short: bid skewed up" (Some (499_975, 2_000_000)) (qo b);
  Alcotest.check q "short: ask skewed up" (Some (500_075, 2_000_000)) (qo a);
  (* tight market: quotes are clamped so they never cross (post-only) *)
  let b, a = Strategy.desired p ~best_bid:499_999 ~best_ask:500_001 ~inventory:(-6_000_000) in
  Alcotest.check q "bid clamped below best ask" (Some (500_000, 2_000_000)) (qo b);
  Alcotest.check q "at max short: no ask" None (qo a);
  let b, _ = Strategy.desired p ~best_bid:499_900 ~best_ask:500_100 ~inventory:6_000_000 in
  Alcotest.check q "at max long: no bid" None (qo b);
  let b, _ = Strategy.desired p ~best_bid:499_900 ~best_ask:500_100 ~inventory:5_000_000 in
  Alcotest.check q "near max long: bid size shrinks to the room left" (Some (499_887, 1_000_000)) (qo b)

let prop_strategy_never_crosses =
  QCheck.Test.make ~name:"strategy: quotes never cross the book or each other" ~count:2000
    QCheck.(triple (int_range 100_000 2_000_000) (int_range 1 500) (int_range (-6_000_000) 6_000_000))
    (fun (bb, spread, inventory) ->
      let ba = bb + spread in
      let p = params (bb + ba) in
      let inventory = max (-p.max_inv) (min p.max_inv inventory) in
      match Strategy.desired p ~best_bid:bb ~best_ask:ba ~inventory with
      | Some b, Some a -> b.price < ba && a.price > bb && b.price < a.price
      | Some b, None -> b.price < ba
      | None, Some a -> a.price > bb
      | None, None -> false)

(* ------------------------------------------------------------------ *)
(* Naive model                                                         *)

let wide_book ?(bq = 1_000_000) ?(aq = 1_000_000) bb ba = book_of [ (bb, bq) ] [ (ba, aq) ]

let test_naive () =
  let n = Naive.create (params 1_000_000) ~fee_ppm:4000 in
  Naive.on_book n ~ts:0 (wide_book 499_900 500_100);
  Alcotest.(check int) "two quotes placed" 2 n.acct.orders_placed;
  Naive.on_trade n ~ts:1 ~price:499_951;
  Alcotest.(check int) "trade above the bid: no fill" 0 n.acct.n_fills;
  Naive.on_trade n ~ts:2 ~price:499_950;
  Alcotest.(check int) "trade AT the bid fills it in full, no queue" 2_000_000 n.acct.inv;
  Naive.on_trade n ~ts:3 ~price:499_000;
  Alcotest.(check int) "filled order is gone until the next requote" 1 n.acct.n_fills;
  Naive.on_book n ~ts:4 (wide_book 499_900 500_100);
  Naive.on_trade n ~ts:5 ~price:600_000;
  Alcotest.(check int) "trade THROUGH the ask fills it" 0 n.acct.inv;
  Alcotest.(check int) "two fills" 2 n.acct.n_fills;
  (* bought 0.02 at 49995.0, sold 0.02 at 50002.5 (skewed ask): +0.15 USD gross *)
  Alcotest.(check string) "realised gross" "0.15" (Sim.usd inst n.acct.realised);
  (* fees: 0.40% of 999.90 + 0.40% of 1000.05 = 3.9996 + 4.0002 *)
  Alcotest.(check string) "fees" "8.00" (Sim.usd inst n.acct.fees)

(* ------------------------------------------------------------------ *)
(* Honest simulator: latency, queue, post-only                          *)

module H = Honest.Fast

let hcfg = Honest.default_config

let test_honest_latency () =
  let h = H.create (params 1_000_000) hcfg ~fee_ppm:4000 in
  H.on_book h ~ts:0 (wide_book 499_900 500_100);
  Alcotest.check view "nothing rests before the order arrives" None (H.view h bid);
  H.on_trade h ~ts:(50 * ms) ~taker:Feed.Sell ~price:400_000 ~qty:1;
  Alcotest.(check int) "a trade during the latency window cannot fill us" 0 h.acct.n_fills;
  H.on_book h ~ts:(100 * ms) (wide_book 499_900 500_100);
  Alcotest.check view "resting after 100 ms, alone at a new level" (v 499_950 2_000_000 0) (H.view h bid);
  H.on_trade h ~ts:(150 * ms) ~taker:Feed.Sell ~price:499_950 ~qty:500_000;
  Alcotest.(check int) "partial fill" 500_000 h.acct.inv;
  Alcotest.(check int) "strategy has not heard about it yet" 0 h.strat_inv;
  H.on_book h ~ts:(250 * ms) (wide_book 499_900 500_100);
  Alcotest.(check int) "fill report arrives one latency later" 500_000 h.strat_inv

let test_honest_queue () =
  let h = H.create (params 1_000_000) hcfg ~fee_ppm:4000 in
  (* The touch is exactly where the strategy wants to quote, with 3.0 displayed. *)
  let bk = book_of [ (499_950, 300_000_000) ] [ (500_050, 300_000_000) ] in
  H.on_book h ~ts:0 bk;
  H.on_book h ~ts:(100 * ms) bk;
  Alcotest.check view "joins behind the displayed 3.0" (v 499_950 2_000_000 300_000_000) (H.view h bid);
  H.on_trade h ~ts:(200 * ms) ~taker:Feed.Sell ~price:499_950 ~qty:250_000_000;
  Alcotest.(check int) "naive would have filled; honest is still queued" 0 h.acct.n_fills;
  Alcotest.check view "queue shrank" (v 499_950 2_000_000 50_000_000) (H.view h bid);
  H.on_trade h ~ts:(300 * ms) ~taker:Feed.Sell ~price:499_950 ~qty:51_000_000;
  Alcotest.(check int) "filled only once the queue is exhausted" 1_000_000 h.acct.inv

let test_honest_cancel_replace_latency () =
  let h = H.create (params 1_000_000) hcfg ~fee_ppm:4000 in
  H.on_book h ~ts:0 (wide_book 499_900 500_100);
  H.on_book h ~ts:(200 * ms) (wide_book 499_900 500_100);
  Alcotest.check view "resting" (v 499_950 2_000_000 0) (H.view h bid);
  (* The market drops 2%; the strategy wants to pull its bid at t=300ms... *)
  H.on_book h ~ts:(300 * ms) (wide_book 489_900 490_100);
  Alcotest.check view "...but the old bid is still live while the cancel travels" (v 499_950 2_000_000 0)
    (H.view h bid);
  (* ...and is picked off by a seller before the cancel lands. *)
  H.on_trade h ~ts:(350 * ms) ~taker:Feed.Sell ~price:490_000 ~qty:10_000_000;
  Alcotest.(check int) "stale quote filled at the old, bad price" 2_000_000 h.acct.inv;
  (match h.acct.fills with
  | [ f ] ->
      Alcotest.(check int) "at our stale price" 499_950 f.price;
      Alcotest.(check bool) "which is far above the new mid" true (2 * f.price > f.mid2)
  | _ -> Alcotest.fail "expected exactly one fill");
  H.on_book h ~ts:(400 * ms) (wide_book 489_900 490_100);
  Alcotest.(check bool) "replacement now rests near the new mid" true
    (match H.view h bid with Some v -> v.v_price < 490_100 | None -> false)

let test_honest_post_only_reject () =
  let h = H.create (params 1_000_000) hcfg ~fee_ppm:4000 in
  H.on_book h ~ts:0 (wide_book 499_900 500_100);
  (* by the time the bid (499950) arrives, the market has fallen through it *)
  H.on_book h ~ts:(50 * ms) (wide_book 489_900 490_100);
  H.on_book h ~ts:(100 * ms) (wide_book 489_900 490_100);
  Alcotest.(check int) "crossing post-only order is rejected" 1 h.acct.rejects;
  Alcotest.check view "nothing rests" None (H.view h bid)

let test_honest_fees_and_pnl () =
  let h = H.create (params 1_000_000) hcfg ~fee_ppm:4000 in
  H.on_book h ~ts:0 (wide_book 499_900 500_100);
  H.on_book h ~ts:(100 * ms) (wide_book 499_900 500_100);
  H.on_trade h ~ts:(200 * ms) ~taker:Feed.Sell ~price:499_000 ~qty:1;
  (* bought 0.02 BTC at 49995.0 = 999.90 USD; fee 0.40% = 3.9996 *)
  Alcotest.(check string) "cash" "-999.90" (Sim.usd inst h.acct.cash);
  Alcotest.(check int) "fee, exact to 1e-9 USD" 3_999_600_000 h.acct.fees;
  (* marked at mid 50000: +0.10 gross, minus the fee *)
  Alcotest.(check string) "gross marked to mid" "0.10" (Sim.usd inst (Account.gross_mtm h.acct ~mid2:1_000_000));
  Alcotest.(check string) "net" "-3.90" (Sim.usd inst (Account.net_mtm h.acct ~mid2:1_000_000))

(* ------------------------------------------------------------------ *)
(* Account                                                             *)

let test_account () =
  let a = Account.create ~fee_ppm:0 in
  let fill side price qty = Account.apply_fill a ~ts:0 ~side ~price ~qty ~mid2:(2 * price) in
  fill bid 100 10;
  fill bid 110 10;
  Alcotest.(check int) "long 20" 20 a.inv;
  fill ask 120 5;
  (* avg cost 105; sold 5 at 120 -> +75 *)
  Alcotest.(check int) "realised on partial close" 75 a.realised;
  fill ask 90 25;
  (* close remaining 15 at 90: 15*(90-105) = -225; then short 10 at 90 *)
  Alcotest.(check int) "realised through zero" (-150) a.realised;
  Alcotest.(check int) "short 10" (-10) a.inv;
  fill bid 80 10;
  Alcotest.(check int) "cover short: +100" (-50) a.realised;
  Alcotest.(check int) "flat" 0 a.inv;
  Alcotest.(check int) "flat: cash equals realised" a.realised a.cash;
  (* drawdown on sampled equity *)
  let a = Account.create ~fee_ppm:0 in
  Account.apply_fill a ~ts:0 ~side:bid ~price:100 ~qty:10 ~mid2:200;
  Account.on_mid a ~ts:0 ~mid2:200;
  Account.on_mid a ~ts:sec ~mid2:220;
  Account.on_mid a ~ts:(2 * sec) ~mid2:180;
  Account.on_mid a ~ts:(3 * sec) ~mid2:210;
  Alcotest.(check int) "max drawdown from +100 to -100" 200 a.max_drawdown

(* ------------------------------------------------------------------ *)
(* Markouts through the whole Sim pipeline                              *)

let test_markouts () =
  let t = Sim.create { Sim.default_config with strat = strat_cfg } in
  let bk ts bb ba =
    Sim.on_event t (Feed.Book { symbol = "X"; ts; exch_ts = ts; book = wide_book bb ba; snapshot = false; checksum = 0 })
  in
  Sim.on_event t (Feed.Instrument { symbol = "X"; ts = 0; inst });
  bk 0 499_900 500_100;
  (* naive bid at 499950 is hit at t = 1 s *)
  Sim.on_event t
    (Feed.Trade
       { ts = sec; trade = { t_symbol = "X"; taker = Feed.Sell; price = 499_950; qty = 1; exch_ts = sec; trade_id = 1 } });
  (* mid falls 1 bp by t = 1.5 s, 3 bp by t = 5 s; data ends at t = 30 s *)
  bk (sec + (500 * ms)) 499_850 500_050;
  bk (5 * sec) 499_750 499_950;
  bk (30 * sec) 499_750 499_950;
  let r = Sim.finish t in
  match r.syms with
  | [ s ] -> (
      Alcotest.(check int) "one naive fill" 1 s.naive.n_fills;
      match s.naive.markouts with
      | [ m1; m10; m60 ] ->
          Alcotest.(check int) "1 s: measured" 1 m1.n;
          Alcotest.(check int) "1 s: mid moved 1 bp against the buy" (-100) m1.mid_move_cbps;
          Alcotest.(check int) "1 s: bought 1 bp under mid, so edge is 0" 0 m1.edge_cbps;
          Alcotest.(check int) "10 s: 3 bp adverse" (-300) m10.mid_move_cbps;
          Alcotest.(check int) "10 s: edge -2 bp" (-200) m10.edge_cbps;
          Alcotest.(check int) "60 s: not enough data after the fill, excluded" 0 m60.n
      | _ -> Alcotest.fail "expected three horizons")
  | _ -> Alcotest.fail "expected one symbol"

let () =
  let qc = List.map QCheck_alcotest.to_alcotest in
  let c n fn = Alcotest.test_case n `Quick fn in
  Alcotest.run "sim"
    [
      Rules_ref.tests "reference";
      Rules_opt.tests "optimised";
      ( "reference-vs-optimised",
        qc
          [
            prop_models_agree Fill.Prorata "fill model agrees on random streams (pro-rata)";
            prop_models_agree Fill.Pessimistic "fill model agrees on random streams (pessimistic)";
            prop_invariants;
            prop_honest_agree Fill.Prorata "whole honest simulator agrees (pro-rata)";
            prop_honest_agree Fill.Pessimistic "whole honest simulator agrees (pessimistic)";
            prop_honest_fills_something;
          ] );
      ("strategy", c "quotes, skew, clamps, limits" test_strategy :: qc [ prop_strategy_never_crosses ]);
      ("naive", [ c "fills at or through, instantly" test_naive ]);
      ( "honest",
        [
          c "order latency each way" test_honest_latency;
          c "queue position" test_honest_queue;
          c "cancel/replace pays latency" test_honest_cancel_replace_latency;
          c "post-only reject" test_honest_post_only_reject;
          c "maker fee and P&L" test_honest_fees_and_pnl;
        ] );
      ("account", [ c "average cost, realised P&L, drawdown" test_account ]);
      ("markouts", [ c "1 s / 10 s / 60 s" test_markouts ]);
    ]
