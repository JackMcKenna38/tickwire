(* The honest simulation: the same strategy, but orders live at the exchange
   under the [Fill] rules and every message to or from the exchange takes
   [latency_ns].

   Timeline of one requote decided at time t:
     t          strategy sends cancel/replace for a side (that side is "busy")
     t + L      it arrives: the old order is cancelled (it could be filled
                right up to this moment) and the new one joins the back of
                the queue of the book as it stands at t + L; a post-only
                order that would cross is rejected
     t + 2L     the acknowledgement is back; the side may be requoted again
   Fills at time f reach the strategy at f + L, so it quotes on a stale
   view of its own inventory. *)

type config = { latency_ns : int; cancel_model : Fill.cancel_model; ttl_ns : int }

let default_config =
  { latency_ns = 100 * Timeutil.ns_per_ms; cancel_model = Fill.Prorata; ttl_ns = Timeutil.ns_per_s }

module Make (F : Fill.S) = struct
  type action =
    | Arrive of { side : Book.side; id : int; quote : Strategy.quote option }
    | Unbusy of Book.side
    | Notify_fill of { side : Book.side; id : int; qty : int; finished : bool }
    | Notify_reject of { side : Book.side; id : int }

  type scheduled = { at : int; seq : int; action : action }

  type side_state = {
    mutable intent : Strategy.quote option; (* what the strategy believes it has working *)
    mutable intent_id : int;
    mutable busy : bool;
    mutable live_id : int; (* id of the order resting at the exchange *)
    mutable counted_fill_id : int;
  }

  type t = {
    p : Strategy.params;
    cfg : config;
    acct : Account.t;
    fm : F.t;
    mutable queue : scheduled list; (* sorted by (at, seq) *)
    mutable seq : int;
    mutable next_id : int;
    mutable book : Book.t option; (* our copy of the book as of the last event *)
    mutable mid2 : int;
    mutable strat_inv : int; (* inventory as the strategy knows it *)
    bid : side_state;
    ask : side_state;
  }

  let new_side () = { intent = None; intent_id = 0; busy = false; live_id = 0; counted_fill_id = 0 }

  let create (p : Strategy.params) (cfg : config) ~fee_ppm =
    {
      p;
      cfg;
      acct = Account.create ~fee_ppm;
      fm = F.create ~cancel_model:cfg.cancel_model ~ttl_ns:cfg.ttl_ns;
      queue = [];
      seq = 0;
      next_id = 0;
      book = None;
      mid2 = 0;
      strat_inv = 0;
      bid = new_side ();
      ask = new_side ();
    }

  let st t = function Book.Bid -> t.bid | Book.Ask -> t.ask

  let schedule t ~at action =
    t.seq <- t.seq + 1;
    let e = { at; seq = t.seq; action } in
    let rec ins = function
      | [] -> [ e ]
      | x :: rest as l -> if x.at > e.at then e :: l else x :: ins rest
    in
    t.queue <- ins t.queue

  let run_action t ~now = function
    | Arrive { side; id; quote } -> (
        let s = st t side in
        F.cancel t.fm side;
        s.live_id <- 0;
        match quote with
        | None -> ()
        | Some q ->
            let crosses, displayed =
              match t.book with
              | None -> (false, None)
              | Some b ->
                  let crosses =
                    match side with
                    | Book.Bid -> ( match Book.best_ask b with Some (a, _) -> q.price >= a | None -> false)
                    | Book.Ask -> ( match Book.best_bid b with Some (bb, _) -> q.price <= bb | None -> false)
                  in
                  (crosses, Book.qty_at b side q.price)
            in
            if crosses then (
              t.acct.rejects <- t.acct.rejects + 1;
              schedule t ~at:(now + t.cfg.latency_ns) (Notify_reject { side; id }))
            else (
              F.place t.fm side ~price:q.price ~qty:q.qty ~displayed;
              s.live_id <- id;
              t.acct.orders_placed <- t.acct.orders_placed + 1;
              t.acct.quoted_qty <- t.acct.quoted_qty + q.qty))
    | Unbusy side -> (st t side).busy <- false
    | Notify_fill { side; id; qty; finished } ->
        let s = st t side in
        t.strat_inv <- (match side with Book.Bid -> t.strat_inv + qty | Book.Ask -> t.strat_inv - qty);
        if finished && s.intent_id = id then s.intent <- None
    | Notify_reject { side; id } ->
        let s = st t side in
        if s.intent_id = id then s.intent <- None

  (* Run everything scheduled at or before [ts], in time order. *)
  let rec advance t ~ts =
    match t.queue with
    | e :: rest when e.at <= ts ->
        t.queue <- rest;
        run_action t ~now:e.at e.action;
        advance t ~ts
    | _ -> ()

  let decide t ~ts ~best_bid ~best_ask =
    let wb, wa = Strategy.desired t.p ~best_bid ~best_ask ~inventory:t.strat_inv in
    let one side want =
      let s = st t side in
      if (not s.busy) && Strategy.should_replace t.p ~current:s.intent ~want then (
        t.next_id <- t.next_id + 1;
        s.intent <- want;
        s.intent_id <- t.next_id;
        s.busy <- true;
        schedule t ~at:(ts + t.cfg.latency_ns) (Arrive { side; id = t.next_id; quote = want });
        schedule t ~at:(ts + (2 * t.cfg.latency_ns)) (Unbusy side))
    in
    one Book.Bid wb;
    one Book.Ask wa

  let on_book t ~ts (book : Book.t) =
    advance t ~ts;
    F.on_book t.fm ~ts (fun side price -> Book.qty_at book side price);
    t.book <- Some (Book.copy book);
    match (Book.best_bid book, Book.best_ask book) with
    | Some (bb, _), Some (ba, _) ->
        t.mid2 <- bb + ba;
        Account.on_mid t.acct ~ts ~mid2:t.mid2;
        decide t ~ts ~best_bid:bb ~best_ask:ba
    | _ -> ()

  let on_trade t ~ts ~taker ~price ~qty =
    advance t ~ts;
    if t.mid2 > 0 then
      List.iter
        (fun (f : Fill.fill) ->
          let s = st t f.f_side in
          Account.apply_fill t.acct ~ts ~side:f.f_side ~price:f.f_price ~qty:f.f_qty ~mid2:t.mid2;
          if s.counted_fill_id <> s.live_id then (
            s.counted_fill_id <- s.live_id;
            t.acct.orders_filled <- t.acct.orders_filled + 1);
          schedule t ~at:(ts + t.cfg.latency_ns)
            (Notify_fill { side = f.f_side; id = s.live_id; qty = f.f_qty; finished = f.f_done });
          if f.f_done then s.live_id <- 0)
        (F.on_trade t.fm ~ts ~taker ~price ~qty)

  (* The book became untrustworthy until the next snapshot. *)
  let on_gap t ~ts =
    advance t ~ts;
    F.on_gap t.fm;
    t.book <- None

  let finish t ~ts =
    advance t ~ts;
    Account.finish t.acct ~ts

  let view t side = F.view t.fm side
end

module Fast = Make (Fill.Opt)
module Reference = Make (Fill.Ref)
