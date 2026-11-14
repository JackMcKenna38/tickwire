(* The naive backtest: quotes change instantly, there is no queue, and an
   order is filled in full the moment any trade prints at or through its
   price.  This is the model that flatters every market-making strategy. *)

type t = {
  p : Strategy.params;
  acct : Account.t;
  mutable bid : Strategy.quote option;
  mutable ask : Strategy.quote option;
  mutable mid2 : int;
}

let create (p : Strategy.params) ~fee_ppm =
  { p; acct = Account.create ~fee_ppm; bid = None; ask = None; mid2 = 0 }

let requote t ~best_bid ~best_ask =
  let wb, wa = Strategy.desired t.p ~best_bid ~best_ask ~inventory:t.acct.inv in
  let place (q : Strategy.quote option) =
    match q with
    | Some q ->
        t.acct.orders_placed <- t.acct.orders_placed + 1;
        t.acct.quoted_qty <- t.acct.quoted_qty + q.qty
    | None -> ()
  in
  if Strategy.should_replace t.p ~current:t.bid ~want:wb then (
    t.bid <- wb;
    place wb);
  if Strategy.should_replace t.p ~current:t.ask ~want:wa then (
    t.ask <- wa;
    place wa)

let on_book t ~ts (book : Book.t) =
  match (Book.best_bid book, Book.best_ask book) with
  | Some (bb, _), Some (ba, _) ->
      t.mid2 <- bb + ba;
      Account.on_mid t.acct ~ts ~mid2:t.mid2;
      requote t ~best_bid:bb ~best_ask:ba
  | _ -> ()

let on_trade t ~ts ~price =
  (match t.bid with
  | Some q when price <= q.price && t.mid2 > 0 ->
      Account.apply_fill t.acct ~ts ~side:Book.Bid ~price:q.price ~qty:q.qty ~mid2:t.mid2;
      t.acct.orders_filled <- t.acct.orders_filled + 1;
      t.bid <- None
  | _ -> ());
  match t.ask with
  | Some q when price >= q.price && t.mid2 > 0 ->
      Account.apply_fill t.acct ~ts ~side:Book.Ask ~price:q.price ~qty:q.qty ~mid2:t.mid2;
      t.acct.orders_filled <- t.acct.orders_filled + 1;
      t.ask <- None
  | _ -> ()

(* A data gap: quotes were set against a book we can no longer trust. *)
let on_gap t =
  t.bid <- None;
  t.ask <- None

let finish t ~ts = Account.finish t.acct ~ts
