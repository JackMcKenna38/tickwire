(* The honest fill model: what happens to one resting order per side at the
   exchange.  Two implementations of the same rules:

   - [Ref]: a plain, immutable, obviously-correct version;
   - [Opt]: the mutable, allocation-free version the simulator uses.

   test_sim.ml checks they agree on random event streams.

   Rules (for a resting bid; asks mirror them):
   R1 Join at the back: on arrival, quantity ahead = the displayed quantity
      at our price.  If our price is beyond the visible depth the quantity
      is unknown; we start at 0 and take the full displayed quantity as
      ahead the first time the level becomes visible.
   R2 A sell-taker trade AT our price consumes the queue ahead first; only
      what is left fills us (possibly partially).
   R3 A sell-taker trade BELOW our price means the whole level traded out,
      so we are filled in full.
   R4 A drop in displayed quantity that trades do not explain is a
      cancellation.  [Pessimistic]: every cancel is behind us.  [Prorata]:
      cancels are spread evenly over the level, so ahead shrinks by
      cancel * ahead / displayed (rounded in our disfavour).
   R5 Quantity ahead can never exceed the displayed quantity.
   R6 Increases in displayed quantity join behind us.
   R7 Trades and book updates arrive on separate channels in either order.
      A trade seen first leaves a credit that explains the following drop;
      a drop seen first is provisionally a cancel and is reversed when the
      trade arrives.  Both expire after [ttl_ns].
   R8 After a data gap the level is treated as unseen; when it reappears,
      ahead is capped by what is displayed. *)

type cancel_model = Pessimistic | Prorata

let cancel_model_to_string = function Pessimistic -> "pessimistic" | Prorata -> "prorata"

type side = Book.side
type fill = { f_side : side; f_price : int; f_qty : int; f_done : bool }
type view = { v_price : int; v_remaining : int; v_ahead : int }

module type S = sig
  type t

  val create : cancel_model:cancel_model -> ttl_ns:int -> t

  (* Rest a new order, replacing any existing one on that side. *)
  val place : t -> side -> price:int -> qty:int -> displayed:int option -> unit
  val cancel : t -> side -> unit

  (* The book changed; [lookup side price] is the displayed quantity. *)
  val on_book : t -> ts:int -> (side -> int -> int option) -> unit
  val on_gap : t -> unit
  val on_trade : t -> ts:int -> taker:Feed.taker -> price:int -> qty:int -> fill list
  val view : t -> side -> view option
end

(* ------------------------------------------------------------------ *)

module Ref : S = struct
  type order = {
    price : int;
    remaining : int;
    ahead : int;
    ahead_known : bool;
    seen : int option;
    credit : int;
    credit_ts : int;
    debit_c : int;
    debit_x : int;
    debit_ts : int;
  }

  type t = { model : cancel_model; ttl : int; mutable bid : order option; mutable ask : order option }

  let create ~cancel_model ~ttl_ns = { model = cancel_model; ttl = ttl_ns; bid = None; ask = None }
  let get t = function Book.Bid -> t.bid | Book.Ask -> t.ask
  let put t side o = match side with Book.Bid -> t.bid <- o | Book.Ask -> t.ask <- o

  let place t side ~price ~qty ~displayed =
    put t side
      (Some
         {
           price;
           remaining = qty;
           ahead = Option.value displayed ~default:0;
           ahead_known = displayed <> None;
           seen = displayed;
           credit = 0;
           credit_ts = 0;
           debit_c = 0;
           debit_x = 0;
           debit_ts = 0;
         })

  let cancel t side = put t side None

  let expire_credit ttl ts o = if o.credit > 0 && ts - o.credit_ts > ttl then { o with credit = 0 } else o

  let expire_debit ttl ts o =
    if o.debit_c > 0 && ts - o.debit_ts > ttl then { o with debit_c = 0; debit_x = 0 } else o

  let book_step model ttl ts (shown : int option) (o : order) : order =
    match shown with
    | None -> { o with seen = None }
    | Some d -> (
        match o.seen with
        | None ->
            let ahead = if o.ahead_known then min o.ahead d else d in
            { o with ahead; ahead_known = true; seen = Some d }
        | Some prev ->
            let o =
              if d >= prev then o
              else
                let o = expire_credit ttl ts o in
                let delta = prev - d in
                let explained = min delta o.credit in
                let o = { o with credit = o.credit - explained } in
                let cancelled = delta - explained in
                if cancelled = 0 then o
                else
                  let base = prev - explained in
                  let x =
                    match model with
                    | Pessimistic -> 0
                    | Prorata -> if base > 0 then Fixed.muldiv cancelled (min o.ahead base) base else 0
                  in
                  let o = expire_debit ttl ts o in
                  {
                    o with
                    ahead = o.ahead - x;
                    debit_c = o.debit_c + cancelled;
                    debit_x = o.debit_x + x;
                    debit_ts = ts;
                  }
            in
            { o with ahead = min o.ahead d; seen = Some d })

  let on_book t ~ts lookup =
    List.iter
      (fun side ->
        match get t side with
        | None -> ()
        | Some o -> put t side (Some (book_step t.model t.ttl ts (lookup side o.price) o)))
      [ Book.Bid; Book.Ask ]

  let on_gap t =
    List.iter
      (fun side ->
        match get t side with
        | None -> ()
        | Some o -> put t side (Some { o with seen = None; credit = 0; debit_c = 0; debit_x = 0 }))
      [ Book.Bid; Book.Ask ]

  (* Returns the new order state and the quantity filled. *)
  let trade_at ttl ts tq (o : order) : order * int =
    let o = expire_debit ttl ts o in
    let m = min tq o.debit_c in
    let o =
      if m = 0 then o
      else
        let giveback = Fixed.muldiv o.debit_x m o.debit_c in
        { o with ahead = o.ahead + giveback; debit_x = o.debit_x - giveback; debit_c = o.debit_c - m }
    in
    let o = if tq - m > 0 then { (expire_credit ttl ts o) with credit_ts = ts } else o in
    let o = { o with credit = o.credit + (tq - m) } in
    let consumed = min o.ahead tq in
    let filled = min o.remaining (tq - consumed) in
    ({ o with ahead = o.ahead - consumed; remaining = o.remaining - filled }, filled)

  let on_trade t ~ts ~taker ~price ~qty =
    let side = match taker with Feed.Sell -> Book.Bid | Feed.Buy -> Book.Ask in
    match get t side with
    | None -> []
    | Some o ->
        let through = match side with Book.Bid -> price < o.price | Book.Ask -> price > o.price in
        if through then (
          put t side None;
          [ { f_side = side; f_price = o.price; f_qty = o.remaining; f_done = true } ])
        else if price <> o.price then []
        else
          let o', filled = trade_at t.ttl ts qty o in
          let finished = o'.remaining = 0 in
          put t side (if finished then None else Some o');
          if filled > 0 then [ { f_side = side; f_price = o.price; f_qty = filled; f_done = finished } ]
          else []

  let view t side =
    Option.map (fun o -> { v_price = o.price; v_remaining = o.remaining; v_ahead = o.ahead }) (get t side)
end

(* ------------------------------------------------------------------ *)

module Opt : S = struct
  (* One slot per side (0 = bid, 1 = ask), all state in flat mutable int
     fields.  [seen] and unknown-ahead use -1 as "none". *)
  type slot = {
    mutable live : bool;
    mutable price : int;
    mutable remaining : int;
    mutable ahead : int;
    mutable ahead_known : bool;
    mutable seen : int;
    mutable credit : int;
    mutable credit_ts : int;
    mutable debit_c : int;
    mutable debit_x : int;
    mutable debit_ts : int;
  }

  type t = { prorata : bool; ttl : int; slots : slot array }

  let new_slot () =
    {
      live = false;
      price = 0;
      remaining = 0;
      ahead = 0;
      ahead_known = false;
      seen = -1;
      credit = 0;
      credit_ts = 0;
      debit_c = 0;
      debit_x = 0;
      debit_ts = 0;
    }

  let create ~cancel_model ~ttl_ns =
    { prorata = cancel_model = Prorata; ttl = ttl_ns; slots = [| new_slot (); new_slot () |] }

  let idx = function Book.Bid -> 0 | Book.Ask -> 1

  let place t side ~price ~qty ~displayed =
    let s = t.slots.(idx side) in
    s.live <- true;
    s.price <- price;
    s.remaining <- qty;
    (match displayed with
    | Some d ->
        s.ahead <- d;
        s.ahead_known <- true;
        s.seen <- d
    | None ->
        s.ahead <- 0;
        s.ahead_known <- false;
        s.seen <- -1);
    s.credit <- 0;
    s.debit_c <- 0;
    s.debit_x <- 0

  let cancel t side = t.slots.(idx side).live <- false

  let book_slot t ts side lookup =
    let s = t.slots.(idx side) in
    if s.live then
      match lookup side s.price with
      | None -> s.seen <- -1
      | Some d ->
          if s.seen < 0 then (
            if s.ahead_known then (if d < s.ahead then s.ahead <- d)
            else (
              s.ahead <- d;
              s.ahead_known <- true))
          else (
            if d < s.seen then (
              if s.credit > 0 && ts - s.credit_ts > t.ttl then s.credit <- 0;
              let delta = s.seen - d in
              let explained = if delta < s.credit then delta else s.credit in
              s.credit <- s.credit - explained;
              let cancelled = delta - explained in
              if cancelled > 0 then (
                let x =
                  if t.prorata then
                    let base = s.seen - explained in
                    if base > 0 then Fixed.muldiv cancelled (if s.ahead < base then s.ahead else base) base
                    else 0
                  else 0
                in
                if s.debit_c > 0 && ts - s.debit_ts > t.ttl then (
                  s.debit_c <- 0;
                  s.debit_x <- 0);
                s.ahead <- s.ahead - x;
                s.debit_c <- s.debit_c + cancelled;
                s.debit_x <- s.debit_x + x;
                s.debit_ts <- ts));
            if d < s.ahead then s.ahead <- d);
          s.seen <- d

  let on_book t ~ts lookup =
    book_slot t ts Book.Bid lookup;
    book_slot t ts Book.Ask lookup

  let on_gap t =
    Array.iter
      (fun s ->
        s.seen <- -1;
        s.credit <- 0;
        s.debit_c <- 0;
        s.debit_x <- 0)
      t.slots

  let on_trade t ~ts ~taker ~price ~qty =
    let side = match taker with Feed.Sell -> Book.Bid | Feed.Buy -> Book.Ask in
    let s = t.slots.(idx side) in
    if not s.live then []
    else if price = s.price then (
      if s.debit_c > 0 && ts - s.debit_ts > t.ttl then (
        s.debit_c <- 0;
        s.debit_x <- 0);
      let m = if qty < s.debit_c then qty else s.debit_c in
      if m > 0 then (
        let giveback = Fixed.muldiv s.debit_x m s.debit_c in
        s.ahead <- s.ahead + giveback;
        s.debit_x <- s.debit_x - giveback;
        s.debit_c <- s.debit_c - m);
      if qty - m > 0 then (
        if s.credit > 0 && ts - s.credit_ts > t.ttl then s.credit <- 0;
        s.credit_ts <- ts;
        s.credit <- s.credit + (qty - m));
      let consumed = if s.ahead < qty then s.ahead else qty in
      s.ahead <- s.ahead - consumed;
      let rest = qty - consumed in
      let filled = if s.remaining < rest then s.remaining else rest in
      if filled = 0 then []
      else (
        s.remaining <- s.remaining - filled;
        let finished = s.remaining = 0 in
        if finished then s.live <- false;
        [ { f_side = side; f_price = s.price; f_qty = filled; f_done = finished } ]))
    else
      let through = match side with Book.Bid -> price < s.price | Book.Ask -> price > s.price in
      if through then (
        s.live <- false;
        [ { f_side = side; f_price = s.price; f_qty = s.remaining; f_done = true } ])
      else []

  let view t side =
    let s = t.slots.(idx side) in
    if s.live then Some { v_price = s.price; v_remaining = s.remaining; v_ahead = s.ahead } else None
end
