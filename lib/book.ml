(* L2 order book: price -> quantity per side, in fixed-point integers.
   Mirrors Kraken's WebSocket v2 "book" channel rules:
   - a level with qty 0 is removed,
   - after each message the book is truncated to the subscribed depth,
   - the checksum covers the top 10 levels of each side. *)

module IntMap = Map.Make (Int)

type side = Bid | Ask

let side_to_string = function Bid -> "bid" | Ask -> "ask"

type t = {
  depth : int;
  mutable bids : int IntMap.t;
  mutable asks : int IntMap.t;
  mutable nbids : int;
  mutable nasks : int;
}

let create ~depth = { depth; bids = IntMap.empty; asks = IntMap.empty; nbids = 0; nasks = 0 }

let clear t =
  t.bids <- IntMap.empty;
  t.asks <- IntMap.empty;
  t.nbids <- 0;
  t.nasks <- 0

let copy t = { t with depth = t.depth }

(* Set the quantity at a price level; qty = 0 removes the level. *)
let set t side price qty =
  match side with
  | Bid ->
      let had = IntMap.mem price t.bids in
      if qty = 0 then (
        if had then (
          t.bids <- IntMap.remove price t.bids;
          t.nbids <- t.nbids - 1))
      else (
        t.bids <- IntMap.add price qty t.bids;
        if not had then t.nbids <- t.nbids + 1)
  | Ask ->
      let had = IntMap.mem price t.asks in
      if qty = 0 then (
        if had then (
          t.asks <- IntMap.remove price t.asks;
          t.nasks <- t.nasks - 1))
      else (
        t.asks <- IntMap.add price qty t.asks;
        if not had then t.nasks <- t.nasks + 1)

(* Drop the worst levels until each side has at most [depth] levels. *)
let truncate t =
  while t.nbids > t.depth do
    let p, _ = IntMap.min_binding t.bids in
    t.bids <- IntMap.remove p t.bids;
    t.nbids <- t.nbids - 1
  done;
  while t.nasks > t.depth do
    let p, _ = IntMap.max_binding t.asks in
    t.asks <- IntMap.remove p t.asks;
    t.nasks <- t.nasks - 1
  done

let best_bid t = IntMap.max_binding_opt t.bids
let best_ask t = IntMap.min_binding_opt t.asks

(* Best first: bids descending, asks ascending. *)
let bids t = List.of_seq (IntMap.to_rev_seq t.bids)
let asks t = List.of_seq (IntMap.to_seq t.asks)

let is_crossed t =
  match (best_bid t, best_ask t) with
  | Some (b, _), Some (a, _) -> b >= a
  | _ -> false

(* Displayed quantity at [price] on [side].
   [Some q] when the price lies inside the range we can see (q = 0 if no
   level is there); [None] when it lies beyond the worst visible level of a
   full side, where the true quantity is unknown. *)
let qty_at t side price : int option =
  match side with
  | Bid -> (
      match IntMap.find_opt price t.bids with
      | Some q -> Some q
      | None ->
          if t.nbids < t.depth then Some 0
          else
            let worst, _ = IntMap.min_binding t.bids in
            if price > worst then Some 0 else None)
  | Ask -> (
      match IntMap.find_opt price t.asks with
      | Some q -> Some q
      | None ->
          if t.nasks < t.depth then Some 0
          else
            let worst, _ = IntMap.max_binding t.asks in
            if price < worst then Some 0 else None)

(* The string Kraken hashes: top 10 asks (ascending) then top 10 bids
   (descending); each level is the price digits then the qty digits, with the
   decimal point removed and leading zeros stripped.  For a value held as an
   integer scaled to the instrument precision, that is exactly its decimal
   integer rendering. *)
let checksum_string t =
  let b = Buffer.create 512 in
  let add seq =
    let n = ref 0 in
    let rec go seq =
      if !n < 10 then
        match seq () with
        | Seq.Nil -> ()
        | Seq.Cons ((p, q), rest) ->
            Buffer.add_string b (string_of_int p);
            Buffer.add_string b (string_of_int q);
            incr n;
            go rest
    in
    go seq
  in
  add (IntMap.to_seq t.asks);
  add (IntMap.to_rev_seq t.bids);
  Buffer.contents b

let checksum t = Crc32.string (checksum_string t)
