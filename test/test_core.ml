open Tickwire

(* ---- Kraken's documented checksum example ----
   https://docs.kraken.com/exchange/guides/websockets/book-checksum-v2
   BTC/USD snapshot, expected checksum 3310070434. *)

let doc_bids =
  [
    ("45283.5", "0.10000000");
    ("45283.4", "1.54582015");
    ("45282.1", "0.10000000");
    ("45281.0", "0.10000000");
    ("45280.3", "1.54592586");
    ("45279.0", "0.07990000");
    ("45277.6", "0.03310103");
    ("45277.5", "0.30000000");
    ("45277.3", "1.54602737");
    ("45276.6", "0.15445238");
  ]

let doc_asks =
  [
    ("45285.2", "0.00100000");
    ("45286.4", "1.54571953");
    ("45286.6", "1.54571109");
    ("45289.6", "1.54560911");
    ("45290.2", "0.15890660");
    ("45291.8", "1.54553491");
    ("45294.7", "0.04454749");
    ("45296.1", "0.35380000");
    ("45297.5", "0.09945542");
    ("45299.5", "0.18772827");
  ]

let doc_checksum = 3310070434

let fx ~decimals s =
  match Fixed.of_literal ~decimals s with
  | Ok v -> v
  | Error e -> Alcotest.failf "parse %s: %s" s (Fixed.error_to_string e)

let doc_book () =
  let b = Book.create ~depth:10 in
  List.iter (fun (p, q) -> Book.set b Book.Bid (fx ~decimals:1 p) (fx ~decimals:8 q)) doc_bids;
  List.iter (fun (p, q) -> Book.set b Book.Ask (fx ~decimals:1 p) (fx ~decimals:8 q)) doc_asks;
  b

let test_doc_checksum () =
  let b = doc_book () in
  let s = Book.checksum_string b in
  Alcotest.(check string)
    "first ask level formats as in the guide" "452852100000" (String.sub s 0 12);
  Alcotest.(check int) "documented checksum" doc_checksum (Book.checksum b)

(* The same example driven through the whole feed as raw JSON, with numbers
   as bare JSON literals (as on the live feed) and as quoted strings (as in
   the guide). *)
let doc_snapshot_json ~quoted =
  let lv (p, q) =
    if quoted then Printf.sprintf {|{"price":"%s","qty":"%s"}|} p q
    else Printf.sprintf {|{"price":%s,"qty":%s}|} p q
  in
  Printf.sprintf
    {|{"channel":"book","type":"snapshot","data":[{"symbol":"BTC/USD","bids":[%s],"asks":[%s],"checksum":%d,"timestamp":"2023-10-06T17:35:55.440295Z"}]}|}
    (String.concat "," (List.map lv doc_bids))
    (String.concat "," (List.map lv doc_asks))
    doc_checksum

let instrument_json =
  {|{"channel":"instrument","type":"snapshot","data":{"assets":[],"pairs":[{"symbol":"BTC/USD","base":"BTC","quote":"USD","status":"online","qty_precision":8,"qty_increment":1e-08,"price_precision":1,"cost_precision":5,"cost_min":0.5,"price_increment":0.1,"qty_min":0.0001,"tick_size":0.1}]}}|}

let mk_feed ?(depth = 10) () =
  let sent = ref [] and events = ref [] in
  let f =
    Feed.create ~symbols:[ "BTC/USD" ] ~depth
      ~send:(fun s -> sent := s :: !sent)
      ~on_event:(fun e -> events := e :: !events)
      ()
  in
  (f, sent, events)

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let test_feed_doc_example quoted () =
  let f, sent, events = mk_feed () in
  Feed.on_connected f ~ts:0;
  Alcotest.(check bool) "asks for instruments first" true (contains (List.hd !sent) "instrument");
  Feed.on_message f ~ts:1 instrument_json;
  Alcotest.(check bool)
    "subscribes to book" true
    (List.exists (fun s -> contains s {|"channel":"book"|} && contains s {|"depth":10|}) !sent);
  Feed.on_message f ~ts:2 (doc_snapshot_json ~quoted);
  let ok, total = Feed.checksum_counts f in
  Alcotest.(check (pair int int)) "one checksum, matched" (1, 1) (ok, total);
  Alcotest.(check bool) "synced" true (Feed.all_synced f);
  match !events with
  | Feed.Book { checksum; snapshot = true; _ } :: _ ->
      Alcotest.(check int) "event carries checksum" doc_checksum checksum
  | _ -> Alcotest.fail "expected a Book snapshot event"

let test_feed_instrument () =
  let f, _, events = mk_feed () in
  Feed.on_connected f ~ts:0;
  Feed.on_message f ~ts:1 instrument_json;
  match List.rev !events with
  | [ Feed.Instrument { inst; _ } ] ->
      Alcotest.(check int) "price decimals" 1 inst.price_decimals;
      Alcotest.(check int) "qty decimals" 8 inst.qty_decimals;
      Alcotest.(check int) "tick = 0.1" 1 inst.price_increment;
      Alcotest.(check int) "qty increment = 1e-08" 1 inst.qty_increment;
      Alcotest.(check int) "qty min = 0.0001" 10_000 inst.qty_min
  | _ -> Alcotest.fail "expected one Instrument event"

(* An update whose checksum we compute from an independently built book. *)
let test_feed_update_and_mismatch () =
  let f, sent, events = mk_feed () in
  Feed.on_connected f ~ts:0;
  Feed.on_message f ~ts:1 instrument_json;
  Feed.on_message f ~ts:2 (doc_snapshot_json ~quoted:false);
  (* Remove the best bid, add a new worse bid, change the best ask qty. *)
  let b = doc_book () in
  Book.set b Book.Bid (fx ~decimals:1 "45283.5") 0;
  Book.set b Book.Bid (fx ~decimals:1 "45270.0") (fx ~decimals:8 "2.5");
  Book.set b Book.Ask (fx ~decimals:1 "45285.2") (fx ~decimals:8 "0.5");
  Book.truncate b;
  let update checksum =
    Printf.sprintf
      {|{"channel":"book","type":"update","data":[{"symbol":"BTC/USD","bids":[{"price":45283.5,"qty":0.00000000},{"price":45270.0,"qty":2.50000000}],"asks":[{"price":45285.2,"qty":0.50000000}],"checksum":%d,"timestamp":"2023-10-06T17:35:56.000000Z"}]}|}
      checksum
  in
  Feed.on_message f ~ts:3 (update (Book.checksum b));
  Alcotest.(check (pair int int)) "update verified" (2, 2) (Feed.checksum_counts f);
  (match !events with
  | Feed.Book { book; snapshot = false; _ } :: _ ->
      Alcotest.(check (option (pair int int)))
        "best bid moved down"
        (Some (fx ~decimals:1 "45283.4", fx ~decimals:8 "1.54582015"))
        (Book.best_bid book)
  | _ -> Alcotest.fail "expected a Book update event");
  (* Now a wrong checksum: must log, drop sync and unsubscribe. *)
  sent := [];
  Feed.on_message f ~ts:4 (update 12345);
  Alcotest.(check (pair int int)) "mismatch counted" (2, 3) (Feed.checksum_counts f);
  Alcotest.(check bool) "no longer synced" false (Feed.all_synced f);
  Alcotest.(check bool)
    "unsubscribe sent" true
    (List.exists (fun s -> contains s {|"method":"unsubscribe"|}) !sent);
  (* Updates while unsynced are ignored, not applied. *)
  Feed.on_message f ~ts:5 (update (Book.checksum b));
  Alcotest.(check (pair int int)) "ignored while unsynced" (2, 3) (Feed.checksum_counts f);
  (* The unsubscribe ack triggers a fresh subscribe; the snapshot restores sync. *)
  sent := [];
  Feed.on_message f ~ts:6
    {|{"method":"unsubscribe","result":{"channel":"book","depth":10,"symbol":"BTC/USD"},"success":true,"time_in":"2023-10-06T17:35:56.0Z","time_out":"2023-10-06T17:35:56.0Z"}|};
  Alcotest.(check bool)
    "resubscribe sent" true
    (List.exists (fun s -> contains s {|"method":"subscribe"|}) !sent);
  Feed.on_message f ~ts:7 (doc_snapshot_json ~quoted:false);
  Alcotest.(check bool) "synced again" true (Feed.all_synced f);
  Alcotest.(check (pair int int)) "snapshot verified" (3, 4) (Feed.checksum_counts f)

let test_feed_reconnect_resets () =
  let f, _, _ = mk_feed () in
  Feed.on_connected f ~ts:0;
  Feed.on_message f ~ts:1 instrument_json;
  Feed.on_message f ~ts:2 (doc_snapshot_json ~quoted:false);
  Feed.on_connected f ~ts:3;
  Alcotest.(check bool) "unsynced after reconnect" false (Feed.all_synced f)

let test_trade_parse () =
  let f, _, events = mk_feed () in
  Feed.on_connected f ~ts:0;
  Feed.on_message f ~ts:1 instrument_json;
  Feed.on_message f ~ts:2
    {|{"channel":"trade","type":"update","data":[{"symbol":"BTC/USD","side":"sell","price":45283.5,"qty":0.00100000,"ord_type":"market","trade_id":4665906,"timestamp":"2023-09-25T07:49:37.708706Z"}]}|};
  match !events with
  | Feed.Trade { trade; _ } :: _ ->
      Alcotest.(check int) "price" 452835 trade.price;
      Alcotest.(check int) "qty" 100_000 trade.qty;
      Alcotest.(check bool) "taker sold" true (trade.taker = Feed.Sell);
      Alcotest.(check int) "trade id" 4665906 trade.trade_id
  | _ -> Alcotest.fail "expected a Trade event"

(* ---- fixed point ---- *)

let res = Alcotest.(result int (of_pp (fun f e -> Format.pp_print_string f (Fixed.error_to_string e))))

let test_fixed () =
  let p = Fixed.of_literal in
  Alcotest.check res "plain" (Ok 452835) (p ~decimals:1 "45283.5");
  Alcotest.check res "trailing zeros kept exact" (Ok 10_000_000) (p ~decimals:8 "0.10000000");
  Alcotest.check res "fewer decimals than precision" (Ok 109839475580) (p ~decimals:8 "1098.3947558");
  Alcotest.check res "integer literal" (Ok 4500) (p ~decimals:2 "45");
  Alcotest.check res "exponent" (Ok 1) (p ~decimals:8 "1e-08");
  Alcotest.check res "exponent with mantissa" (Ok 25) (p ~decimals:8 "2.5E-7");
  Alcotest.check res "positive exponent" (Ok 1_250_000) (p ~decimals:2 "1.25e4");
  Alcotest.check res "negative" (Ok (-150)) (p ~decimals:2 "-1.5");
  Alcotest.check res "zero" (Ok 0) (p ~decimals:8 "0.00000000");
  Alcotest.check res "extra zero digits are exact" (Ok 15) (p ~decimals:1 "1.5000");
  Alcotest.check res "too precise" (Error Fixed.Inexact) (p ~decimals:1 "1.25");
  Alcotest.check res "truncate" (Ok 12) (Fixed.of_literal_trunc ~decimals:1 "1.25");
  Alcotest.check res "malformed" (Error Fixed.Malformed) (p ~decimals:1 "1.2.3");
  Alcotest.check res "empty" (Error Fixed.Malformed) (p ~decimals:1 "");
  Alcotest.check res "overflow" (Error Fixed.Overflow) (p ~decimals:8 "99999999999999999999");
  Alcotest.(check string) "render" "-12.34" (Fixed.to_string ~decimals:2 (-1234));
  Alcotest.(check string) "render small" "0.00000001" (Fixed.to_string ~decimals:8 1);
  Alcotest.(check int) "rescale up" 12300 (Fixed.rescale ~from_decimals:1 ~to_decimals:3 123);
  Alcotest.(check int) "rescale down rounds" 13 (Fixed.rescale ~from_decimals:2 ~to_decimals:1 125)

let prop_fixed_roundtrip =
  QCheck.Test.make ~name:"fixed: to_string/of_literal round-trip" ~count:2000
    QCheck.(pair (int_range 0 12) (int_range (-1_000_000_000_000) 1_000_000_000_000))
    (fun (decimals, v) ->
      Fixed.of_literal ~decimals (Fixed.to_string ~decimals v) = Ok v)

let prop_muldiv =
  QCheck.Test.make ~name:"fixed: muldiv = floor(a*b/c) on small values" ~count:5000
    QCheck.(triple (int_range 0 1_000_000_000) (int_range 0 1_000_000_000) (int_range 1 1_000_000_000))
    (fun (a, b, c) -> Fixed.muldiv a b c = a * b / c)

let test_muldiv_large () =
  (* (10^15 * 10^15) / 10^15 overflows a naive product. *)
  let e15 = 1_000_000_000_000_000 in
  Alcotest.(check int) "large exact" e15 (Fixed.muldiv e15 e15 e15);
  Alcotest.(check int) "large floor" (e15 / 3) (Fixed.muldiv e15 (e15 / 3) e15);
  Alcotest.(check int) "a > c" (7 * e15) (Fixed.muldiv (7 * e15) e15 e15)

(* ---- CRC32, JSON, time ---- *)

let test_crc32 () =
  Alcotest.(check int) "standard check value" 0xCBF43926 (Crc32.string "123456789");
  Alcotest.(check int) "empty" 0 (Crc32.string "")

let test_json () =
  let j = Json.parse {| {"a": [1, 2.50, -3e-8, "x\né"], "b": {"c": null, "d": true}} |} in
  (match Json.member "a" j with
  | Json.Arr [ Json.Num a; Json.Num b; Json.Num c; Json.Str s ] ->
      Alcotest.(check string) "int literal" "1" a;
      Alcotest.(check string) "trailing zero preserved" "2.50" b;
      Alcotest.(check string) "exponent literal preserved" "-3e-8" c;
      Alcotest.(check string) "escapes" "x\n\xc3\xa9" s
  | _ -> Alcotest.fail "bad array");
  Alcotest.(check bool) "nested" true (Json.member "d" (Json.member "b" j) = Json.Bool true);
  Alcotest.check_raises "trailing data" (Json.Parse_error "trailing data at byte 3") (fun () ->
      ignore (Json.parse "{} x"))

let test_time () =
  Alcotest.(check (option int)) "epoch" (Some 0) (Timeutil.parse_rfc3339 "1970-01-01T00:00:00Z");
  Alcotest.(check (option int))
    "doc timestamp"
    (Some ((1696613755 * 1_000_000_000) + 440_295_000))
    (Timeutil.parse_rfc3339 "2023-10-06T17:35:55.440295Z");
  Alcotest.(check (option int))
    "offset" (Some 0)
    (Timeutil.parse_rfc3339 "1970-01-01T01:00:00+01:00");
  Alcotest.(check string) "utc date" "2023-10-06" (Timeutil.utc_date (1696613755 * 1_000_000_000));
  Alcotest.(check (option int))
    "date index round trip"
    (Some (Timeutil.utc_day_index (1696613755 * 1_000_000_000)))
    (Timeutil.date_to_day_index "2023-10-06")

(* ---- book properties ---- *)

(* Valid data: bids strictly below a pivot, asks at or above it. *)
let gen_ops =
  QCheck.Gen.(
    list_size (int_range 0 200)
      (map3
         (fun is_bid off qty -> (is_bid, off, qty))
         bool (int_range 1 60)
         (oneof_weighted [ (1, return 0); (4, int_range 1 1_000_000) ])))

let apply_ops ~depth ops =
  let pivot = 100_000 in
  let b = Book.create ~depth in
  List.iter
    (fun (is_bid, off, qty) ->
      if is_bid then Book.set b Book.Bid (pivot - off) qty else Book.set b Book.Ask (pivot + off - 1) qty;
      Book.truncate b)
    ops;
  b

let rec strictly cmp = function
  | a :: (b :: _ as rest) -> cmp a b && strictly cmp rest
  | _ -> true

let prop_book name check =
  QCheck.Test.make ~name ~count:500
    (QCheck.make ~print:(fun l -> string_of_int (List.length l) ^ " ops") gen_ops)
    (fun ops -> check (apply_ops ~depth:10 ops))

let prop_never_crossed = prop_book "book: never crossed after valid data" (fun b -> not (Book.is_crossed b))

let prop_sorted =
  prop_book "book: levels sorted best-first" (fun b ->
      strictly (fun (p1, _) (p2, _) -> p1 > p2) (Book.bids b)
      && strictly (fun (p1, _) (p2, _) -> p1 < p2) (Book.asks b))

let prop_no_zero =
  prop_book "book: zero-qty levels removed" (fun b ->
      List.for_all (fun (_, q) -> q > 0) (Book.bids b)
      && List.for_all (fun (_, q) -> q > 0) (Book.asks b))

let prop_depth =
  prop_book "book: truncated to depth, keeping the best levels" (fun b ->
      List.length (Book.bids b) <= 10 && List.length (Book.asks b) <= 10)

(* Model check: the book equals a naive assoc-list model. *)
let prop_model =
  QCheck.Test.make ~name:"book: agrees with a naive model" ~count:500
    (QCheck.make gen_ops) (fun ops ->
      let depth = 5 in
      let b = apply_ops ~depth ops in
      let pivot = 100_000 in
      let take n l = List.filteri (fun i _ -> i < n) l in
      let model =
        List.fold_left
          (fun (bids, asks) (is_bid, off, qty) ->
            let upd l p = if qty = 0 then List.remove_assoc p l else (p, qty) :: List.remove_assoc p l in
            if is_bid then
              (take depth (List.sort (fun (a, _) (b, _) -> compare b a) (upd bids (pivot - off))), asks)
            else (bids, take depth (List.sort compare (upd asks (pivot + off - 1)))))
          ([], []) ops
      in
      (Book.bids b, Book.asks b) = model)

let test_qty_at () =
  let b = Book.create ~depth:3 in
  Book.set b Book.Bid 100 5;
  Book.set b Book.Bid 98 7;
  Alcotest.(check (option int)) "level" (Some 5) (Book.qty_at b Book.Bid 100);
  Alcotest.(check (option int)) "gap inside" (Some 0) (Book.qty_at b Book.Bid 99);
  Alcotest.(check (option int)) "side not full: all visible" (Some 0) (Book.qty_at b Book.Bid 50);
  Book.set b Book.Bid 97 1;
  Alcotest.(check (option int)) "beyond a full side is unknown" None (Book.qty_at b Book.Bid 96);
  Alcotest.(check (option int)) "improving price is visible" (Some 0) (Book.qty_at b Book.Bid 101)

let () =
  let q = List.map QCheck_alcotest.to_alcotest in
  Alcotest.run "core"
    [
      ( "kraken-doc-example",
        [
          Alcotest.test_case "book checksum example = 3310070434" `Quick test_doc_checksum;
          Alcotest.test_case "through the feed, bare numbers" `Quick (test_feed_doc_example false);
          Alcotest.test_case "through the feed, quoted numbers" `Quick (test_feed_doc_example true);
        ] );
      ( "feed",
        [
          Alcotest.test_case "instrument precision" `Quick test_feed_instrument;
          Alcotest.test_case "update, mismatch, resync" `Quick test_feed_update_and_mismatch;
          Alcotest.test_case "reconnect resets" `Quick test_feed_reconnect_resets;
          Alcotest.test_case "trade parsing" `Quick test_trade_parse;
        ] );
      ( "fixed",
        [
          Alcotest.test_case "literals" `Quick test_fixed;
          Alcotest.test_case "muldiv large" `Quick test_muldiv_large;
        ]
        @ q [ prop_fixed_roundtrip; prop_muldiv ] );
      ( "primitives",
        [
          Alcotest.test_case "crc32" `Quick test_crc32;
          Alcotest.test_case "json" `Quick test_json;
          Alcotest.test_case "time" `Quick test_time;
        ] );
      ( "book",
        Alcotest.test_case "qty_at visibility" `Quick test_qty_at
        :: q [ prop_never_crossed; prop_sorted; prop_no_zero; prop_depth; prop_model ] );
    ]
