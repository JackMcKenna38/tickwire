(* The feed state machine.  It consumes raw Kraken WebSocket v2 messages and
   maintains verified books.  It performs no I/O: outgoing requests go
   through the [send] callback and results through [on_event].  Live capture
   and replay drive exactly this code; replay simply discards [send]. *)

type taker = Buy | Sell

type instrument = {
  price_decimals : int;
  qty_decimals : int;
  price_increment : int; (* in price units *)
  qty_increment : int; (* in qty units *)
  qty_min : int; (* in qty units *)
}

type trade = {
  t_symbol : string;
  taker : taker;
  price : int;
  qty : int;
  exch_ts : int;
  trade_id : int;
}

type event =
  | Book of { symbol : string; ts : int; exch_ts : int; book : Book.t; snapshot : bool; checksum : int }
  | Trade of { ts : int; trade : trade }
  | Resync of { symbol : string; ts : int; reason : string }
  | Instrument of { symbol : string; ts : int; inst : instrument }

(* Latency histogram: 1 ms buckets up to 5 s. *)
let lat_buckets = 5001

type sym_stats = {
  mutable snapshots : int;
  mutable updates : int;
  mutable checksum_ok : int;
  mutable checksum_bad : int;
  mutable resyncs : int;
  mutable ignored_unsynced : int;
  mutable trades : int;
  mutable inexact_trade_px : int;
  mutable lat_n : int;
  mutable lat_sum_ns : int;
  mutable lat_min_ns : int;
  mutable lat_max_ns : int;
  lat_hist : int array;
}

type sym = {
  name : string;
  mutable inst : instrument option;
  book : Book.t;
  mutable synced : bool;
  mutable awaiting_unsub_since : int option;
  mutable unsynced_since : int option;
  st : sym_stats;
}

type t = {
  symbols : sym list;
  depth : int;
  send : string -> unit;
  on_event : event -> unit;
  log : string -> unit;
  request_reconnect : string -> unit;
  mutable subscribed : bool;
  mutable req_id : int;
  mutable messages : int;
  mutable heartbeats : int;
  mutable connects : int;
  mutable errors : int;
}

let new_stats () =
  {
    snapshots = 0;
    updates = 0;
    checksum_ok = 0;
    checksum_bad = 0;
    resyncs = 0;
    ignored_unsynced = 0;
    trades = 0;
    inexact_trade_px = 0;
    lat_n = 0;
    lat_sum_ns = 0;
    lat_min_ns = max_int;
    lat_max_ns = min_int;
    lat_hist = Array.make lat_buckets 0;
  }

let create ?(log = fun _ -> ()) ?(request_reconnect = fun _ -> ()) ~symbols ~depth
    ~send ~on_event () =
  {
    symbols =
      List.map
        (fun name ->
          {
            name;
            inst = None;
            book = Book.create ~depth;
            synced = false;
            awaiting_unsub_since = None;
            unsynced_since = None;
            st = new_stats ();
          })
        symbols;
    depth;
    send;
    on_event;
    log;
    request_reconnect;
    subscribed = false;
    req_id = 0;
    messages = 0;
    heartbeats = 0;
    connects = 0;
    errors = 0;
  }

let find t name = List.find_opt (fun s -> s.name = name) t.symbols

let next_req t =
  t.req_id <- t.req_id + 1;
  t.req_id

let json_strings l = "[" ^ String.concat "," (List.map (Printf.sprintf "%S") l) ^ "]"

let sub_instrument t =
  Printf.sprintf
    {|{"method":"subscribe","params":{"channel":"instrument","snapshot":true},"req_id":%d}|}
    (next_req t)

let unsub_instrument t =
  Printf.sprintf {|{"method":"unsubscribe","params":{"channel":"instrument"},"req_id":%d}|}
    (next_req t)

let sub_book t syms =
  Printf.sprintf
    {|{"method":"subscribe","params":{"channel":"book","symbol":%s,"depth":%d,"snapshot":true},"req_id":%d}|}
    (json_strings syms) t.depth (next_req t)

let unsub_book t syms =
  Printf.sprintf
    {|{"method":"unsubscribe","params":{"channel":"book","symbol":%s,"depth":%d},"req_id":%d}|}
    (json_strings syms) t.depth (next_req t)

let sub_trade t syms =
  Printf.sprintf
    {|{"method":"subscribe","params":{"channel":"trade","symbol":%s,"snapshot":false},"req_id":%d}|}
    (json_strings syms) (next_req t)

let record_latency st ~recv_ts ~exch_ts =
  let d = recv_ts - exch_ts in
  st.lat_n <- st.lat_n + 1;
  st.lat_sum_ns <- st.lat_sum_ns + d;
  if d < st.lat_min_ns then st.lat_min_ns <- d;
  if d > st.lat_max_ns then st.lat_max_ns <- d;
  let ms = d / Timeutil.ns_per_ms in
  let b = if ms < 0 then 0 else if ms >= lat_buckets then lat_buckets - 1 else ms in
  st.lat_hist.(b) <- st.lat_hist.(b) + 1

(* Latency percentile in ms from the histogram (pct in 0..100). *)
let latency_pct_ms st pct =
  if st.lat_n = 0 then 0
  else
    let target = ((st.lat_n * pct) + 99) / 100 in
    let acc = ref 0 and res = ref (lat_buckets - 1) and found = ref false in
    Array.iteri
      (fun i c ->
        if not !found then (
          acc := !acc + c;
          if !acc >= target then (
            res := i;
            found := true)))
      st.lat_hist;
    !res

(* A new connection: forget all book state and start the handshake. *)
let on_connected t ~ts =
  t.connects <- t.connects + 1;
  t.subscribed <- false;
  List.iter
    (fun s ->
      if s.synced then (
        s.st.resyncs <- s.st.resyncs + 1;
        t.on_event (Resync { symbol = s.name; ts; reason = "reconnect" }));
      s.synced <- false;
      s.awaiting_unsub_since <- None;
      s.unsynced_since <- Some ts;
      Book.clear s.book)
    t.symbols;
  t.send (sub_instrument t)

let resync t s ~ts reason =
  t.log (Printf.sprintf "%s resync %s: %s" (Timeutil.utc_string ts) s.name reason);
  s.synced <- false;
  s.st.resyncs <- s.st.resyncs + 1;
  s.awaiting_unsub_since <- Some ts;
  s.unsynced_since <- Some ts;
  Book.clear s.book;
  t.on_event (Resync { symbol = s.name; ts; reason });
  t.send (unsub_book t [ s.name ])

exception Bad of string

let lit j name =
  match Json.to_literal_opt (Json.member name j) with
  | Some s -> s
  | None -> raise (Bad ("missing " ^ name))

let fixed ~decimals j name =
  match Fixed.of_literal ~decimals (lit j name) with
  | Ok v -> v
  | Error e -> raise (Bad (name ^ ": " ^ Fixed.error_to_string e ^ " (" ^ lit j name ^ ")"))

let apply_levels s inst side j =
  List.iter
    (fun lv ->
      let p = fixed ~decimals:inst.price_decimals lv "price" in
      let q = fixed ~decimals:inst.qty_decimals lv "qty" in
      if p <= 0 || q < 0 then raise (Bad "non-positive price or negative qty");
      Book.set s.book side p q)
    (Json.to_list j)

let exch_ts_of j =
  match Json.member "timestamp" j with
  | Json.Str s -> Timeutil.parse_rfc3339 s
  | _ -> None

let handle_book t ~ts ~is_snapshot data =
  List.iter
    (fun d ->
      match Json.to_string_opt (Json.member "symbol" d) with
      | None -> t.errors <- t.errors + 1
      | Some name -> (
          match find t name with
          | None -> ()
          | Some s -> (
              match s.inst with
              | None -> s.st.ignored_unsynced <- s.st.ignored_unsynced + 1
              | Some inst ->
                  if (not is_snapshot) && not s.synced then
                    s.st.ignored_unsynced <- s.st.ignored_unsynced + 1
                  else (
                    try
                      if is_snapshot then (
                        Book.clear s.book;
                        s.st.snapshots <- s.st.snapshots + 1)
                      else s.st.updates <- s.st.updates + 1;
                      apply_levels s inst Book.Ask (Json.member "asks" d);
                      apply_levels s inst Book.Bid (Json.member "bids" d);
                      Book.truncate s.book;
                      let expected =
                        match int_of_string_opt (lit d "checksum") with
                        | Some c -> c
                        | None -> raise (Bad "bad checksum field")
                      in
                      let actual = Book.checksum s.book in
                      if actual <> expected then (
                        s.st.checksum_bad <- s.st.checksum_bad + 1;
                        resync t s ~ts
                          (Printf.sprintf "checksum mismatch: local %d, exchange %d" actual
                             expected))
                      else (
                        s.st.checksum_ok <- s.st.checksum_ok + 1;
                        if is_snapshot then (
                          s.synced <- true;
                          s.awaiting_unsub_since <- None;
                          s.unsynced_since <- None);
                        let exch_ts =
                          match exch_ts_of d with
                          | Some e ->
                              if not is_snapshot then record_latency s.st ~recv_ts:ts ~exch_ts:e;
                              e
                          | None -> ts
                        in
                        t.on_event
                          (Book
                             {
                               symbol = name;
                               ts;
                               exch_ts;
                               book = s.book;
                               snapshot = is_snapshot;
                               checksum = actual;
                             }))
                    with Bad msg ->
                      t.errors <- t.errors + 1;
                      s.st.checksum_bad <- s.st.checksum_bad + 1;
                      resync t s ~ts ("bad book message: " ^ msg)))))
    data

let handle_trades t ~ts data =
  List.iter
    (fun d ->
      match Json.to_string_opt (Json.member "symbol" d) with
      | None -> t.errors <- t.errors + 1
      | Some name -> (
          match find t name with
          | None -> ()
          | Some s -> (
              match s.inst with
              | None -> ()
              | Some inst -> (
                  try
                    let taker =
                      match Json.member "side" d with
                      | Json.Str "buy" -> Buy
                      | Json.Str "sell" -> Sell
                      | _ -> raise (Bad "bad side")
                    in
                    (* The trade price is documented as an average price, so
                       it may not sit on a tick; truncate and count it. *)
                    let plit = lit d "price" in
                    let price =
                      match Fixed.of_literal ~decimals:inst.price_decimals plit with
                      | Ok p -> p
                      | Error Fixed.Inexact -> (
                          s.st.inexact_trade_px <- s.st.inexact_trade_px + 1;
                          match Fixed.of_literal_trunc ~decimals:inst.price_decimals plit with
                          | Ok p -> p
                          | Error _ -> raise (Bad "bad trade price"))
                      | Error _ -> raise (Bad "bad trade price")
                    in
                    let qty = fixed ~decimals:inst.qty_decimals d "qty" in
                    let exch_ts = match exch_ts_of d with Some e -> e | None -> ts in
                    let trade_id =
                      match Json.member "trade_id" d with
                      | Json.Num l -> ( match int_of_string_opt l with Some i -> i | None -> 0)
                      | _ -> 0
                    in
                    s.st.trades <- s.st.trades + 1;
                    t.on_event
                      (Trade { ts; trade = { t_symbol = name; taker; price; qty; exch_ts; trade_id } })
                  with Bad msg ->
                    t.errors <- t.errors + 1;
                    t.log ("bad trade message: " ^ msg)))))
    data

let handle_instrument t ~ts data =
  List.iter
    (fun p ->
      match Json.to_string_opt (Json.member "symbol" p) with
      | None -> ()
      | Some name -> (
          match find t name with
          | None -> ()
          | Some s -> (
              try
                let int_field n =
                  match Json.member n p with
                  | Json.Num l -> (
                      match int_of_string_opt l with
                      | Some i when i >= 0 && i <= 18 -> i
                      | _ -> raise (Bad ("bad " ^ n)))
                  | _ -> raise (Bad ("missing " ^ n))
                in
                let price_decimals = int_field "price_precision" in
                let qty_decimals = int_field "qty_precision" in
                let inst =
                  {
                    price_decimals;
                    qty_decimals;
                    price_increment = fixed ~decimals:price_decimals p "price_increment";
                    qty_increment = fixed ~decimals:qty_decimals p "qty_increment";
                    qty_min = fixed ~decimals:qty_decimals p "qty_min";
                  }
                in
                let changed = s.inst <> Some inst in
                let had = s.inst <> None in
                s.inst <- Some inst;
                if changed then t.on_event (Instrument { symbol = name; ts; inst });
                if changed && had && s.synced then resync t s ~ts "instrument precision changed"
              with Bad msg ->
                t.errors <- t.errors + 1;
                t.log ("bad instrument entry for " ^ name ^ ": " ^ msg))))
    (Json.to_list (Json.member "pairs" data));
  if not t.subscribed then
    if List.for_all (fun s -> s.inst <> None) t.symbols then (
      t.subscribed <- true;
      let names = List.map (fun s -> s.name) t.symbols in
      t.send (sub_book t names);
      t.send (sub_trade t names);
      (* Instrument updates are not needed once precisions are known. *)
      t.send (unsub_instrument t))
    else
      t.log
        ("instrument snapshot is missing: "
        ^ String.concat ", "
            (List.filter_map (fun s -> if s.inst = None then Some s.name else None) t.symbols))

let handle_ack t ~ts j meth =
  let ok = Json.to_bool_opt (Json.member "success" j) = Some true in
  let result = Json.member "result" j in
  let channel = Json.to_string_opt (Json.member "channel" result) in
  let symbol = Json.to_string_opt (Json.member "symbol" result) in
  if not ok then (
    t.errors <- t.errors + 1;
    t.log
      (Printf.sprintf "%s %s failed: %s" (Timeutil.utc_string ts) meth
         (Option.value ~default:"?" (Json.to_string_opt (Json.member "error" j)))))
  else if meth = "unsubscribe" && channel = Some "book" then
    match Option.bind symbol (find t) with
    | Some s when s.awaiting_unsub_since <> None ->
        s.awaiting_unsub_since <- None;
        t.send (sub_book t [ s.name ])
    | _ -> ()

(* Timeouts, driven only by message receive times so replay is identical. *)
let unsub_timeout_ns = 5 * Timeutil.ns_per_s
let unsynced_timeout_ns = 20 * Timeutil.ns_per_s

let check_timeouts t ~ts =
  List.iter
    (fun s ->
      (match s.awaiting_unsub_since with
      | Some since when ts - since > unsub_timeout_ns ->
          s.awaiting_unsub_since <- None;
          t.send (sub_book t [ s.name ])
      | _ -> ());
      match s.unsynced_since with
      | Some since when t.subscribed && ts - since > unsynced_timeout_ns ->
          s.unsynced_since <- Some ts;
          t.request_reconnect (s.name ^ " has no valid book for 20 s")
      | _ -> ())
    t.symbols

let on_message t ~ts (raw : string) =
  t.messages <- t.messages + 1;
  (match Json.parse raw with
  | exception Json.Parse_error msg ->
      t.errors <- t.errors + 1;
      t.log ("unparseable message: " ^ msg)
  | j -> (
      match Json.member "channel" j with
      | Json.Str "heartbeat" -> t.heartbeats <- t.heartbeats + 1
      | Json.Str "book" ->
          let is_snapshot = Json.member "type" j = Json.Str "snapshot" in
          handle_book t ~ts ~is_snapshot (Json.to_list (Json.member "data" j))
      | Json.Str "trade" ->
          if Json.member "type" j = Json.Str "update" then
            handle_trades t ~ts (Json.to_list (Json.member "data" j))
      | Json.Str "instrument" -> handle_instrument t ~ts (Json.member "data" j)
      | Json.Str _ -> ()
      | _ -> (
          match Json.member "method" j with
          | Json.Str (("subscribe" | "unsubscribe") as m) -> handle_ack t ~ts j m
          | _ -> ())));
  check_timeouts t ~ts

(* ---- summaries ---- *)

let total f t = List.fold_left (fun a s -> a + f s.st) 0 t.symbols

let all_synced t = List.for_all (fun s -> s.synced) t.symbols

(* Checksum match rate as an exact "ok/total" pair. *)
let checksum_counts t = (total (fun s -> s.checksum_ok) t, total (fun s -> s.checksum_ok + s.checksum_bad) t)

let pct_string ok total =
  if total = 0 then "n/a"
  else if ok = total then "100%"
  else
    (* floor to 4 decimals so anything short of perfect never prints 100% *)
    let bp = Fixed.muldiv ok 1_000_000 total in
    Printf.sprintf "%d.%04d%%" (bp / 10000) (bp mod 10000)

let stats_lines t =
  let b = Buffer.create 512 in
  Printf.bprintf b "messages=%d heartbeats=%d connects=%d errors=%d\n" t.messages t.heartbeats
    t.connects t.errors;
  List.iter
    (fun s ->
      let st = s.st in
      let tot = st.checksum_ok + st.checksum_bad in
      Printf.bprintf b
        "%s snapshots=%d updates=%d checksum_ok=%d checksum_bad=%d match=%s resyncs=%d trades=%d ignored=%d inexact_trade_px=%d"
        s.name st.snapshots st.updates st.checksum_ok st.checksum_bad
        (pct_string st.checksum_ok tot) st.resyncs st.trades st.ignored_unsynced
        st.inexact_trade_px;
      if st.lat_n > 0 then
        Printf.bprintf b " latency_ms(min=%d p50=%d p99=%d max=%d mean=%d)"
          (st.lat_min_ns / Timeutil.ns_per_ms)
          (latency_pct_ms st 50) (latency_pct_ms st 99)
          (st.lat_max_ns / Timeutil.ns_per_ms)
          (st.lat_sum_ns / st.lat_n / Timeutil.ns_per_ms);
      Buffer.add_char b '\n')
    t.symbols;
  Buffer.contents b
