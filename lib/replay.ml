(* Replay recorded logs through the same Feed code as live capture, and
   fingerprint everything the feed produces. *)

type result = {
  feeds : Feed.t list; (* one per distinct session configuration, in order *)
  records : int;
  events : int;
  hash : string; (* hex digest of every event plus the final statistics *)
  ends : (string * Twlog.read_end) list;
  first_ts : int;
  last_ts : int;
}

(* One canonical line per event.  Any change in book state, trade, or resync
   changes the chained digest. *)
let event_line (e : Feed.event) : string =
  match e with
  | Feed.Book { symbol; ts; exch_ts; book; snapshot; checksum } ->
      let lv = function Some (p, q) -> Printf.sprintf "%d@%d" q p | None -> "-" in
      Printf.sprintf "B %d %d %s %c %d %s %s" ts exch_ts symbol
        (if snapshot then 'S' else 'U')
        checksum
        (lv (Book.best_bid book))
        (lv (Book.best_ask book))
  | Feed.Trade { ts; trade = t } ->
      Printf.sprintf "T %d %d %s %c %d %d %d" ts t.exch_ts t.t_symbol
        (match t.taker with Feed.Buy -> 'b' | Feed.Sell -> 's')
        t.price t.qty t.trade_id
  | Feed.Resync { symbol; ts; reason } -> Printf.sprintf "R %d %s %s" ts symbol reason
  | Feed.Instrument { symbol; ts; inst } ->
      Printf.sprintf "I %d %s %d %d %d %d %d" ts symbol inst.price_decimals inst.qty_decimals
        inst.price_increment inst.qty_increment inst.qty_min

let run ?(on_event = fun (_ : Feed.event) -> ()) ?(log = fun (_ : string) -> ()) ?start
    (files : string list) : result =
  let digest = ref (Digest.string "tickwire-replay-v1") in
  let events = ref 0 and records = ref 0 in
  let first_ts = ref 0 and last_ts = ref 0 in
  let feeds = ref [] in
  let current : (Twlog.meta * Feed.t) option ref = ref None in
  let handle_event e =
    incr events;
    digest := Digest.string (!digest ^ event_line e);
    on_event e
  in
  let on_record kind ~ts payload =
    if !records = 0 then first_ts := ts;
    incr records;
    last_ts := ts;
    match kind with
    | Twlog.Connected -> (
        match Twlog.meta_of_json payload with
        | None -> log "connection record without a readable configuration; skipped"
        | Some meta ->
            let feed =
              match !current with
              | Some (m, f) when m = meta -> f
              | _ ->
                  let f =
                    Feed.create ~log ~symbols:meta.symbols ~depth:meta.depth
                      ~send:(fun _ -> ())
                      ~on_event:handle_event ()
                  in
                  feeds := f :: !feeds;
                  current := Some (meta, f);
                  f
            in
            Feed.on_connected feed ~ts)
    | Twlog.Message -> (
        match !current with Some (_, f) -> Feed.on_message f ~ts payload | None -> ())
  in
  let ends =
    List.mapi
      (fun i path ->
        let start = if i = 0 then start else None in
        (path, Twlog.iter ?start path on_record))
      files
  in
  let feeds = List.rev !feeds in
  List.iter (fun f -> digest := Digest.string (!digest ^ Feed.stats_lines f)) feeds;
  {
    feeds;
    records = !records;
    events = !events;
    hash = Digest.to_hex !digest;
    ends;
    first_ts = !first_ts;
    last_ts = !last_ts;
  }

let summary (r : result) : string =
  let b = Buffer.create 512 in
  Printf.bprintf b "records=%d events=%d\n" r.records r.events;
  List.iter (fun f -> Buffer.add_string b (Feed.stats_lines f)) r.feeds;
  List.iter
    (fun (p, e) ->
      match e with
      | Twlog.Complete -> ()
      | Twlog.Truncated_tail off ->
          Printf.bprintf b "note: %s ends with a partial record at byte %d (ignored)\n" p off
      | Twlog.Corrupt off -> Printf.bprintf b "ERROR: %s is corrupt at byte %d\n" p off)
    r.ends;
  Printf.bprintf b "output_hash=%s\n" r.hash;
  Buffer.contents b
