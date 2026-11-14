open Tickwire

let fixture = "../fixtures/btcusd_depth10_90s.twlog"

(* Pinned results for the committed fixture.  If the feed handling, the
   fill model or the strategy defaults change, these change, and that must
   be a deliberate decision (update docs/DESIGN.md and these values). *)
let fixture_replay_hash = "9a42f87a383b79e97ab08452501e9089"
let fixture_sim_hash = "df3242dafcec5dc0262e08d627f35376"

let tmp_dir name =
  let d = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "tickwire_test_%s_%d" name (Unix.getpid ())) in
  Twlog.mkdir_p d;
  (* process ids are reused, so always start from an empty directory *)
  Array.iter (fun f -> try Sys.remove (Filename.concat d f) with Sys_error _ -> ()) (Sys.readdir d);
  d

let read_all ?start path =
  let acc = ref [] in
  let e = Twlog.iter ?start path (fun k ~ts p -> acc := (k, ts, p) :: !acc) in
  (List.rev !acc, e)

let day = Timeutil.ns_per_day

let test_roundtrip () =
  let dir = tmp_dir "rt" in
  let t0 = 20_000 * day in
  let w = Twlog.open_writer ~dir ~keep_days:14 ~ts:t0 in
  let recs =
    [
      (Twlog.Connected, t0, Twlog.meta_to_json { Twlog.symbols = [ "BTC/USD"; "ETH/USD" ]; depth = 25 });
      (Twlog.Message, t0 + 1, "{\"channel\":\"heartbeat\"}");
      (Twlog.Message, t0 + 2, "");
      (Twlog.Message, t0 + 3, String.make 100_000 'x' ^ "\x00\xff\n");
    ]
  in
  List.iter (fun (k, ts, p) -> Twlog.write w k ~ts p) recs;
  let mid_pos = w.pos in
  Twlog.write w Twlog.Message ~ts:(t0 + 4) "tail";
  Twlog.close w;
  let got, e = read_all w.path in
  Alcotest.(check bool) "complete" true (e = Twlog.Complete);
  Alcotest.(check bool) "records identical" true (got = recs @ [ (Twlog.Message, t0 + 4, "tail") ]);
  let got, _ = read_all ~start:mid_pos w.path in
  Alcotest.(check bool) "start offset" true (got = [ (Twlog.Message, t0 + 4, "tail") ]);
  Alcotest.(check string) "file named by UTC day" "2024-10-04.twlog" (Filename.basename w.path);
  (* append-only: reopening adds to the same file *)
  let w2 = Twlog.open_writer ~dir ~keep_days:14 ~ts:(t0 + 10) in
  Twlog.write w2 Twlog.Message ~ts:(t0 + 10) "again";
  Twlog.close w2;
  let got, _ = read_all w.path in
  Alcotest.(check int) "appended, nothing overwritten" 6 (List.length got);
  (* a writer killed mid-record leaves a partial tail: reported, not fatal *)
  let full = In_channel.with_open_bin w.path In_channel.input_all in
  let cut = Filename.concat dir "cut.bin" in
  Out_channel.with_open_bin cut (fun oc -> output_string oc (String.sub full 0 (String.length full - 3)));
  let got, e = read_all cut in
  Alcotest.(check int) "all whole records still read" 5 (List.length got);
  Alcotest.(check bool) "partial tail reported" true (match e with Twlog.Truncated_tail _ -> true | _ -> false);
  (* a flipped payload byte is caught by the per-record CRC *)
  let bad = Bytes.of_string full in
  Bytes.set bad (String.length Twlog.magic + Twlog.header_len + 5) 'Z';
  let badp = Filename.concat dir "bad.bin" in
  Out_channel.with_open_bin badp (fun oc -> output_bytes oc bad);
  let _, e = read_all badp in
  Alcotest.(check bool) "corruption detected" true (match e with Twlog.Corrupt _ -> true | _ -> false)

let test_rotation_and_retention () =
  let dir = tmp_dir "rot" in
  let t0 = (20_000 * day) + (23 * 3600 * Timeutil.ns_per_s) in
  (* stale files from long ago, plus an unrelated file that must survive *)
  let touch name = Out_channel.with_open_bin (Filename.concat dir name) (fun oc -> output_string oc Twlog.magic) in
  touch "2024-09-01.twlog";
  touch "2024-09-19.twlog";
  touch "2024-09-20.twlog";
  touch "notes.txt";
  let w = Twlog.open_writer ~dir ~keep_days:14 ~ts:t0 in
  let exists n = Sys.file_exists (Filename.concat dir n) in
  Alcotest.(check bool) "33 days old: deleted" false (exists "2024-09-01.twlog");
  Alcotest.(check bool) "15 days old: deleted" false (exists "2024-09-19.twlog");
  Alcotest.(check bool) "14 days old: kept" true (exists "2024-09-20.twlog");
  Alcotest.(check bool) "other files untouched" true (exists "notes.txt");
  Twlog.write w Twlog.Message ~ts:t0 "day one";
  Alcotest.(check bool) "same day: no rotation" false (Twlog.needs_rotation w ~ts:(t0 + 1));
  let t1 = t0 + (2 * 3600 * Timeutil.ns_per_s) in
  Alcotest.(check bool) "past midnight UTC: rotate" true (Twlog.needs_rotation w ~ts:t1);
  Twlog.rotate w ~ts:t1;
  Twlog.write w Twlog.Message ~ts:t1 "day two";
  Twlog.close w;
  Alcotest.(check bool) "day one file" true (exists "2024-10-04.twlog");
  Alcotest.(check bool) "day two file" true (exists "2024-10-05.twlog");
  Alcotest.(check bool) "rotation re-runs retention" false (exists "2024-09-20.twlog");
  let got, _ = read_all (Filename.concat dir "2024-10-05.twlog") in
  Alcotest.(check bool) "day two holds only day two" true (got = [ (Twlog.Message, t1, "day two") ])

(* ---- the committed fixture ---- *)

let test_fixture_replay () =
  let r = Replay.run [ fixture ] in
  Alcotest.(check bool) "log read to the end" true (List.for_all (fun (_, e) -> e = Twlog.Complete) r.ends);
  match r.feeds with
  | [ f ] ->
      let ok, total = Feed.checksum_counts f in
      Alcotest.(check bool) "fixture has thousands of checksummed updates" true (total > 2000);
      Alcotest.(check int) "replay reproduces every checksum match" total ok;
      Alcotest.(check int) "no resyncs" 0 (Feed.total (fun s -> s.resyncs) f);
      Alcotest.(check int) "no errors" 0 f.errors;
      Alcotest.(check bool) "has trades" true (Feed.total (fun s -> s.trades) f > 10);
      Alcotest.(check string) "pinned output hash" fixture_replay_hash r.hash
  | l -> Alcotest.failf "expected one session, got %d" (List.length l)

let test_fixture_replay_deterministic () =
  let a = Replay.run [ fixture ] and b = Replay.run [ fixture ] in
  Alcotest.(check string) "two replays, identical output hash" a.hash b.hash;
  Alcotest.(check string) "identical summaries" (Replay.summary a) (Replay.summary b)

(* Live and replay share one code path: re-recording the fixture's records
   through a writer and replaying the copy gives the same result. *)
let test_rerecord () =
  let dir = tmp_dir "rerec" in
  let recs, _ = read_all fixture in
  let _, ts0, _ = List.hd recs in
  let w = Twlog.open_writer ~dir ~keep_days:0 ~ts:ts0 in
  List.iter (fun (k, ts, p) -> Twlog.write w k ~ts p) recs;
  Twlog.close w;
  Alcotest.(check string) "same hash from the copy" fixture_replay_hash (Replay.run [ w.path ]).hash

(* A corrupted update must be caught by the checksum and trigger a resync. *)
let test_fixture_tamper () =
  let recs, _ = read_all fixture in
  let dir = tmp_dir "tamper" in
  let _, ts0, _ = List.hd recs in
  let w = Twlog.open_writer ~dir ~keep_days:0 ~ts:ts0 in
  let n = ref 0 and dropped = ref false in
  List.iter
    (fun (k, ts, p) ->
      let is_update = k = Twlog.Message && String.length p > 40 && String.sub p 0 33 = {|{"channel":"book","type":"update"|} in
      if is_update then incr n;
      (* silently lose the 500th..520th updates, as a network gap would *)
      if is_update && !n >= 500 && !n <= 520 then dropped := true else Twlog.write w k ~ts p)
    recs;
  Twlog.close w;
  Alcotest.(check bool) "tampering happened" true !dropped;
  let r = Replay.run [ w.path ] in
  let f = List.hd r.feeds in
  let ok, total = Feed.checksum_counts f in
  Alcotest.(check bool) "the gap is detected by a checksum mismatch" true (total - ok >= 1);
  Alcotest.(check bool) "and triggers a resync" true (Feed.total (fun s -> s.resyncs) f >= 1);
  Alcotest.(check bool) "hash differs from the clean run" true (r.hash <> fixture_replay_hash)

let test_fixture_sim () =
  let _, a = Sim.run_files Sim.default_config [ fixture ] in
  let _, b = Sim.run_files Sim.default_config [ fixture ] in
  Alcotest.(check string) "two simulations, byte-identical output" (Sim.dump a) (Sim.dump b);
  (match a.syms with
  | [ s ] ->
      Alcotest.(check string) "symbol" "BTC/USD" s.symbol;
      Alcotest.(check bool) "naive placed orders" true (s.naive.orders_placed > 0);
      Alcotest.(check bool) "honest placed orders" true (s.honest.orders_placed > 0);
      Alcotest.(check bool) "equity sampled every second" true (Array.length s.honest.samples > 60)
  | _ -> Alcotest.fail "expected one symbol");
  Alcotest.(check string) "pinned simulation hash" fixture_sim_hash (Sim.hash a)

let count_sub hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i acc = if i + n > h then acc else go (i + 1) (if String.sub hay i n = needle then acc + 1 else acc) in
  go 0 0

let test_fixture_report () =
  let make () =
    let rr, sr = Sim.run_files Sim.default_config [ fixture ] in
    Report.html ~meta:{ Report.title_date = "2026-11-14"; files = [ "fixture" ]; replay_hash = rr.hash } ~replay:rr sr
  in
  let a = make () in
  Alcotest.(check string) "report is byte-identical across runs" (Digest.to_hex (Digest.string a))
    (Digest.to_hex (Digest.string (make ())));
  Alcotest.(check bool) "self-contained: no external resources" true
    (count_sub a "http://" = 0 && count_sub a "https://" = 0 && count_sub a "<script" = 0 && count_sub a "<link" = 0);
  (* one markout chart, one inventory chart, one P&L chart for the one symbol *)
  Alcotest.(check int) "three inline SVG charts" 3 (count_sub a "<svg ");
  Alcotest.(check int) "every SVG closed" 3 (count_sub a "</svg>");
  Alcotest.(check bool) "no NaN or infinity leaked into the charts" true
    (List.for_all
       (fun bad -> count_sub a bad = 0)
       [ "=\"nan"; "=\"inf"; "=\"-inf"; ">nan<"; ">inf<"; ">-inf<"; "Mnan"; "Lnan"; "Hnan"; "Vnan"; " nan"; "Minf"; "Linf"; "Hinf"; "Vinf"; "M-inf"; "L-inf"; "H-inf"; "V-inf" ]);
  List.iter
    (fun needle -> Alcotest.(check bool) ("contains: " ^ needle) true (count_sub a needle > 0))
    [ "Naive says"; "Honest says"; "Markouts"; "Inventory path"; "Data quality"; "100%"; "BTC/USD"; "Assumptions" ]

let () =
  let c n f = Alcotest.test_case n `Quick f in
  Alcotest.run "replay"
    [
      ("log", [ c "write/read round trip, append, truncation, corruption" test_roundtrip; c "daily rotation and retention" test_rotation_and_retention ]);
      ( "fixture",
        [
          c "replay reproduces every checksum match" test_fixture_replay;
          c "two replays give identical hashes" test_fixture_replay_deterministic;
          c "re-recorded copy replays identically" test_rerecord;
          c "dropped updates are detected and resynced" test_fixture_tamper;
          c "simulation is deterministic" test_fixture_sim;
          c "report is deterministic, self-contained and well-formed" test_fixture_report;
        ] );
    ]
