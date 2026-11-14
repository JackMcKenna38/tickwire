open Tickwire
open Tickwire_net
open Lwt.Infix
open Cmdliner

(* ---------- shared options ---------- *)

let symbols_t =
  Arg.(
    value
    & opt (list string) Live.default.symbols
    & info [ "symbols" ] ~docv:"SYMBOLS" ~doc:"Comma-separated Kraken symbols, e.g. BTC/USD,ETH/USD.")

let depth_t =
  Arg.(
    value
    & opt (enum [ ("10", 10); ("25", 25); ("100", 100); ("500", 500); ("1000", 1000) ]) Live.default.depth
    & info [ "depth" ] ~docv:"N" ~doc:"Book depth to subscribe to (10, 25, 100, 500 or 1000).")

let data_dir_t =
  Arg.(value & opt string "data" & info [ "data-dir" ] ~docv:"DIR" ~doc:"Directory for recorded session logs.")

let keep_days_t =
  Arg.(
    value & opt int 14
    & info [ "keep-days" ] ~docv:"N" ~doc:"Delete recorded logs older than N days (0 keeps everything).")

let out_t =
  Arg.(value & opt (some string) None & info [ "o"; "out" ] ~docv:"FILE" ~doc:"Also write the final statistics to FILE.")

let write_file path s =
  Twlog.mkdir_p (Filename.dirname path);
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

(* ---------- probe: raw connectivity check ---------- *)

let probe seconds =
  let run =
    Ws.connect ~host:"ws.kraken.com" ~port:443 ~path:"/v2" >>= fun ws ->
    Ws.send_text ws
      {|{"method":"subscribe","params":{"channel":"book","symbol":["BTC/USD"],"depth":10}}|}
    >>= fun () ->
    let t_end = Unix.gettimeofday () +. float_of_int seconds in
    let n = ref 0 and bytes = ref 0 in
    let rec loop () =
      if Unix.gettimeofday () >= t_end then Lwt.return_unit
      else
        Ws.recv ws >>= function
        | None -> Lwt.return_unit
        | Some m ->
            incr n;
            bytes := !bytes + String.length m;
            print_endline (if String.length m > 300 then String.sub m 0 300 ^ "..." else m);
            loop ()
    in
    loop () >>= fun () ->
    Ws.close ws >|= fun () ->
    Printf.printf "probe: %d messages, %d bytes in %d s\n%!" !n !bytes seconds
  in
  Lwt_main.run run

let probe_cmd =
  let seconds = Arg.(value & pos 0 int 60 & info [] ~docv:"SECONDS") in
  Cmd.v
    (Cmd.info "probe" ~doc:"Open a TLS websocket to Kraken and print raw book messages.")
    Term.(const probe $ seconds)

(* ---------- live capture ---------- *)

let status_line (f : Feed.t) =
  let ok, total = Feed.checksum_counts f in
  let resyncs = Feed.total (fun s -> s.resyncs) f in
  let trades = Feed.total (fun s -> s.trades) f in
  let p50 =
    List.map (fun (s : Feed.sym) -> Printf.sprintf "%s %dms" s.name (Feed.latency_pct_ms s.st 50)) f.symbols
  in
  Printf.sprintf "msgs %d | checksums %d/%d (%s) | resyncs %d | trades %d | latency p50 %s%s" f.messages
    ok total (Feed.pct_string ok total) resyncs trades (String.concat ", " p50)
    (if Feed.all_synced f then "" else " | SYNCING")

let capture ~quiet ~symbols ~depth ~minutes ~record_dir ~keep_days : Live.outcome =
  let stop = ref false in
  Sys.catch_break true;
  let cfg =
    {
      Live.default with
      symbols;
      depth;
      duration_s = Option.map (fun m -> m *. 60.0) minutes;
      record_dir;
      keep_days;
    }
  in
  let ticks = ref 0 in
  let on_tick f =
    incr ticks;
    if (not quiet) && !ticks mod 5 = 0 then Printf.printf "%s\n%!" (status_line f)
  in
  let log s = Printf.eprintf "%s\n%!" s in
  try Lwt_main.run (Live.run ~log ~on_tick ~stop cfg)
  with Sys.Break ->
    prerr_endline "interrupted";
    exit 130

let final_stats (o : Live.outcome) =
  let b = Buffer.create 512 in
  Printf.bprintf b "session %s -> %s UTC (%d s)\n"
    (Timeutil.utc_string o.started_ns) (Timeutil.utc_string o.ended_ns)
    ((o.ended_ns - o.started_ns) / Timeutil.ns_per_s);
  Buffer.add_string b (Feed.stats_lines o.feed);
  let ok, total = Feed.checksum_counts o.feed in
  Printf.bprintf b "checksum match rate: %s (%d of %d)\n" (Feed.pct_string ok total) ok total;
  List.iter (fun p -> Printf.bprintf b "recorded to %s\n" p) o.log_files;
  Buffer.contents b

let live symbols depth minutes record data_dir keep_days out =
  let o =
    capture ~quiet:false ~symbols ~depth ~minutes
      ~record_dir:(if record then Some data_dir else None)
      ~keep_days
  in
  let s = final_stats o in
  print_string s;
  Option.iter (fun p -> write_file p s) out;
  let ok, total = Feed.checksum_counts o.feed in
  if total = 0 || ok <> total then exit 2

let minutes_opt_t =
  Arg.(
    value
    & opt (some float) None
    & info [ "minutes" ] ~docv:"N" ~doc:"Stop after N minutes (default: run until Ctrl-C).")

let live_cmd =
  let record = Arg.(value & flag & info [ "record" ] ~doc:"Record every raw message to the data directory.") in
  Cmd.v
    (Cmd.info "live" ~doc:"Maintain checksum-verified live books and print verification statistics.")
    Term.(const live $ symbols_t $ depth_t $ minutes_opt_t $ record $ data_dir_t $ keep_days_t $ out_t)

(* ---------- replay ---------- *)

let replay files start out =
  let r = Replay.run ~log:prerr_endline ?start files in
  let s = Replay.summary r in
  print_string s;
  Option.iter (fun p -> write_file p s) out;
  if List.exists (fun (_, e) -> match e with Twlog.Corrupt _ -> true | _ -> false) r.ends then exit 2

let files_t = Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE" ~doc:"Recorded .twlog file(s), in order.")

let start_t =
  Arg.(value & opt (some int) None & info [ "start" ] ~docv:"BYTE" ~doc:"Start at this byte offset of the first file.")

let replay_cmd =
  Cmd.v
    (Cmd.info "replay" ~doc:"Replay a recorded log through the live code path and re-verify every checksum.")
    Term.(const replay $ files_t $ start_t $ out_t)

(* ---------- simulation options ---------- *)

(* A decimal with up to two places, held as an integer number of hundredths. *)
let hundredths =
  let parse s =
    match Fixed.of_literal ~decimals:2 s with
    | Ok v when v >= 0 -> Ok v
    | _ -> Error (`Msg (Printf.sprintf "%S is not a non-negative number with at most 2 decimals" s))
  in
  Arg.conv (parse, fun f v -> Format.pp_print_string f (Fixed.to_string ~decimals:2 v))

let sim_config_t =
  let d = Sim.default_config in
  let latency =
    Arg.(value & opt int 100 & info [ "latency-ms" ] ~docv:"MS" ~doc:"Order latency each way, in milliseconds.")
  in
  let half_spread =
    Arg.(
      value
      & opt hundredths d.strat.half_spread_cbps
      & info [ "half-spread-bps" ] ~docv:"BPS" ~doc:"Quote this many basis points either side of the mid.")
  in
  let skew =
    Arg.(
      value & opt hundredths d.strat.skew_cbps
      & info [ "skew-bps" ] ~docv:"BPS" ~doc:"Shift quotes this many basis points against inventory, per order held.")
  in
  let order_usd =
    Arg.(value & opt int d.strat.order_notional_usd & info [ "order-usd" ] ~docv:"USD" ~doc:"Size of each quote in USD.")
  in
  let max_inv =
    Arg.(
      value
      & opt int d.strat.max_inventory_orders
      & info [ "max-inventory" ] ~docv:"N" ~doc:"Maximum inventory, in multiples of the order size.")
  in
  let requote =
    Arg.(
      value & opt int d.strat.requote_ticks
      & info [ "requote-ticks" ] ~docv:"N" ~doc:"Replace a quote only when its target price moves at least N ticks.")
  in
  let cancel_model =
    Arg.(
      value
      & opt (enum [ ("prorata", Fill.Prorata); ("pessimistic", Fill.Pessimistic) ]) Fill.Prorata
      & info [ "cancel-model" ] ~docv:"MODEL"
          ~doc:"How cancellations ahead of us are assumed: $(b,prorata) or $(b,pessimistic).")
  in
  let fee_tier =
    Arg.(value & opt int 1 & info [ "fee-tier" ] ~docv:"N" ~doc:"Kraken Pro spot fee tier (1-17) for the maker fee.")
  in
  let fee_bps =
    Arg.(
      value
      & opt (some hundredths) None
      & info [ "maker-fee-bps" ] ~docv:"BPS" ~doc:"Override the maker fee, in basis points (40 = 0.40%).")
  in
  let build latency half_spread_cbps skew_cbps order_notional_usd max_inventory_orders requote_ticks cancel_model
      fee_tier fee_bps =
    let fee_ppm =
      match fee_bps with
      | Some c -> c (* hundredths of a bp = ppm *)
      | None -> (
          match Fees.maker_ppm_of_tier fee_tier with
          | Some p -> p
          | None ->
              prerr_endline "fee tier must be between 1 and 17";
              exit 124)
    in
    {
      Sim.strat =
        {
          Strategy.half_spread_cbps;
          skew_cbps;
          order_notional_usd = max 1 order_notional_usd;
          max_inventory_orders = max 1 max_inventory_orders;
          requote_ticks = max 1 requote_ticks;
        };
      honest = { Honest.default_config with latency_ns = max 0 latency * Timeutil.ns_per_ms; cancel_model };
      fee_ppm;
    }
  in
  Term.(
    const build $ latency $ half_spread $ skew $ order_usd $ max_inv $ requote $ cancel_model $ fee_tier $ fee_bps)

let reports_dir_t =
  Arg.(value & opt string "reports" & info [ "reports-dir" ] ~docv:"DIR" ~doc:"Where reports are written.")

let local_date () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday

(* Replay [files], run both simulations, write the report.  Returns false
   if the data failed verification. *)
let simulate_and_report ~cfg ~files ~start ~report_path ~title_date : bool =
  let rr, sr = Sim.run_files ?start ~log:prerr_endline cfg files in
  let html = Report.html ~meta:{ Report.title_date; files; replay_hash = rr.hash } ~replay:rr sr in
  write_file report_path html;
  let summary = Replay.summary rr ^ "\n" ^ Summary.text sr ^ "\nsim_hash=" ^ Sim.hash sr ^ "\n" in
  write_file (Filename.concat (Filename.dirname report_path) "summary.txt") summary;
  print_string summary;
  Printf.printf "report: %s\n%!" report_path;
  let ok = ref true in
  List.iter
    (fun f ->
      let good, total = Feed.checksum_counts f in
      if total = 0 || good <> total then ok := false)
    rr.feeds;
  if rr.feeds = [] || sr.syms = [] then ok := false;
  if List.exists (fun (_, e) -> match e with Twlog.Corrupt _ -> true | _ -> false) rr.ends then ok := false;
  !ok

let sim cfg files start report =
  let report_path =
    match report with Some p -> p | None -> Filename.concat (Filename.concat "reports" "adhoc") "report.html"
  in
  let title_date =
    match files with f :: _ -> Filename.remove_extension (Filename.basename f) | [] -> "replay"
  in
  if not (simulate_and_report ~cfg ~files ~start ~report_path ~title_date) then (
    prerr_endline "WARNING: the recording did not verify cleanly (see data quality).";
    exit 2)

let sim_cmd =
  let report =
    Arg.(value & opt (some string) None & info [ "report" ] ~docv:"FILE" ~doc:"Where to write the HTML report.")
  in
  Cmd.v
    (Cmd.info "sim" ~doc:"Run the naive and honest simulations over recorded log(s) and write a report.")
    Term.(const sim $ sim_config_t $ files_t $ start_t $ report)

(* ---------- nightly: capture, verify, record, simulate, report ---------- *)

let nightly cfg symbols depth minutes data_dir keep_days reports_dir =
  let date = local_date () in
  Printf.printf "tickwire nightly: capturing %g min of %s at depth %d\n%!" minutes (String.concat ", " symbols) depth;
  let o = capture ~quiet:false ~symbols ~depth ~minutes:(Some minutes) ~record_dir:(Some data_dir) ~keep_days in
  print_string (final_stats o);
  let start = Option.map snd o.log_start in
  let report_path = Filename.concat (Filename.concat reports_dir date) "report.html" in
  let ok = simulate_and_report ~cfg ~files:o.log_files ~start ~report_path ~title_date:date in
  if not ok then (
    prerr_endline "WARNING: the session did not verify cleanly (see data quality in the report).";
    exit 2)

let nightly_cmd =
  let minutes =
    Arg.(value & opt float 60.0 & info [ "minutes" ] ~docv:"N" ~doc:"Minutes of live data to capture.")
  in
  Cmd.v
    (Cmd.info "nightly"
       ~doc:"Capture live data, verify and record it, run both simulations, and write reports/YYYY-MM-DD/report.html.")
    Term.(const nightly $ sim_config_t $ symbols_t $ depth_t $ minutes $ data_dir_t $ keep_days_t $ reports_dir_t)

let () =
  let info =
    Cmd.info "tickwire" ~version:"0.1.0"
      ~doc:"Honest market-making simulator on checksum-verified Kraken order books."
  in
  exit (Cmd.eval (Cmd.group info [ nightly_cmd; live_cmd; replay_cmd; sim_cmd; probe_cmd ]))
