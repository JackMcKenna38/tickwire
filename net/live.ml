(* Live capture: connect, feed every raw message to [Feed], optionally record
   it, reconnect on any failure.  This is the only place that reads the
   wall clock. *)

open Lwt.Infix
open Tickwire

type config = {
  host : string;
  port : int;
  path : string;
  symbols : string list;
  depth : int;
  duration_s : float option; (* None = until interrupted *)
  record_dir : string option;
  keep_days : int;
  idle_timeout_s : float;
}

let default =
  {
    host = "ws.kraken.com";
    port = 443;
    path = "/v2";
    symbols = [ "BTC/USD"; "ETH/USD" ];
    depth = 25;
    duration_s = None;
    record_dir = None;
    keep_days = 14;
    idle_timeout_s = 15.0;
  }

type outcome = {
  feed : Feed.t;
  log_start : (string * int) option; (* first file and byte offset of this run *)
  log_files : string list; (* every file this run wrote to *)
  started_ns : int;
  ended_ns : int;
}

exception Reconnect of string
exception Idle_timeout

(* Wall clock in ns, forced non-decreasing so recorded times never go back. *)
let last_ns = ref 0

let now_ns () =
  let t = int_of_float (Unix.gettimeofday () *. 1e9) in
  if t > !last_ns then last_ns := t;
  !last_ns

let run ?(log = prerr_endline) ?(on_event = fun (_ : Feed.event) -> ())
    ?(on_tick = fun (_ : Feed.t) -> ()) ?(stop = ref false) (cfg : config) : outcome Lwt.t =
  let started_ns = now_ns () in
  let deadline = Option.map (fun d -> Unix.gettimeofday () +. d) cfg.duration_s in
  let expired () =
    !stop || match deadline with Some d -> Unix.gettimeofday () >= d | None -> false
  in
  let outq : string Queue.t = Queue.create () in
  let reconnect_reason = ref None in
  let feed =
    Feed.create ~log
      ~request_reconnect:(fun r -> reconnect_reason := Some r)
      ~symbols:cfg.symbols ~depth:cfg.depth
      ~send:(fun s -> Queue.push s outq)
      ~on_event ()
  in
  let writer =
    Option.map (fun dir -> Twlog.open_writer ~dir ~keep_days:cfg.keep_days ~ts:started_ns) cfg.record_dir
  in
  let log_start = Option.map (fun (w : Twlog.writer) -> (w.path, w.pos)) writer in
  let log_files = ref (match writer with Some w -> [ w.path ] | None -> []) in
  let meta = Twlog.meta_to_json { Twlog.symbols = cfg.symbols; depth = cfg.depth } in
  let flush_out ws =
    let rec go () =
      match Queue.take_opt outq with
      | None -> Lwt.return_unit
      | Some s -> Ws.send_text ws s >>= go
    in
    go ()
  in
  let ticker_running = ref true in
  let rec ticker () =
    Lwt_unix.sleep 1.0 >>= fun () ->
    if !ticker_running then (
      Option.iter Twlog.flush writer;
      on_tick feed;
      ticker ())
    else Lwt.return_unit
  in
  Lwt.async ticker;
  let session () =
    Queue.clear outq;
    reconnect_reason := None;
    Lwt.pick
      [
        Ws.connect ~host:cfg.host ~port:cfg.port ~path:cfg.path;
        (Lwt_unix.sleep 20.0 >>= fun () -> Lwt.fail (Reconnect "connect timed out"));
      ]
    >>= fun ws ->
    Lwt.finalize
      (fun () ->
        let ts = now_ns () in
        Option.iter (fun w -> Twlog.write w Twlog.Connected ~ts meta) writer;
        Feed.on_connected feed ~ts;
        flush_out ws >>= fun () ->
        let rec loop () =
          if expired () then Lwt.return_unit
          else
            Lwt.pick
              [
                Ws.recv ws;
                (Lwt_unix.sleep cfg.idle_timeout_s >>= fun () -> Lwt.fail Idle_timeout);
              ]
            >>= function
            | None -> Lwt.fail (Reconnect "server closed the connection")
            | Some raw -> (
                let ts = now_ns () in
                match writer with
                | Some w when Twlog.needs_rotation w ~ts ->
                    (* New UTC day: start a new file with a fresh session so
                       every file replays on its own. *)
                    Twlog.rotate w ~ts;
                    log_files := w.path :: !log_files;
                    Lwt.fail (Reconnect "daily log rotation")
                | _ -> (
                    Option.iter (fun w -> Twlog.write w Twlog.Message ~ts raw) writer;
                    Feed.on_message feed ~ts raw;
                    flush_out ws >>= fun () ->
                    match !reconnect_reason with
                    | Some r -> Lwt.fail (Reconnect r)
                    | None -> loop ()))
        in
        loop ())
      (fun () -> Ws.close ws)
  in
  let rec supervise backoff =
    if expired () then Lwt.return_unit
    else
      Lwt.catch
        (fun () -> session () >|= fun () -> `Done)
        (fun exn ->
          let msg =
            match exn with
            | Reconnect r -> r
            | Idle_timeout -> "no data for too long"
            | e -> Printexc.to_string e
          in
          Lwt.return (`Failed msg))
      >>= function
      | `Done -> Lwt.return_unit
      | `Failed msg ->
          if expired () then Lwt.return_unit
          else (
            log (Printf.sprintf "%s connection lost (%s); reconnecting in %.0f s"
                   (Timeutil.utc_string (now_ns ())) msg backoff);
            Lwt_unix.sleep backoff >>= fun () -> supervise (Float.min 30.0 (backoff *. 2.0)))
  in
  supervise 1.0 >|= fun () ->
  ticker_running := false;
  Option.iter Twlog.close writer;
  { feed; log_start; log_files = List.rev !log_files; started_ns; ended_ns = now_ns () }
