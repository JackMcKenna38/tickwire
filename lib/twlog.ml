(* Append-only binary session log.

   File   = magic (8 bytes) then records.
   Record = payload length  u32 LE   (the length prefix)
            kind            u8       (0 = raw message, 1 = connection start)
            receive time    i64 LE   (ns since Unix epoch)
            payload         bytes    (the raw WebSocket text, untouched)
            crc32(payload)  u32 LE

   A connection-start record carries the session configuration as JSON, so a
   file can be replayed with no other input.  Files are named by UTC day
   (YYYY-MM-DD.twlog) and old ones are deleted by [cleanup]. *)

let magic = "TWLOG\x01\r\n"
let ext = ".twlog"

type kind = Message | Connected

let kind_to_int = function Message -> 0 | Connected -> 1
let header_len = 13

let put_u32 b v =
  for i = 0 to 3 do
    Buffer.add_char b (Char.chr ((v lsr (8 * i)) land 0xFF))
  done

let put_i64 b v =
  for i = 0 to 7 do
    Buffer.add_char b (Char.chr ((v asr (8 * i)) land 0xFF))
  done

let get_u32 s off =
  Char.code s.[off]
  lor (Char.code s.[off + 1] lsl 8)
  lor (Char.code s.[off + 2] lsl 16)
  lor (Char.code s.[off + 3] lsl 24)

let get_i64 s off =
  let v = ref 0 in
  for i = 7 downto 0 do
    v := (!v lsl 8) lor Char.code s.[off + i]
  done;
  !v

let encode kind ~ts payload =
  let b = Buffer.create (String.length payload + header_len + 4) in
  put_u32 b (String.length payload);
  Buffer.add_char b (Char.chr (kind_to_int kind));
  put_i64 b ts;
  Buffer.add_string b payload;
  put_u32 b (Crc32.string payload);
  Buffer.contents b

(* ---- writing ---- *)

type writer = {
  dir : string;
  keep_days : int;
  mutable day : int;
  mutable path : string;
  mutable oc : out_channel;
  mutable pos : int;
  mutable records : int;
}

let path_for dir day_ns = Filename.concat dir (Timeutil.utc_date day_ns ^ ext)

let rec mkdir_p d =
  if d <> "" && d <> "." && not (Sys.file_exists d) then (
    mkdir_p (Filename.dirname d);
    try Sys.mkdir d 0o755 with Sys_error _ -> ())

(* Delete log files more than [keep_days] days older than [today]. *)
let cleanup ~dir ~keep_days ~today : string list =
  if keep_days <= 0 || not (Sys.file_exists dir) then []
  else
    let files = Sys.readdir dir in
    Array.sort compare files;
    Array.fold_left
      (fun acc f ->
        if Filename.check_suffix f ext then
          match Timeutil.date_to_day_index (Filename.chop_suffix f ext) with
          | Some d when today - d > keep_days ->
              (try Sys.remove (Filename.concat dir f) with Sys_error _ -> ());
              f :: acc
          | _ -> acc
        else acc)
      [] files
    |> List.rev

let open_file path =
  let existing = if Sys.file_exists path then (Unix.stat path).Unix.st_size else 0 in
  let oc = open_out_gen [ Open_wronly; Open_creat; Open_append; Open_binary ] 0o644 path in
  if existing = 0 then (
    output_string oc magic;
    (oc, String.length magic))
  else (oc, existing)

let open_writer ~dir ~keep_days ~ts : writer =
  mkdir_p dir;
  let day = Timeutil.utc_day_index ts in
  ignore (cleanup ~dir ~keep_days ~today:day);
  let path = path_for dir ts in
  let oc, pos = open_file path in
  { dir; keep_days; day; path; oc; pos; records = 0 }

(* True when [ts] falls on a later UTC day than the open file. *)
let needs_rotation w ~ts = Timeutil.utc_day_index ts <> w.day

let rotate w ~ts =
  close_out w.oc;
  let day = Timeutil.utc_day_index ts in
  ignore (cleanup ~dir:w.dir ~keep_days:w.keep_days ~today:day);
  let path = path_for w.dir ts in
  let oc, pos = open_file path in
  w.day <- day;
  w.path <- path;
  w.oc <- oc;
  w.pos <- pos

let write w kind ~ts payload =
  let r = encode kind ~ts payload in
  output_string w.oc r;
  w.pos <- w.pos + String.length r;
  w.records <- w.records + 1

let flush w = Stdlib.flush w.oc
let close w = close_out w.oc

(* ---- reading ---- *)

type read_end = Complete | Truncated_tail of int | Corrupt of int

(* Call [f kind ~ts payload] for each record from [start] (a byte offset on
   a record boundary; default just after the magic).  A partial final record
   (e.g. the writer was killed) is reported, not raised. *)
let iter ?start path (f : kind -> ts:int -> string -> unit) : read_end =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let len = in_channel_length ic in
      let m = really_input_string ic (min len (String.length magic)) in
      if m <> magic then Corrupt 0
      else (
        (match start with Some s when s > String.length magic -> seek_in ic s | _ -> ());
        let rec loop () =
          let off = pos_in ic in
          if off >= len then Complete
          else if len - off < header_len then Truncated_tail off
          else
            let h = really_input_string ic header_len in
            let plen = get_u32 h 0 in
            let k = Char.code h.[4] in
            let ts = get_i64 h 5 in
            if k > 1 then Corrupt off
            else if len - pos_in ic < plen + 4 then Truncated_tail off
            else
              let payload = really_input_string ic plen in
              let crc = get_u32 (really_input_string ic 4) 0 in
              if crc <> Crc32.string payload then Corrupt off
              else (
                f (if k = 0 then Message else Connected) ~ts payload;
                loop ())
        in
        loop ()))

(* ---- session configuration carried in Connected records ---- *)

type meta = { symbols : string list; depth : int }

let meta_to_json m =
  Printf.sprintf {|{"version":1,"symbols":[%s],"depth":%d}|}
    (String.concat "," (List.map (Printf.sprintf "%S") m.symbols))
    m.depth

let meta_of_json s : meta option =
  match Json.parse s with
  | exception Json.Parse_error _ -> None
  | j -> (
      let symbols = List.filter_map Json.to_string_opt (Json.to_list (Json.member "symbols" j)) in
      match Json.member "depth" j with
      | Json.Num d when symbols <> [] -> Option.map (fun depth -> { symbols; depth }) (int_of_string_opt d)
      | _ -> None)
