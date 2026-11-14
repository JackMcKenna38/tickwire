(* Minimal RFC 6455 WebSocket client over TLS (ocaml-tls + lwt).
   Text frames only, no extensions; answers pings; reassembles fragments.
   The server certificate is verified against the Windows certificate store
   through ca-certs. *)

open Lwt.Infix

type t = {
  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
  wlock : Lwt_mutex.t;
  mutable closed : bool;
}

exception Handshake_failed of string
exception Protocol_error of string

let rng = lazy (Mirage_crypto_rng_unix.use_default ())

let base64 (s : string) : string =
  let tbl = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
  let n = String.length s in
  let b = Buffer.create ((n + 2) / 3 * 4) in
  let byte i = if i < n then Char.code s.[i] else 0 in
  let i = ref 0 in
  while !i < n do
    let v = (byte !i lsl 16) lor (byte (!i + 1) lsl 8) lor byte (!i + 2) in
    Buffer.add_char b tbl.[(v lsr 18) land 63];
    Buffer.add_char b tbl.[(v lsr 12) land 63];
    Buffer.add_char b (if !i + 1 < n then tbl.[(v lsr 6) land 63] else '=');
    Buffer.add_char b (if !i + 2 < n then tbl.[v land 63] else '=');
    i := !i + 3
  done;
  Buffer.contents b

let connect ~host ~port ~path : t Lwt.t =
  Lazy.force rng;
  let authenticator =
    match Ca_certs.authenticator () with
    | Ok a -> a
    | Error (`Msg m) -> raise (Handshake_failed ("cannot load system CA certificates: " ^ m))
  in
  Tls_lwt.connect authenticator (host, port) >>= fun r ->
  (match r with
  | Ok chans -> Lwt.return chans
  | Error (`Msg m) -> Lwt.fail (Handshake_failed ("TLS: " ^ m)))
  >>= fun (ic, oc) ->
  let key = base64 (Mirage_crypto_rng.generate 16) in
  let req =
    Printf.sprintf
      "GET %s HTTP/1.1\r\nHost: %s\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: %s\r\nSec-WebSocket-Version: 13\r\nUser-Agent: tickwire\r\n\r\n"
      path host key
  in
  Lwt_io.write oc req >>= fun () ->
  Lwt_io.flush oc >>= fun () ->
  Lwt_io.read_line ic >>= fun status ->
  let ok =
    match String.split_on_char ' ' status with _ :: "101" :: _ -> true | _ -> false
  in
  if not ok then
    Lwt.catch (fun () -> Lwt_io.close ic) (fun _ -> Lwt.return_unit) >>= fun () ->
    Lwt.fail (Handshake_failed ("unexpected HTTP status: " ^ String.trim status))
  else
    let rec skip_headers () =
      Lwt_io.read_line ic >>= fun l -> if String.trim l = "" then Lwt.return_unit else skip_headers ()
    in
    skip_headers () >|= fun () -> { ic; oc; wlock = Lwt_mutex.create (); closed = false }

let send_frame t ~opcode (payload : string) : unit Lwt.t =
  let len = String.length payload in
  let b = Buffer.create (len + 14) in
  Buffer.add_char b (Char.chr (0x80 lor opcode));
  if len < 126 then Buffer.add_char b (Char.chr (0x80 lor len))
  else if len < 65536 then (
    Buffer.add_char b (Char.chr (0x80 lor 126));
    Buffer.add_char b (Char.chr (len lsr 8));
    Buffer.add_char b (Char.chr (len land 0xFF)))
  else (
    Buffer.add_char b (Char.chr (0x80 lor 127));
    for i = 7 downto 0 do
      Buffer.add_char b (Char.chr ((len lsr (8 * i)) land 0xFF))
    done);
  let mask = Mirage_crypto_rng.generate 4 in
  Buffer.add_string b mask;
  String.iteri
    (fun i c -> Buffer.add_char b (Char.chr (Char.code c lxor Char.code mask.[i land 3])))
    payload;
  Lwt_mutex.with_lock t.wlock (fun () ->
      Lwt_io.write t.oc (Buffer.contents b) >>= fun () -> Lwt_io.flush t.oc)

let send_text t s = send_frame t ~opcode:1 s

let read_exact t n =
  let buf = Bytes.create n in
  Lwt_io.read_into_exactly t.ic buf 0 n >|= fun () -> Bytes.unsafe_to_string buf

let max_message = 64 * 1024 * 1024

(* Next complete text message, or [None] when the server closes. *)
let recv t : string option Lwt.t =
  let rec frame acc =
    read_exact t 2 >>= fun h ->
    let b0 = Char.code h.[0] and b1 = Char.code h.[1] in
    let fin = b0 land 0x80 <> 0 in
    let opcode = b0 land 0x0F in
    let masked = b1 land 0x80 <> 0 in
    let len7 = b1 land 0x7F in
    (if len7 < 126 then Lwt.return len7
     else if len7 = 126 then
       read_exact t 2 >|= fun s -> (Char.code s.[0] lsl 8) lor Char.code s.[1]
     else
       read_exact t 8 >|= fun s ->
       let v = ref 0 in
       String.iter (fun c -> v := (!v lsl 8) lor Char.code c) s;
       !v)
    >>= fun len ->
    if len < 0 || len > max_message then Lwt.fail (Protocol_error "frame too large")
    else
      (if masked then read_exact t 4 >|= Option.some else Lwt.return_none) >>= fun mask ->
      (if len = 0 then Lwt.return "" else read_exact t len) >>= fun payload ->
      let payload =
        match mask with
        | None -> payload
        | Some m -> String.mapi (fun i c -> Char.chr (Char.code c lxor Char.code m.[i land 3])) payload
      in
      match opcode with
      | 0x8 ->
          t.closed <- true;
          Lwt.return_none
      | 0x9 -> send_frame t ~opcode:0xA payload >>= fun () -> frame acc
      | 0xA -> frame acc
      | 0x0 | 0x1 | 0x2 ->
          let acc = payload :: acc in
          if fin then
            Lwt.return_some (match acc with [ one ] -> one | l -> String.concat "" (List.rev l))
          else frame acc
      | _ -> Lwt.fail (Protocol_error (Printf.sprintf "unknown opcode %d" opcode))
  in
  frame []

let close t : unit Lwt.t =
  let quiet f = Lwt.catch f (fun _ -> Lwt.return_unit) in
  quiet (fun () ->
      if t.closed then Lwt.return_unit
      else Lwt.pick [ send_frame t ~opcode:0x8 ""; Lwt_unix.sleep 1.0 ])
  >>= fun () ->
  t.closed <- true;
  quiet (fun () -> Lwt.pick [ Lwt_io.close t.oc; Lwt_unix.sleep 2.0 ]) >>= fun () ->
  quiet (fun () -> Lwt.pick [ Lwt_io.close t.ic; Lwt_unix.sleep 2.0 ])
