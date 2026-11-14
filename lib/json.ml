(* A small JSON parser that keeps number literals as their exact source
   text.  Prices and quantities must never pass through a float, so [Num]
   carries the literal and callers convert it with [Fixed]. *)

type t =
  | Null
  | Bool of bool
  | Num of string
  | Str of string
  | Arr of t list
  | Obj of (string * t) list

exception Parse_error of string

let fail pos msg = raise (Parse_error (Printf.sprintf "%s at byte %d" msg pos))

let parse (s : string) : t =
  let n = String.length s in
  let pos = ref 0 in
  let peek () = if !pos < n then s.[!pos] else '\000' in
  let skip_ws () =
    while
      !pos < n
      && match s.[!pos] with ' ' | '\t' | '\n' | '\r' -> true | _ -> false
    do
      incr pos
    done
  in
  let expect c =
    if !pos < n && s.[!pos] = c then incr pos
    else fail !pos (Printf.sprintf "expected '%c'" c)
  in
  let literal word v =
    let l = String.length word in
    if !pos + l <= n && String.sub s !pos l = word then (
      pos := !pos + l;
      v)
    else fail !pos "bad literal"
  in
  let hex4 () =
    if !pos + 4 > n then fail !pos "short \\u escape";
    let v = ref 0 in
    for i = 0 to 3 do
      let c = s.[!pos + i] in
      let d =
        match c with
        | '0' .. '9' -> Char.code c - 48
        | 'a' .. 'f' -> Char.code c - 87
        | 'A' .. 'F' -> Char.code c - 55
        | _ -> fail (!pos + i) "bad hex digit"
      in
      v := (!v * 16) + d
    done;
    pos := !pos + 4;
    !v
  in
  let add_utf8 b cp =
    if cp < 0x80 then Buffer.add_char b (Char.chr cp)
    else if cp < 0x800 then (
      Buffer.add_char b (Char.chr (0xC0 lor (cp lsr 6)));
      Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3F))))
    else if cp < 0x10000 then (
      Buffer.add_char b (Char.chr (0xE0 lor (cp lsr 12)));
      Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
      Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3F))))
    else (
      Buffer.add_char b (Char.chr (0xF0 lor (cp lsr 18)));
      Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
      Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
      Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3F))))
  in
  let parse_string () =
    expect '"';
    (* Fast path: no escapes. *)
    let start = !pos in
    let rec scan i =
      if i >= n then fail i "unterminated string"
      else match s.[i] with '"' -> `Plain i | '\\' -> `Esc | _ -> scan (i + 1)
    in
    match scan start with
    | `Plain e ->
        pos := e + 1;
        String.sub s start (e - start)
    | `Esc ->
        let b = Buffer.create 32 in
        let rec go () =
          if !pos >= n then fail !pos "unterminated string";
          let c = s.[!pos] in
          incr pos;
          match c with
          | '"' -> ()
          | '\\' ->
              if !pos >= n then fail !pos "unterminated escape";
              let e = s.[!pos] in
              incr pos;
              (match e with
              | '"' -> Buffer.add_char b '"'
              | '\\' -> Buffer.add_char b '\\'
              | '/' -> Buffer.add_char b '/'
              | 'b' -> Buffer.add_char b '\b'
              | 'f' -> Buffer.add_char b '\012'
              | 'n' -> Buffer.add_char b '\n'
              | 'r' -> Buffer.add_char b '\r'
              | 't' -> Buffer.add_char b '\t'
              | 'u' ->
                  let cp = hex4 () in
                  let cp =
                    if cp >= 0xD800 && cp <= 0xDBFF
                       && !pos + 1 < n
                       && s.[!pos] = '\\'
                       && s.[!pos + 1] = 'u'
                    then (
                      pos := !pos + 2;
                      let lo = hex4 () in
                      0x10000 + ((cp - 0xD800) lsl 10) + (lo - 0xDC00))
                    else cp
                  in
                  add_utf8 b cp
              | _ -> fail !pos "bad escape");
              go ()
          | c ->
              Buffer.add_char b c;
              go ()
        in
        go ();
        Buffer.contents b
  in
  let parse_number () =
    let start = !pos in
    let digits () =
      let d0 = !pos in
      while !pos < n && match s.[!pos] with '0' .. '9' -> true | _ -> false do
        incr pos
      done;
      if !pos = d0 then fail !pos "expected digit"
    in
    if peek () = '-' then incr pos;
    digits ();
    if peek () = '.' then (
      incr pos;
      digits ());
    (match peek () with
    | 'e' | 'E' ->
        incr pos;
        (match peek () with '+' | '-' -> incr pos | _ -> ());
        digits ()
    | _ -> ());
    Num (String.sub s start (!pos - start))
  in
  let rec value () =
    skip_ws ();
    match peek () with
    | '{' ->
        incr pos;
        skip_ws ();
        if peek () = '}' then (
          incr pos;
          Obj [])
        else
          let rec members acc =
            skip_ws ();
            let k = parse_string () in
            skip_ws ();
            expect ':';
            let v = value () in
            skip_ws ();
            match peek () with
            | ',' ->
                incr pos;
                members ((k, v) :: acc)
            | '}' ->
                incr pos;
                Obj (List.rev ((k, v) :: acc))
            | _ -> fail !pos "expected ',' or '}'"
          in
          members []
    | '[' ->
        incr pos;
        skip_ws ();
        if peek () = ']' then (
          incr pos;
          Arr [])
        else
          let rec elems acc =
            let v = value () in
            skip_ws ();
            match peek () with
            | ',' ->
                incr pos;
                elems (v :: acc)
            | ']' ->
                incr pos;
                Arr (List.rev (v :: acc))
            | _ -> fail !pos "expected ',' or ']'"
          in
          elems []
    | '"' -> Str (parse_string ())
    | 't' -> literal "true" (Bool true)
    | 'f' -> literal "false" (Bool false)
    | 'n' -> literal "null" Null
    | '-' | '0' .. '9' -> parse_number ()
    | _ -> fail !pos "unexpected character"
  in
  let v = value () in
  skip_ws ();
  if !pos <> n then fail !pos "trailing data";
  v

let member k = function
  | Obj l -> ( match List.assoc_opt k l with Some v -> v | None -> Null)
  | _ -> Null

let to_string_opt = function Str s -> Some s | _ -> None
let to_list = function Arr l -> l | _ -> []
let to_bool_opt = function Bool b -> Some b | _ -> None

(* Numeric literal text of a value.  Kraken's docs show prices both as bare
   numbers and as quoted strings, so accept either. *)
let to_literal_opt = function Num s -> Some s | Str s -> Some s | _ -> None
