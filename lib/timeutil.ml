(* Time helpers.  All timestamps are int nanoseconds since the Unix epoch. *)

let ns_per_s = 1_000_000_000
let ns_per_ms = 1_000_000
let ns_per_day = 86_400 * ns_per_s

(* Days since 1970-01-01 for a proleptic Gregorian date (Howard Hinnant's
   days_from_civil). *)
let days_from_civil y m d =
  let y = if m <= 2 then y - 1 else y in
  let era = (if y >= 0 then y else y - 399) / 400 in
  let yoe = y - (era * 400) in
  let mp = (m + 9) mod 12 in
  let doy = ((153 * mp) + 2) / 5 + d - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468

let civil_from_days z =
  let z = z + 719468 in
  let era = (if z >= 0 then z else z - 146096) / 146097 in
  let doe = z - (era * 146097) in
  let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
  let y = yoe + (era * 400) in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let d = doy - (((153 * mp) + 2) / 5) + 1 in
  let m = if mp < 10 then mp + 3 else mp - 9 in
  ((if m <= 2 then y + 1 else y), m, d)

(* Parse an RFC 3339 UTC timestamp such as "2023-10-06T17:35:55.440295Z". *)
let parse_rfc3339 (s : string) : int option =
  let n = String.length s in
  let num off len =
    if off + len > n then None
    else
      let v = ref 0 and ok = ref true in
      for i = off to off + len - 1 do
        match s.[i] with
        | '0' .. '9' as c -> v := (!v * 10) + Char.code c - 48
        | _ -> ok := false
      done;
      if !ok then Some !v else None
  in
  if n < 20 then None
  else
    match (num 0 4, num 5 2, num 8 2, num 11 2, num 14 2, num 17 2) with
    | Some y, Some mo, Some d, Some h, Some mi, Some sec
      when s.[4] = '-' && s.[7] = '-' && (s.[10] = 'T' || s.[10] = 't')
           && s.[13] = ':' && s.[16] = ':' ->
        let i = ref 19 in
        let frac = ref 0 and fd = ref 0 in
        if !i < n && s.[!i] = '.' then (
          incr i;
          while !i < n && match s.[!i] with '0' .. '9' -> true | _ -> false do
            if !fd < 9 then (
              frac := (!frac * 10) + Char.code s.[!i] - 48;
              incr fd);
            incr i
          done);
        while !fd < 9 do
          frac := !frac * 10;
          incr fd
        done;
        let offset_s =
          if !i >= n then None
          else
            match s.[!i] with
            | 'Z' | 'z' -> Some 0
            | ('+' | '-') as sg -> (
                match (num (!i + 1) 2, num (!i + 4) 2) with
                | Some oh, Some om ->
                    let o = (oh * 3600) + (om * 60) in
                    Some (if sg = '-' then -o else o)
                | _ -> None)
            | _ -> None
        in
        Option.map
          (fun off ->
            let days = days_from_civil y mo d in
            let secs = (days * 86400) + (h * 3600) + (mi * 60) + sec - off in
            (secs * ns_per_s) + !frac)
          offset_s
    | _ -> None

(* "YYYY-MM-DD" of the UTC day containing [ns]. *)
let utc_date (ns : int) : string =
  let days = if ns >= 0 then ns / ns_per_day else ((ns + 1) / ns_per_day) - 1 in
  let y, m, d = civil_from_days days in
  Printf.sprintf "%04d-%02d-%02d" y m d

let utc_day_index (ns : int) : int =
  if ns >= 0 then ns / ns_per_day else ((ns + 1) / ns_per_day) - 1

let date_to_day_index (s : string) : int option =
  if String.length s <> 10 then None
  else
    match
      ( int_of_string_opt (String.sub s 0 4),
        int_of_string_opt (String.sub s 5 2),
        int_of_string_opt (String.sub s 8 2) )
    with
    | Some y, Some m, Some d when s.[4] = '-' && s.[7] = '-' ->
        Some (days_from_civil y m d)
    | _ -> None

(* "YYYY-MM-DD HH:MM:SS.mmm" UTC, for logs and reports. *)
let utc_string (ns : int) : string =
  let day = utc_day_index ns in
  let rem = ns - (day * ns_per_day) in
  let secs = rem / ns_per_s in
  let ms = rem mod ns_per_s / ns_per_ms in
  let y, m, d = civil_from_days day in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d.%03d" y m d (secs / 3600)
    (secs / 60 mod 60) (secs mod 60) ms
