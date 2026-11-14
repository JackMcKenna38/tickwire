(* Exact fixed-point numbers: an integer count of 10^-decimals units.
   Stored in OCaml's native 63-bit int (see docs/DESIGN.md).  No floats. *)

type error = Malformed | Inexact | Overflow

let error_to_string = function
  | Malformed -> "malformed number"
  | Inexact -> "more decimal places than the instrument precision"
  | Overflow -> "number too large"

let pow10 n =
  if n < 0 || n > 18 then invalid_arg "Fixed.pow10";
  let r = ref 1 in
  for _ = 1 to n do
    r := !r * 10
  done;
  !r

let max_safe = max_int / 10

(* Parse a decimal literal (optional sign, digits, optional fraction,
   optional exponent) into an integer scaled by 10^decimals.

   [`Exact] fails with [Inexact] when the literal has non-zero digits beyond
   [decimals]; [`Truncate] drops them toward zero. *)
let of_literal_gen ~mode ~decimals (s : string) : (int, error) result =
  let n = String.length s in
  if n = 0 then Error Malformed
  else
    let i = ref 0 in
    let neg =
      match s.[0] with
      | '-' ->
          incr i;
          true
      | '+' ->
          incr i;
          false
      | _ -> false
    in
    (* Collect mantissa digits and count those after the point. *)
    let digits = Buffer.create 24 in
    let frac = ref 0 in
    let seen_point = ref false in
    let ok = ref true in
    let stop = ref false in
    while (not !stop) && !i < n do
      (match s.[!i] with
      | '0' .. '9' as c ->
          Buffer.add_char digits c;
          if !seen_point then incr frac
      | '.' -> if !seen_point then ok := false else seen_point := true
      | 'e' | 'E' -> stop := true
      | _ -> ok := false);
      if not !stop then incr i
    done;
    let exp =
      if !stop then (
        incr i;
        let eneg =
          if !i < n then
            match s.[!i] with
            | '-' ->
                incr i;
                true
            | '+' ->
                incr i;
                false
            | _ -> false
          else false
        in
        let e = ref 0 in
        let any = ref false in
        while !i < n do
          (match s.[!i] with
          | '0' .. '9' as c ->
              any := true;
              if !e < 10000 then e := (!e * 10) + Char.code c - 48
          | _ -> ok := false);
          incr i
        done;
        if not !any then ok := false;
        if eneg then - !e else !e)
      else 0
    in
    if (not !ok) || Buffer.length digits = 0 then Error Malformed
    else
      let d = Buffer.contents digits in
      (* value = d * 10^(exp - frac); scaled = d * 10^shift *)
      let shift = decimals + exp - !frac in
      let len = String.length d in
      let keep = if shift >= 0 then len else max 0 (len + shift) in
      let dropped_nonzero =
        let r = ref false in
        for k = keep to len - 1 do
          if d.[k] <> '0' then r := true
        done;
        !r
      in
      if dropped_nonzero && mode = `Exact then Error Inexact
      else
        let acc = ref 0 in
        let overflow = ref false in
        let push digit =
          if !acc > max_safe then overflow := true
          else
            let v = (!acc * 10) + digit in
            if v < 0 then overflow := true else acc := v
        in
        for k = 0 to keep - 1 do
          push (Char.code d.[k] - 48)
        done;
        if shift > 0 && !acc <> 0 then
          for _ = 1 to shift do
            push 0
          done;
        if !overflow then Error Overflow
        else Ok (if neg then - !acc else !acc)

let of_literal ~decimals s = of_literal_gen ~mode:`Exact ~decimals s
let of_literal_trunc ~decimals s = of_literal_gen ~mode:`Truncate ~decimals s

(* Exact decimal rendering, e.g. to_string ~decimals:2 (-1234) = "-12.34". *)
let to_string ~decimals (v : int) : string =
  let neg = v < 0 in
  let a = abs v in
  let body =
    if decimals = 0 then string_of_int a
    else
      let p = pow10 decimals in
      Printf.sprintf "%d.%0*d" (a / p) decimals (a mod p)
  in
  if neg then "-" ^ body else body

(* Rescale between decimal precisions, rounding half away from zero when
   precision is lost. *)
let rescale ~from_decimals ~to_decimals (v : int) : int =
  if to_decimals >= from_decimals then v * pow10 (to_decimals - from_decimals)
  else
    let p = pow10 (from_decimals - to_decimals) in
    let a = abs v in
    let q = (a + (p / 2)) / p in
    if v < 0 then -q else q

(* floor(a * b / c) for 0 <= a, 0 <= b, 0 < c without overflowing the
   intermediate product.  Requires c < 2^60. *)
let muldiv (a : int) (b : int) (c : int) : int =
  if a < 0 || b < 0 || c <= 0 then invalid_arg "Fixed.muldiv";
  let qa = a / c and ra = a mod c in
  (* a*b/c = qa*b + (ra*b)/c with ra < c; Horner over the bits of b. *)
  let q = ref 0 and r = ref 0 in
  for bit = 61 downto 0 do
    q := !q * 2;
    r := !r * 2;
    if !r >= c then (
      incr q;
      r := !r - c);
    if (b lsr bit) land 1 = 1 then (
      r := !r + ra;
      if !r >= c then (
        incr q;
        r := !r - c))
  done;
  (qa * b) + !q
