(* CRC-32 (IEEE 802.3, reflected polynomial 0xEDB88320), as used by zlib and
   by Kraken's book checksum.  Result is an unsigned 32-bit value in an int. *)

let table =
  Array.init 256 (fun i ->
      let c = ref i in
      for _ = 0 to 7 do
        if !c land 1 = 1 then c := 0xEDB88320 lxor (!c lsr 1) else c := !c lsr 1
      done;
      !c)

let update crc (s : string) off len =
  let c = ref (crc lxor 0xFFFFFFFF) in
  for i = off to off + len - 1 do
    c := table.((!c lxor Char.code (String.unsafe_get s i)) land 0xFF) lxor (!c lsr 8)
  done;
  !c lxor 0xFFFFFFFF

let string (s : string) : int = update 0 s 0 (String.length s)
