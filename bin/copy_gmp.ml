(* Build helper: copy the toolchain's static libgmp.a into the build
   directory so the linker picks it instead of the DLL import library.
   Run by the rule in bin/dune. *)

let capture cmd =
  let tmp = Filename.temp_file "tw" ".txt" in
  let rc = Sys.command (cmd ^ " > " ^ Filename.quote tmp) in
  let s = In_channel.with_open_bin tmp In_channel.input_all in
  Sys.remove tmp;
  if rc <> 0 then failwith ("command failed: " ^ cmd);
  String.trim s

let () =
  let p = capture "x86_64-w64-mingw32-gcc -print-file-name=libgmp.a" in
  let p = if String.length p > 0 && p.[0] = '/' then capture ("cygpath -w " ^ Filename.quote p) else p in
  if not (Sys.file_exists p) then failwith ("static libgmp.a not found at " ^ p);
  let data = In_channel.with_open_bin p In_channel.input_all in
  Out_channel.with_open_bin "libgmp.a" (fun oc -> output_string oc data)
