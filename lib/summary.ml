(* Plain-text summary of a simulation result, for the terminal. *)

let pct num den = if den = 0 then "n/a" else Fixed.to_string ~decimals:1 (num * 1000 / den) ^ "%"

let total_micro (r : Sim.result) (f : Sim.sym_result -> int) =
  List.fold_left (fun acc (s : Sim.sym_result) -> acc + Sim.to_micro s.inst (f s)) 0 r.syms

let headline (r : Sim.result) : string =
  let naive_gross = total_micro r (fun s -> s.naive.gross) in
  let honest_gross = total_micro r (fun s -> s.honest.gross) in
  let honest_net = total_micro r (fun s -> s.honest.net) in
  let signed v = (if v >= 0 then "+$" else "-$") ^ Sim.usd_of_micro (abs v) in
  Printf.sprintf "Naive backtest says %s. Honest simulation says %s before fees and %s after fees."
    (signed naive_gross) (signed honest_gross) (signed honest_net)

let text (r : Sim.result) : string =
  let b = Buffer.create 2048 in
  Buffer.add_string b (headline r);
  Buffer.add_char b '\n';
  List.iter
    (fun (s : Sim.sym_result) ->
      let i = s.inst in
      Printf.bprintf b "\n%s  (%d s of data, order size %s, tick %s)\n" s.symbol
        ((s.last_ts - s.first_ts) / Timeutil.ns_per_s)
        (Fixed.to_string ~decimals:i.qty_decimals s.params.size)
        (Fixed.to_string ~decimals:i.price_decimals s.params.tick);
      Printf.bprintf b "  trades arriving before / after their book update: %d / %d\n" s.trades_trade_first s.trades_book_first;
      Printf.bprintf b "  %-22s %14s %14s\n" "" "naive" "honest";
      let row name f = Printf.bprintf b "  %-22s %14s %14s\n" name (f s.naive) (f s.honest) in
      row "orders placed" (fun (m : Sim.model_result) -> string_of_int m.orders_placed);
      row "fills" (fun m -> string_of_int m.n_fills);
      row "fill rate (orders)" (fun m -> pct m.orders_filled m.orders_placed);
      row "fill rate (quantity)" (fun m -> pct m.filled_qty m.quoted_qty);
      row "gross P&L (to mid) $" (fun m -> Sim.usd i m.gross);
      row "  realised $" (fun m -> Sim.usd i m.realised);
      row "  unrealised $" (fun m -> Sim.usd i m.unrealised);
      row "fees $" (fun m -> Sim.usd i m.fees);
      row "net P&L $" (fun m -> Sim.usd i m.net);
      row "max drawdown $" (fun m -> Sim.usd i m.max_drawdown);
      row "final inventory" (fun m -> Fixed.to_string ~decimals:i.qty_decimals m.final_inv);
      row "max |inventory|" (fun m -> Fixed.to_string ~decimals:i.qty_decimals m.max_abs_inv);
      row "post-only rejects" (fun m -> string_of_int m.rejects);
      List.iteri
        (fun k h ->
          let get (m : Sim.model_result) =
            let x = List.nth m.markouts k in
            if x.n = 0 then "n/a" else Printf.sprintf "%s bp (n=%d)" (Sim.cbps_string x.mid_move_cbps) x.n
          in
          row (Printf.sprintf "mid move after %d s" (h / Timeutil.ns_per_s)) get)
        Sim.horizons)
    r.syms;
  Buffer.contents b
