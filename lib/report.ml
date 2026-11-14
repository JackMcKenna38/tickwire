(* The HTML report: one self-contained file, charts as inline SVG.
   Floats appear here only to place things on screen; every number printed
   in a table is formatted from exact integers. *)

let esc s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '&' -> Buffer.add_string b "&amp;"
      | '"' -> Buffer.add_string b "&quot;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let css =
  {|
:root{--bg:#fbfaf7;--panel:#ffffff;--ink:#1c1b19;--muted:#6b6862;--line:#e4e1da;--grid:#eceae4;
--naive:#c2681c;--honest:#1f5fa8;--good:#1d7a46;--bad:#b3261e;}
@media (prefers-color-scheme: dark){:root{--bg:#141413;--panel:#1d1c1a;--ink:#ecebe7;--muted:#a09c94;--line:#33312d;--grid:#2a2926;
--naive:#e39a55;--honest:#6ea8e6;--good:#5bbf85;--bad:#ef8a80;}}
*{box-sizing:border-box}
body{margin:0;background:var(--bg);color:var(--ink);font:15px/1.5 system-ui,-apple-system,"Segoe UI",sans-serif}
main{max-width:980px;margin:0 auto;padding:32px 16px 64px}
h1{font-size:15px;font-weight:600;letter-spacing:.04em;text-transform:uppercase;color:var(--muted);margin:0 0 4px}
h2{font-size:19px;margin:44px 0 6px}
h3{font-size:15px;margin:22px 0 4px}
p{margin:6px 0 12px;max-width:72ch}
.sub{color:var(--muted);font-size:13.5px}
.headline{font-size:27px;line-height:1.25;font-weight:650;margin:10px 0 14px;max-width:26ch;text-wrap:balance}
.naive{color:var(--naive)}.honest{color:var(--honest)}.pos{color:var(--good)}.neg{color:var(--bad)}
.panel{background:var(--panel);border:1px solid var(--line);border-radius:10px;padding:16px;margin:12px 0;overflow-x:auto}
table{border-collapse:collapse;width:100%;font-variant-numeric:tabular-nums}
th,td{padding:7px 10px;text-align:right;border-bottom:1px solid var(--line);white-space:nowrap}
th:first-child,td:first-child{text-align:left}
thead th{font-size:12.5px;font-weight:600;color:var(--muted);text-transform:uppercase;letter-spacing:.04em}
tr.total td{font-weight:650}
tr:last-child td{border-bottom:none}
.bridge td:first-child{white-space:normal}
svg{display:block;width:100%;height:auto}
svg text{font:12px system-ui,-apple-system,"Segoe UI",sans-serif;fill:var(--muted)}
svg .lab{fill:var(--ink)}
.legend{display:flex;gap:18px;font-size:13px;color:var(--muted);margin:2px 0 8px}
.legend i{display:inline-block;width:14px;height:3px;border-radius:2px;margin-right:6px;vertical-align:middle}
.grid2{display:grid;grid-template-columns:repeat(auto-fit,minmax(320px,1fr));gap:12px}
ul{padding-left:20px;max-width:76ch}li{margin:5px 0}
code{font:13px ui-monospace,Consolas,monospace;background:var(--grid);padding:1px 5px;border-radius:4px}
footer{margin-top:40px;color:var(--muted);font-size:12.5px;overflow-wrap:anywhere}
|}

(* ---------- tiny SVG chart kit ---------- *)

let nice_step range target =
  if range <= 0.0 then 1.0
  else
    let raw = range /. float_of_int target in
    let mag = 10.0 ** Float.floor (Float.log10 raw) in
    let n = raw /. mag in
    (if n <= 1.0 then 1.0 else if n <= 2.0 then 2.0 else if n <= 5.0 then 5.0 else 10.0) *. mag

let fmt_tick step v =
  let v = if Float.abs v < step /. 1000.0 then 0.0 else v in
  if step >= 1.0 then Printf.sprintf "%.0f" v
  else if step >= 0.1 then Printf.sprintf "%.1f" v
  else if step >= 0.01 then Printf.sprintf "%.2f" v
  else if step >= 0.001 then Printf.sprintf "%.3f" v
  else Printf.sprintf "%.4f" v

type series = { cls : string; pts : (float * float) array; step : bool }

(* Line chart: x in seconds since the start, y in the caller's unit. *)
let line_chart ~y_label ~(series : series list) : string =
  let w = 940.0 and h = 260.0 in
  let ml = 62.0 and mr = 14.0 and mt = 12.0 and mb = 30.0 in
  let all = List.concat_map (fun s -> Array.to_list s.pts) series in
  let b = Buffer.create 8192 in
  Printf.bprintf b {|<svg viewBox="0 0 %.0f %.0f" role="img" aria-label="%s">|} w h (esc y_label);
  if all = [] then Printf.bprintf b {|<text x="%.0f" y="%.0f">no data</text>|} (w /. 2.0) (h /. 2.0)
  else (
    let xmax = List.fold_left (fun a (x, _) -> Float.max a x) 1.0 all in
    let ymin = List.fold_left (fun a (_, y) -> Float.min a y) 0.0 all in
    let ymax = List.fold_left (fun a (_, y) -> Float.max a y) 0.0 all in
    let ymin, ymax = if ymax -. ymin < 1e-12 then (ymin -. 1.0, ymax +. 1.0) else (ymin, ymax) in
    let st = nice_step (ymax -. ymin) 5 in
    let y0 = Float.floor (ymin /. st) *. st and y1 = Float.ceil (ymax /. st) *. st in
    let px x = ml +. (x /. xmax *. (w -. ml -. mr)) in
    let py y = mt +. ((y1 -. y) /. (y1 -. y0) *. (h -. mt -. mb)) in
    let n = int_of_float (Float.round ((y1 -. y0) /. st)) in
    for i = 0 to n do
      let v = y0 +. (float_of_int i *. st) in
      let zero = Float.abs v < st /. 1000.0 in
      Printf.bprintf b {|<line x1="%.1f" x2="%.1f" y1="%.1f" y2="%.1f" stroke="var(%s)" stroke-width="1"/>|} ml
        (w -. mr) (py v) (py v)
        (if zero then "--muted" else "--grid");
      Printf.bprintf b {|<text x="%.1f" y="%.1f" text-anchor="end">%s</text>|} (ml -. 8.0) (py v +. 4.0)
        (fmt_tick st v)
    done;
    (* x ticks in minutes (or seconds for short runs) *)
    let unit_s, unit_name = if xmax >= 300.0 then (60.0, "min") else (1.0, "s") in
    let xst = nice_step (xmax /. unit_s) 8 in
    let k = ref 0.0 in
    while !k *. unit_s <= xmax +. 1e-9 do
      Printf.bprintf b {|<text x="%.1f" y="%.1f" text-anchor="middle">%s</text>|}
        (px (!k *. unit_s))
        (h -. 10.0) (fmt_tick xst !k);
      k := !k +. xst
    done;
    Printf.bprintf b {|<text x="%.1f" y="%.1f" text-anchor="end">%s</text>|} (w -. mr) (h -. 10.0) unit_name;
    List.iter
      (fun s ->
        if Array.length s.pts > 0 then (
          Printf.bprintf b {|<path fill="none" stroke="var(--%s)" stroke-width="1.8" stroke-linejoin="round" d="|} s.cls;
          Array.iteri
            (fun i (x, y) ->
              if i = 0 then Printf.bprintf b "M%.1f %.1f" (px x) (py y)
              else if s.step then Printf.bprintf b "H%.1fV%.1f" (px x) (py y)
              else Printf.bprintf b "L%.1f %.1f" (px x) (py y))
            s.pts;
          Buffer.add_string b {|"/>|}))
      series);
  Buffer.add_string b "</svg>";
  Buffer.contents b

(* Grouped bars: one group per label, one bar per (class, value option). *)
let bar_chart ~(groups : (string * (string * float option) list) list) ~unit_name : string =
  let w = 460.0 and h = 250.0 in
  let ml = 50.0 and mr = 10.0 and mt = 20.0 and mb = 34.0 in
  let vals = List.concat_map (fun (_, l) -> List.filter_map snd l) groups in
  let b = Buffer.create 4096 in
  Printf.bprintf b {|<svg viewBox="0 0 %.0f %.0f" role="img" aria-label="markouts in %s">|} w h unit_name;
  let ymin = List.fold_left Float.min 0.0 vals and ymax = List.fold_left Float.max 0.0 vals in
  let ymin, ymax = if ymax -. ymin < 1e-9 then (-1.0, 1.0) else (ymin, ymax) in
  let st = nice_step (ymax -. ymin) 4 in
  let y0 = Float.floor (ymin /. st) *. st and y1 = Float.ceil (ymax /. st) *. st in
  let y0 = if y0 = ymin && ymin < 0.0 then y0 -. st else y0 in
  let y1 = if y1 = ymax && ymax > 0.0 then y1 +. st else y1 in
  let py y = mt +. ((y1 -. y) /. (y1 -. y0) *. (h -. mt -. mb)) in
  let n = int_of_float (Float.round ((y1 -. y0) /. st)) in
  for i = 0 to n do
    let v = y0 +. (float_of_int i *. st) in
    let zero = Float.abs v < st /. 1000.0 in
    Printf.bprintf b {|<line x1="%.1f" x2="%.1f" y1="%.1f" y2="%.1f" stroke="var(%s)" stroke-width="1"/>|} ml
      (w -. mr) (py v) (py v)
      (if zero then "--muted" else "--grid");
    Printf.bprintf b {|<text x="%.1f" y="%.1f" text-anchor="end">%s</text>|} (ml -. 6.0) (py v +. 4.0) (fmt_tick st v)
  done;
  let ng = max 1 (List.length groups) in
  let gw = (w -. ml -. mr) /. float_of_int ng in
  List.iteri
    (fun gi (label, bars) ->
      let nb = max 1 (List.length bars) in
      let bw = Float.min 46.0 (gw *. 0.7 /. float_of_int nb) in
      let x0 = ml +. (float_of_int gi *. gw) +. ((gw -. (bw *. float_of_int nb) -. (4.0 *. float_of_int (nb - 1))) /. 2.0) in
      List.iteri
        (fun bi (cls, v) ->
          let x = x0 +. (float_of_int bi *. (bw +. 4.0)) in
          match v with
          | None ->
              Printf.bprintf b {|<text x="%.1f" y="%.1f" text-anchor="middle">n/a</text>|} (x +. (bw /. 2.0)) (py 0.0 -. 6.0)
          | Some v ->
              let top = Float.min (py v) (py 0.0) and hh = Float.abs (py v -. py 0.0) in
              Printf.bprintf b {|<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="2" fill="var(--%s)"/>|} x top bw
                (Float.max hh 1.0) cls;
              Printf.bprintf b {|<text class="lab" x="%.1f" y="%.1f" text-anchor="middle">%.2f</text>|}
                (x +. (bw /. 2.0))
                (if v >= 0.0 then top -. 5.0 else top +. hh +. 14.0)
                v)
        bars;
      Printf.bprintf b {|<text x="%.1f" y="%.1f" text-anchor="middle">%s</text>|}
        (ml +. (float_of_int gi *. gw) +. (gw /. 2.0))
        (h -. 10.0) (esc label))
    groups;
  Buffer.add_string b "</svg>";
  Buffer.contents b

(* ---------- report ---------- *)

type meta = {
  title_date : string; (* YYYY-MM-DD shown in the title *)
  files : string list;
  replay_hash : string;
}

let legend =
  {|<div class="legend"><span><i style="background:var(--naive)"></i>Naive backtest</span><span><i style="background:var(--honest)"></i>Honest simulation</span></div>|}

let signed_usd micro =
  let cls = if micro > 0 then "pos" else if micro < 0 then "neg" else "" in
  Printf.sprintf {|<span class="%s">%s$%s</span>|} cls (if micro < 0 then "&minus;" else if micro > 0 then "+" else "")
    (Sim.usd_of_micro (abs micro))

let html ~(meta : meta) ~(replay : Replay.result) (r : Sim.result) : string =
  let b = Buffer.create 65536 in
  let p fmt = Printf.bprintf b fmt in
  let tot f = Summary.total_micro r f in
  let naive_gross = tot (fun s -> s.naive.gross) in
  let honest_gross = tot (fun s -> s.honest.gross) in
  let honest_fees = tot (fun s -> s.honest.fees) in
  let honest_net = tot (fun s -> s.honest.net) in
  let naive_fees = tot (fun s -> s.naive.fees) in
  let naive_net = tot (fun s -> s.naive.net) in
  let cfg = r.cfg in
  let dur_s = (replay.last_ts - replay.first_ts) / Timeutil.ns_per_s in
  p {|<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>tickwire %s</title><style>%s</style></head><body><main>|}
    (esc meta.title_date) css;
  p "<h1>tickwire &middot; %s</h1>" (esc meta.title_date);
  p {|<div class="headline">Naive says %s. Honest says %s.</div>|} (signed_usd naive_gross) (signed_usd honest_net);
  p
    {|<p>The same market-making strategy, run over the same %d min %02d s of recorded Kraken data (%s UTC onward), under two sets of rules. The <span class="naive">naive backtest</span> fills an order the instant any trade prints at its price and pays nothing. The <span class="honest">honest simulation</span> waits in the queue behind the orders already there, takes %d ms to reach the exchange and %d ms to hear back, and pays Kraken's maker fee of %s.</p>|}
    (dur_s / 60) (dur_s mod 60)
    (esc (Timeutil.utc_string replay.first_ts))
    (cfg.honest.latency_ns / Timeutil.ns_per_ms)
    (cfg.honest.latency_ns / Timeutil.ns_per_ms)
    (Fees.ppm_to_pct_string cfg.fee_ppm);

  (* ---- headline table ---- *)
  p {|<div class="panel"><table><thead><tr><th></th><th class="naive">Naive</th><th class="honest">Honest</th></tr></thead><tbody>|};
  let sum f = List.fold_left (fun a (s : Sim.sym_result) -> a + f s) 0 r.syms in
  let row ?(cls = "") name a c = p {|<tr class="%s"><td>%s</td><td>%s</td><td>%s</td></tr>|} cls name a c in
  row "Orders placed"
    (string_of_int (sum (fun s -> s.naive.orders_placed)))
    (string_of_int (sum (fun s -> s.honest.orders_placed)));
  row "Fills" (string_of_int (sum (fun s -> s.naive.n_fills))) (string_of_int (sum (fun s -> s.honest.n_fills)));
  row "Fill rate (orders with a fill)"
    (Summary.pct (sum (fun s -> s.naive.orders_filled)) (sum (fun s -> s.naive.orders_placed)))
    (Summary.pct (sum (fun s -> s.honest.orders_filled)) (sum (fun s -> s.honest.orders_placed)));
  row "Gross P&amp;L, marked to mid" (signed_usd naive_gross) (signed_usd honest_gross);
  row "&nbsp;&nbsp;of which realised"
    (signed_usd (tot (fun s -> s.naive.realised)))
    (signed_usd (tot (fun s -> s.honest.realised)));
  row
    (Printf.sprintf "Maker fees at %s" (Fees.ppm_to_pct_string cfg.fee_ppm))
    (Printf.sprintf {|<span class="sub">ignored (would be $%s)</span>|} (Sim.usd_of_micro naive_fees))
    ("$" ^ Sim.usd_of_micro honest_fees);
  row ~cls:"total" "What each model reports" (signed_usd naive_gross) (signed_usd honest_net);
  row "Max drawdown (net)"
    ("$" ^ Sim.usd_of_micro (tot (fun s -> s.naive.max_drawdown)))
    ("$" ^ Sim.usd_of_micro (tot (fun s -> s.honest.max_drawdown)));
  p "</tbody></table></div>";
  p
    {|<p class="sub">A naive backtest ignores fees, so its headline is its gross figure; with the same fee applied it would report %s. Drawdown is summed across symbols.</p>|}
    (signed_usd naive_net);

  (* ---- bridge ---- *)
  p "<h2>Where the gap comes from</h2>";
  p {|<div class="panel"><table class="bridge"><tbody>|};
  p {|<tr><td>Naive backtest, gross</td><td>%s</td></tr>|} (signed_usd naive_gross);
  p {|<tr><td>Queue position, latency and stale quotes (fewer and worse fills)</td><td>%s</td></tr>|}
    (signed_usd (honest_gross - naive_gross));
  p {|<tr><td>Honest simulation, before fees</td><td>%s</td></tr>|} (signed_usd honest_gross);
  p {|<tr><td>Maker fees on %d fills</td><td>%s</td></tr>|} (sum (fun s -> s.honest.n_fills)) (signed_usd (-honest_fees));
  p {|<tr class="total"><td>Honest simulation, after fees</td><td>%s</td></tr>|} (signed_usd honest_net);
  p "</tbody></table></div>";

  (* ---- per symbol table ---- *)
  p "<h3>By symbol</h3>";
  p {|<div class="panel"><table><thead><tr><th>Symbol</th><th>Model</th><th>Fills</th><th>Fill rate</th><th>Gross</th><th>Fees</th><th>Net</th><th>Max drawdown</th><th>Final inventory</th></tr></thead><tbody>|};
  List.iter
    (fun (s : Sim.sym_result) ->
      List.iter
        (fun (m : Sim.model_result) ->
          p {|<tr><td>%s</td><td class="%s">%s</td><td>%d</td><td>%s</td><td>%s</td><td>$%s</td><td>%s</td><td>$%s</td><td>%s</td></tr>|}
            (esc s.symbol) m.model m.model m.n_fills
            (Summary.pct m.orders_filled m.orders_placed)
            (signed_usd (Sim.to_micro s.inst m.gross))
            (Sim.usd s.inst m.fees)
            (signed_usd (Sim.to_micro s.inst m.net))
            (Sim.usd s.inst m.max_drawdown)
            (Fixed.to_string ~decimals:s.inst.qty_decimals m.final_inv))
        [ s.naive; s.honest ])
    r.syms;
  p "</tbody></table></div>";

  (* ---- markouts ---- *)
  p "<h2>Markouts: what the price did after each fill</h2>";
  p
    {|<p>For every fill, how far the mid price moved in the following 1, 10 and 60 seconds, in basis points, signed so that negative means the market moved against the position just taken. This is adverse selection: a resting order tends to be filled exactly when someone better informed wants the other side.</p>|};
  p "%s" legend;
  p {|<div class="grid2">|};
  List.iter
    (fun (s : Sim.sym_result) ->
      let groups =
        List.mapi
          (fun k h ->
            let v (m : Sim.model_result) =
              let x = List.nth m.markouts k in
              if x.n = 0 then None else Some (float_of_int x.mid_move_cbps /. 100.0)
            in
            (Printf.sprintf "%d s" (h / Timeutil.ns_per_s), [ ("naive", v s.naive); ("honest", v s.honest) ]))
          Sim.horizons
      in
      p {|<div class="panel"><div class="sub">%s &middot; average mid move after a fill, bp</div>%s</div>|} (esc s.symbol)
        (bar_chart ~groups ~unit_name:"bp"))
    r.syms;
  p "</div>";
  p {|<div class="panel"><table><thead><tr><th>Symbol</th><th>Model</th><th>Horizon</th><th>Fills measured</th><th>Mid move (bp)</th><th>Edge vs fill price (bp)</th></tr></thead><tbody>|};
  List.iter
    (fun (s : Sim.sym_result) ->
      List.iter
        (fun (m : Sim.model_result) ->
          List.iter
            (fun (k : Sim.markout) ->
              p {|<tr><td>%s</td><td class="%s">%s</td><td>%d s</td><td>%d</td><td>%s</td><td>%s</td></tr>|} (esc s.symbol)
                m.model m.model
                (k.horizon_ns / Timeutil.ns_per_s)
                k.n
                (if k.n = 0 then "n/a" else Sim.cbps_string k.mid_move_cbps)
                (if k.n = 0 then "n/a" else Sim.cbps_string k.edge_cbps))
            m.markouts)
        [ s.naive; s.honest ])
    r.syms;
  p "</tbody></table></div>";
  p
    {|<p class="sub">Edge vs fill price adds the spread captured at the fill to the mid move: it is what the fill was worth, before fees, if closed at the mid that much later. Fills too close to the end of the data to be measured at a horizon are left out of it.</p>|};

  (* ---- inventory + equity ---- *)
  let series_of (s : Sim.sym_result) f step =
    let mk (m : Sim.model_result) =
      {
        cls = m.model;
        step;
        pts =
          Array.map
            (fun (x : Account.sample) -> (float_of_int (x.s_ts - s.first_ts) /. 1e9, f x))
            m.samples;
      }
    in
    [ mk s.naive; mk s.honest ]
  in
  p "<h2>Inventory path</h2>";
  p {|<p>Position over time, in units of the base currency. Positive is long.</p>%s|} legend;
  List.iter
    (fun (s : Sim.sym_result) ->
      let q = float_of_int (Fixed.pow10 s.inst.qty_decimals) in
      p {|<div class="panel"><div class="sub">%s &middot; inventory</div>%s</div>|} (esc s.symbol)
        (line_chart ~y_label:(s.symbol ^ " inventory")
           ~series:(series_of s (fun x -> float_of_int x.inv /. q) true)))
    r.syms;
  p "<h2>Profit and loss over time</h2>";
  p {|<p>Marked to the mid price each second, in US dollars. Naive is shown gross, as a naive backtest reports it; honest is shown net of fees.</p>%s|} legend;
  List.iter
    (fun (s : Sim.sym_result) ->
      let c = float_of_int (Fixed.pow10 (Sim.cash_decimals s.inst)) in
      let naive = List.hd (series_of s (fun x -> float_of_int x.gross /. c) false) in
      let honest = List.nth (series_of s (fun x -> float_of_int x.net /. c) false) 1 in
      p {|<div class="panel"><div class="sub">%s &middot; P&amp;L, USD</div>%s</div>|} (esc s.symbol)
        (line_chart ~y_label:(s.symbol ^ " P&L") ~series:[ naive; honest ]))
    r.syms;

  (* ---- data quality ---- *)
  p "<h2>Data quality</h2>";
  p
    {|<p>Every book update is checked against Kraken's own CRC32 checksum of the top ten levels. The simulations above ran on a replay of the recording, through the same code that handled the live feed.</p>|};
  p {|<div class="panel"><table><thead><tr><th>Symbol</th><th>Updates</th><th>Checksums matched</th><th>Match rate</th><th>Resyncs</th><th>Trades</th><th>Latency p50 / p99</th></tr></thead><tbody>|};
  List.iter
    (fun (f : Feed.t) ->
      List.iter
        (fun (s : Feed.sym) ->
          let st = s.st in
          let total = st.checksum_ok + st.checksum_bad in
          p {|<tr><td>%s</td><td>%d</td><td>%d of %d</td><td class="%s">%s</td><td>%d</td><td>%d</td><td>%d / %d ms</td></tr>|}
            (esc s.name) st.updates st.checksum_ok total
            (if st.checksum_ok = total && total > 0 then "pos" else "neg")
            (Feed.pct_string st.checksum_ok total)
            st.resyncs st.trades (Feed.latency_pct_ms st 50) (Feed.latency_pct_ms st 99))
        f.symbols)
    replay.feeds;
  p "</tbody></table></div>";
  let conns = List.fold_left (fun a (f : Feed.t) -> a + f.connects) 0 replay.feeds in
  let errs = List.fold_left (fun a (f : Feed.t) -> a + f.errors) 0 replay.feeds in
  p
    {|<p class="sub">%d recorded messages, %d connection(s), %d message errors. Latency is local receive time minus the exchange timestamp, so it includes any offset between the two clocks.</p>|}
    replay.records conns errs;

  (* ---- assumptions ---- *)
  p "<h2>Assumptions</h2><ul>";
  p "<li>Strategy: quote one bid and one ask %s bp either side of the mid, shifted %s bp against inventory per order held; $%d per order; at most %d orders of inventory; requote when the target moves %d tick(s). Post-only.</li>"
    (Sim.cbps_string cfg.strat.half_spread_cbps) (Sim.cbps_string cfg.strat.skew_cbps) cfg.strat.order_notional_usd
    cfg.strat.max_inventory_orders cfg.strat.requote_ticks;
  p "<li>Latency: %d ms each way, for new orders, cancels and fill reports alike. An order being replaced can still be filled until the cancel arrives.</li>"
    (cfg.honest.latency_ns / Timeutil.ns_per_ms);
  p "<li>Queue: a new order joins behind all quantity displayed at its price. Trades at that price are taken from the front of the queue.</li>";
  p "<li>Cancellations (%s): %s</li>"
    (Fill.cancel_model_to_string cfg.honest.cancel_model)
    (match cfg.honest.cancel_model with
    | Fill.Prorata ->
        "a drop in displayed quantity not explained by trades is assumed to be spread evenly across the level, so the queue ahead shrinks in proportion."
    | Fill.Pessimistic -> "every cancellation is assumed to come from behind our order, so only trades move us forward.");
  p "<li>Fees: Kraken Pro spot maker fee of %s (published schedule, retrieved 2026-11-14), charged on every honest fill.</li>"
    (Fees.ppm_to_pct_string cfg.fee_ppm);
  p "<li>No market impact: the recorded market does not react to our orders. Both models share this limitation; it flatters both.</li>";
  p "<li>This is a simulation on public market data. No orders were placed.</li>";
  p "</ul>";
  p "<footer>Recording: %s<br>Replay hash %s &middot; simulation hash %s</footer>"
    (esc (String.concat ", " meta.files)) (esc meta.replay_hash) (Sim.hash r);
  p "</main></body></html>\n";
  Buffer.contents b
