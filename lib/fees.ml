(* Kraken Pro spot maker/taker fees.
   Source: https://www.kraken.com/pro/fees ("Spot & Futures" table),
   retrieved 2026-11-14.  Rates are in parts per million of notional
   (0.40% = 4000 ppm).  The tier is selected by 30-day spot volume in USD. *)

type tier = { tier : int; min_volume_usd : int; maker_ppm : int; taker_ppm : int }

let schedule =
  [
    { tier = 1; min_volume_usd = 0; maker_ppm = 4000; taker_ppm = 8000 };
    { tier = 2; min_volume_usd = 2_500; maker_ppm = 3000; taker_ppm = 6000 };
    { tier = 3; min_volume_usd = 10_000; maker_ppm = 2200; taker_ppm = 3800 };
    { tier = 4; min_volume_usd = 25_000; maker_ppm = 2000; taker_ppm = 3500 };
    { tier = 5; min_volume_usd = 50_000; maker_ppm = 1500; taker_ppm = 3000 };
    { tier = 6; min_volume_usd = 100_000; maker_ppm = 1200; taker_ppm = 2500 };
    { tier = 7; min_volume_usd = 250_000; maker_ppm = 1000; taker_ppm = 2200 };
    { tier = 8; min_volume_usd = 500_000; maker_ppm = 800; taker_ppm = 2000 };
    { tier = 9; min_volume_usd = 1_000_000; maker_ppm = 600; taker_ppm = 1800 };
    { tier = 10; min_volume_usd = 2_500_000; maker_ppm = 400; taker_ppm = 1500 };
    { tier = 11; min_volume_usd = 5_000_000; maker_ppm = 200; taker_ppm = 1200 };
    { tier = 12; min_volume_usd = 10_000_000; maker_ppm = 0; taker_ppm = 1000 };
    { tier = 13; min_volume_usd = 50_000_000; maker_ppm = 0; taker_ppm = 900 };
    { tier = 14; min_volume_usd = 100_000_000; maker_ppm = 0; taker_ppm = 800 };
    { tier = 15; min_volume_usd = 250_000_000; maker_ppm = 0; taker_ppm = 700 };
    { tier = 16; min_volume_usd = 400_000_000; maker_ppm = 0; taker_ppm = 600 };
    { tier = 17; min_volume_usd = 500_000_000; maker_ppm = 0; taker_ppm = 500 };
  ]

let maker_ppm_of_tier n =
  match List.find_opt (fun t -> t.tier = n) schedule with
  | Some t -> Some t.maker_ppm
  | None -> None

(* Fee on a notional amount, rounded half up, in the notional's own units. *)
let fee ~ppm (notional : int) : int = ((notional * ppm) + 500_000) / 1_000_000

let ppm_to_pct_string ppm = Fixed.to_string ~decimals:2 (ppm / 100) ^ "%"
