(lang dune 3.16)

(name tickwire)

(generate_opam_files true)

(license MIT)

(package
 (name tickwire)
 (synopsis "Honest market-making simulator on verified Kraken order books")
 (description
  "Maintains checksum-verified Kraken L2 books, records and replays sessions deterministically, and compares a naive backtest with an honest queue/latency/fee simulation.")
 (depends
  (ocaml
   (>= 5.2))
  dune
  cmdliner
  lwt
  tls-lwt
  ca-certs
  mirage-crypto-rng
  (alcotest :with-test)
  (qcheck :with-test)
  (qcheck-alcotest :with-test)))
