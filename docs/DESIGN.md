# Design notes

The significant choices, what else was considered, and why.

## Toolchain

**OCaml 5.4, native Windows (mingw-w64), dune.** Developed and tested on Windows 11 without WSL.

**Networking: `lwt` + `tls-lwt` (pure-OCaml TLS) + `ca-certs`, with a small WebSocket client of our own (`net/ws.ml`).**
Alternatives: `ocaml-ssl` (needs OpenSSL DLLs under mingw), `websocket-lwt-unix`/`cohttp` (large dependency tree), `eio` (Windows support still partial). The WebSocket client is about 150 lines (text frames, ping/pong, fragments, close). Certificates are verified against the Windows certificate store. The `Sec-WebSocket-Accept` header is not checked; TLS already authenticates the server.

**JSON: a small parser (`lib/json.ml`) that keeps number literals as text.**
Alternative: `Yojson.Raw`. Ours is about 200 lines, has no dependency, and makes it impossible to get a float by accident.

**CRC32, base64 and RFC 3339 parsing are implemented in the project.** Each is a few lines and tested against a known value.

**libgmp is linked statically.** The TLS stack pulls in zarith, which needs GMP. Linked dynamically, the executable only starts where `libgmp-10.dll` happens to be on `PATH`, which is not the case under Task Scheduler. `bin/dune` copies the toolchain's static `libgmp.a` into the build directory (`bin/copy_gmp.ml`) and links against that, so the executable depends only on Windows system DLLs.

## Numbers

**Prices and quantities are native `int` (63-bit), scaled by the instrument's `price_precision` / `qty_precision`.**
Native `int` on 64-bit OCaml is unboxed and much faster than `Int64.t`, and large enough: the largest values are cash amounts in price × quantity units (10^9 per dollar for BTC/USD), which leaves room for billions of dollars. Parsing checks for overflow. `Fixed.muldiv` computes a × b ÷ c without overflowing the intermediate product.

**Number text is converted exactly, or rejected.** `Fixed.of_literal` handles plain decimals and exponent notation (`1e-08`) and fails if a book price or quantity has more decimal places than the instrument allows. Trade prices alone are allowed to truncate (Kraken documents them as averages), and each such case is counted; so far the count is zero.

**Checksum string from integers.** Kraken's rule (remove the decimal point, strip leading zeros) applied to a value formatted at the instrument's precision is exactly the decimal rendering of the scaled integer. The documented example and every live update so far confirm this.

**Floats appear in two places only:** placing things in SVG charts, and reading the wall clock during live capture. Never for a price, quantity, or money amount.

## Feed handling

**Precisions come from the `instrument` channel**, subscribed first on every connection and unsubscribed once the symbols' precisions are known (its ongoing updates are large and not needed).

**The feed is a pure state machine (`lib/feed.ml`)** with callbacks for "send this" and "this happened". Live capture and replay both drive it; replay ignores the sends. This is what "replay uses the same code path" means concretely.

**Resync on mismatch: unsubscribe that symbol, wait for the acknowledgement, subscribe again, ignore updates until the snapshot.** Alternative: reconnect the whole socket. Per-symbol resync does not disturb the other symbols. Fallbacks: resubscribe anyway after 5 s without an acknowledgement; full reconnect after 20 s without a valid book. The book channel has no sequence numbers, so a gap can only be detected by the checksum or by the connection dropping.

**Timeouts are driven by message receive times, not a timer,** so replay makes the same decisions.

**Default depth 25.** Depth 10 is enough for the checksum but too shallow to see the queue where quotes a few basis points from the mid sit. Depth 100 roughly triples the data.

## Recording

**Format:** 8-byte magic, then records of `u32 length | u8 kind | i64 receive-time-ns | payload | u32 crc32`. Raw message text is stored untouched. Alternative: one JSON line per message (bigger, and needs escaping). A per-record CRC distinguishes a cut-off tail (tolerated) from corruption (reported).

**Each connection writes a "connected" record carrying the symbols and depth,** so a file replays with no other input.

**Daily rotation by UTC date, with a deliberate reconnect at midnight UTC,** so every day's file starts with its own instrument data and snapshots and can be replayed alone. Cost: one resync per day.

**Retention: files older than 14 days are deleted when a writer opens or rotates.** Only files named `YYYY-MM-DD.twlog` in the data directory are ever touched.

**Disk use is significant: roughly 400 to 650 MB per hour** for two symbols at depth 25 (about 500 messages a second). There is no compression, to avoid a native zlib dependency; lower `--keep-days` or `--depth` if this matters.

**The test fixture is 90 seconds of BTC/USD at depth 10 (2.6 MB).** It has few fills and only a handful of 60-second markouts; it is there to pin behaviour, not to produce results.

**The replay fingerprint is a chained MD5 over one line per event plus the final statistics.** MD5 is from the standard library and is used only to detect change, not for security.

## Simulation

**Simulated time is the recorded receive time** (what the capturing machine saw), forced to never run backwards during recording.

**Naive model:** quotes update instantly on every book change; any trade at or through a quote's price fills it completely, regardless of which side the taker was on; no fees in its headline figure. Fees at the same rate are still computed for it and shown, so the comparison can be made either way.

**Honest model:** see [HOW_IT_WORKS.md](HOW_IT_WORKS.md) for the rules in plain English and `lib/fill.ml` for rules R1 to R8.

- *Cancel/replace is atomic at the exchange* at decision time + latency; the old order is live until then.
- *One request in flight per side*; the side is busy until the acknowledgement returns (2 × latency).
- *Post-only:* an order that would cross on arrival is rejected and the strategy is told one latency later.
- *Cancellation assumption, default pro-rata.* Alternatives: pessimistic (all cancels behind us; `--cancel-model pessimistic`) and optimistic (all ahead; not offered, because it is the flattering one). Pro-rata rounds against us.
- *Trade/book ordering.* The two channels are separate. In recordings so far the trade message has arrived before the matching book update every time. The model handles both orders anyway: a trade leaves a credit that explains the next drop in displayed quantity, and a drop seen first is provisionally a cancellation and is reversed if the trade then arrives. Both expire after one second.
- *Levels beyond the visible depth* have unknown quantity: treated as zero ahead until first seen, then set to everything displayed.
- *The strategy's inventory view is delayed* by one latency, so it can overshoot the cap by one order.

**Reference vs optimised.** `Fill.Ref` is immutable and written for clarity; `Fill.Opt` is mutable and allocation-free on the common path. Property tests compare them event by event on random streams, and compare whole honest simulations built on each. A guard test checks that the random markets actually produce fills, so the agreement cannot pass vacuously.

**Fees.** Kraken Pro spot schedule from https://www.kraken.com/pro/fees as of 2026-11-14 (17 tiers, in `lib/fees.ml`). Default tier 1: maker 0.40%. This is what a new account pays and it dominates the result; tier 12 and above is 0% maker. Kraken lists a small discount on some pairs; none is applied.

**Strategy defaults:** quote 3 bp either side of the mid, skew 1 bp per order of inventory, $1,000 per order, cap of 5 orders. The half-spread was chosen by trying 1, 2, 3, 5 and 8 bp on one ten-minute recording and taking the setting where the naive backtest looked best. That is how a naive backtest gets used in practice, and it is a single short sample, so it is not evidence the setting is good.

**P&L accounting:** average-cost realised P&L; "gross" is cash plus inventory at the mid; "net" subtracts fees. Mid prices are carried doubled (bid + ask) to stay whole numbers. Drawdown is measured on net equity sampled once a second.

**Markouts:** signed mid move after each fill (negative is adverse), simple average over fills, in basis points. A fill is left out of a horizon if the data ends before that horizon is reached. "Edge" adds the distance between fill price and mid.

**The headline compares naive gross with honest net.** A naive backtest does not charge fees, so its own headline is gross. The report shows the bridge (queue and latency, then fees) and also states what naive would be with fees.

**No randomness is used anywhere in the simulation,** so there is no seed to set.

## Report and operation

**The report is one HTML file with inline SVG and CSS,** no scripts, no external resources. It follows the system light/dark setting.

**`nightly` simulates from the byte offset where its own session started** in the day's log, so earlier sessions that day are not mixed in.

**Report folders are named by local date; log files by UTC date.** The report is for a person; the logs are for the machine.

**The scheduled-task script runs the built executable** as the current user with normal rights, headless, only while logged on.

**Exit codes:** `live`, `sim` and `nightly` exit with 2 if the data did not verify 100%, so a scheduled run can be checked.
