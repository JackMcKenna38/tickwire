# How tickwire works

A plain-English explanation, with no code.

## The one-paragraph version

tickwire watches a real exchange (Kraken) and pretends to be a small market maker on it, without ever placing an order. It runs the same pretend strategy twice over the same recorded data. The first run uses the easy rules most backtests use, and the second uses rules much closer to how an exchange behaves. The report shows both results side by side and explains the gap. The point is the gap: the easy rules flatter almost any strategy.

## What an order book is

An exchange is a list of people willing to buy and a list of people willing to sell.

- Each buyer says "I'll buy this much at this price." These are **bids**. The highest bid is the **best bid**.
- Each seller says "I'll sell this much at this price." These are **asks**. The lowest ask is the **best ask**.
- The gap between the best bid and the best ask is the **spread**. Halfway between them is the **mid price**.

Orders at the same price form a **queue**. Whoever got there first is served first. If 3 BTC is already waiting to buy at $65,000 and you add your order at $65,000, the 3 BTC ahead of you must be sold to before anyone sells to you.

A **trade** happens when someone accepts a waiting price. They are the **taker**; the person whose order was waiting is the **maker**. A market maker is someone who always has orders waiting on both sides, hoping to buy slightly below the mid and sell slightly above it, and keep the difference.

## How tickwire knows its copy of the book is right

Kraken first sends a full picture of the book (a **snapshot**), then a stream of small changes ("the quantity at $65,010 is now 0.4"). tickwire applies each change to its own copy.

A copy can silently go wrong: one lost message and every number after it is off. So with every change Kraken also sends a **checksum**, a short fingerprint of what the top ten prices on each side should now look like. tickwire computes the same fingerprint from its own copy and compares. If they ever differ, it throws its copy away, asks for a fresh snapshot, and counts the incident.

The fingerprint is computed from the exact digits of each price and quantity, so tickwire never stores these as ordinary computer decimals (which round). It stores them as whole numbers of the smallest unit, the way you might store money as cents.

## Recording and replaying

Every message from Kraken is written to a file exactly as it arrived, stamped with the time it arrived. Later, that file can be played back through the very same code that handled the live feed. Because nothing in the playback depends on the clock or on chance, playing the same file twice gives the same result down to the last byte. That is what makes the comparison between the two simulations fair: both see exactly the same market.

## The strategy

Deliberately the plainest one in the textbook:

- Keep one buy order a little below the mid and one sell order the same distance above it.
- If it has ended up holding inventory, shift both prices to encourage getting rid of it (holding a lot: lower both prices so it sells more easily and buys less eagerly).
- Never hold more than a set maximum.
- Never place an order that would trade immediately; it only ever waits.

The strategy is not the interesting part. It is the fixed thing both simulations share.

## Why naive backtests lie

The **naive** simulation uses these rules:

1. An order is in the market the instant the strategy thinks of it.
2. If any trade happens at the order's price or better, the order is filled, completely.
3. Trading is free.

Each rule is wrong in the strategy's favour.

- **There is a queue.** A trade at your price goes to the people who were there before you. Often the price touches your level, a little trades, and the price leaves again. The naive rules say you were filled. In reality the people ahead of you were.
- **Messages take time.** By the time your new order reaches the exchange, the price it was based on is a little old. By the time your cancel reaches the exchange, someone may already have traded against the order you were trying to pull. Those are exactly the trades you did not want.
- **You are filled when it is bad for you.** Your buy order gets filled when somebody is keen to sell, which is often just before the price falls. This is called **adverse selection**, and it is the main thing a market maker is paid the spread to endure.
- **Fees.** Kraken charges makers a percentage of every trade. For a small account it is 0.40%, far more than the fraction of a percent a market maker captures per trade.

## How the honest simulation models each of these

**Latency.** Every message takes 100 milliseconds each way (changeable). A new order starts waiting 100 ms after the strategy decides on it. A cancel takes 100 ms to land, and until it lands the old order can still be filled. News of a fill takes 100 ms to come back, so the strategy is always working from a slightly out-of-date idea of what it owns.

**Queue position.** When an order arrives, tickwire looks at how much is already waiting at that price in the real book and puts the order behind all of it. From then on:

- A trade at that price eats into the quantity ahead. Only once that is used up does the trade start filling the order, and it may fill only part of it.
- A trade at a *worse* price (for a buy order: a lower price) means everything at the order's price was traded out, so the order is filled in full.
- New quantity arriving at that price joins behind the order and does not affect it.

**Cancellations.** Sometimes the quantity waiting at a price shrinks without a trade: somebody cancelled. The data does not say whether they were ahead of the order or behind it. This needs an assumption, and it is the biggest one in the model:

- *Pro-rata* (the default): cancellations are spread evenly along the queue. If a tenth of the quantity at a price is cancelled, a tenth of what was ahead of the order is assumed gone.
- *Pessimistic* (optional): every cancellation is assumed to be behind the order. Only trades move it forward.

The truth is somewhere between. Running both gives a range.

**Fees.** Kraken's published maker fee is charged on every fill. The default is the 0.40% a new account pays; any tier from the published schedule can be chosen.

**Rejections.** If the market has moved so that a new order would trade immediately on arrival, the exchange would refuse it (the strategy only sends orders that must wait), so the simulation refuses it too.

## What the report measures

- **Profit and loss**, before and after fees, both "realised" (from trades that have been closed out) and "marked to mid" (also valuing whatever is still held at the current mid price).
- **Fills and fill rate**: how many orders actually traded.
- **Inventory over time**: what the strategy was holding.
- **Max drawdown**: the largest fall from a high point to a later low point.
- **Markouts**: for each fill, where the mid price was 1, 10 and 60 seconds later. If the price tends to move against the position right after a fill, the fills are being adversely selected.
- **Data quality**: how many updates were checked, what fraction of checksums matched, and how many times the book had to be rebuilt.

## Every assumption, in one place

1. Orders join the back of the queue: everything displayed at that price when the order arrives is ahead of it.
2. Latency is a fixed 100 ms each way. Real latency varies.
3. Cancellations are spread evenly over the queue (or all behind the order, in pessimistic mode).
4. Trades and book updates arrive as separate messages. On Kraken the trade message has so far always arrived first. A drop in quantity that follows a trade within one second is treated as that trade and not as a cancellation. The reverse order is handled too.
5. If the order's price is further from the market than the part of the book being watched (25 price levels each side by default), the quantity there is unknown. It is treated as zero at first and corrected to the full displayed quantity the moment the level comes into view.
6. A trade reported at a price is assumed to have happened at that price for its whole quantity. Kraken describes the trade price as an average; in the recordings so far it has always been an exact price level.
7. The strategy only acts when the book changes, and waits for the exchange's acknowledgement before changing an order again.
8. Because news of fills is delayed, the strategy can briefly exceed its inventory limit by one order.
9. After a gap in the data, the simulation assumes nothing about what happened during it: the quantity ahead of an order can only stay the same or shrink to what is displayed.
10. Positions still open at the end are valued at the final mid price, with no cost for closing them.
11. **The market does not react to the pretend orders.** In reality, an order improves the visible price and other traders respond; and when the pretend order is filled, the real order that was actually filled would still be there. Both simulations share this limitation. It makes both look better than reality, so the honest result is still an upper bound.
12. The clock used for latency statistics is the local clock compared with Kraken's, so those figures include any difference between the two.

## What tickwire never does

It never logs in, never uses an API key, and contains no code that can place an order. It only listens to Kraken's public data.
