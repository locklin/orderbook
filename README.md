Modification of the orderbook package on CRAN.

This consists of adding two ways of making a trade.
The original worked via some odd order book format I do not recognize (I 
vaguely recall ARCA looks like this).
The original thing's read.orders format, you had the tags A, R, T, and C 
which meant Add, Replace, Trade, and Cancel. 

"Trade" in the original framework was effectively a dark trade. So, you'd need to do
your own book keeping in making the data output to make crossing trades,
and remove things manually.

Since ITCH style files includes things like partial cancels and crosses, I added
"E" for partial execute, "F" for full execute and "P" for partial cancel of an order.

This is to make sense of the CSV data provided by tradingphysics.com (TP), 
which I believe is extremely close to native ITCH 4.0 format.

Thanks to Artem at tradingphysics.com for the sample data file.

TODO: really simple order book: same thing without order-id/time priority
TODO vacuum in the whole OB file as a data.table [0%] 
    - [ ] remove references to kane.tl in the read.orders.tp thing 
    - [ ] alternate to .read.orders.tp 
    - [ ] alternate to .get.time.row (presently broken anyway)
    - [ ] alternate to .get.next.trade (presently broken anyway)
    - [ ] remove .read.orders
    - [ ] WTF does midpoint.returns do? It doesn't do anything good yet
    - [ ] to.ms and to.time as exported methods for ms values


