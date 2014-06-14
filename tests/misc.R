## works; convert to RUnit later
## with checkEqualsNumeric calls and wrapping as test.function()
library(orderbook)
library(RUnit)


filename <- system.file("extdata", "testTP.csv",
                        package = "orderbook")

ob <- orderbook(file = filename)
ob5k <- read.orders(ob, 5000)


## Best bid

stopifnot(isTRUE(identical(1811000, best.bid(ob5k)[[1]])))

## Best ask

stopifnot(isTRUE(identical(1811700 , best.ask(ob5k)[[1]])))

## Bid Price Levels

stopifnot(isTRUE(identical(as.integer(161), bid.price.levels(ob5k))))

## Ask Price Levels

stopifnot(isTRUE(identical(as.integer(161), ask.price.levels(ob5k))))

## Total Price Levels

stopifnot(isTRUE(identical(as.integer(322), total.price.levels(ob5k))))

## Bid orders

stopifnot(isTRUE(identical(292, bid.orders(ob5k))))

## Ask orders

stopifnot(isTRUE(identical(297, ask.orders(ob5k))))

## Total orders

stopifnot(isTRUE(identical(589, total.orders(ob5k))))

## Midpoint

stopifnot(isTRUE(identical(1811350, mid.point(ob5k)[[1]])))

## Inside.market

test <- inside.market(ob5k, invis = TRUE)

stopifnot(isTRUE(identical(1811700, test[[1]])))
stopifnot(isTRUE(identical(1811000, test[[2]])))
stopifnot(isTRUE(identical(3300, test[[3]])))
stopifnot(isTRUE(identical(100, test[[4]])))

## Spread

stopifnot(isTRUE(identical(700, spread(ob5k))))

## "["

ids = c("8747983", "8747984", "8747971", "8747978", "8748246", "8748620", "8748050", "8748054")

stopifnot(isTRUE(all(ids %in% ob5k["1640500"]$id)))

## Midpoint Returns test BROKEN; FIX0r -SCL

## midpoints <- midpoint.return(ob5k, 5, c(5, 10))

## stopifnot(isTRUE(identical(round(midpoints[[1]], 2), 0.02)))
## stopifnot(isTRUE(identical(round(midpoints[[2]], 2), 0.02)))
