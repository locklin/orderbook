\name{add.order.function}
\alias{add.order,orderbook-method}
\alias{remove.order,orderbook-method}
\alias{replace.order,orderbook-method}
\alias{market.order,orderbook-method}

\alias{add.order}
\alias{remove.order}
\alias{replace.order}
\alias{market.order}

\title{add.order}
\description{Modify the order book.}
\usage{
add.order(object, price, size, type, time = NULL, id = NULL, status =
FALSE)
remove.order(object, id)
replace.order(object, id, size)
market.order(object, size, type)
}
\arguments{
  \item{object}{ Object of class \code{orderbook}}
  \item{price}{ of the order
    to be added.}
  \item{size}{ the size of the order to
    be added. In the case of a \code{replace.order} it is the new size
  of the order. \code{market.order} takes this value to be the number
  of shares to be bought or sold.}
  \item{type}{ the user can specify "bid" or "ask." For a \code{market.order} the
    user can specify "buy" or "sell."}
  \item{time}{ of the order. If no time is specified the order book will automatically add
    1000ms to the current time for the new order.}
  \item{id}{ of the order. It must be unique. If no ID is specfied then \code{add.order} will
    automatically add 1 to the largest numeric ID and use it as the new
    ID.\code{cancel.order} and \code{replace.order} identify the
    order to remove/modify using the ID.}
  \item{status}{ indicating whether or
    not the order belongs to the user.}
}
\details{The user can add, remove, and replace orders from the order
  book. Additionally, the user can issue market orders. These functions
  are useful for simulating an order book.
}
\value{
  Return an object of class \code{orderbook}
}
\examples{
library(orderbook)
filename <- system.file("extdata", "sample.txt",
                        package = "orderbook")

ob <- orderbook(file = filename)

ob <- add.order(ob, price = 123, size = 123, type = "BID", time = 1, id = 1)
ob <- replace.order(ob, 1, 100)
ob <- market.order(ob, 50, "SELL")
ob <- remove.order(ob, 1)
}
\keyword{classes}