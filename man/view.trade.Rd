\name{view.trade.function}

\alias{view.trade,orderbook-method}
\alias{next.trade,orderbook-method}
\alias{previous.trade,orderbook-method}
\alias{midpoint.return,orderbook-method}

\alias{previous.trade}
\alias{next.trade}
\alias{view.trade}
\alias{midpoint.return}

\title{view.trade}
\description{View the \code{orderbook} object when a particular trade happens.}
\usage{
view.trade(object, tradenum)
next.trade(object)
previous.trade(object)
midpoint.return(object, tradenum, time)
}
\arguments{
  \item{object}{ Object of class \code{orderbook}}
  \item{tradenum}{ the trade we want to view. }
  \item{time}{ Specifies the number of
    seconds after the trade to calculate the midpoint return for. Can
    also be a vector of times.}
}
\details{The orderbook object internally keeps track of the
the trades it has read from the data file. The view.trade function
returns information about the nth trade. This function can only view trades
that occurred prior to the current order.book time.

\code{previous.trade} sets the state of the orderbook to the first trade to
have happened before the current time. \code{next.trade} sets the state to the
orderbook at the very next trade to have happened after the current
time.

\code{midpoint.return} calculates the midpoint return for a trade a
specified number of seconds into the future.

}
\value{
  Return an object of class \code{orderbook}. \code{view.trade} and
  \code{midpoint.return} print information to the terminal about the
  trade and midpoint return, respectively.
}
\examples{
library(orderbook)
filename <- system.file("extdata", "sample.txt",
                        package = "orderbook")

ob <- orderbook(file = filename)
ob <- next.trade(ob)
view.trade(ob, 1)
midpoint.return(ob, 1, c(5, 10))
}
\keyword{file}