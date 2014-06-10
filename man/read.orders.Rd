\name{read.orders.function}
\alias{read.orders,orderbook-method}
\alias{read.orders}

\title{read.orders}
\description{Read the next n messages of the data file; now ITCH 4.0 compatible!}
\usage{
read.orders(object, n = 1000)
}
\arguments{
  \item{object}{ Object of class \code{orderbook}}
  \item{n}{ number of messages we want to read.}
}
\details{The orderbook keeps track of its current position within the
  data file. The read.orders function will read and
  process the next n messages from the file.
  The data file has the following format keys, with needed information
  for each line with the given keys:
  \itemize{
    \item{A: }{Add order: A,time,id,price,size,BID/ASK}
    \item{R: }{Replace order: R,time,id}
    \item{T: }{Trade: dark cross trade, just prints to accumulated trades
      T,time,id,price,size}
    \item{C: }{Cancel in full: C,time,id}
    \item{P: }{Partial cancel: P,time,id,size}
    \item{E: }{Execute, partial: E,time,id,size}
    \item{F: }{execute, Full: F,time,id}
  }
  It also reads the ITCH 4.0 formatted CSV files provided by TradingPhysics.
}
\value{
  Return an object of class \code{orderbook}
}
\examples{
library(orderbook)
filename <- system.file("extdata", "sample.txt",
                        package = "orderbook")

ob <- orderbook(file = filename)
ob <- read.orders(ob, 100)
}
\keyword{file}