\name{itch.to.orderbook}

\alias{itch.to.orderbook,itch.to.kane}
\alias{itch.to.orderbook}

\title{itch.to.orderbook}
\description{Converts a NASDAQ ITCH CSV file (as provided by Trading
  Physics) to   the format  required by this package}

\usage{
itch.to.orderbook(xx, filen)
}
\arguments{
  \item{xx}{Object of class \code{data.frame}}
  \item{filen}{ Output file for order book.}
}
\details{A (slow) convenience function for converting ITCH formatted CSV
  files to the format required here.}
\value{
  Returns nil
}
  \examples{
\dontrun{

library(orderbook)
library(data.table)
itch.to.order(fread("myTpFile"),"~/out")
ob <- orderbook(file = "~/out")
ob <- read.time(ob, "9:30:00")
}
}
\keyword{file}