\name{read.time.function}

\alias{read.time,orderbook-method}
\alias{read.time}

\title{read.time}
\description{Returns the state of the orderbook at the specified time.}

\usage{
read.time(object, n)
}
\arguments{
  \item{object}{Object of class \code{orderbook}}
  \item{n}{ A \code{character} in the form \code{"HH:MM:SS"}.}
}
\details{Sets the state of the orderbook to the specified time. Can be used to
  move backwards or forwards in time.}
\value{
  Return an object of class \code{orderbook}
}
\examples{
library(orderbook)
filename <- system.file("extdata", "sample.txt",
                        package = "orderbook")

ob <- orderbook(file = filename)
ob <- read.time(ob, "9:30:00")
}
\keyword{file}