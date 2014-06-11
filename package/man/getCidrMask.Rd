\name{getCidrMask}
\alias{getCidrMask}
\title{Get CIDR Mask}
\usage{
getCidrMask(block)
}
\arguments{
  \item{block}{number of leading 1 bits in the routing
  prefix mask (integer from 0 to 32)}
}
\description{
Get CIDR mask for a given routing prefix size
}
\author{
Ryan Hafen
}
\references{
\url{http://en.wikipedia.org/wiki/Cidr}
}
\seealso{
\code{\link{ip2cidr}}
}

