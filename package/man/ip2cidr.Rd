\name{ip2cidr}
\alias{ip2cidr}
\title{Convert IP Address to CIDR}
\usage{
ip2cidr(ip, block = 24)
}
\arguments{
  \item{ip}{IP address or vector of IP addresses}

  \item{block}{routing prefix size (integer from 0 to 32)}
}
\description{
Convert IP address of form "x.x.x.x" to a CIDR block
}
\examples{
ip2cidr("192.168.0.1")
}
\author{
Ryan Hafen
}
\references{
\url{http://en.wikipedia.org/wiki/Cidr}
}
\seealso{
\code{\link{getCidrMask}}
}

