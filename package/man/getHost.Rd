\name{getHost}
\alias{getHost}
\title{Get Inside Host IP}
\usage{
getHost(x, src = "firstSeenSrcIp", dest = "firstSeenDestIp")
}
\arguments{
  \item{x}{data frame of netflow records}

  \item{src}{name of the column that contains the source
  IP}

  \item{dest}{name of the column that contains the
  destination IP}
}
\value{
data frame with additional columns \code{hostIP} and
\code{srcIsHost}
}
\description{
Take VAST netflow data and determine which IP is the inside
host (assumes no inside-to-inside connections)
}
\author{
Ryan Hafen
}

