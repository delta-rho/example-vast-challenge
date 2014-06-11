\name{mergeHostList}
\alias{mergeHostList}
\title{Merge hostList}
\usage{
mergeHostList(x, ipVar, original = FALSE)
}
\arguments{
  \item{x}{input data frame}

  \item{ipVar}{name of the variable in "x" that contains
  the host IP #param should the original host list provided
  be used?  If \code{FALSE} (default), modifications found
  throughout the analysis will be incorprated.}
}
\value{
data frame with merged host info
}
\description{
Merge \code{\link{hostList}} data with an input data frame
}
\author{
Ryan Hafen
}

