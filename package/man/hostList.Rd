\docType{data}
\name{hostList}
\alias{hostList}
\title{"Internal Host List" Dataset}
\format{\itemize{
   \item IP: host IP address
   \item hostName: name of host
   \item type: host type (HTTP, SMTP, Workstation, etc.)
   \item externalIP: external IP address for internal hosts are NATed
}}
\source{
Link:
\url{http://vacommunity.org/VAST+Challenge+2013%3A+Mini-Challenge+3}
}
\usage{
hostList
}
\description{
List of internal hosts from VAST Cyber Challenge data
}
\note{
\code{hostListOrig} is the original host list provided with
the data.  \code{hostList} contains some modifications that
have been made after exploring the data.
}
\keyword{data}

