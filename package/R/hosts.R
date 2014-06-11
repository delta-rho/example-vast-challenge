#' Merge hostList
#' 
#' Merge \code{\link{hostList}} data with an input data frame
#' 
#' @param x input data frame
#' @param ipVar name of the variable in "x" that contains the host IP
#' #param should the original host list provided be used?  If \code{FALSE} (default), modifications found throughout the analysis will be incorprated.
#' 
#' @return data frame with merged host info
#' 
#' @author Ryan Hafen
#' 
#' @export
mergeHostList <- function(x, ipVar, original = FALSE) {
   if(original) {
      x <- merge(x, hostListOrig, by.x = ipVar, by.y = "IP", all.x = TRUE, sort = FALSE)
      otherInd <- which(grepl("^172\\.", x[[ipVar]]) & is.na(x$type))
      x$type[otherInd] <- "Other 172.*"      
   } else {
      x <- merge(x, hostList, by.x = ipVar, by.y = "IP", all.x = TRUE, sort = FALSE)
      
   }
   x$type[grepl("^10\\.", x[[ipVar]])] <- "External"
   x$type[is.na(x$type)] <- "Other"
   
   x
}

#' Get Inside Host IP
#' 
#' Take VAST netflow data and determine which IP is the inside host (assumes no inside-to-inside connections)
#' 
#' @param x data frame of netflow records
#' @param src name of the column that contains the source IP
#' @param dest name of the column that contains the destination IP
#' 
#' @return data frame with additional columns \code{hostIP} and \code{srcIsHost}
#' 
#' @author Ryan Hafen
#' 
#' @export
getHost <- function(x, src = "firstSeenSrcIp", dest = "firstSeenDestIp") {
   goodIPs <- hostList$IP[hostList$type != "Other 172.*"]
   srcIsHost <- x[[src]] %in% goodIPs
   destIsHost <- x[[dest]] %in% goodIPs
   x$hostIP <- NA
   if(length(which(srcIsHost)) > 0)
      x$hostIP[srcIsHost] <- x[[src]][srcIsHost]
   if(length(which(destIsHost)) > 0)
      x$hostIP[destIsHost] <- x[[dest]][destIsHost]
   x$srcIsHost <- srcIsHost
   subset(x, !is.na(hostIP))
}
