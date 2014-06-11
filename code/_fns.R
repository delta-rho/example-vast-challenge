# additional packages to attach
library(lattice)
library(plyr)
# library(ed)

load("data/hostList.Rdata")

# merges hostList data frame with an input data frame "x"
# ipVar is the name of the variable in "x" that contains the host IP
mergeHostList <- function(x, ipVar) {
   x <- merge(x, hostList, by.x = ipVar, by.y = "IP", all.x = TRUE)
   x$type[grepl("^10\\.", x[[ipVar]])] <- "External"
   x$type[grepl("^172\\.", x[[ipVar]]) & is.na(x$type)] <- "Other 172.*"
   x
}

# utility function to compute quantiles with each group
# specified by "by"
groupQuantile <- function(x, by, var = "Freq") {
   require(plyr)
   ddply(x, by, function(a) {
      a <- a[order(a[[var]]),]
      data.frame(a, p = ppoints(nrow(a)))
   })
}

# get the subnet bitmask for a given block
getCidrMask <- function(block) {
   if(block > 32) block <- 32
   if(block < 1) block <- 1
   mask <- rep(0, 4)
   octets <- floor(block / 8)
   mask[seq_len(octets)] <- 255
   
   rem <- block %% 8
   if(octets < 4)
      mask[octets + 1] <- c(0, 128, 192, 224, 240, 248, 252, 254)[rem + 1]   
   mask
}

# convert an IP address to a CIDR block
ip2cidr <- function(ip, block = 24) {
   require(bitops)
   mask <- getCidrMask(block)
   splits <- strsplit(ip, "\\.")
   sapply(splits, function(x) {
      paste(paste(bitAnd(as.integer(x), mask), collapse="."), "/", block, sep = "")
   })
}

# http://en.wikipedia.org/wiki/List_of_TCP_and_UDP_port_numbers
commonPortList <- list(
   "80"   = list(name="HTTP",   TCP=TRUE,  UDP=FALSE, desc="Hypertext Transfer Protocol"),
   "3389" = list(name="RDP",    TCP=TRUE,  UDP=TRUE,  desc="Microsoft Terminal Server"),
   "1900" = list(name="SSDP",   TCP=FALSE, UDP=TRUE,  desc="Microsoft Simple Service Discovery Protocol (UPnP discovery)"),
   "25"   = list(name="SMTP",   TCP=TRUE,  UDP=FALSE, desc="Simple Mail Transfer Protocol"),
   "138"  = list(name="nBIOSd", TCP=TRUE,  UDP=TRUE,  desc="NetBIOS Datagram Service"),
   "137"  = list(name="nBIOSn", TCP=TRUE,  UDP=TRUE,  desc="NetBIOS Name Service"),
   "123"  = list(name="NTP",    TCP=FALSE, UDP=TRUE,  desc="Network Time Protocol"),
   "0"    = list(name="Res",    TCP=FALSE, UDP=TRUE,  desc="Reserved"),
   "22"   = list(name="SSH",    TCP=TRUE,  UDP=TRUE,  desc="Secure Shell Protocol"),
   "5355" = list(name="LLMNR",  TCP=TRUE,  UDP=TRUE,  desc="Link-Local Multicast Name Resolution")
)
commonPorts <- as.integer(names(commonPortList))

# LLMNR allows hosts to perform name resolution for hosts on the same local link (only provided by Windows Vista and Server 2008)
# SMTP used for email routing between mail servers
# NetBIOS allows applications on separate computers to communicate over a local area network

# take netflow data and determine which IP is the inside host
# append the variable and return
# (assumes no inside-to-inside connections, which we have verified)
getHost <- function(x) {
   srcIsHost <- x$firstSeenSrcIp %in% hostList$IP || x$firstSeenSrcIp %in% hostList$externalIP
   destIsHost <- x$firstSeenDestIp %in% hostList$IP || x$firstSeenDestIp %in% hostList$externalIP
   # all((srcIsHost | destIsHost))
   # any(srcIsHost & destIsHost)
   
   x$hostIP <- NA
   if(length(which(srcIsHost)) > 0)
      x$hostIP[srcIsHost] <- x$firstSeenSrcIp[srcIsHost]
   if(length(which(destIsHost)) > 0)
   x$hostIP[destIsHost] <- x$firstSeenDestIp[destIsHost]
   x$srcIsHost <- srcIsHost
   x
}


v <- 10^c(0:10)
log10labels <- v
log10at <- log10(v)

v <- c(0, 10^c(1:10))
log10p1labels <- v
log10p1at <- log10(v + 1)
log10p1scales <- list(at=log10p1at, labels=log10p1labels)

log10p1panel <- function(x, y, logx = FALSE, logy = FALSE, grid = FALSE, ...) {
   cl <- trellis.par.get("reference.line")$col
   if(grid) {
      if(logy) {
         panel.abline(h=log10p1at, col = cl)      
      } else {
         panel.grid(h=-1, v=FALSE)
      }
      if(logx) {
         panel.abline(v=log10p1at, col = cl)      
      } else {
         panel.grid(v=-1, h=FALSE)
      }      
   }
   panel.xyplot(x, y, ...)
}

# color schemes from Tableau
tableau20 <- c("#1f77b4", "#aec7e8", "#ff7f0e", "#ffbb78", "#2ca02c", "#98df8a", "#d62728", "#ff9896", "#9467bd", "#c5b0d5", "#8c564b", "#c49c94", "#e377c2", "#f7b6d2", "#7f7f7f", "#c7c7c7", "#bcbd22", "#dbdb8d", "#17becf", "#9edae5")

tableau10 <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
