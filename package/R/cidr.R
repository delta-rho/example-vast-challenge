#' Get CIDR Mask
#' 
#' Get CIDR mask for a given routing prefix size
#' 
#' @param block number of leading 1 bits in the routing prefix mask (integer from 0 to 32)
#' 
#' @references \url{http://en.wikipedia.org/wiki/Cidr}
#' @author Ryan Hafen
#' @seealso \code{\link{ip2cidr}}
#' 
#' @export
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

#' Convert IP Address to CIDR
#' 
#' Convert IP address of form "x.x.x.x" to a CIDR block
#' 
#' @param ip IP address or vector of IP addresses
#' @param block routing prefix size (integer from 0 to 32)
#' 
#' @references \url{http://en.wikipedia.org/wiki/Cidr}
#' @author Ryan Hafen
#' @seealso \code{\link{getCidrMask}}
#' 
#' @examples
#' ip2cidr("192.168.0.1")
#' @export
ip2cidr <- function(ip, block = 24) {
   require(bitops)
   mask <- getCidrMask(block)
   splits <- strsplit(ip, "\\.")
   sapply(splits, function(x) {
      paste(paste(bitAnd(as.integer(x), mask), collapse="."), "/", block, sep = "")
   })
}
