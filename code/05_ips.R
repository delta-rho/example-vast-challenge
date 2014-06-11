



head(ipsRaw[[1]][[2]])



summary(ipsRaw)$flags$freqTab



ipsByHost <- divide(ipsRaw, by = "hostIP",
   preTransFn = function(x) {
      names(x)[names(x) == "destIp"] <- "destIpNat"
      # need to un-NAT the destination IP
      x <- merge(x, hostList[,c("IP", "externalIP")], by.x = "destIpNat", by.y = "externalIP", all.x = TRUE)
      names(x)[ncol(x)] <- "destIp"
      ind <- is.na(x$destIp)
      x$destIp[ind] <- x$destIpNat[ind]
      x <- getHost(x, src = "srcIp", dest = "destIp")
      subset(x, !x$hostIP %in% bigIPs)
   },
   output = localDiskConn("data/ipsByHost"),
   control = clc
)
ipsByHost <- updateAttributes(ipsByHost, control = clc)



ipsRows <- recombine(ipsByHost, apply = nrow, combine = combRbind(), control = clc)
save(ipsRows, file = "data/artifacts/ipsRows.Rdata")



plot(log10(sort(ipsRows$val)))



ipsBig <- subset(ipsRows, val > 90000)
merge(ipsBig, hostList, by. = "hostIP", by.y = "IP")


