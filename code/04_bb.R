



bbRaw <- ddf(localDiskConn("data/bbRaw"))
head(bbRaw[[1]][[2]][,-5])



bbHostFreq <- summary(bbRaw)$hostname$freqTable
head(bbHostFreq)





fields <- list(
   list(name = "disk", fields = "diskUsagePercent"),
   list(name = "page", fields = "pageFileUsagePercent"),
   list(name = "proc", fields = c("numProcs", "loadAveragePercent", "physicalMemoryUsagePercent")),
   list(name = "connMade", fields = "connMade")
)



bigIPs <- c("172.20.0.15", "172.20.0.4", "172.10.0.4", "172.30.0.4")

bbByHost <- divide(bbRaw, by = "hostIP",
   preTransFn = function(x) {
      x <- merge(x, hostList, by.x = "hostname", by.y = "hostName", all.x = TRUE)
      x <- subset(x, !x$IP %in% bigIPs)
      names(x)[names(x) == "IP"] <- "hostIP"
      x
   },
   postTransFn = function(x) {
      x <- x[order(x$time),]
      res <- list()
      for(fld in fields) {
         ind <- which(!is.na(x[[fld$fields[1]]]))
         if(length(ind) > 0) {
            nms <- c("receivedfrom", "time", fld$fields)
            res[[fld$name]] <- x[ind, nms]
         }
      }
      res
   },
   output = localDiskConn("data/bbByHost"),
   control = clc
)
bbByHost <- updateAttributes(bbByHost, control = clc)



d <- bbByHost[[11]][[2]]
xyplot(connMade ~ time, data = d$conn)
xyplot(numProcs ~ time, data = d$proc)
xyplot(physicalMemoryUsagePercent ~ time, data = d$proc)
xyplot(loadAveragePercent ~ time, data = d$proc)
xyplot(diskUsagePercent ~ time, data = d$disk)
xyplot(pageFileUsagePercent ~ time, data = d$page)



nfBbByHost <- drJoin(bb = bbByHost, nf = nfByHost, 
   output = localDiskConn("data/nfBbByHost"), control = clc)



str(nfBbByHost[[1]][[2]])


