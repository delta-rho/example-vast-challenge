



load("data/artifacts/bigTimeAgg.Rdata")
bigTimes <- sort(unique(bigTimeAgg$timeMinute[bigTimeAgg$Freq > 1000]))

bigIPs <- c("172.20.0.15", "172.20.0.4", "172.10.0.4", "172.30.0.4")
badIPs <- c("10.138.214.18", "10.17.15.10", "10.12.15.152", "10.170.32.110", "10.170.32.181", "10.10.11.102", "10.247.106.27", "10.247.58.182", "10.78.100.150", "10.38.217.48", "10.6.6.7", "10.12.14.15", "10.15.7.85", "10.156.165.120", "10.0.0.42", "10.200.20.2", "10.70.68.127", "10.138.235.111", "10.13.77.49", "10.250.178.101")



nfByHost <- divide(nfRaw, by = "hostIP",
   preTransFn = function(x) {
      library(cyberTools)
      x$timeMinute <- as.POSIXct(trunc(x$date, 0, units = "mins"))
      x <- subset(x, !(timeMinute %in% bigTimes & 
         firstSeenSrcIp %in% c(bigIPs, badIPs) & 
         firstSeenDestIp %in% c(bigIPs, badIPs)))
      getHost(x)
   },
   output = localDiskConn("data/nfByHost"),
   control = clc
)
nfByHost <- updateAttributes(nfByHost, control = clc)



nfByHost



plot(log10(splitRowDistn(nfByHost)))





hostTimeAgg <- recombine(nfByHost, 
   apply = function(x) {
      timeHour <- as.POSIXct(trunc(x$date, 0, units = "hours"))
      res <- data.frame(xtabs(~ timeHour))
      res$timeHour <- as.POSIXct(res$timeHour)
      res
   }, 
   combine = combDdo(), control = clc)
save(hostTimeAgg, file = "data/artifacts/hostTimeAgg.Rdata")



hostTimeAggDF <- recombine(hostTimeAgg, 
   apply = identity, 
   combine = combRbind())
save(hostTimeAggDF, file = "data/artifacts/hostTimeAggDF.Rdata")



xyplot(sqrt(Freq) ~ timeHour, data = hostTimeAggDF, alpha = 0.5)



library(trelliscope)
vdbConn("vdb")



timePanel <- function(x) {
   xyplot(sqrt(Freq) ~ timeHour, data = x, type = c("p", "g"))
}
timePanel(hostTimeAgg[[1]][[2]])



timeCog <- function(x) {
   IP <- attr(x, "split")$hostIP
   curHost <- hostList[hostList$IP == IP,]
   
   list(
      hostName = cog(curHost$hostName, desc = "host name"),
      type = cog(curHost$type, desc = "host type"),
      nobs = cog(sum(x$Freq), "log 10 total number of connections"),
      timeCover = cog(nrow(x), desc = "number of hours containing connections"),
      medHourCt = cog(median(sqrt(x$Freq)), 
         desc = "median square root number of connections"),
      madHourCt = cog(mad(sqrt(x$Freq)), 
         desc = "median absolute deviation square root number of connections"),
      max = cog(max(x$Freq), desc = "maximum number of connections in an hour")
   )
}

timeCog(hostTimeAgg[[1]][[2]])



makeDisplay(hostTimeAgg,
   name = "hourly_count",
   group = "inside_hosts",
   desc = "time series plot of hourly counts of connections for each inside host",
   panelFn = timePanel,
   panelDim = list(width = 800, height = 400),
   cogFn = timeCog,
   lims = list(x = "same", y = "same"))



hostTimeDirAgg <- recombine(nfByHost, 
   apply = function(x) {
      x$timeHour <- as.POSIXct(trunc(x$date, 0, units = "hours"))
      res <- data.frame(xtabs(~ timeHour + srcIsHost, data = x))
      res$timeHour <- as.POSIXct(res$timeHour)
      res$direction <- "incoming"
      res$direction[as.logical(as.character(res$srcIsHost))] <- "outgoing"
      subset(res, Freq > 0)
   }, 
   combine = combDdo(), control = clc)
save(hostTimeDirAgg, file = "data/artifacts/hostTimeDirAgg.Rdata")



timePanelDir <- function(x) {
   xyplot(sqrt(Freq) ~ timeHour, groups = direction, data = x, type = c("p", "g"), auto.key = TRUE)
}

makeDisplay(hostTimeDirAgg,
   name = "hourly_count_src_dest",
   group = "inside_hosts",
   desc = "time series plot of hourly counts of connections for each inside host by source / destination",
   panelFn = timePanelDir,
   panelDim = list(width = 800, height = 400),
   cogFn = timeCog,
   lims = list(x = "same", y = "same"))





bigHosts <- nfByHost[paste("hostIP=", 
   c("172.10.2.106", "172.30.1.218", "172.20.1.23", 
   "172.10.2.135", "172.20.1.81", "172.20.1.47", 
   "172.30.1.223", "172.10.2.66"), sep = "")]



hostOne <- bigHosts[[1]][[2]]
hostOne <- subset(hostOne, srcIsHost)
table(hostOne$firstSeenDestPort)



hostOne$timeHour <- as.POSIXct(trunc(hostOne$date, 0, units = "hours"), tz = "UTC")
hostOneTab <- data.frame(xtabs(~ firstSeenDestPort + timeHour, data = hostOne))
hostOneTab$timeHour <- as.POSIXct(hostOneTab$timeHour)
hostOneTab <- subset(hostOneTab, Freq > 0)

xyplot(sqrt(Freq) ~ timeHour, groups = firstSeenDestPort, 
   data = hostOneTab, auto.key = TRUE, type = c("p", "g"))



subset(hostOneTab, Freq > 1500)



spike1 <- subset(hostOne, 
   date >= as.POSIXct("2013-04-13 07:00:00", tz = "UTC") &
   date <= as.POSIXct("2013-04-13 09:00:00", tz = "UTC") & 
   firstSeenDestPort == 80)
table(spike1$firstSeenDestIp)





spike1$logPB <- log10(spike1$firstSeenDestPayloadBytes + 1)
spike1pbQuant <- groupQuantile(spike1, "firstSeenDestIp", "logPB")

xyplot(logPB ~ p * 100 | firstSeenDestIp, data = spike1pbQuant,
   xlab = "Percentile",
   ylab = "log10(firstSeenDestPayload + 1)",
   layout = c(11, 1),
   between = list(x = 0.25)
)



spike2 <- subset(hostOne, 
   date >= as.POSIXct("2013-04-14 07:00:00") & 
   date <= as.POSIXct("2013-04-14 09:00:00"))
table(spike2$firstSeenDestIp)



nrow(subset(hostOne, firstSeenDestIp == "10.1.0.100"))



{
x <- nfByHost[[1]][[2]]

portProp <- drLapply(nfByHost, function(x) {
   commonPortDF2 <- droplevels(subset(commonPortDF, portName %in% c("RDP", "SMTP", "SSH", "HTTP")))
   tmp <- x[,c("firstSeenDestPort", "srcIsHost")]
   names(tmp)[1] <- "port"
   tmp$dir <- "in"
   tmp$dir[tmp$srcIsHost] <- "out"
   tmp$dir <- factor(tmp$dir, levels = c("in", "out"))
   tmp <- merge(tmp, commonPortDF2)
   a1 <- data.frame(xtabs(~ portName + dir, data = tmp))

   tmp <- x[,c("firstSeenSrcPort", "srcIsHost")]
   names(tmp)[1] <- "port"
   tmp$dir <- "out"
   tmp$dir[tmp$srcIsHost] <- "in"
   tmp$dir <- factor(tmp$dir, levels = c("in", "out"))
   tmp <- merge(tmp, commonPortDF2)
   a2 <- data.frame(xtabs(~ portName + dir, data = tmp))

   res <- data.frame(t(a1$Freq + a2$Freq))
   res[1:4] <- res[1:4] / max(sum(res[1:4]), 1)
   res[5:8] <- res[5:8] / max(sum(res[5:8]), 1)
   names(res) <- paste(a1$portName, a1$dir, sep = "_")
   res
}, combine = combRbind(), control = clc)


library(flexclust)

#Perform k-means clustering

portClust <- stepFlexclust(portProp[,-1], k = 4:12)
plot(portClust)

portClust <- kcca(portProp[,-1], k = 10)

portProp2 <- merge(portProp, hostList, by.x = "hostIP", by.y = "IP")
portProp2$cluster <- portClust@cluster

subset(portProp2, cluster == 1)
}



nfPanel <- function(x) {
   x$group <- ifelse(x$firstSeenSrcIp == attributes(x)$split$hostIP, "sending", "receiving")
   x$group <- factor(x$group, levels = c("sending", "receiving"))
   x$zeroDur <- ifelse(x$durationSeconds == 0, "0 seconds", ">0 seconds")
   x$zeroDur <- factor(x$zeroDur, c("0 seconds", ">0 seconds"))
   xyplot(log10(firstSeenSrcPayloadBytes + 1) ~ log10(firstSeenDestPayloadBytes + 1) | zeroDur, groups = group, data = x, 
      auto.key = TRUE, 
      # panel = log10p1panel,
      # scales = log10p1scales,
      between = list(x = 0.25),
      grid = TRUE, logx = TRUE, logy = TRUE,
      xlab = "log10(Destination Payload Bytes + 1)",
      ylab = "log10(Source Payload Bytes + 1)"
   )
}

nfPanel(nfByHost[[1]][[2]])

nfCog <- function(x) {
   IP <- attr(x, "split")$hostIP
   curHost <- hostList[hostList$IP == IP,]
   
   c(list(
      hostName = cog(curHost$hostName, desc = "host name"),
      IP = cog(IP, desc = "host IP address"),
      type = cog(curHost$type, desc = "host type"),
      nobs = cog(log10(nrow(x)), "log 10 total number of connections"),
      propZeroDur = cog(length(which(x$durationSeconds == 0)), desc = "proportion of zero duration connections")
   ),
   cogScagnostics(log10(x$firstSeenSrcPayloadBytes + 1), 
      log10(x$firstSeenDestPayloadBytes + 1)))
}

nfCog(nfByHost[[1]][[2]])



makeDisplay(nfByHost,
   name = "srcPayload_vs_destPayload",
   panelFn = nfPanel,
   cogFn = nfCog,
   control = clc,
   panelDim = list(width = 900, height = 600))



nfByExtHost <- divide(nfByHost, by = "extIP",
   preTransFn = function(x) {
      x$extIP <- x$firstSeenSrcIp
      x$extIP[x$srcIsHost] <- x$firstSeenDestIp[x$srcIsHost]
      x
   },
   output = localDiskConn("data/nfByExtHost"),
   control = clc
)
nfByExtHost <- updateAttributes(nfByExtHost, control = clc)





nfByTime <- divide(nfByHost, by = "time10",
   preTransFn = function(x) {
      tmp <- paste(substr(x$date, 1, 15), "0:00", sep = "")
      x$time10 <- as.POSIXct(tmp, tz = "UTC")
      x
   },
   output = localDiskConn("data/nfByTime"),
   control = clc
)
nfByTime <- updateAttributes(nfByTime, control = clc)








