



# load our data back if we are in a new session
nfRaw <- ddf(localDiskConn("data/nfRaw"))
nfRaw



# see what variables are available
names(nfRaw)



# get total number of rows
nrow(nfRaw)



# look at the structure of the first key-value pair
str(nfRaw[[1]])



# look at summaries (computed from updateAttributes)
summary(nfRaw)



# grab the full frequency table for firstSeenSrcIp
srcIpFreq <- summary(nfRaw)$firstSeenSrcIp$freqTable
# look at the top few IPs
head(srcIpFreq)



head(hostListOrig)



srcIpFreq <- mergeHostList(srcIpFreq, "value", original = TRUE)
head(srcIpFreq)



# see how many of each type we have
table(srcIpFreq$type)





# look at 172.x addresses that aren't in our host list
sort(subset(srcIpFreq, type == "Other 172.*")$value)



hostListOrig$IP[grepl("172\\.20\\.1", hostListOrig$IP)]



srcIpFreq <- summary(nfRaw)$firstSeenSrcIp$freqTable
srcIpFreq <- mergeHostList(srcIpFreq, "value")
table(srcIpFreq$type)



# for each type, get the quantiles
srcIpFreqQuant <- groupQuantile(srcIpFreq, "type")

# quantile plot by host type
xyplot(Freq ~ p | type, data = srcIpFreqQuant, 
   layout = c(7, 1), type = c("p", "g"), 
   between = list(x = 0.25), 
   scales = list(y = list(log = 10)),
   xlab = "Sample Fraction",
   ylab = "Number of Connections as Source IP"
)



destIpFreq <- summary(nfRaw)$firstSeenDestIp$freqTable
destIpFreq <- mergeHostList(destIpFreq, "value", original = TRUE)





subset(destIpFreq, type == "Other")



destIpFreq <- summary(nfRaw)$firstSeenDestIp$freqTable
destIpFreq <- mergeHostList(destIpFreq, "value")



subset(destIpFreq, type == "Other 172.*")



destIpFreqQuant <- groupQuantile(destIpFreq, "type")

srcDestIpFreqQuant <- make.groups(source = srcIpFreqQuant, destination = destIpFreqQuant)

xyplot(Freq ~ 100 * p | type, groups = which, 
   data = srcDestIpFreqQuant, 
   layout = c(7, 1), type = c("p", "g"), 
   between = list(x = 0.25), 
   scales = list(y = list(log = 10)),
   xlab = "Percentile",
   ylab = "Number of Connections",
   subset = type != "Other",
   auto.key = TRUE
)



freqMerge <- merge(srcIpFreq, destIpFreq[,c("value", "Freq", "type")], by="value",
suffixes = c(".src", ".dest"), all = TRUE)
freqMerge$type <- freqMerge$type.src
freqMerge$type[is.na(freqMerge$type)] <- freqMerge$type.dest[is.na(freqMerge$type)]
freqMerge$Freq.src[is.na(freqMerge$Freq.src)] <- 0
freqMerge$Freq.dest[is.na(freqMerge$Freq.dest)] <- 0

xyplot(log10(Freq.dest + 1) ~ log10(Freq.src + 1) | type, data = freqMerge,
   # scales = list(relation = "free"),
   xlab = "log10 number of times host is first seen source",
   ylab = "log10 number of times host is first seen dest",
   # subset = !type %in% c("SMTP", "Administrator", "Domain controller"),
   type = c("p", "g"),
   panel = function(x, y, ...) {
      panel.xyplot(x, y, ...)
      panel.abline(a = 0, b = 1)
   },
   between = list(x = 0.25, y = 0.25),
   as.table = TRUE,
   aspect = "iso"
)





freqMerge$tot <- freqMerge$Freq.src + freqMerge$Freq.dest
topTot <- head(freqMerge[order(freqMerge$tot, decreasing = TRUE),], 10)
topTot



bigIPs <- topTot$value[1:4]



# aggregate by minute and IP for just "bigIPs"
bigTimeAgg <- drAggregate(~ timeMinute + firstSeenDestIp, data = nfRaw, transFn = function(x) {
   x <- subset(x, firstSeenDestIp %in% bigIPs)
   x$timeMinute <- as.POSIXct(trunc(x$date, 0, units = "mins"))
   x
}, control = clc)
# sort by IP and time
bigTimeAgg <- bigTimeAgg[order(bigTimeAgg$firstSeenSrcIp, bigTimeAgg$timeMinute),]
# convert time back to POSIXct
bigTimeAgg$timeMinute <- as.POSIXct(bigTimeAgg$timeMinute, tz = "UTC")
save(bigTimeAgg, file = "data/artifacts/bigTimeAgg.Rdata")



xyplot(Freq ~ timeMinute | firstSeenDestIp, 
   data = bigTimeAgg, 
   layout = c(1, 4), as.table = TRUE, 
   strip = FALSE, strip.left = TRUE, 
   between = list(y = 0.25),
   type = c("p", "g"))



bigTimeAgg[which.max(bigTimeAgg$Freq),]



# retrieve rows from netflow data with highest count
busiest <- drSubset(nfRaw, 
   (firstSeenDestIp == "172.30.0.4" | firstSeenSrcIp == "172.30.0.4") &
   trunc(date, 0, units = "mins") == as.POSIXct("2013-04-11 12:55:00", tz = "UTC"), 
   control = clc)
# order by time
busiest <- busiest[order(busiest$date),]
save(busiest, file = "data/artifacts/busiest.Rdata")



table(busiest$firstSeenSrcIp)



busiest$cumulative <- seq_len(nrow(busiest))
xyplot(cumulative ~ date | firstSeenSrcIp, data = busiest, pch = ".",
   xlab = "Time (seconds)",
   ylab = "Cumulatuve Number of Connections",
   between = list(x = 0.25, y = 0.25),
   layout = c(3, 3),
   type = c("p", "g"),
   strip = FALSE, strip.left = TRUE
)



table(subset(busiest, firstSeenSrcIp == "172.30.0.4")$firstSeenSrcPort)



busiest2 <- busiest[busiest$firstSeenSrcIp != "172.30.0.4",]
table(busiest2$firstSeenDestPort)



table(busiest2$firstSeenSrcPayloadBytes)



# get all times with more than 1000 hits in a minute
bigTimes <- sort(unique(bigTimeAgg$timeMinute[bigTimeAgg$Freq > 1000]))



xyplot(Freq ~ timeMinute | firstSeenDestIp, data = bigTimeAgg, 
   layout = c(1, 4), 
   strip = FALSE, strip.left = TRUE, 
   as.table = TRUE, 
   between = list(y = 0.25), 
   groups = timeMinute %in% bigTimes)



bigTimesHostAgg <- drAggregate(~ firstSeenSrcIp, by = "firstSeenDestIp", 
   data = nfRaw, 
   transFn = function(x) {
      x$timeMinute <- as.POSIXct(trunc(x$date, 0, units = "mins"))
      x <- subset(x, firstSeenDestIp %in% bigIPs & timeMinute %in% bigTimes)
      x
   }, 
   control = clc)
save(bigTimesHostAgg, file = "data/artifacts/bigTimesHostAgg.Rdata")



lapply(bigTimesHostAgg, function(x) x[x$Freq > 100000,])



# get all IPs involved in the DDoS
badIPs <- unique(do.call(c, lapply(bigTimesHostAgg, 
   function(x) x$firstSeenSrcIp[x$Freq > 100000])))
# do these match with the large values in srcIpFreq for "External"?
head(subset(srcIpFreq, type == "External"), 20)



timeAgg <- drAggregate(~ timeMinute + firstSeenDestIp, data = nfRaw, 
   transFn = function(x) {
      x$timeMinute <- as.POSIXct(trunc(x$date, 0, units = "mins"))
      subset(x, firstSeenDestIp %in% bigIPs &
         !(timeMinute %in% bigTimes & 
         firstSeenSrcIp %in% c(bigIPs, badIPs) & 
         firstSeenDestIp %in% c(bigIPs, badIPs)))
   }, control = clc)
timeAgg <- timeAgg[order(timeAgg$timeMinute),]
timeAgg$timeMinute <- as.POSIXct(timeAgg$timeMinute, tz = "UTC")
save(timeAgg, file = "data/artifacts/timeAgg.Rdata")



xyplot(log10(Freq + 1) ~ timeMinute | firstSeenDestIp, 
   data = timeAgg, 
   layout = c(1, 4), as.table = TRUE, 
   strip = FALSE, strip.left = TRUE, 
   between = list(y = 0.25),
   type = c("p", "g"))



data.frame(xtabs(~ Species, data = iris))



srcIpByte <- drAggregate(firstSeenSrcPayloadBytes ~ firstSeenSrcIp, 
   data = nfRaw, control = clc)
# merge in hostList
srcIpByte <- mergeHostList(srcIpByte, "firstSeenSrcIp")
save(srcIpByte, file = "data/artifacts/srcIpByte.Rdata")



head(srcIpByte)





srcIpByteQuant <- groupQuantile(srcIpByte, "type")
# quantile plot by host type
xyplot(log10(Freq) ~ p | type, data = srcIpByteQuant, layout = c(7, 1))



# look at distribution for workstations only
wFreq <- log2(subset(srcIpByteQuant, type == "Workstation")$Freq)
histogram(~ wFreq, breaks = 100, col = "darkgray", border = "white")



subset(srcIpByteQuant, Freq > 2^20 & type == "Workstation")



histogram(~ wFreq[wFreq < 20], breaks = 30, col = "darkgray", border = "white")





set.seed(1234)



library(mixtools)
mixmdl <- normalmixEM(wFreq[wFreq < 20], mu = c(16.78, 17.54, 18.2))
plot(mixmdl, which = 2, main2 = "", breaks = 50)
breakPoints <- c(17.2, 17.87)
abline(v = breakPoints)





# categorize IPs
srcIpByte$byteCat <- cut(log2(srcIpByte$Freq), 
   breaks = c(0, breakPoints, 100), labels = c("low", "mid", "high"))

# create CIDR for subnets
srcIpByte$cidr24 <- ip2cidr(srcIpByte$firstSeenSrcIp, 24)

# tabulate by CIDR and category
cidrCatTab <- xtabs(~ cidr24 + byteCat, data = subset(srcIpByte, type == "Workstation"))
cidrCatTab



# mosaic plot
plot(cidrCatTab, color = tableau10[1:3], border = FALSE, main = NA)



srcByteQuant <- groupQuantile(
   subset(srcIpByte, type == "Workstation" & Freq < 2^20), "cidr24")

xyplot(log2(Freq) ~ p | cidr24, data = srcByteQuant,
   panel = function(x, y, ...) {
      panel.xyplot(x, y, ...)
      panel.abline(h = breakPoints, lty = 2, col = "darkgray")
   },
   between = list(x = 0.25),
   layout = c(6, 1)
)



# see if there are any inside-inside connections
srcDestInsideTab <- drAggregate(~ srcCat + destCat, data = nfRaw, 
   transFn = function(x) {
      x$srcCat <- "outside"
      x$srcCat[grepl("^172", x$firstSeenSrcIp)] <- "inside"
      x$destCat <- "outside"
      x$destCat[grepl("^172", x$firstSeenDestIp)] <- "inside"
      x
   }, control = clc)
save(srcDestInsideTab, file = "data/artifacts/srcDestInsideTab.Rdata")



srcDestInsideTab





# get a data frame of all inside to inside connections
in2in <- recombine(nfRaw, 
   apply = function(x) {
      srcIn <- grepl("^172", x$firstSeenSrcIp)
      destIn <- grepl("^172", x$firstSeenDestIp)
      x[srcIn & destIn,]
   }, 
   combine = combRbind(), control = clc)
save(in2in, file = "data/artifacts/in2in.Rdata")



in2in[1:10, 1:5]



otherIPs <- subset(hostList, type == "Other 172.*")$IP
ind <- which(!(
   in2in$firstSeenSrcIp %in% otherIPs | 
   in2in$firstSeenDestIp %in% otherIPs))
ind



in2in[ind,1:5]



subset(hostList, IP %in% c("172.30.0.3", "172.30.1.94"))



dsq <- drQuantile(nfRaw, var = "durationSeconds", control = clc)
save(dsq, file = "data/artifacts/dsq.Rdata")



xyplot(log2(q + 1) ~ fval * 100, data = dsq, type = "p",
   xlab = "Percentile",
   ylab = "log2(duration + 1) (seconds)",
   panel = function(x, y, ...) {
      panel.grid(h=-1, v = FALSE)
      panel.abline(v = seq(0, 100, by = 10), col = "#e6e6e6")
      panel.xyplot(x, y, ...)
      panel.abline(h = log2(1801), lty = 2)
   }
)



dsqSrcType <- drQuantile(nfRaw, var = "durationSeconds", by = "type", control = clc,
   preTransFn = function(x) {
      mergeHostList(x[,c("firstSeenSrcIp", "durationSeconds")], "firstSeenSrcIp")
   },
   params = list(mergeHostList = mergeHostList, hostList = hostList)
)
save(dsqSrcType, file = "data/artifacts/dsqSrcType.Rdata")



xyplot(log2(q + 1) ~ fval * 100 | group, data = dsqSrcType, type = "p",
   xlab = "Percentile",
   ylab = "log2(duration + 1)",
   panel = function(x, y, ...) {
      panel.abline(v = seq(0, 100, by = 10), col = "#e6e6e6")
      panel.xyplot(x, y, ...)
      panel.abline(h = log2(1801), lty = 2)
   },
   layout = c(7, 1)
)



dsqDestType <- drQuantile(nfRaw, var = "durationSeconds", by = "type", control = clc,
   preTransFn = function(x) {
      mergeHostList(x[,c("firstSeenDestIp", "durationSeconds")], "firstSeenDestIp")
   },
   params = list(mergeHostList = mergeHostList, hostList = hostList)
)
save(dsqDestType, file = "data/artifacts/dsqDestType.Rdata")



dsqType <- make.groups(source = dsqSrcType, dest = dsqDestType)
xyplot(log2(q + 1) ~ fval * 100 | group, groups = which, data = dsqType, type = "p",
   xlab = "Percentile",
   ylab = "log2(duration + 1)",
   panel = function(x, y, ...) {
      panel.abline(v = seq(0, 100, by = 10), col = "#e6e6e6")
      panel.xyplot(x, y, ...)
      panel.abline(h = log2(1801), lty = 2)
   },
   layout = c(8, 1),
   auto.key = TRUE
)







topPorts <- as.integer(names(commonPortList))



dsqPort <- drQuantile(nfRaw, var = "durationSeconds", by = "port", control = clc,
   preTransFn = function(x) {
      srcInd <- which(x$firstSeenSrcPort %in% topPorts)
      destInd <- which(x$firstSeenDestPort %in% topPorts)
      data.frame(
         durationSeconds = c(x$durationSeconds[srcInd], x$durationSeconds[destInd]),
         port = c(x$firstSeenSrcPort[srcInd], x$firstSeenDestPort[destInd])
      )
   }
)
save(dsqPort, file = "data/artifacts/dsqPort.Rdata")



dsqPort$group <- factor(dsqPort$group)
nms <- sapply(commonPortList[levels(dsqPort$group)], function(x) x$name)
levels(dsqPort$group) <- paste(levels(dsqPort$group), nms)
xyplot(log2(q + 1) ~ fval * 100 | group, data = dsqPort,
   xlab = "Percentile",
   ylab = "log2(duration + 1)",
   panel = function(x, y, ...) {
      panel.xyplot(x, y, ...)
      panel.abline(h = log2(1801), lty = 2)
   },
   type = c("p", "g"),
   between = list(x = 0.25, y = 0.25),
   layout = c(5, 2)
)






