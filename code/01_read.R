



# read in 10 rows of netflow data
nfHead <- read.csv("data/raw/nf-week2.csv", nrows = 10, stringsAsFactors = FALSE)



nfHead[1:10,3:7]



# look at structure of the data
str(nfHead)



# make new date variable
nfHead$date <- as.POSIXct(nfHead$TimeSeconds, origin = "1970-01-01", tz = "UTC")
# remove old time variables
nfHead <- nfHead[,setdiff(names(nfHead), c("TimeSeconds", "parsedDate"))]



nfTransform <- function(x) {
   x$date <- as.POSIXct(x$TimeSeconds, origin = "1970-01-01", tz = "UTC")
   x[,setdiff(names(x), c("TimeSeconds", "parsedDate"))]
}



# initiate a new connection where parsed netflow data will be stored
nfConn <- localDiskConn("data/nfRaw")



# look at the connection
nfConn



# read in netflow data
nfRaw <- drRead.csv("data/raw/nf-week2.csv", output = nfConn, postTransFn = nfTransform)







nfRaw





nfRaw <- updateAttributes(nfRaw, control = clc)





nfRaw





library(Rhipe)
rhinit()

# create directory on HDFS for csv file
rhmkdir("/tmp/vast/raw")
# copy netflow csv from local disk to /tmp/vast/raw on HDFS
rhput("data/raw/nf-week2.csv", "/tmp/vast/raw")



nfRaw <- drRead.csv(hdfsConn("tmp/vast/raw/nf-week2.csv", type = "text"), 
   output = hdfsConn("/tmp/vast/nfRaw"),
   postTransFn = nfTransform)



# take a look at the data
ipsHead <- read.csv("data/raw/IPS-syslog-week2.csv", nrow = 10, stringsAsFactors = FALSE)
str(ipsHead)



ipsHead$dateTime <- gsub("Apr", "04", ipsHead$dateTime)
ipsHead$dateTime <- fast_strptime(ipsHead$dateTime, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")



# transformation to handle time variable
ipsTransform <- function(x) {
   require(lubridate)
   x$dateTime <- gsub("Apr", "04", x$dateTime)
   x$dateTime <- fast_strptime(x$dateTime, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
   names(x)[c(1, 6)] <- c("time", "srcIp")
   x
}

# read the data in
ipsRaw <- drRead.csv("data/raw/IPS-syslog-week2.csv",
   output = localDiskConn("data/ipsRaw"),
   postTransFn = ipsTransform)





ipsRaw <- updateAttributes(ipsRaw, control = clc)



ipsRaw



# look at first few rows
bbHead <- read.csv("data/raw/bb-week2.csv", nrows = 10, stringsAsFactors = FALSE)
str(bbHead)



# transformation to handle time parsing
bbTransform <- function(x) {
   x$time <- as.POSIXct(x$parsedDate, tz = "UTC")
   x[,setdiff(names(x), c("currenttime", "parsedDate"))]
}

bbRaw <- drRead.csv("data/raw/bb-week2.csv", 
   output = localDiskConn("data/bbRaw"), 
   postTransFn = bbTransform,
   autoColClasses = FALSE)
bbRaw <- updateAttributes(bbRaw, control = clc)





bbRaw






