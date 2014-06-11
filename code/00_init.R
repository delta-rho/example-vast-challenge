



install.packages("devtools")
library(devtools)
install_github("datadr", "hafen")
install_github("trelliscope", "hafen")



install_github("vastChallenge", "hafen", subdir = "package")





# create directory for raw text data
dir.create("data/raw", recursive = TRUE)





# use this code to initialize a new R session
library(datadr)
library(trelliscope)
library(cyberTools)
setwd("~/Documents/Code/vastChallenge")

# make a local "cluster" of 8 cores
cl <- makeCluster(8)
clc <- localDiskControl(cluster = cl)


