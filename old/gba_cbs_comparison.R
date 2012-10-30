rm(list = ls(all = T))
gc()

setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/data")

load("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/GBA_tables.RData") #loading benchmarks

ebb2006 <- read.csv("ebb2006_register.csv")
ebb2008 <- read.csv("ebb2008_register.csv")

ebb2006.list <- split(ebb2006, ebb2006$variable)
ebb2008.list <- split(ebb2008, ebb2008$variable)
