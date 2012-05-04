rm(list=ls(all=T))
gc()

library(xtable)

setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/")

source("analysis.R")


star = function(x){
  as.character(symnum(x, corr=FALSE,
                      cutpoints = c(0,  .001,.01,.05, .1, 1),
                      symbols = c("***","**","*","+"," ")))
}
