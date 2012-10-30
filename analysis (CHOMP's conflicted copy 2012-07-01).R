#This script performs all major analyses:
#1) Average absolute error
#2) T-tests of modal categories
#2) Mahalanobis distances

rm(list=ls(all=T))
gc()
setwd("/Users/Rebecca/Dropbox/research/NOPVO/analysis/scripts/")

#TODO: Fix the mahalanobis distance function to account for CBS standard errors
#TODO: Write ttest.R

# install.packages("Hmisc")
# install.packages("gmodels")
# install.packages("gdata")
# install.packages("reshape")
# install.packages("foreign")
# install.packages("descr")
# install.packages("sqldf")
# install.packages("plyr")
# install.packages("reshape2")
# install.packages("car")

#load libraries
library(Hmisc)
library(car)
library(gmodels)
library(gdata)
library(foreign)
library(descr)
library(sqldf)
library(memisc)
library(plyr)
library(reshape2)
library(ggplot2)


source("/Users/Rebecca/Dropbox/research/NOPVO/analysis/scripts/preprocessing.R")

star = function(x){
  as.character(symnum(x, corr=FALSE,
                      cutpoints = c(0,  .001,.01,.05, .1, 1),
                      symbols = c("***","**","*","+"," ")))
}

########################################
#Getting estimate errors in data.frames#
########################################

#NOPVO

#Register

#unweighted
nopvo_regvars_err <- sqldf("select nre.variable, nre.bureau, nre.category, nre.value as est, nre.value-rb.value as err, abs(nre.value-rb.value) as abs_err FROM nopvo_regvars_est nre, reg_benchmarks rb where nre.variable = rb.variable and nre.category = rb.category")
nopvo_regvars_err <- sqldf("select err.variable, err.bureau, err.category, err.est, err.err, err.abs_err, se.value as se from nopvo_regvars_err err join nopvo_regvars_se se on err.variable=se.variable and err.bureau=se.bureau and err.category = se.category")

#weighted
nopvo_regvars_wtd_err <- sqldf("select nre.variable, nre.bureau, nre.category, nre.value as est, nre.value-rb.value as err, abs(nre.value-rb.value) as abs_err FROM nopvo_regvars_wtd_est nre, reg_benchmarks rb where nre.variable = rb.variable and nre.category = rb.category")
nopvo_regvars_wtd_err <- sqldf("select err.variable, err.bureau, err.category, err.est, err.err, err.abs_err, se.value as se from nopvo_regvars_wtd_err err join nopvo_regvars_wtd_se se on err.variable=se.variable and err.bureau=se.bureau and err.category = se.category")

#Nonregister
#TODO: recode nonregister benchmark categories (this must be done in cbs_script.R)

#unweighted
nopvo_nonregvars_err <- sqldf("select nre.variable, nre.bureau, nre.category, nre.value as est, nre.value-rb.var as err, abs(nre.value-rb.var) as abs_err FROM nopvo_nonregvars_est nre, nonreg_benchmarks rb where nre.variable = rb.variable and nre.category = rb.category")
nopvo_nonregvars_err <- sqldf("select err.variable, err.bureau, err.category, err.est, err.err, err.abs_err, se.value as se from nopvo_nonregvars_err err join nopvo_nonregvars_se se on err.variable=se.variable and err.bureau=se.bureau and err.category = se.category")

#weighted
nopvo_nonregvars_wtd_err <- sqldf("select nre.variable, nre.bureau, nre.category, nre.value as est, nre.value-rb.var as err, abs(nre.value-rb.var) as abs_err FROM nopvo_nonregvars_wtd_est nre, nonreg_benchmarks rb where nre.variable = rb.variable and nre.category = rb.category")
nopvo_nonregvars_wtd_err <- sqldf("select err.variable, err.bureau, err.category, err.est, err.err, err.abs_err, se.value as se from nopvo_nonregvars_wtd_err err join nopvo_nonregvars_wtd_se se on err.variable=se.variable and err.bureau=se.bureau and err.category = se.category")

#get nopvo bureau n
nopvo_n = read.csv("/Users/Rebecca/Dropbox/research/NOPVO/analysis/csv output/nopvo_n.csv")

#LISS

#Register 

liss_err = sqldf("select le.category, le.value as est, le.variable, le.value - rb.value as err from liss_est le, reg_benchmarks rb where le.category = rb.category and le.variable = rb.variable")

liss_err = sqldf("select distinct * from liss_err natural join liss_se")

#Average absolute errors
source('/Users/Rebecca/Dropbox/research/NOPVO/analysis/scripts/avg_abs_err.R')

#T-tests
source('/Users/Rebecca/Dropbox/research/NOPVO/analysis/scripts/ttests.R')

# #Mahalanobis distances
# source('/Users/Rebecca/Dropbox/research/NOPVO/analysis/scripts/mahalanobis.R')

#Regressions
#source('/Users/Rebecca/Dropbox/research/NOPVO/analysis/scripts/regression.R')

#Creating tables
#source('/Users/Rebecca/Dropbox/research/NOPVO/analysis/scripts/tables.R')

# #Creating plots
# source('/Users/Rebecca/Dropbox/research/NOPVO/analysis/scripts/plots.R')