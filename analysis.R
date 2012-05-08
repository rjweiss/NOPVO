#This script performs all major analyses:
#1) Average absolute error
#2) T-tests of modal categories
#2) Mahalanobis distances

rm(list=ls(all=T))
gc()

#TODO: Fix all of the sqldf calls to refer to the right data.frame dimensions (this is because nopvo_script.R renamed all of the dimensions)
#TODO: Fix the mahalanobis distance function to account for CBS standard errors
#TODO: Move all table generation code to tables.R
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
library(gmodels)
library(gdata)
library(reshape)
library(foreign)
library(descr)
library(sqldf)
library(plyr)
library(reshape2)

setwd("/Users/Rebecca/Dropbox/research/NOPVO/analysis/scripts/")
source("preprocessing.R")

star = function(x){
  as.character(symnum(x, corr=FALSE,
                      cutpoints = c(0,  .001,.01,.05, .1, 1),
                      symbols = c("***","**","*","+"," ")))
}

########################################
#Getting estimate errors in data.frames#
########################################

#Register Variables

#unweighted
nopvo_regvars_err <- sqldf("select nre._id, nre._bureau, nre.variable, nre.value, nre.value-rb.value as adjusted, abs(nre.value-rb.value) as abs_adjusted FROM nopvo_regvars_est nre, reg_benchmarks rb where nre._id = rb._id and nre.variable = rb._categories")
colnames(nopvo_regvars_err) <- c(".id", ".bureau", "variable", "value", "adjusted", "abs_adjusted")
nopvo_regvars_err <- sqldf("select err._id, err._bureau, err.variable, err.value, err.adjusted, err.abs_adjusted, se.value as se from nopvo_regvars_err err join nopvo_regvars_se se on err._id=se._id and err._bureau=se._bureau and err.variable = se.variable")
names(nopvo_regvars_err)[1:2] = names(nopvo_regvars_est)[1:2]

#weighted
nopvo_regvars_wtd_err <- sqldf("select nre._id, nre._bureau, nre.variable, nre.value, nre.value-rb.value as adjusted, abs(nre.value-rb.value) as abs_adjusted from nopvo_regvars_wtd_est nre, reg_benchmarks rb where nre._id = rb._id and nre.variable = rb._categories")
colnames(nopvo_regvars_wtd_err) <- c(".id", ".bureau", "variable", "value", "adjusted", "abs_adjusted")
nopvo_regvars_wtd_err <- sqldf("select err._id, err._bureau, err.variable, err.value, err.adjusted, err.abs_adjusted, se.value as se from nopvo_regvars_wtd_err err join nopvo_regvars_wtd_se se on err._id=se._id and err._bureau=se._bureau and err.variable = se.variable")
names(nopvo_regvars_wtd_err)[1:2] = names(nopvo_regvars_wtd_est)[1:2]

#Nonregister variables

#unweighted
nopvo_nonregvars_err <- sqldf("select nre._id, nre._bureau, nre.variable, nre.value, nre.value-rb.value as adjusted, abs(nre.value-rb.value) as abs_adjusted from nopvo_nonregvars_est nre, nonreg_benchmarks rb where nre._id = rb.id and nre.variable = rb.categories")
colnames(nopvo_nonregvars_err) <- c(".id", ".bureau", "variable", "value", "adjusted", "abs_adjusted")
nopvo_nonregvars_err <- sqldf("select err._id, err._bureau, err.variable, err.value, err.adjusted, err.abs_adjusted, se.value as se from nopvo_nonregvars_err err join nopvo_nonregvars_se se on err._id=se._id and err._bureau=se._bureau and err.variable = se.variable")
names(nopvo_nonregvars_err)[1:2] = names(nopvo_nonregvars_est)[1:2]

#weighted
nopvo_nonregvars_wtd_err <- sqldf("select nre._id, nre._bureau, nre.variable, nre.value, nre.value-rb.value as adjusted, abs(nre.value-rb.value) as abs_adjusted from nopvo_nonregvars_wtd_est nre, nonreg_benchmarks rb where nre._id = rb.id and nre.variable = rb.categories")
colnames(nopvo_nonregvars_wtd_err) <- c(".id", ".bureau", "variable", "value", "adjusted", "abs_adjusted")
nopvo_nonregvars_wtd_err <- sqldf("select err._id, err._bureau, err.variable, err.value, err.adjusted, err.abs_adjusted, se.value as se from nopvo_nonregvars_wtd_err err join nopvo_nonregvars_wtd_se se on err._id=se._id and err._bureau=se._bureau and err.variable = se.variable")
names(nopvo_nonregvars_wtd_err)[1:2] = names(nopvo_nonregvars_wtd_est)[1:2]

#get nopvo bureau n
nopvo_n = read.csv("/Users/Rebecca/Dropbox/research/NOPVO/analysis/csv output/nopvo_n.csv")

#Average absolute errors
source('avg_abs_err.R')

#T-tests
source('ttests.R')

#Mahalanobis distances
source('mahalanobis.R')
