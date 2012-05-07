rm(list=ls(all=T))
gc()

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

#Helper functions

mahal_dist = function(diff, mat){
#TODO: Fix Dirty Hack
  dist = as.numeric(tryCatch(t(diff) %*% solve(mat, tol=1e-21) %*% diff, error = function(e) return("0")))
  return(dist)
  }

get_mahal_dist = function(dat, n, regvars = T, cbs = F, nopvo = F, liss = F){
  if(liss){
    dat$n = n
    ddply(dat, .(variable), function(x){
      #variance-covariance matrix
      mat = -1 * ((x$est %*% t(x$est)) / x$n)
      diag(mat) = x$se^2      
      for (k in 1:length(mat)){if(mat[k]==0) mat[k]=1e-14}      
      #difference column vector
      diff = x$err
      for (i in 1:length(diff)){if(diff[i]==0) diff[i]=1e-14}
      dof = length(diff) - 1
      #mahalanobis distance    
      distance = mahal_dist(diff, mat)
      sig = round(1 - pchisq(distance, df = dof), digits=3)
      return(c("dist" = distance, "sig" = sig))  
    })
  }
  #   if(cbs){
  #     
  #   }
  else { #if(nopvo){
    if(regvars){
      data = join(dat, n)
      ddply(data, .(bureau), function(df){
        ddply(df, .(id), function(x){
          #variance-covariance matrix
          mat = -1 * ((x$est %*% t(x$est)) / x$n)
          diag(mat) = x$se^2      
          for (k in 1:length(mat)){if(mat[k]==0) mat[k]=1e-14}      
          #difference column vector
          diff = x$err
          for (i in 1:length(diff)){if(diff[i]==0) diff[i]=1e-14}
          dof = length(diff) - 1
          #mahalanobis distance    
          distance = mahal_dist(diff, mat)
          sig = round(1 - pchisq(distance, df = dof), digits=3)
          return(c("dist" = distance, "sig" = sig))
        })
      })
    } else {
      data = join(dat, n)
      ddply(data, .(bureau), function(df){
        ddply(df, .(id), function(x){
          #variance-covariance matrix
          mat = -1 * ((x$est %*% t(x$est)) / x$n)
          diag(mat) = x$se^2      
          for (k in 1:length(mat)){if(mat[k]==0) mat[k]=1e-14}      
          #difference column vector
          diff = x$err
          for (i in 1:length(diff)){if(diff[i]==0) diff[i]=1e-14}
          dof = length(diff) - 1
          #mahalanobis distance    
          distance = mahal_dist(diff, mat)
          sig = round(1 - pchisq(distance, df = dof), digits=3)
          return(c("dist" = distance, "sig" = sig))
        })
      })  
    }
  }
}
    
star = function(x){
  as.character(symnum(x, corr=FALSE,
                      cutpoints = c(0,  .001,.01,.05, .1, 1),
                      symbols = c("***","**","*","+"," ")))
}


#Creating estimate error data.frames

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

#################################
#FINDING AVERAGE ABSOLUTE ERRORS#
#################################

#####
#CBS#
#####

#creating structured data for EBB, POLS
#for now, let's just look at at 2006

#EBB
ebb_cali_err = subset(ebb_reg_err, select = c(ebb_cali_err6, variable, categories))
ebb_cali_est = subset(ebb_reg_est, select = c(calibrated2006, variable, categories))
ebb_cali_se = subset(ebb_reg_se, select = c(calibrated_se2006, variable, categories))

ebb_cali = sqldf("select * from ebb_cali_err natural join ebb_cali_est natural join ebb_cali_se")
names(ebb_cali) = c("err", "variable", "category", "est", "se")

ebb_incl_err = subset(ebb_reg_err, select = c(ebb_incl_err6, variable, categories))
ebb_incl_est = subset(ebb_reg_est, select = c(inclusion2006, variable, categories))
ebb_incl_se = subset(ebb_reg_se, select = c(inclusion_se2006, variable, categories))

ebb_incl = sqldf("select * from ebb_incl_err natural join ebb_incl_est natural join ebb_incl_se")
names(ebb_incl) = c("err", "variable", "category", "est", "se")

#POLS
pols_cali_err = subset(pols_reg_err, select = c(pols_cali_err6, variable, categories))
pols_cali_est = subset(pols_reg_est, select = c(calibrated2006, variable, categories))
pols_cali_se = subset(pols_reg_se, select = c(calibrated_se2006, variable, categories))

pols_cali = sqldf("select * from pols_cali_err natural join pols_cali_est natural join pols_cali_se")
names(pols_cali) = c("err", "variable", "category", "est", "se")

pols_incl_err = subset(pols_reg_err, select = c(pols_incl_err6, variable, categories))
pols_incl_est = subset(pols_reg_est, select = c(inclusion2006, variable, categories))
pols_incl_se = subset(pols_reg_se, select = c(inclusion_se2006, variable, categories))

pols_incl = sqldf("select * from pols_incl_err natural join pols_incl_est natural join pols_incl_se")
names(pols_incl) = c("err", "variable", "category", "est", "se")

#calibration weighted register variables

#EBB

ebb_regvars_err_modes = sqldf("select ebb.variable, ebb.category, ebb.est, ebb.se, ebb.err from ebb_cali ebb, reg_benchmark_modes rbm where ebb.category = rbm.value and ebb.variable = rbm.L1")

ebb_nvars = sqldf("select count(distinct variable) from ebb_regvars_err_modes")
names(ebb_nvars) = c("nvars")

ebb_regvar_abs_err = sqldf("select *, sum(abs(err)) from ebb_regvars_err_modes")
names(ebb_regvar_abs_err)[6] = c("sum_abs_err")
ebb_regvar_abs_err$abs_err = ebb_regvar_abs_err$sum_abs_err/ebb_nvars

#POLS

pols_regvars_err_modes = sqldf("select pols.variable, pols.category, pols.est, pols.se, pols.err from pols_cali pols, reg_benchmark_modes rbm where pols.category = rbm.value and pols.variable = rbm.L1")

pols_nvars = sqldf("select count(distinct variable) from pols_regvars_err_modes")
names(pols_nvars) = c("nvars")

pols_regvar_abs_err = sqldf("select *, sum(abs(err)) from pols_regvars_err_modes")
names(pols_regvar_abs_err)[6] = c("sum_abs_err")
pols_regvar_abs_err$abs_err = pols_regvar_abs_err$sum_abs_err/pols_nvars

#inclusion weighted register variables

#EBB

ebb_regvars_wtd_err_modes = sqldf("select ebb.variable, ebb.category, ebb.est, ebb.se, ebb.err from ebb_incl ebb, reg_benchmark_modes rbm where ebb.category = rbm.value and ebb.variable = rbm.L1")

ebb_regvar_wtd_abs_err = sqldf("select *, sum(abs(err)) from ebb_regvars_wtd_err_modes")
names(ebb_regvar_wtd_abs_err)[6] = c("sum_abs_err")
ebb_regvar_wtd_abs_err$abs_err = ebb_regvar_wtd_abs_err$sum_abs_err/ebb_nvars

#POLS

pols_regvars_wtd_err_modes = sqldf("select pols.variable, pols.category, pols.est, pols.se, pols.err from pols_incl pols, reg_benchmark_modes rbm where pols.category = rbm.value and pols.variable = rbm.L1")

pols_regvar_wtd_abs_err = sqldf("select *, sum(abs(err)) from pols_regvars_wtd_err_modes")
names(pols_regvar_wtd_abs_err)[6] = c("sum_abs_err")
pols_regvar_wtd_abs_err$abs_err = pols_regvar_wtd_abs_err$sum_abs_err/pols_nvars


######
#LISS#
######

liss_err = sqldf("select le.category, le.value, le.variable, le.value - rb.value as err from liss_est le, reg_benchmarks rb where le.category = rb._categories and le.variable = rb._id")

liss_err = sqldf("select distinct * from liss_err natural join liss_se")
names(liss_err) = c("category","est", "variable","err","se")

liss_regvars_err_modes = sqldf("select le.category, le.est, le.variable, le.err, le.se from liss_err le, reg_benchmark_modes rbm where le.category = rbm.value and le.variable = rbm.L1")

liss_nvars = sqldf("select count(distinct variable) from liss_regvars_err_modes")
names(liss_nvars) = c("nvars")

liss_regvar_abs_errs = sqldf("select *, sum(abs(err)) from (select distinct * from liss_regvars_err_modes)")
names(liss_regvar_abs_errs) = c("category","est", "variable","err","se","sum_abs_err")
liss_regvar_abs_errs$avg_abs_err_regvar = liss_regvar_abs_errs$sum_abs_err/liss_nvars

#######
#NOPVO#
#######

#unweighted register variables 

#TODO: something wrong here, multiple originself, originmother, originfather
nopvo_regvars_err_modes = sqldf("select nre._id, nre.variable, nre._bureau, nre.value, nre.adjusted, nre.abs_adjusted, nre.se from nopvo_regvars_err nre, reg_benchmark_modes rbm where nre.variable = rbm.value")
names(nopvo_regvars_err_modes) = c("id", "variable", "bureau", "value", "adjusted", "abs_adjusted", "se")
nvars = sqldf("select count(distinct id) from nopvo_regvars_err_modes group by bureau")
names(nvars) = c("nvars")
sum_abs_errs = sqldf("select *, sum(abs_adjusted) from (select distinct * from nopvo_regvars_err_modes) group by bureau")
names(sum_abs_errs) = c("id", "variable", "bureau", "value", "adjusted", "abs_adjusted", "se", "sum_abs_adjusted")
sum_abs_errs$avg_abs_err_regvar = sum_abs_errs$sum_abs_adjusted/nvars
regvar_avg_abs_err = melt(subset(sum_abs_errs, select = c(bureau, avg_abs_err_regvar)))

#unweighted nonregister variables
nopvo_nonregvars_err_modes = sqldf("select nre._id, nre.variable, nre._bureau, nre.value, nre.adjusted, nre.abs_adjusted, nre.se from nopvo_nonregvars_err nre, nonreg_benchmark_modes rbm where nre.variable = rbm.value")
names(nopvo_nonregvars_err_modes) = c("id", "variable", "bureau", "value", "adjusted", "abs_adjusted", "se")
nvars = sqldf("select count(distinct id) from nopvo_nonregvars_err_modes group by bureau")
names(nvars) = c("nvars")
sum_abs_errs = sqldf("select *, sum(abs_adjusted) from (select distinct * from nopvo_nonregvars_err_modes) group by bureau")
names(sum_abs_errs) = c("id", "variable", "bureau", "value", "adjusted", "abs_adjusted", "se","sum_abs_adjusted")
sum_abs_errs$avg_abs_err_nonregvar = sum_abs_errs$sum_abs_adjusted/nvars
nonregvar_avg_abs_err = melt(subset(sum_abs_errs, select = c(bureau, avg_abs_err_nonregvar)))

#weighted register variables
nopvo_regvars_wtd_err_modes = sqldf("select nre._id, nre.variable, nre._bureau, nre.value, nre.adjusted, nre.abs_adjusted, nre.se from nopvo_regvars_wtd_err nre, reg_benchmark_modes rbm where nre.variable = rbm.value")
names(nopvo_regvars_wtd_err_modes) = c("id", "variable", "bureau", "value", "adjusted", "abs_adjusted", "se")
nvars_wtd = nvars = sqldf("select count(distinct id) from nopvo_regvars_wtd_err_modes group by bureau")
names(nvars_wtd) = c("nvars")
sum_abs_errs_wtd = sqldf("select *, sum(abs_adjusted) from (select distinct * from nopvo_regvars_wtd_err_modes) group by bureau")
names(sum_abs_errs_wtd) = c("id",  "variable", "bureau","value", "adjusted", "abs_adjusted", "se","sum_abs_adjusted")
sum_abs_errs_wtd$avg_abs_err_regvar_wtd = sum_abs_errs_wtd$sum_abs_adjusted/nvars_wtd
regvar_avg_abs_err_wtd = melt(subset(sum_abs_errs_wtd, select = c(bureau, avg_abs_err_regvar_wtd)))

#weighted nonregister variables
nopvo_nonregvars_wtd_err_modes = sqldf("select nre._id, nre.variable, nre._bureau, nre.value, nre.adjusted, nre.abs_adjusted, nre.se from nopvo_nonregvars_wtd_err nre, nonreg_benchmark_modes rbm where nre.variable = rbm.value")
names(nopvo_nonregvars_wtd_err_modes) = c("id", "variable", "bureau","value", "adjusted", "abs_adjusted", "se")
#nopvo_nonregvars_err_modes = unique(nopvo_nonregvars_err_modes)
nvars_wtd = sqldf("select count(distinct id) from nopvo_nonregvars_wtd_err_modes group by bureau")
names(nvars_wtd) = c("nvars")
sum_abs_errs_wtd = sqldf("select *, sum(abs_adjusted) from (select distinct * from nopvo_nonregvars_wtd_err_modes) group by bureau")
names(sum_abs_errs_wtd) = c("id", "variable", "bureau",  "value", "adjusted", "abs_adjusted", "se","sum_abs_adjusted")
sum_abs_errs_wtd$avg_abs_err_nonregvar_wtd = sum_abs_errs_wtd$sum_abs_adjusted/nvars_wtd
nonregvar_avg_abs_err_wtd = melt(subset(sum_abs_errs_wtd, select = c(bureau, avg_abs_err_nonregvar_wtd)))

###############################
#FINDING MAHALANOBIS DISTANCES#
###############################

#NOPVO

colnames(nopvo_regvars_se)[4] <- "se"
colnames(nopvo_regvars_est)[4] <- "est"
nopvo_regvars_err = sqldf("select _id, _bureau, variable, adjusted as err from nopvo_regvars_err")
colnames(nopvo_regvars_err) <- c(".id", ".bureau", "variable", "err")

regvars = sqldf("select * from nopvo_regvars_se natural join nopvo_regvars_est natural join nopvo_regvars_err")
names(regvars) = c("id", "bureau", "variable", "se", "est", "err")

regvars_wtd = sqldf("select * from nopvo_regvars_wtd_se natural join nopvo_regvars_wtd_est natural join nopvo_regvars_wtd_err")

regvars_wtd = sqldf("select * from nopvo_regvars_wtd_est natural join nopvo_regvars_wtd_err")

names(regvars_wtd) = c("id", "bureau", "variable", "est", "err", "abs_err", "se")
regvars_wtd$abs_err = NULL

regvars_dist = get_mahal_dist(regvars, nopvo_n, regvars = T, cbs = F, nopvo = T, liss = F)
regvars_dist$star = star(regvars_dist$sig)

regvars_wtd_dist = get_mahal_dist(regvars_wtd, nopvo_n, regvars = T, cbs = F, nopvo = T, liss = F)
regvars_wtd_dist$star = star(regvars_wtd_dist$sig)

# nonregvars = sqldf("select * from nopvo_nonregvars_wtd_se natural join nopvo_nonregvars_wtd_est natural join nopvo_nonregvars_wtd_err")
# 
# names(nonregvars) = c("id", "bureau", "variable", "se", "est", "err")

#LISS
#get_mahal_dist = function(dat, n, regvars = T, cbs = F, nopvo = T, liss = F){
  
liss_regvars_dist = get_mahal_dist(liss_err, liss_n, regvars = T, cbs = F, nopvo = F, liss = T)
liss_regvars_dist$star = star(liss_regvars_dist$sig)
liss_regvars_dist$bureau = c("liss")
names(liss_regvars_dist) = c("id", "dist", "sig", "star", "bureau")

#regvars_dist = rbind(liss_regvars_dist, regvars_dist)

liss_regvars_mahal_stars = dcast(liss_regvars_dist, id ~ ., value.var = c("star"))

liss_mahal_stars_count = melt(ddply(liss_regvars_dist, .(id), summarise, 
                                    three = sum(star %in% "***"),
                                    two = sum(star %in% "**"),
                                    one = sum(star %in% "*"),
                                    cross = sum(star %in% "+")))

#table of stars
regvars_mahal_stars = arrange(dcast(regvars_dist, bureau ~ id, value.var = c("star")), desc(as.numeric(bureau)))
regvars_wtd_mahal_stars = arrange(dcast(regvars_wtd_dist, bureau ~ id, value.var = c("star")), desc(as.numeric(bureau)))

regvars_mahal = arrange(dcast(regvars_dist, bureau ~ id, value.var = c("dist")), desc(as.numeric(bureau)))

#distribution of distance significances
regvars_mahal_stars_count = melt(ddply(regvars_dist, .(bureau), summarise, 
                                       three = sum(star %in% "***"),
                                       two = sum(star %in% "**"),
                                       one = sum(star %in% "*"),
                                       cross = sum(star %in% "+")))

regvars_wtd_mahal_stars_count = melt(ddply(regvars_wtd_dist, .(bureau), summarise, 
                                           three = sum(star %in% "***"),
                                           two = sum(star %in% "**"),
                                           one = sum(star %in% "*"),
                                           cross = sum(star %in% "+")))
