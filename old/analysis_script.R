rm(list=ls(all=T))

setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/")
source("nopvo_cleaned.R")

library(plyr)
library(sqldf)

#HELPER FUNCTIONS
reformat <- function(l, benchmarks=F){
  if(benchmarks){
    ld = ldply(l, function(df){df$.categories = rownames(df); melt(df)})
  } else {
    ld = ldply(l, .progress = "none", function(df){df$.bureau = rownames(df); melt(df)})
  }
  return(ld)
}


#reformat all lists into single data.frames
nopvo_regvars_est <- reformat(nopvo_regvars.est)
nopvo_regvars.se <- reformat(nopvo_regvars.se)
nopvo_regvars_wtd_est <- reformat(nopvo_regvars_wtd.est)
nopvo_regvars_wtd.se <- reformat(nopvo_regvars_wtd.se)
nopvo_nonregvars_est <- reformat(nopvo_nonregvars.est)
nopvo_nonregvars.se <- reformat(nopvo_nonregvars.se)
nopvo_nonregvars_wtd_est <- reformat(nopvo_nonregvars_wtd.est)
nopvo_nonregvars_wtd.se <- reformat(nopvo_nonregvars_wtd.se)

reg_benchmarks <- reformat(reg_benchmarks, benchmarks=T)
nonreg_benchmarks <- reformat(nonreg_benchmarks, benchmarks=T)
reg_benchmark_modes <- melt(reg_benchmark_modes)
nonreg_benchmark_modes <- melt(nonreg_benchmark_modes)

#Register Variables

#unweighted
nopvo_regvars_err <- sqldf("select nre._id, nre._bureau, nre.variable, nre.value, nre.value-rb.value as adjusted, abs(nre.value-rb.value) as abs_adjusted from nopvo_regvars_est nre, reg_benchmarks rb where nre._id = rb._id and nre.variable = rb._categories")

names(nopvo_regvars_err)[1:2] = names(nopvo_regvars_est)[1:2]

#weighted

nopvo_regvars_wtd_err <- sqldf("select nre._id, nre._bureau, nre.variable, nre.value, nre.value-rb.value as adjusted, abs(nre.value-rb.value) as abs_adjusted from nopvo_regvars_wtd_est nre, reg_benchmarks rb where nre._id = rb._id and nre.variable = rb._categories")

names(nopvo_regvars_wtd_err)[1:2] = names(nopvo_regvars_wtd_est)[1:2]

#Nonregister variables
nopvo_nonregvars_err <- sqldf("select nre._id, nre._bureau, nre.variable, nre.value, nre.value-rb.value as adjusted, abs(nre.value-rb.value) as abs_adjusted from nopvo_nonregvars_est nre, nonreg_benchmarks rb where nre._id = rb._id and nre.variable = rb._categories")

names(nopvo_nonregvars_err)[1:2] = names(nopvo_nonregvars_est)[1:2]

#weighted

nopvo_nonregvars_wtd_err <- sqldf("select nre._id, nre._bureau, nre.variable, nre.value, nre.value-rb.value as adjusted, abs(nre.value-rb.value) as abs_adjusted from nopvo_nonregvars_wtd_est nre, nonreg_benchmarks rb where nre._id = rb._id and nre.variable = rb._categories")

names(nopvo_nonregvars_wtd_err)[1:2] = names(nopvo_nonregvars_wtd_est)[1:2]


setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/csv output")

#n for now...changes per panel
nopvo_n = read.csv("nopvo_n.csv")
nopvo_n$X = NULL

#################################
#FINDING AVERAGE ABSOLUTE ERRORS#
#################################

#NOPVO unweighted

#register variables

#TODO: something wrong here, multiple originself, originmother, originfather
nopvo_regvars_err_modes = sqldf("select nre._id, nre.variable, nre._bureau, nre.adjusted, nre.abs_adjusted from nopvo_regvars_err nre, reg_benchmark_modes rbm where nre.variable = rbm.value")

names(nopvo_regvars_err_modes) = c("id", "variable", "bureau", "adjusted", "abs_adjusted")

nvars = sqldf("select count(distinct id) from nopvo_regvars_err_modes group by bureau")
names(nvars) = c("nvars")
sum_abs_errs = sqldf("select *, sum(abs_adjusted) from (select distinct * from nopvo_regvars_err_modes) group by bureau")
names(sum_abs_errs) = c("id", "variable", "bureau", "adjusted", "abs_adjusted", "sum_abs_adjusted")

sum_abs_errs$avg_abs_err = sum_abs_errs$sum_abs_adjusted/nvars
regvar_avg_abs_err = subset(sum_abs_errs, select = c(bureau, avg_abs_err))


#nonregister
nopvo_nonregvars_err_modes = sqldf("select nre._id, nre.variable, nre._bureau, nre.adjusted, nre.abs_adjusted from nopvo_nonregvars_err nre, nonreg_benchmark_modes rbm where nre.variable = rbm.value")

names(nopvo_nonregvars_err_modes) = c("id", "variable", "bureau", "adjusted", "abs_adjusted")

nvars = sqldf("select count(distinct id) from nopvo_nonregvars_err_modes group by bureau")
names(nvars) = c("nvars")
sum_abs_errs = sqldf("select *, sum(abs_adjusted) from (select distinct * from nopvo_nonregvars_err_modes) group by bureau")
names(sum_abs_errs) = c("id", "variable", "bureau", "adjusted", "abs_adjusted", "sum_abs_adjusted")

sum_abs_errs$avg_abs_err = sum_abs_errs$sum_abs_adjusted/nvars
nonregvar_avg_abs_err = subset(sum_abs_errs, select = c(bureau, avg_abs_err))

#NOPVO weighted

#register variables

#TODO: something wrong here, multiple originself, originmother, originfather
nopvo_regvars_wtd_err_modes = sqldf("select nre._id, nre.variable, nre._bureau, nre.adjusted, nre.abs_adjusted from nopvo_regvars_wtd_err nre, reg_benchmark_modes rbm where nre.variable = rbm.value")

names(nopvo_regvars_wtd_err_modes) = c("id", "variable", "bureau", "adjusted", "abs_adjusted")

nvars_wtd = sqldf("select count(distinct id) from nopvo_regvars_wtd_err_modes group by bureau")
names(nvars_wtd) = c("nvars")
sum_abs_errs_wtd = sqldf("select *, sum(abs_adjusted) from (select distinct * from nopvo_regvars_wtd_err_modes) group by bureau")
names(sum_abs_errs_wtd) = c("id", "variable", "bureau", "adjusted", "abs_adjusted", "sum_abs_adjusted")

sum_abs_errs_wtd$avg_abs_err = sum_abs_errs_wtd$sum_abs_adjusted/nvars_wtd
regvar_avg_abs_err_wtd = subset(sum_abs_errs_wtd, select = c(bureau, avg_abs_err))


#nonregister
nopvo_nonregvars_wtd_err_modes = sqldf("select nre._id, nre.variable, nre._bureau, nre.adjusted, nre.abs_adjusted from nopvo_nonregvars_wtd_err nre, nonreg_benchmark_modes rbm where nre.variable = rbm.value")

names(nopvo_nonregvars_wtd_err_modes) = c("id", "variable", "bureau", "adjusted", "abs_adjusted")

nvars_wtd = sqldf("select count(distinct id) from nopvo_nonregvars_wtd_err_modes group by bureau")
names(nvars_wtd) = c("nvars")
sum_abs_errs_wtd = sqldf("select *, sum(abs_adjusted) from (select distinct * from nopvo_nonregvars_wtd_err_modes) group by bureau")
names(sum_abs_errs_wtd) = c("id", "variable", "bureau", "adjusted", "abs_adjusted", "sum_abs_adjusted")

sum_abs_errs_wtd$avg_abs_err = sum_abs_errs_wtd$sum_abs_adjusted/nvars_wtd
nonregvar_avg_abs_err_wtd = subset(sum_abs_errs_wtd, select = c(bureau, avg_abs_err))

###############################
#FINDING MAHALANOBIS DISTANCES#
###############################