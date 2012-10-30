#This is the main script to do the NOPVO comparison against the reg_benchmarks.

#Change the working directory to the R directory
setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/")

rm(list=ls(all=T))
gc()

library(plyr)
library(reshape)


###########
#Functions#
###########

#Get NOPVO errors from benchmarks
get_errors = function(estimates, benchmarks, wtd){
  error_list = list(NA)
  for (iter in 1:length(estimates)){
    error_list[[iter]] = as.data.frame(apply(estimates[[iter]], 1, function(x) x - t(benchmarks[[iter]])))     
    if(!wtd) {
    names(estimates[[iter]]) = rownames(benchmarks[[iter]])
    rownames(error_list[[iter]]) = names(estimates[[iter]])
    } else {
    colnames(error_list[[iter]]) = rownames(estimates[[1]]) #TODO: Fix the absolute labeling problem
        error_list[[iter]] = t(error_list[[iter]])
      }
  }
  names(error_list) = names(benchmarks)
  return(error_list)
}

#Create a csv file from a NOPVO list object
create_csv = function(list_obj, trans, cats, wtd){
  temp_list = list(NA)
  for(x in 1:length(names(list_obj))){
    temp.name = names(list_obj[x])
    if(!wtd) {
        temp.df = eval(parse(text = paste("list_obj$", temp.name, sep = "")))
      } else {
        temp.df = as.data.frame(eval(parse(text = paste("list_obj$", temp.name, sep = ""))))
        }
    if(trans) temp.df = as.data.frame(t(temp.df))
    temp.df$id = rownames(temp.df)
    temp.df = melt(temp.df, variable_name = "categories")
    temp_cast.df = as.data.frame(as.matrix(t(cast(temp.df, id ~ categories))))
    temp_cast.df$variable = temp.name
    if(cats) temp_cast.df$categories = rownames(temp_cast.df)
    temp_list[[x]] = temp_cast.df
  }
  return(do.call(rbind.fill, temp_list))
}


#################
#Reading in data#
#################

source("nopvo_cleaned.R")

##############################################
#NOPVO against GBA percentage estimate errors#
##############################################

#Generates the estimate errors for each variable for each NOPVO panel
#Creates a list object to hold all the errors

nopvo_regvars.err = get_errors(nopvo_regvars.est, reg_benchmarks, F)

#Creating giant data.frames of all the estimates and errors

nopvo_reg_est_csv = create_csv(nopvo_regvars.est, F, T, F)
nopvo_reg_se_csv = create_csv(nopvo_regvars.se, F, T, F)
nopvo_reg_err_csv = create_csv(nopvo_regvars.err, T, T, F)
reg_benchmarks_csv = create_csv(reg_benchmarks, T, F, F)

#######################################################
#NOPVO against EBB/POLS/IPO percentage estimate errors#
#######################################################

#Generates the estimate errors for each variable for each NOPVO panel
#Creates a list object to hold all the errors

nopvo_nonregvars.err = get_errors(nopvo_nonregvars.est, nonreg_benchmarks, F)

#Creating giant data.frames of all the estimates and errors
nopvo_nonreg_est_csv = create_csv(nopvo_nonregvars.est, F, T, F)
nopvo_nonreg_se_csv = create_csv(nopvo_nonregvars.se, F, T, F)
nopvo_nonreg_err_csv = create_csv(nopvo_nonregvars.err, F, T, F)
nonreg_benchmarks_csv = create_csv(reg_benchmarks, T, F, F)

##########
#WEIGHTED#
##########

#Register variables

nopvo_regvars_wtd.err = get_errors(nopvo_regvars_wtd.est, reg_benchmarks, T)

nopvo_reg_est_wtd_csv = create_csv(nopvo_regvars_wtd.est, F, T, T)
nopvo_reg_se_wtd_csv = create_csv(nopvo_regvars_wtd.se, F, T, T)
nopvo_reg_err_wtd_csv = create_csv(nopvo_regvars_wtd.err, F, T, T)

#Non-register variables

nopvo_nonregvars_wtd.err = get_errors(nopvo_nonregvars_wtd.est, nonreg_benchmarks, T)

nopvo_nonreg_est_wtd_csv = create_csv(nopvo_nonregvars_wtd.est, F, T, T)
nopvo_nonreg_se_wtd_csv = create_csv(nopvo_nonregvars_wtd.se, F, T, T) #TODO:error
nopvo_nonreg_err_wtd_csv = create_csv(nopvo_nonregvars_wtd.err, F, T, T)

###############
#SAVING OUTPUT#
###############

#Writing comma-separated ASCII files of the giant data.frames
# 
# setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/csv output")
# 
# write.csv(reg_benchmarks_csv, "reg_benchmarks.csv")
# 
# write.csv(nopvo_reg_est_csv, "nopvo_reg_est.csv")
# write.csv(nopvo_reg_se_csv, "nopvo_reg_se.csv")
# write.csv(nopvo_reg_err_csv, "nopvo_reg_err.csv")
# 
# write.csv(nopvo_nonreg_est_csv, "nopvo_nonreg_est.csv")
# write.csv(nopvo_nonreg_se_csv, "nopvo_nonreg_se.csv")
# write.csv(nopvo_nonreg_err_csv, "nopvo_nonreg_err.csv")
# 
# write.csv(nopvo_reg_est_wtd_csv, "nopvo_reg_est_wtd.csv")
# write.csv(nopvo_reg_se_wtd_csv, "nopvo_reg_se_wtd.csv")
# write.csv(nopvo_reg_err_wtd_csv, "nopvo_reg_err_wtd.csv")
# 
# write.csv(nopvo_nonreg_est_wtd_csv, "nopvo_nonreg_est_wtd.csv")
# write.csv(nopvo_nonreg_se_wtd_csv, "nopvo_nonreg_se_wtd.csv")
# write.csv(nopvo_nonreg_err_wtd_csv, "nopvo_nonreg_err_wtd.csv")
# 
# rm(list=(setdiff(ls(), c(
#   "nopvo_regvars.est",
#   "nopvo_regvars.err",
#   "nopvo_regvars.se",
#   "nopvo_regvars_wtd.est",
#   "nopvo_regvars_wtd.err",
#   "nopvo_regvars_wtd.se",
#   "nopvo_nonregvars.est",
#   "nopvo_nonregvars.err",
#   "nopvo_nonregvars.se",
#   "nopvo_nonregvars_wtd.est",
#   "nopvo_nonregvars_wtd.err",
#   "nopvo_nonregvars_wtd.se",
#   "reg_var_labels",
#   "nonreg_var_labels"))))
# gc()
