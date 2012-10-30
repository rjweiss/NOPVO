#NOPVO MAHALANOBIS DISTANCE SCRIPT
#This script takes the NOPVO estimates, errors, and standard errors, as well as benchmarks, and finds mahalanobis distances for each variable in each sample
#1) Prints out csv files of mahalanobis distances
#2) Creates list objects with mahalanobis distances

rm(list=ls(all=T))
gc()

##############
#Loading data#
##############

#NOPVO

setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts")

source("nopvo_comparison_script.R")

#other data (benchmarks and n)

setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/csv output")

nopvo_n = read.csv("nopvo_n.csv")
nopvo_n$X = NULL
reg_benchmarks = read.csv("reg_benchmarks.csv")
reg_benchmarks$X = NULL
nonreg_benchmarks = read.csv("nonreg_benchmarks.csv")
nonreg_benchmarks$X = NULL
nonreg_benchmarks_se = read.csv("nonreg_benchmarks_se.csv")
nonreg_benchmarks_se$X = NULL
cbs_n = read.csv("cbs_n.csv")
cbs_n$X = NULL

setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts")

#CBS

#ebb_reg_est = read.csv("ebb_reg_est.csv")
#ebb_reg_err = read.csv("ebb_reg_err.csv")
#ebb_reg_se = read.csv("ebb_reg_se.csv")


###########
#FUNCTIONS#
###########

passthru = function(x, y){eval(parse(text = paste(x, y, sep="")))}

mahal_dist = function(diff, mat){
	distance = t(diff) %*% solve(mat, tol=1e-21) %*% diff
	return(distance)
}

mahal_sig = function(dist, dof){
	round(1 - pchisq(dist, df = dof), digits=3)
}

star = function(x){
	as.character(symnum(x, corr=FALSE,
               cutpoints = c(0,  .001,.01,.05, .1, 1),
               symbols = c("***","**","*","+"," ")))
}

var_covs = function(var_labels, regvars, cbs, wtd){
  varcovs = list(NA)
  for (i in 1:length(var_labels)){  
    if(!cbs){
      if(wtd){
        if(regvars){
          temp_est = passthru(c("nopvo_regvars_wtd.est$"), var_labels[i])
          temp_se = passthru(c("nopvo_regvars_wtd.est$"), var_labels[i])
        } else {
          temp_est = passthru(c("nopvo_nonregvars_wtd.est$"), var_labels[i])
          temp_se = passthru(c("nopvo_nonregvars_wtd.est$"), var_labels[i])      
        }          
      } else {
        if(regvars){
          temp_est = passthru(c("nopvo_regvars.est$"), var_labels[i])
          temp_se = passthru(c("nopvo_regvars.est$"), var_labels[i])
        } else {
          temp_est = passthru(c("nopvo_nonregvars.est$"), var_labels[i])
          temp_se = passthru(c("nopvo_nonregvars.est$"), var_labels[i])      
        }        
      }
    } else {
      temp_est = eval(parse(text = paste("cbs_",var_labels[i],"_est",sep="")))
      temp_se = eval(parse(text = paste("cbs_",var_labels[i],"_se",sep="")))
    }
    covs = list(NA)
    for (j in 1:dim(temp_est)[1]){
      #covariance
      if(!cbs){
        est_vec = as.numeric(temp_est[j,])
        # print(est_vec)
        mat = -1 * (est_vec %*% t(est_vec)) / nopvo_n$x[j]
      } else {
        mat = -1 * (est_vec %*% t(est_vec))
      }
      #variance along diag
      sigma_sq = as.numeric(temp_se[j,])^2
      diag(mat) = sigma_sq
      for (k in 1:length(mat)){
        if(mat[k]==0) mat[k]=1e-14
      }
      covs[[j]] = as.matrix(mat)
    }
    varcovs[[i]] = na.omit(covs)
  }
  names(varcovs) = var_labels
  return(varcovs)
}

get_mahal = function(var_labels, varcovs, wtd, regvars){
  
  distances = list(NA)
  dist_df = data.frame(NA)
  p_df = data.frame(NA)
  star_df = data.frame(NA)
  
  for (i in 1:length(var_labels)){
    if(regvars){
      if(!wtd){
        temp_err = eval(parse(text = paste("nopvo_regvars.err$", var_labels[i],sep="")))
      } else {
        temp_err = eval(parse(text = paste("nopvo_regvars_wtd.err$", var_labels[i],sep="")))    
      }
    } else {
      if(wtd){
        temp_err = eval(parse(text = paste("nopvo_nonregvars.err$", var_labels[i],sep="")))
      } else {
        temp_err = eval(parse(text = paste("nopvo_nonregvars_wtd.err$", var_labels[i],sep="")))    
      }
      temp_err = eval(parse(text = paste("nopvo_nonregvars.err$", var_labels[i],sep="")))
    }
    for (j in 1:dim(temp_err)[2]){
      diff = as.matrix(temp_err[,j])
      for (k in 1:length(diff)){
        if(diff[k]=="NA") diff[k]=1e-14
      }
      df = length(diff)-1
      mat = varcovs[[i]][[j]]
      result = as.numeric(
        tryCatch(mahal_dist(diff, mat), error = function(e) 
          print("NA")))
      dist_df[i,j] = result
      p_df[i,j] = mahal_sig(result, df)
      star_df[i,j] = star(mahal_sig(result, df))
    }
  }
  
  rownames(dist_df) = reg_var_labels
  rownames(p_df) = reg_var_labels
  rownames(star_df) = reg_var_labels
  
  colnames(dist_df) = colnames(temp_err)
  colnames(p_df) = colnames(temp_err)
  colnames(star_df) = colnames(temp_err)
  
  distances[[c("dist_df")]] = dist_df
  distances[[c("p_df")]] = p_df
  distances[[c("star_df")]] = star_df
  
  return(distances)
}

####################
#REGISTER VARIABLES#
####################

# reg_var_labels <- c(
# 	"gender",
# 	"region",
# 	"province",
# 	"nationality",
# 	"originself",
# 	"originfather",
# 	"originmother",
# 	#"urbanization",
# 	"numpersonshh",
# 	"agecats"
# )

#Finding variance-covariance matrices
#Returns a list of list: variables by bureau

#NOPVO unweighted variance-covariance matrices
nopvo_regvars.vcovs = var_covs(reg_var_labels, regvars = T, cbs = F, wtd = F)

#NOPVO weighted variance-covariance matrices
nopvo_regvars_wtd.vcovs = var_covs(reg_var_labels, regvars = T, cbs = F, wtd = T)

#EBB


#######################
#NONREGISTER VARIABLES#
#######################

# nonreg_var_labels <- c(
# 	"education",
# 	"employment",
# 	"religion",
# 	"move2years",
# 	"health",
# 	"lifesat",
# 	"domicile"
# )

#Finding variance-covariance matrices

#unweighted
nopvo_nonregvars.vcovs = var_covs(nonreg_var_labels, regvars = F, cbs = F, wtd = F)

#weighted 
nopvo_nonregvars_wtd.vcovs = var_covs(nonreg_var_labels, regvars = F, cbs = F, wtd = T)

#cbs variance-covariance
#cbs_nonreg_varcovs = var_covs(nonreg_var_labels, cbs = T, wtd = F)
#TODO: Fix

#######################
#MAHALANOBIS DISTANCES#
#######################

#EBB

#NOPVO 

#register unweighted

nopvo_reg_mahal = get_mahal(reg_var_labels, nopvo_regvars.vcovs, wtd = F, regvars = T)

nopvo_reg_mahal_wtd = get_mahal(reg_var_labels, nopvo_regvars_wtd.vcovs, wtd = T, regvars = T)


for (i in 1:length(reg_var_labels)){
	temp_err = eval(parse(text = paste("nopvo_",reg_var_labels[i],"_err",sep="")))
	temp_err$categories = NULL
	temp_err$variable = NULL
	for (j in 1:length(temp_err)){
		diff = as.matrix(temp_err[,j])
		for (k in 1:length(diff)){if(diff[k]=="NA") diff[k]=1e-14}
		df = length(diff)-1
		mat = nopvo_reg_varcovs[[i]][[j]]
		result = as.numeric(tryCatch(mahal_dist(diff, mat), error = function(e) print("0")))
		nopvo_reg_mahal[i,j] = result
		nopvo_reg_mahal_p[i,j] = mahal_sig(result, df)
		nopvo_reg_mahal_star[i,j] = star(mahal_sig(result, df))
	}
}

#register weighted

for (i in 1:length(reg_var_labels)){
	temp_err = eval(parse(text = paste("nopvo_",reg_var_labels[i],"_err_wtd",sep="")))
	temp_err$X = NULL
	temp_err$categories = NULL
	temp_err$variable = NULL
	for (j in 1:length(temp_err)){
		diff = as.matrix(temp_err[,j])
		for (k in 1:length(diff)){if(diff[k]=="NA") diff[k]=1e-14}
		df = length(diff)-1
		mat = nopvo_reg_varcovs_wtd[[i]][[j]]
		result = as.numeric(tryCatch(mahal_dist(diff, mat), error = function(e) print("0")))
		nopvo_reg_wtd_mahal[i,j] = result
		nopvo_reg_wtd_mahal_p[i,j] = mahal_sig(result, df)
		nopvo_reg_wtd_mahal_star[i,j] = star(mahal_sig(result, df))
	}
}

#nonregister unweighted

nopvo_nonreg_mahal = matrix(0,nrow=length(nonreg_var_labels), ncol=length(nopvo_nonreg_err)-1)
nopvo_nonreg_mahal_p = matrix(0,nrow=length(nonreg_var_labels), ncol=length(nopvo_nonreg_err)-1)
nopvo_nonreg_mahal_star = matrix(0,nrow=length(nonreg_var_labels), ncol=length(nopvo_nonreg_err)-1)

rownames(nopvo_nonreg_mahal) = nonreg_var_labels
rownames(nopvo_nonreg_mahal_p) = nonreg_var_labels
rownames(nopvo_nonreg_mahal_star) = nonreg_var_labels

for (i in 1:length(nonreg_var_labels)){
	temp_err = eval(parse(text = paste("nopvo_",nonreg_var_labels[i],"_err",sep="")))
	temp_err$categories = NULL
	temp_err$variable = NULL
	for (j in 1:length(temp_err)){
		diff = as.matrix(temp_err[,j])
		for (k in 1:length(diff)){if(diff[k]=="NA") diff[k]=1e-14}
		df = length(diff)-1
		mat = nopvo_nonreg_varcovs[[i]][[j]] + cbs_nonreg_varcovs[[i]][[1]]
		result = as.numeric(tryCatch(mahal_dist(diff, mat), error = function(e) print("0")))
		nopvo_nonreg_mahal[i,j] = result
		nopvo_nonreg_mahal_p[i,j] = mahal_sig(result, df)
		nopvo_nonreg_mahal_star[i,j] = star(mahal_sig(result, df))
	}
}

#nonregister weighted

nopvo_nonreg_wtd_mahal = matrix(0,nrow=length(nonreg_var_labels), ncol=length(nopvo_nonreg_err)-1)
nopvo_nonreg_wtd_mahal_p = matrix(0,nrow=length(nonreg_var_labels), ncol=length(nopvo_nonreg_err)-1)
nopvo_nonreg_wtd_mahal_star = matrix(0,nrow=length(nonreg_var_labels), ncol=length(nopvo_nonreg_err)-1)

rownames(nopvo_nonreg_wtd_mahal) = nonreg_var_labels
rownames(nopvo_nonreg_wtd_mahal_p) = nonreg_var_labels
rownames(nopvo_nonreg_wtd_mahal_star) = nonreg_var_labels

for (i in 1:length(nonreg_var_labels)){
	temp_err = eval(parse(text = paste("nopvo_",nonreg_var_labels[i],"_err_wtd",sep="")))
	temp_err$categories = NULL
	temp_err$variable = NULL
	for (j in 1:length(temp_err)){
		diff = as.matrix(temp_err[,j])
		for (k in 1:length(diff)){if(diff[k]=="NA") diff[k]=1e-14}
		df = length(diff)-1
		mat = nopvo_nonreg_varcovs[[i]][[j]] + cbs_nonreg_varcovs[[i]][[1]]
		result = as.numeric(tryCatch(mahal_dist(diff, mat), error = function(e) print("0")))
		nopvo_nonreg_wtd_mahal[i,j] = result
		nopvo_nonreg_wtd_mahal_p[i,j] = mahal_sig(result, df)
		nopvo_nonreg_wtd_mahal_star[i,j] = star(mahal_sig(result, df))
	}
}


#######################
#Writing out CSV files#
#######################

setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/csv output")

#Register, unweighted

# write.csv(nopvo_reg_mahal, "nopvo_reg_mahal.csv")
# write.csv(nopvo_reg_mahal_p, "nopvo_reg_mahal_p.csv")
# write.csv(nopvo_reg_mahal_star, "nopvo_reg_mahal_star.csv")
# 
# #Register, weighted
# 
# write.csv(nopvo_reg_wtd_mahal, "nopvo_reg_wtd_mahal.csv")
# write.csv(nopvo_reg_wtd_mahal_p, "nopvo_reg_wtd_mahal_p.csv")
# write.csv(nopvo_reg_wtd_mahal_star, "nopvo_reg_wtd_mahal_star.csv")
# 
# #Nonregister, unweighted
# 
# write.csv(nopvo_nonreg_mahal, "nopvo_nonreg_mahal.csv")
# write.csv(nopvo_nonreg_mahal_p, "nopvo_nonreg_mahal_p.csv")
# write.csv(nopvo_nonreg_mahal_star, "nopvo_nonreg_mahal_star.csv")
# 
# #Nonregister, weighted
# 
# write.csv(nopvo_nonreg_wtd_mahal, "nopvo_nonreg_wtd_mahal.csv")
# write.csv(nopvo_nonreg_wtd_mahal_p, "nopvo_nonreg_wtd_mahal_p.csv")
# write.csv(nopvo_nonreg_wtd_mahal_star, "nopvo_nonreg_wtd_mahal_star.csv")
