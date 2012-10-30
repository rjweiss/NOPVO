#This is the main script to do the NOPVO comparison against the benchmarks.

rm(list=ls(all=T))
gc()

library(plyr)
library(reshape)

#Load in the NOPVO and benchmark data
#Change the working directory to the R directory
setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/")

#loading benchmarks
load("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/GBA_tables.RData")
rm(
	"freqtables",                    
	"i",                             
	"j",                             
	"nopvo.crosstables",             
	"nopvo.prop_dfs",                
	"nopvo_estimates.df",            
	"nopvo.n",         
	"tmp",                           
	"variables",
	"crosstables",                  
	"gba_dimnames",                 
	"index",                        
	"nopvo",                        
	"nopvo.spss",                   
	"nopvo_gba_comparison.prop_dfs",
	"variable_labels"
)

#loading nopvo register and nonregister variables
#source("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/nopvo_script.R") #loading NOPVO estimates/std errs
#source("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/gba_script.R") #only at CBS
load("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/NOPVO_GBA.RData")

#remove children in house since we're not using this variable anymore
benchmarks$NUMCHILDHH = NULL
benchmarks$URBANIZATION = NULL
names(benchmarks) = tolower(names(benchmarks))

#####################################################################
#Recoding (GBA) register benchmark variables for comparison to NOPVO#
#####################################################################

#IMPORTANT: need to reorder variables to make sure they match
province_order <- names(nopvo_gba_regvars.est$province)
region_order <- names(nopvo_gba_regvars.est$region)
benchmarks$province <- benchmarks$province[province_order,, drop=F]
benchmarks$region <- benchmarks$region[region_order,, drop=F]
rm(province_order, region_order)

#IMPORTANT: need to remove the "Origin unknowns" from benchmarks
#TODO: FIX THIS, IT'S HORRIBLE
tmp <- rownames(benchmarks$originself)
tmp <- tmp[-8]
benchmarks$originself <- as.data.frame(benchmarks$originself[-8,])
rownames(benchmarks$originself) = tmp
names(benchmarks$originself) = c("Percent")
benchmarks$originfather <- as.data.frame(benchmarks$originfather[-8,])
rownames(benchmarks$originfather) = tmp
names(benchmarks$originfather) = c("Percent")
benchmarks$originmother <- as.data.frame(benchmarks$originmother[-8,])
rownames(benchmarks$originmother) = tmp
names(benchmarks$originmother) = c("Percent")

#tmp = rownames(benchmarks$urbanization)
#tmp = tmp[-6]
#benchmarks$urbanization = as.data.frame(benchmarks$urbanization[-6,])
#names(benchmarks$urbanization)= c("Percent")
#rownames(benchmarks$urbanization) = tmp

tmp = rownames(benchmarks$numpersonshh)
tmp = tmp[-7]
benchmarks$numpersonshh = as.data.frame(benchmarks$numpersonshh[-7,])
names(benchmarks$numpersonshh)= c("Percent")
rownames(benchmarks$numpersonshh) = tmp

#Finding modes per benchmarks
benchmark_modes <- list(NA)
benchmark_modes <- lapply(benchmarks, function(x) {
	row.names(x)[which(x == max(x, na.rm = TRUE))]
})

#some hacky stuff for now to take care of the origin questions
#TODO: Fix the hacky 

nopvo_gba_regvars.est[[3]][,7] = nopvo_gba_regvars.est[[3]][,7] + nopvo_gba_regvars.est[[3]][,8]
nopvo_gba_regvars.est[[3]] = nopvo_gba_regvars.est[[3]][,-8]

nopvo_gba_regvars.est[[4]][,7] = nopvo_gba_regvars.est[[4]][,7] + nopvo_gba_regvars.est[[4]][,8]
nopvo_gba_regvars.est[[4]] = nopvo_gba_regvars.est[[4]][,-8]


nopvo_gba_regvars.est[[5]][,7] = nopvo_gba_regvars.est[[5]][,7] + nopvo_gba_regvars.est[[5]][,8]
nopvo_gba_regvars.est[[5]] = nopvo_gba_regvars.est[[5]][,-8]

nopvo_gba_regvars.se[[3]][,7] = nopvo_gba_regvars.se[[3]][,7] + nopvo_gba_regvars.se[[3]][,8]
nopvo_gba_regvars.se[[3]] = nopvo_gba_regvars.se[[3]][,-8]

nopvo_gba_regvars.se[[4]][,7] = nopvo_gba_regvars.se[[4]][,7] + nopvo_gba_regvars.se[[4]][,8]
nopvo_gba_regvars.se[[4]] = nopvo_gba_regvars.se[[4]][,-8]


nopvo_gba_regvars.se[[5]][,7] = nopvo_gba_regvars.se[[5]][,7] + nopvo_gba_regvars.se[[5]][,8]
nopvo_gba_regvars.se[[5]] = nopvo_gba_regvars.se[[5]][,-8]

#TODO: fix the urbanization hack
nopvo_gba_regvars.est$urbanization = NULL
nopvo_gba_regvars.se$urbanization = NULL


##############################################
#NOPVO against GBA percentage estimate errors#
##############################################

#Generates the estimate errors for each variable for each NOPVO panel
#Creates a list object to hold all the errors

nopvo_gba_regvars.err = list(NA)
for (i in 1:length(nopvo_gba_regvars.est)){
	nopvo_gba_regvars.err[[i]] = as.data.frame(apply(nopvo_gba_regvars.est[[i]], 1, function(x) x - t(benchmarks[[i]])))
	names(nopvo_gba_regvars.est[[i]]) = rownames(benchmarks[[i]])
	#names(nopvo_gba_regvars.est[[i]]) = rownames(benchmarks[[i]])
	rownames(nopvo_gba_regvars.err[[i]]) = names(nopvo_gba_regvars.est[[i]])
}

names(nopvo_gba_regvars.err) = names(benchmarks)

#THIS IS JUST TO MAKE EVERYTHING OUT OF 100 AND TO THE .XX
#nopvo_gba_regvars.est <- lapply(nopvo_gba_regvars.est, function(x){
#	round(x, digits=4)*100
#})

#nopvo_gba_regvars.se <- lapply(nopvo_gba_regvars.se, function(x){
#	round(x, digits=4)*100
#})

#nopvo_gba_regvars.err <- lapply(nopvo_gba_regvars.err, function(x){
#	round(x, digits=4)*100
#})


nopvo_reg_est_csv = list(NA)
for(x in 1:length(names(nopvo_gba_regvars.est))){
	temp.name = names(nopvo_gba_regvars.est[x])
	temp.df = eval(parse(text = paste("nopvo_gba_regvars.est$", temp.name, sep = "")))
	temp.df$id = rownames(temp.df)
	temp.df = melt(temp.df, variable_name = "categories")
	temp_cast.df = as.data.frame(as.matrix(t(cast(temp.df, id ~ categories))))
	#temp_cast.df = temp_cast.df[, names(nopvo_reg.n[[i]])]
	temp_cast.df$variable = temp.name
	temp_cast.df$categories = rownames(temp_cast.df)
	nopvo_reg_est_csv[[x]] = temp_cast.df
}

nopvo_reg_se_csv = list(NA)
for(x in 1:length(names(nopvo_gba_regvars.se))){
	temp.name = names(nopvo_gba_regvars.se[x])
	temp.df = eval(parse(text = paste("nopvo_gba_regvars.se$", temp.name, sep = "")))
	temp.df$id = rownames(temp.df)
	temp.df = melt(temp.df, variable_name = "categories")
	temp_cast.df = as.data.frame(as.matrix(t(cast(temp.df, id ~ categories))))
	#temp_cast.df = temp_cast.df[, names(nopvo_reg.n[[x]])]
	temp_cast.df$variable = temp.name
	temp_cast.df$categories = rownames(temp_cast.df)
	nopvo_reg_se_csv[[x]] = temp_cast.df
}

nopvo_reg_err_csv = list(NA)
for(x in 1:length(names(nopvo_gba_regvars.err))){
	temp.name = names(nopvo_gba_regvars.err[x])
	temp.df = eval(parse(text = paste("nopvo_gba_regvars.err$", temp.name, sep = "")))
	temp.df = as.data.frame(t(temp.df))
	temp.df$id = rownames(temp.df)
	temp.df = melt(temp.df, variable_name = "categories")
	temp_cast.df = as.data.frame(as.matrix(t(cast(temp.df, id ~ categories))))
#	temp_cast.df = temp_cast.df[, names(nopvo.n)]
	temp_cast.df$variable = temp.name
	temp_cast.df$categories = rownames(temp_cast.df)
	nopvo_reg_err_csv[[x]] = temp_cast.df
}

benchmarks_csv = list(NA)
for(x in 1:length(names(benchmarks))){
	temp.name = names(benchmarks[x])
	temp.df = eval(parse(text = paste("benchmarks$", temp.name, sep = "")))
	temp.df = as.data.frame(t(temp.df))
	temp.df$id = rownames(temp.df)
	temp.df = melt(temp.df, variable_name = "categories")
	temp_cast.df = as.data.frame(as.matrix(t(cast(temp.df, id ~ categories))))
	temp_cast.df$variable = temp.name
	benchmarks_csv[[x]] = temp_cast.df
}

#Creating giant data.frames of all the estimates and errors
nopvo_reg_est_csv <- do.call(rbind.fill, nopvo_reg_est_csv)
nopvo_reg_se_csv <- do.call(rbind.fill, nopvo_reg_se_csv)
nopvo_reg_err_csv <- do.call(rbind.fill, nopvo_reg_err_csv)
benchmarks_csv <- do.call(rbind.fill, benchmarks_csv)

#Writing comma-separated ASCII files of the giant data.frames
setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/csv output")
write.csv(nopvo_reg_est_csv, "nopvo_reg_est.csv")
write.csv(nopvo_reg_se_csv, "nopvo_reg_se.csv")
write.csv(nopvo_reg_err_csv, "nopvo_reg_err.csv")
write.csv(benchmarks_csv, "benchmarks.csv")

##############################################
#NOPVO against EBB/POLS/IPO percentage estimate errors#
##############################################

#Generates the estimate errors for each variable for each NOPVO panel
#Creates a list object to hold all the errors

#nopvo_gba_nonregvars.err = list(NA)
#for (i in 1:length(nopvo_gba_nonregvars.est)){
#	nopvo_gba_nonregvars.err[[i]] = as.data.frame(apply(nopvo_gba_nonregvars.est[[i]], 1, function(x) x - t(benchmarks[[i]])))
#	names(nopvo_gba_nonregvars.est[[i]]) = rownames(benchmarks[[i]])
#	names(nopvo_gba_nonregvars.est[[i]]) = rownames(benchmarks[[i]])
#	rownames(nopvo_gba_nonregvars.err[[i]]) = names(nopvo_gba_nonregvars.est[[i]])
#}

#names(nopvo_gba_nonregvars.err) = names(benchmarks)

#THIS IS JUST TO MAKE EVERYTHING OUT OF 100 AND TO THE .XX
#nopvo_cbs_nonregvars.est <- lapply(nopvo_cbs_nonregvars.est, function(x){
#	round(x, digits=4)*100
#})

#nopvo_cbs_nonregvars.se <- lapply(nopvo_cbs_nonregvars.se, function(x){
#	round(x, digits=4)*100
#})

#nopvo_cbs_nonregvars.err <- lapply(nopvo_cbs_nonregvars.err, function(x){
#	round(x, digits=4)*100
#})


nopvo_nonreg_est_csv = list(NA)
for(x in 1:length(names(nopvo_cbs_nonregvars.est))){
	temp.name = names(nopvo_cbs_nonregvars.est[x])
	temp.df = eval(parse(text = paste("nopvo_cbs_nonregvars.est$", temp.name, sep = "")))
	temp.df$id = rownames(temp.df)
	temp.df = melt(temp.df, variable_name = "categories")
	temp_cast.df = as.data.frame(as.matrix(t(cast(temp.df, id ~ categories))))
	#temp_cast.df = temp_cast.df[, names(nopvo.n)]
	temp_cast.df$variable = temp.name
	temp_cast.df$categories = rownames(temp_cast.df)
	nopvo_nonreg_est_csv[[x]] = temp_cast.df
}

nopvo_nonreg_se_csv = list(NA)
for(x in 1:length(names(nopvo_cbs_nonregvars.se))){
	temp.name = names(nopvo_cbs_nonregvars.se[x])
	temp.df = eval(parse(text = paste("nopvo_cbs_nonregvars.se$", temp.name, sep = "")))
	temp.df$id = rownames(temp.df)
	temp.df = melt(temp.df, variable_name = "categories")
	temp_cast.df = as.data.frame(as.matrix(t(cast(temp.df, id ~ categories))))
	#temp_cast.df = temp_cast.df[, names(nopvo.n)]
	temp_cast.df$variable = temp.name
	temp_cast.df$categories = rownames(temp_cast.df)
	nopvo_nonreg_se_csv[[x]] = temp_cast.df
}

#nopvo_nonreg_err_csv = list(NA)
#for(x in 1:length(names(nopvo_gba_nonregvars.err))){
#	temp.name = names(nopvo_gba_nonregvars.err[x])
#	temp.df = eval(parse(text = paste("nopvo_gba_nonregvars.err$", temp.name, sep = "")))
#	temp.df = as.data.frame(t(temp.df))
#	temp.df$id = rownames(temp.df)
#	temp.df = melt(temp.df, variable_name = "categories")
#	temp_cast.df = as.data.frame(as.matrix(t(cast(temp.df, id ~ categories))))
#	temp_cast.df = temp_cast.df[, names(nopvo.n)]
#	temp_cast.df$variable = temp.name
#	nopvo_nonreg_err_csv[[x]] = temp_cast.df
#}


#Creating giant data.frames of all the estimates and errors
nopvo_nonreg_est_csv <- do.call(rbind.fill, nopvo_nonreg_est_csv)
nopvo_nonreg_se_csv <- do.call(rbind.fill, nopvo_nonreg_se_csv)
#nopvo_nonreg_err_csv <- do.call(rbind.fill, nopvo_nonreg_err_csv)
benchmarks_csv <- do.call(rbind.fill, benchmarks_csv)

#Writing comma-separated ASCII files of the giant data.frames
setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/csv output")
write.csv(nopvo_nonreg_est_csv, "nopvo_nonreg_est.csv")
write.csv(nopvo_nonreg_se_csv, "nopvo_nonreg_se.csv")
#write.csv(nopvo.n, "nopvo_n.csv")
#write.csv(nopvo_nonreg_err_csv, "nopvo_nonreg_err.csv")
