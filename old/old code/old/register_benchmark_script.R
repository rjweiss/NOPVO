#This is the main script to do the NOPVO comparison against the benchmarks.

rm(list=ls(all=T))
gc()

#Load in the NOPVO and benchmark data
#Change the working directory to the R directory
setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/")

#loading benchmarks

load("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/GBA_tables.RData")
rm(
	"benchmarks",                    
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
source("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/nopvo_script.R") #loading NOPVO estimates/std errs
#source("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/gba_script.R") #only at CBS

#remove children in house since we're not using this variable anymore
benchmarks$NUMCHILDHH = NULL
names(benchmarks) = tolower(names(benchmarks))

#####################################################################
#Recoding (GBA) register benchmark variables for comparison to NOPVO#
#####################################################################

#IMPORTANT: need to reorder variables to make sure they match
province_order <- names(nopvo_gba_comparison.prop_dfs$province)
region_order <- names(nopvo_gba_comparison.prop_dfs$region)
benchmarks$PROVINCE <- benchmarks$PROVINCE[province_order,, drop=F]
benchmarks$REGION <- benchmarks$REGION[region_order,, drop=F]
rm(province_order, region_order)

#IMPORTANT: need to remove the "Origin unknowns" from benchmarks
#THIS IS HORRIBLE
tmp <- rownames(benchmarks$ORIGINSELF)
tmp <- tmp[-8]
benchmarks$ORIGINSELF <- as.data.frame(benchmarks$ORIGINSELF[-8,])
rownames(benchmarks$ORIGINSELF) = tmp
names(benchmarks$ORIGINSELF) = c("Percent")
benchmarks$ORIGINFATHER <- as.data.frame(benchmarks$ORIGINFATHER[-8,])
rownames(benchmarks$ORIGINFATHER) = tmp
names(benchmarks$ORIGINFATHER) = c("Percent")
benchmarks$ORIGINMOTHER <- as.data.frame(benchmarks$ORIGINMOTHER[-8,])
rownames(benchmarks$ORIGINMOTHER) = tmp
names(benchmarks$ORIGINMOTHER) = c("Percent")

names(estimate_errors) = names(benchmarks)

#Finding modes per benchmarks
benchmark_modes <- list(NA)
benchmark_modes <- lapply(benchmarks, function(x) {
	row.names(x)[which(x == max(x, na.rm = TRUE))]
})


#Generates the estimate errors for each variable for each NOPVO panel
#Creates a list object to hold all the errors
estimate_errors = list(NA)
for (i in 1:length(nopvo_gba_comparison.prop_dfs)){
	estimate_errors[[i]] = as.data.frame(apply(nopvo_gba_comparison.prop_dfs[[i]], 1, function(x) x - t(benchmarks[[i]])))
	names(nopvo_gba_comparison.prop_dfs[[i]]) = rownames(benchmarks[[i]])
	names(nopvo_gba_comparison.se_dfs[[i]]) = rownames(benchmarks[[i]])
	rownames(estimate_errors[[i]]) = names(nopvo_gba_comparison.prop_dfs[[i]])
}



nopvo_gba_comparison.prop_dfs <- lapply(nopvo_gba_comparison.prop_dfs, function(x){
	round(x, digits=4)#*100
})

nopvo_gba_comparison.se_dfs <- lapply(nopvo_gba_comparison.se_dfs, function(x){
	round(x, digits=4)#*100
})



nopvo_est_csv = list(NA)
for(x in 1:length(names(nopvo_gba_comparison.prop_dfs))){
	temp.name = names(nopvo_gba_comparison.prop_dfs[x])
	temp.df = eval(parse(text = paste("nopvo_gba_comparison.prop_dfs$", temp.name, sep = "")))
	temp.df$id = rownames(temp.df)
	temp.df = melt(temp.df, variable_name = "categories")
	temp_cast.df = as.data.frame(as.matrix(t(cast(temp.df, id ~ categories))))
	temp_cast.df$variable = temp.name
	nopvo_est_csv[[x]] = temp_cast.df
}

nopvo_se_csv = list(NA)
for(x in 1:length(names(nopvo_gba_comparison.se_dfs))){
	temp.name = names(nopvo_gba_comparison.se_dfs[x])
	temp.df = eval(parse(text = paste("nopvo_gba_comparison.se_dfs$", temp.name, sep = "")))
	temp.df$id = rownames(temp.df)
	temp.df = melt(temp.df, variable_name = "categories")
	temp_cast.df = as.data.frame(as.matrix(t(cast(temp.df, id ~ categories))))
	temp_cast.df$variable = temp.name
	nopvo_se_csv[[x]] = temp_cast.df
}

nopvo_err_csv = list(NA)
for(x in 1:length(names(estimate_errors))){
	temp.name = names(estimate_errors[x])
	temp.df = eval(parse(text = paste("estimate_errors$", temp.name, sep = "")))
	temp.df = as.data.frame(t(temp.df))
	temp.df$id = rownames(temp.df)
	temp.df = melt(temp.df, variable_name = "categories")
	temp_cast.df = as.data.frame(as.matrix(t(cast(temp.df, id ~ categories))))
	temp_cast.df$variable = temp.name
	nopvo_err_csv[[x]] = temp_cast.df
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
nopvo_est_csv <- do.call(rbind, nopvo_est_csv)
nopvo_se_csv <- do.call(rbind, nopvo_se_csv)
nopvo_err_csv <- do.call(rbind, nopvo_err_csv)
benchmarks_csv <- do.call(rbind, benchmarks_csv)

#Writing comma-separated ASCII files of the giant data.frames
setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/csv output")
write.csv(nopvo_est_csv, "nopvo_register_estimates.csv")
write.csv(nopvo_se_csv, "nopvo_register_standarderrors.csv")
write.csv(nopvo_err_csv, "nopvo_register_errors.csv")
write.csv(benchmarks_csv, "benchmarks.csv")