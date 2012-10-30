#This is the main script to do the NOPVO comparison against the benchmarks.

rm(list=ls(all=T))
gc()

#Load in the NOPVO and benchmark data
#Change the working directory to the R directory
setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/")
load("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/GBA_tables.RData")
#load('GBA_tables.RData')
#source('nopvo_script.R')
#loading benchmarks

source("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/nopvo_script.R") #loading NOPVO estimates/std errs
#source("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/gba_script.R") #only at CBS

#rename benchmark nationality for comparison
benchmarks$NATIONALITY[1,] = benchmarks$NATIONALITY[1,] + benchmarks$NATIONALITY[3,]
benchmarks$NATIONALITY = as.data.frame(benchmarks$NATIONALITY[-3,])
names(benchmarks$NATIONALITY) = c("Percent")
rownames(benchmarks$NATIONALITY) = c("Dutch", "non-Dutch")

benchmarks$NUMCHILDHH = NULL

#cleaning up my act
rm(
crosstables,
freqtables,
index,
tmp,
gba_dimnames)

#IMPORTANT: need to reorder variables to make sure they match
province_order <- names(nopvo_gba_comparison.prop_dfs$PROVINCE)
region_order <- names(nopvo_gba_comparison.prop_dfs$REGION)
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


#Generates the estimate errors for each variable for each NOPVO panel
#Creates a list object to hold all the errors
estimate_errors = list(NA)
for (i in 1:length(nopvo_gba_comparison.prop_dfs)){
	estimate_errors[[i]] = as.data.frame(apply(nopvo_gba_comparison.prop_dfs[[i]], 1, function(x) x - t(benchmarks[[i]])))
	names(nopvo_gba_comparison.prop_dfs[[i]]) = rownames(benchmarks[[i]])
	names(nopvo_gba_comparison.se_dfs[[i]]) = rownames(benchmarks[[i]])
	rownames(estimate_errors[[i]]) = names(nopvo_gba_comparison.prop_dfs[[i]])
}


#multiplying by 100 for better tables
#might not want to do this here
estimate_errors <- lapply(estimate_errors, function(x){
	round(x, digits=4)*100
})

names(estimate_errors) = names(benchmarks)

nopvo_gba_comparison.prop_dfs <- lapply(nopvo_gba_comparison.prop_dfs, function(x){
	round(x, digits=4)*100
})

nopvo_gba_comparison.se_dfs <- lapply(nopvo_gba_comparison.se_dfs, function(x){
	round(x, digits=4)*100
})

#Finding modes per benchmarks
benchmark_modes <- list(NA)
benchmark_modes <- lapply(benchmarks, function(x) {
	row.names(x)[which(x == max(x, na.rm = TRUE))]
})

#Creating giant data.frames of all the estimates and errors
nopvo_register_estimates.df <- t(do.call(cbind, nopvo_gba_comparison.prop_dfs))
nopvo_register_ses.df <- t(do.call(cbind, nopvo_gba_comparison.se_dfs))
nopvo_register_errors.df <- do.call(rbind, estimate_errors)
benchmarks.df <- do.call(rbind, benchmarks)
benchmarks.df <- as.data.frame(t(round(benchmarks.df, digits=4)*100))
benchmark_modes.temp <- do.call(cbind, benchmark_modes)

benchmark_modes.df = NULL
for (i in 1:length(benchmark_modes.temp)) {
	benchmark_modes.df[i] = paste(colnames(benchmark_modes.temp)[i], ".", benchmark_modes.temp[i], sep = "")
}
benchmark_modes.df <-  as.matrix(benchmark_modes.df)

#final improvements
benchmarks.df <- as.data.frame(benchmarks.df)
#names(nopvo_register_estimates.df) = names(benchmarks.df)
names(nopvo_register_ses.df) = names(benchmarks.df)
nopvo_register_errors.df <- as.data.frame(nopvo_register_errors.df)
colnames(nopvo_register_errors.df) = names(benchmarks.df)
modal_variable_names <- c("Males",
"Only Dutch Nationality",
"Born in Netherlands",
"Mother born in Netherlands",
"Father born in Netherlands",
"Urbanization: Strong",
"Two people in the household",
#"No children in the household",
"Ages 33 to 44",
"Nielsen Region West",
"Zuid-Holland"
)


rm(
i,
benchmark_modes.temp,
nopvo_se.df)

#Writing comma-separated ASCII files of the giant data.frames
setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/output")
write.csv(nopvo_register_estimates.df, "register_estimates.csv")
write.csv(nopvo_register_ses.df, "register_standarderrors.csv")
write.csv(nopvo_register_errors.df, "register_errors.csv")
write.csv(t(benchmarks.df), "benchmarks.csv")
write.csv(benchmark_modes.df, "benchmark_modes.csv")

#Saving the workspace as an .RData file (to send to others easily)
#save.image("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/NOPVO_GBA.RData")
