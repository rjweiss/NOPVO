rm(list=ls(all=T))
gc()

#TODO: Fix the 

#source scripts
#source("/Users/Rebecca/Dropbox/NOPVO/analysis/scripts/nopvo_script.R")
#source("/Users/Rebecca/Dropbox/NOPVO/analysis/scripts/cbs_script.R")
#source("/Users/Rebecca/Dropbox/NOPVO/analysis/scripts/liss_script.R") #unloads Hmisc, loads car

#Load in data

load("/Users/Rebecca/Dropbox/research/NOPVO/analysis/scripts/GBA_tables.RData")
load("/Users/Rebecca/Dropbox/research/NOPVO/analysis/scripts/CBS.RData")
load("/Users/Rebecca/Dropbox/research/NOPVO/analysis/scripts/LISS.RData")
load("/Users/Rebecca/Dropbox/research/NOPVO/analysis/scripts/NOPVO.RData")

#functions
reformat <- function(l, benchmarks=F){
  if(benchmarks){
    ld = ldply(l, function(df){df$.categories = rownames(df); melt(df)})
  } else {
    ld = ldply(l, .progress = "none", function(df){df$.bureau = rownames(df); melt(df)})
  }
  return(ld)
}

#register variable benchmarks
reg_benchmarks = benchmarks

rownames(reg_benchmarks$AGECATS)[5] = c("(54,66]")

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
  "variable_labels",
  "var",
  "n"
  )

#loading nopvo register and nonregister variables

#remove children in house since we're not using this variable anymore
reg_benchmarks$NUMCHILDHH = NULL
reg_benchmarks$URBANIZATION = NULL #temporarily removing urbanization

#make sure benchmark names match variable names
names(reg_benchmarks) = tolower(names(reg_benchmarks))

#####################################################################
#Recoding (GBA) register benchmark variables for comparison to NOPVO#
#####################################################################

reg_benchmarks$nationality[1,] = reg_benchmarks$nationality[1,] + reg_benchmarks$nationality[3,]
tmp = rownames(reg_benchmarks$nationality)
tmp <- tmp[-3]
reg_benchmarks$nationality = as.data.frame(reg_benchmarks$nationality[-3,])
names(reg_benchmarks$nationality) = c("Percent")
rownames(reg_benchmarks$nationality) = tmp

#IMPORTANT: need to reorder variables to make sure they match
province_order <- names(nopvo_regvars_est$province)
region_order <- names(nopvo_regvars_est$region)
reg_benchmarks$province <- reg_benchmarks$province[province_order,, drop=F]
reg_benchmarks$region <- reg_benchmarks$region[region_order,, drop=F]
rm(province_order, region_order)

#IMPORTANT: need to remove the "Origin unknowns" from reg_benchmarks
#TODO: FIX THIS, IT'S HORRIBLE
tmp <- rownames(reg_benchmarks$originself)
tmp <- tmp[-8]
reg_benchmarks$originself <- as.data.frame(reg_benchmarks$originself[-8,])
rownames(reg_benchmarks$originself) = tmp
names(reg_benchmarks$originself) = c("Percent")
reg_benchmarks$originfather <- as.data.frame(reg_benchmarks$originfather[-8,])
rownames(reg_benchmarks$originfather) = tmp
names(reg_benchmarks$originfather) = c("Percent")
reg_benchmarks$originmother <- as.data.frame(reg_benchmarks$originmother[-8,])
rownames(reg_benchmarks$originmother) = tmp
names(reg_benchmarks$originmother) = c("Percent")

tmp = rownames(reg_benchmarks$numpersonshh)
tmp = tmp[-7]
reg_benchmarks$numpersonshh = as.data.frame(reg_benchmarks$numpersonshh[-7,])
names(reg_benchmarks$numpersonshh)= c("Percent")
rownames(reg_benchmarks$numpersonshh) = tmp


#Finding modes per reg_benchmarks
reg_benchmark_modes <- list(NA)
reg_benchmark_modes <- lapply(reg_benchmarks, function(x) {
  row.names(x)[which(x == max(x, na.rm = TRUE))]
})

#Finding modes per nonreg_benchmarks
nonreg_benchmark_modes <- list(NA)
nonreg_benchmark_modes <- lapply(nonreg_benchmarks, function(x) {
  row.names(x)[which(x == max(x, na.rm = TRUE))]
})

#reformatting


#Reformat all lists into single data.frames
nopvo_regvars_est <- reformat(nopvo_regvars_est)
nopvo_regvars_se <- reformat(nopvo_regvars_se)
nopvo_nonregvars_est <- reformat(nopvo_nonregvars_est)
nopvo_nonregvars_se <- reformat(nopvo_nonregvars_se)
reg_benchmarks <- reformat(reg_benchmarks, benchmarks=T)
nonreg_benchmarks <- reformat(nonreg_benchmarks, benchmarks=T)
nonreg_benchmarks_se <- reformat(nonreg_benchmarks_se, benchmarks=T)

nonreg_benchmarks = sqldf("select est._id, est._categories, est.value, se.value as se from nonreg_benchmarks as est, nonreg_benchmarks_se as se where est._categories = se._categories")
names(nonreg_benchmarks) = c("id", "categories", "value", "se")
reg_benchmark_modes <- melt(reg_benchmark_modes)
nonreg_benchmark_modes <- melt(nonreg_benchmark_modes)

names(nopvo_regvars_est) = c("variable", "bureau", "category", "value")
names(nopvo_regvars_se) = c("variable", "bureau", "category", "value")
names(nopvo_nonregvars_est) = c("variable", "bureau", "category", "value")
names(nopvo_nonregvars_se) = c("variable", "bureau", "category", "value")
names(nopvo_regvars_est) = c("variable", "bureau", "category", "value")

names(reg_benchmarks) = c("variable", "category", "var", "value")
names(nonreg_benchmarks) = c("variable", "category", "var", "value")
names(nonreg_benchmarks_se) = c("variable", "category", "var", "value")

rm(list = setdiff(ls(), c(
  "cbs_n",
  "liss_n",
  "nopvo_n",
  "ebb_reg_err",
  "ebb_reg_est",
  "ebb_reg_se",
  "liss_est",
  "liss_se",
  "nonreg_benchmark_modes",
  "nonreg_benchmarks",
  "nonreg_benchmarks_se",
  "nopvo_nonregvars_est",
  "nopvo_nonregvars_se",
  "nopvo_nonregvars_err",
  "nopvo_nonregvars_wtd_est",
  "nopvo_nonregvars_wtd_se",
  "nopvo_nonregvars_wtd_err",
  "nopvo_regvars_est",
  "nopvo_regvars_se",
  "nopvo_regvars_err",
  "nopvo_regvars_wtd_est",
  "nopvo_regvars_wtd_se",
  "nopvo_regvars_wtd_err",
  "pols_reg_est",
  "pols_reg_err",
  "pols_reg_se",
  "reg_benchmark_modes",
  "reg_benchmarks"
  )))