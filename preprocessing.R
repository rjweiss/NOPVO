# rm(list=ls(all=T))
# gc()

#source scripts
#source("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/nopvo_script.R")
#source("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/cbs_script.R")
#source("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/liss_script.R") #unloads Hmisc, loads car

#Load in data

load("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/GBA_tables.RData")
load("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/CBS.RData")
load("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/LISS.RData")
load("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/NOPVO.RData")

#helper functions
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
  "variable_labels"
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
province_order <- names(nopvo_regvars.est$province)
region_order <- names(nopvo_regvars.est$region)
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

#TODO: bring back urbanization
#tmp = rownames(reg_benchmarks$urbanization)
#tmp = tmp[-6]
#reg_benchmarks$urbanization = as.data.frame(reg_benchmarks$urbanization[-6,])
#names(reg_benchmarks$urbanization)= c("Percent")
#rownames(reg_benchmarks$urbanization) = tmp

tmp = rownames(reg_benchmarks$numpersonshh)
tmp = tmp[-7]
reg_benchmarks$numpersonshh = as.data.frame(reg_benchmarks$numpersonshh[-7,])
names(reg_benchmarks$numpersonshh)= c("Percent")
rownames(reg_benchmarks$numpersonshh) = tmp




#some hacky stuff for now to take care of the origin questions
#TODO: Fix the hacky 

nopvo_regvars.est[[3]][,7] = nopvo_regvars.est[[3]][,7] + nopvo_regvars.est[[3]][,8]
nopvo_regvars.est[[3]] = nopvo_regvars.est[[3]][,-8]

nopvo_regvars.est[[4]][,7] = nopvo_regvars.est[[4]][,7] + nopvo_regvars.est[[4]][,8]
nopvo_regvars.est[[4]] = nopvo_regvars.est[[4]][,-8]


nopvo_regvars.est[[5]][,7] = nopvo_regvars.est[[5]][,7] + nopvo_regvars.est[[5]][,8]
nopvo_regvars.est[[5]] = nopvo_regvars.est[[5]][,-8]

nopvo_regvars.se[[3]][,7] = nopvo_regvars.se[[3]][,7] + nopvo_regvars.se[[3]][,8]
nopvo_regvars.se[[3]] = nopvo_regvars.se[[3]][,-8]

nopvo_regvars.se[[4]][,7] = nopvo_regvars.se[[4]][,7] + nopvo_regvars.se[[4]][,8]
nopvo_regvars.se[[4]] = nopvo_regvars.se[[4]][,-8]


nopvo_regvars.se[[5]][,7] = nopvo_regvars.se[[5]][,7] + nopvo_regvars.se[[5]][,8]
nopvo_regvars.se[[5]] = nopvo_regvars.se[[5]][,-8]

#TODO: fix the urbanization hack
nopvo_regvars.est$urbanization = NULL
nopvo_regvars.se$urbanization = NULL

#make sure nopvo weighted matches nopvo unweighted for nonregvars
#this is a horrible hack TODO: fix this
names(nopvo_nonregvars_wtd.est$education) = names(nopvo_nonregvars.est$education)
names(nopvo_nonregvars_wtd.est$employment) = names(nopvo_nonregvars.est$employment)
names(nopvo_nonregvars_wtd.est$religion) = names(nopvo_nonregvars.est$religion)
names(nopvo_nonregvars_wtd.est$move2years) = names(nopvo_nonregvars.est$move2years)
names(nopvo_nonregvars_wtd.est$health) = names(nopvo_nonregvars.est$health)
names(nopvo_nonregvars_wtd.est$lifesat) = names(nopvo_nonregvars.est$lifesat)
names(nopvo_nonregvars_wtd.est$domicile) = names(nopvo_nonregvars.est$domicile)

names(nopvo_nonregvars_wtd.se$education) = names(nopvo_nonregvars_wtd.est$education)
names(nopvo_nonregvars_wtd.se$employment) = names(nopvo_nonregvars_wtd.est$employment)
names(nopvo_nonregvars_wtd.se$religion) = names(nopvo_nonregvars_wtd.est$religion)
names(nopvo_nonregvars_wtd.se$move2years) = names(nopvo_nonregvars_wtd.est$move2years)
names(nopvo_nonregvars_wtd.se$health) = names(nopvo_nonregvars_wtd.est$health)
names(nopvo_nonregvars_wtd.se$lifesat) = names(nopvo_nonregvars_wtd.est$lifesat)
names(nopvo_nonregvars_wtd.se$domicile) = names(nopvo_nonregvars_wtd.est$domicile)

rownames(nonreg_benchmarks$education) = names(nopvo_nonregvars.est$education)
rownames(nonreg_benchmarks$employment) = names(nopvo_nonregvars.est$employment)
rownames(nonreg_benchmarks$religion) = names(nopvo_nonregvars.est$religion)
rownames(nonreg_benchmarks$move2years) = names(nopvo_nonregvars.est$move2years)
rownames(nonreg_benchmarks$health) = names(nopvo_nonregvars.est$health)
rownames(nonreg_benchmarks$lifesat) = names(nopvo_nonregvars.est$lifesat)
rownames(nonreg_benchmarks$domicile) = names(nopvo_nonregvars.est$domicile)

rownames(nonreg_benchmarks_se$education) = names(nopvo_nonregvars.est$education)
rownames(nonreg_benchmarks_se$employment) = names(nopvo_nonregvars.est$employment)
rownames(nonreg_benchmarks_se$religion) = names(nopvo_nonregvars.est$religion)
rownames(nonreg_benchmarks_se$move2years) = names(nopvo_nonregvars.est$move2years)
rownames(nonreg_benchmarks_se$health) = names(nopvo_nonregvars.est$health)
rownames(nonreg_benchmarks_se$lifesat) = names(nopvo_nonregvars.est$lifesat)
rownames(nonreg_benchmarks_se$domicile) = names(nopvo_nonregvars.est$domicile)

colnames(nopvo_regvars.est$gender) = rownames(reg_benchmarks$gender)
colnames(nopvo_regvars.est$nationality) = rownames(reg_benchmarks$nationality)
colnames(nopvo_regvars.est$originself) = rownames(reg_benchmarks$originself)
colnames(nopvo_regvars.est$originmother) = rownames(reg_benchmarks$originmother)
colnames(nopvo_regvars.est$originfather) = rownames(reg_benchmarks$originfather)
colnames(nopvo_regvars.est$numpersonshh) = rownames(reg_benchmarks$numpersonshh)

colnames(nopvo_regvars.se$gender) = rownames(reg_benchmarks$gender)
colnames(nopvo_regvars.se$nationality) = rownames(reg_benchmarks$nationality)
colnames(nopvo_regvars.se$originself) = rownames(reg_benchmarks$originself)
colnames(nopvo_regvars.se$originmother) = rownames(reg_benchmarks$originmother)
colnames(nopvo_regvars.se$originfather) = rownames(reg_benchmarks$originfather)
colnames(nopvo_regvars.se$numpersonshh) = rownames(reg_benchmarks$numpersonshh)

colnames(nopvo_regvars_wtd.est$gender) = rownames(reg_benchmarks$gender)
colnames(nopvo_regvars_wtd.est$nationality) = rownames(reg_benchmarks$nationality)
colnames(nopvo_regvars_wtd.est$originself) = rownames(reg_benchmarks$originself)
colnames(nopvo_regvars_wtd.est$originmother) = rownames(reg_benchmarks$originmother)
colnames(nopvo_regvars_wtd.est$originfather) = rownames(reg_benchmarks$originfather)
colnames(nopvo_regvars_wtd.est$numpersonshh) = rownames(reg_benchmarks$numpersonshh)
colnames(nopvo_regvars_wtd.est$region) = rownames(reg_benchmarks$region)
colnames(nopvo_regvars_wtd.est$agecats) = rownames(reg_benchmarks$agecats)

colnames(nopvo_regvars_wtd.se$gender) = rownames(reg_benchmarks$gender)
colnames(nopvo_regvars_wtd.se$nationality) = rownames(reg_benchmarks$nationality)
colnames(nopvo_regvars_wtd.se$originself) = rownames(reg_benchmarks$originself)
colnames(nopvo_regvars_wtd.se$originmother) = rownames(reg_benchmarks$originmother)
colnames(nopvo_regvars_wtd.se$originfather) = rownames(reg_benchmarks$originfather)
colnames(nopvo_regvars_wtd.se$numpersonshh) = rownames(reg_benchmarks$numpersonshh)
colnames(nopvo_regvars_wtd.se$region) = rownames(reg_benchmarks$region)
colnames(nopvo_regvars_wtd.se$agecats) = rownames(reg_benchmarks$agecats)


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
nopvo_regvars_est <- reformat(nopvo_regvars.est)
nopvo_regvars_se <- reformat(nopvo_regvars.se)
nopvo_regvars_wtd_est <- reformat(nopvo_regvars_wtd.est)
nopvo_regvars_wtd_se <- reformat(nopvo_regvars_wtd.se)
nopvo_nonregvars_est <- reformat(nopvo_nonregvars.est)
nopvo_nonregvars_se <- reformat(nopvo_nonregvars.se)
nopvo_nonregvars_wtd_est <- reformat(nopvo_nonregvars_wtd.est)
nopvo_nonregvars_wtd_se <- reformat(nopvo_nonregvars_wtd.se)
reg_benchmarks <- reformat(reg_benchmarks, benchmarks=T)
nonreg_benchmarks <- reformat(nonreg_benchmarks, benchmarks=T)
nonreg_benchmarks_se <- reformat(nonreg_benchmarks_se, benchmarks=T)
nonreg_benchmarks = sqldf("select est._id, est._categories, est.value, se.value as se from nonreg_benchmarks as est, nonreg_benchmarks_se as se where est._categories = se._categories")
names(nonreg_benchmarks) = c("id", "categories", "value", "se")
reg_benchmark_modes <- melt(reg_benchmark_modes)
nonreg_benchmark_modes <- melt(nonreg_benchmark_modes)




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