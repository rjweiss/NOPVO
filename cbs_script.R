#CBS SCRIPT

rm(list=ls(all=T))
gc()

setwd("/Users/Rebecca/Dropbox/research/NOPVO/analysis/data")

#################
#Reading in data#
#################

#register variables

ebb_reg_est <- read.csv("ebb_reg_est.csv")
ebb_reg_se <- read.csv("ebb_reg_se.csv")
pols_reg_est <- read.csv("pols_reg_est.csv")
pols_reg_se <- read.csv("pols_reg_se.csv")
#ipo <- read.csv("ipo.csv")

#nonregister variables

ebb_nonreg_est <- read.csv("ebb_nonreg_est.csv")
ebb_nonreg_se <- read.csv("ebb_nonreg_se.csv")
pols_nonreg_est <- read.csv("pols_nonreg_est.csv")
pols_nonreg_se <- read.csv("pols_nonreg_se.csv")

pols_nonreg_est = pols_nonreg_est[which(pols_nonreg_est$variable != "education"),]
pols_nonreg_se = pols_nonreg_se[which(pols_nonreg_se$variable != "education"),]


#####
#EBB#
#####

#register variables

#percentage point estimates 2006
ebb_cali_err6 <- ebb_reg_est$calibrated2006 - ebb_reg_est$benchmarks
ebb_incl_err6 <- ebb_reg_est$calibrated2006 - ebb_reg_est$benchmarks

ebb_cali_err8 <- ebb_reg_est$calibrated2008 - ebb_reg_est$benchmarks
ebb_incl_err8 <- ebb_reg_est$inclusion2008 - ebb_reg_est$benchmarks

ebb_reg_err = data.frame(ebb_cali_err6,ebb_incl_err6, ebb_cali_err8,ebb_incl_err8)
ebb_reg_err$variable = ebb_reg_est$variable
ebb_reg_err$categories = ebb_reg_est$X

ebb_reg_est$categories = ebb_reg_est$X
ebb_reg_est$benchmarks = NULL
ebb_reg_est$X = NULL

#names(ebb_reg_est) = names(ebb_reg_err)

ebb_reg_se$categories = ebb_reg_se$X
ebb_reg_se$benchmarks = NULL
ebb_reg_se$X = NULL

#names(ebb_reg_se) = names(ebb_reg_err)

#####
#IPO#
#####


######
#POLS#
######

#percentage point estimates 2006
pols_cali_err6 <- pols_reg_est$calibrated2006 - pols_reg_est$benchmarks
pols_incl_err6 <- pols_reg_est$inclusion2006 - pols_reg_est$benchmarks

pols_cali_err8 <- pols_reg_est$calibrated2008 - pols_reg_est$benchmarks
pols_incl_err8 <- pols_reg_est$inclusion2008 - pols_reg_est$benchmarks

pols_reg_err = data.frame(pols_cali_err6,pols_incl_err6, pols_cali_err8,pols_incl_err8)
pols_reg_err$variable = pols_reg_est$variable
pols_reg_err$categories = pols_reg_est$X

pols_reg_est$categories = pols_reg_est$X
pols_reg_est$benchmarks = NULL
pols_reg_est$X = NULL

#names(pols_reg_est) = names(pols_reg_err)

pols_reg_se$categories = pols_reg_se$X
pols_reg_se$benchmarks = NULL
pols_reg_se$X = NULL

#names(pols_reg_se) = names(pols_reg_err)

################################
#Making non-register benchmarks#
################################

nonreg_var_labels <- c(
	"education",
	"employment",
	"religion",
	"move2years",
	"health",
	"lifesat",
	"domicile"
)

nonreg_benchmarks = rbind(pols_nonreg_est, ebb_nonreg_est)
nonreg_benchmarks_se = rbind(pols_nonreg_se, ebb_nonreg_se)

nonreg_benchmarks$cali2006 = NULL
nonreg_benchmarks$cali2008 = NULL
nonreg_benchmarks$incl2008 = NULL

nonreg_benchmarks_se$cali2006_se = NULL
nonreg_benchmarks_se$cali2008_se = NULL
nonreg_benchmarks_se$incl2008_se = NULL

names(nonreg_benchmarks) = c("categories", "percent", "variable")

names(nonreg_benchmarks_se) = c("categories", "percent", "variable")

write.csv(nonreg_benchmarks, "/Users/Rebecca/Dropbox/research/NOPVO/analysis/csv output/nonreg_benchmarks.csv")
write.csv(nonreg_benchmarks_se, "/Users/Rebecca/Dropbox/research/NOPVO/analysis/csv output/nonreg_benchmarks_se.csv")

for (i in 1:length(nonreg_var_labels)){
	assign(
		paste(nonreg_var_labels[i],sep=""),
		nonreg_benchmarks[nonreg_benchmarks$variable == nonreg_var_labels[i],]
)}

nonreg_benchmarks = list(NA)
for (i in 1:length(nonreg_var_labels)){
	temp = eval(parse(text = paste(nonreg_var_labels[i])))
	rownames(temp) = as.character(temp$categories)
	temp$variable = NULL
	temp$categories = NULL
	nonreg_benchmarks[[i]] = temp
}

names(nonreg_benchmarks) = nonreg_var_labels

for (i in 1:length(nonreg_var_labels)){
  assign(
    paste(nonreg_var_labels[i],sep=""),
    nonreg_benchmarks_se[nonreg_benchmarks_se$variable == nonreg_var_labels[i],]
    )}

nonreg_benchmarks_se = list(NA)
for (i in 1:length(nonreg_var_labels)){
  temp = eval(parse(text = paste(nonreg_var_labels[i])))
  rownames(temp) = as.character(temp$categories)
  temp$variable = NULL
  temp$categories = NULL
  nonreg_benchmarks_se[[i]] = temp
}

names(nonreg_benchmarks_se) = nonreg_var_labels
################
#Constructing n#
################

#n given by Jan

ipo2006 = 88593
ipo2008 = 90726
ebb2006 = 10589
ebb2008 = 10600
pols2006 = 9607
pols2008 = 9499

cbs_total_n = data.frame(ipo2006, ipo2008, ebb2006, ebb2008, pols2006, pols2008)

cbs_n = data.frame(cbs_total_n$ebb2006, cbs_total_n$ebb2006, cbs_total_n$pols2006, cbs_total_n$pols2006, cbs_total_n$pols2006, cbs_total_n$pols2006, cbs_total_n$pols2006)
names(cbs_n) = nonreg_var_labels

#######################
#Writing out CSV files#
#######################
# 
# setwd("/Users/Rebecca/research/Dropbox/NOPVO/analysis/csv output")
# 
# write.csv(ebb_reg_est, "ebb_reg_est.csv")
# write.csv(ebb_reg_err, "ebb_reg_err.csv")
# write.csv(ebb_reg_se, "ebb_reg_se.csv")
# 
# write.csv(pols_reg_est, "pols_reg_est.csv")
# write.csv(pols_reg_err, "pols_reg_err.csv")
# write.csv(pols_reg_se, "pols_reg_se.csv")
# 
# write.csv(cbs_n, "cbs_n.csv")

setwd("/Users/Rebecca/Dropbox/research/NOPVO/analysis/data")
rm(list=(setdiff(ls(), c(
  "nonreg_benchmarks", 
  "nonreg_benchmarks_est", 
  "nonreg_benchmarks_se", 
  "pols_reg_est",
  "pols_reg_se",
  "pols_reg_err",
#  "ipo_reg_est", #need to add still
  "ebb_reg_est",
  "ebb_reg_se",
  "ebb_reg_err",
  "cbs_n"))))

save.image("/Users/Rebecca/Dropbox/research/NOPVO/analysis/scripts/CBS.RData")
