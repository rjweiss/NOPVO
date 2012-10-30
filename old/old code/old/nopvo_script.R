#NOPVO PREPROCESSING SCRIPT
#This script takes the NOPVO english .SAV file and creates a data.frame with the following:
#1) Subsets on the variables of interest
#2) Creates crosstables of all the variables
#3) Takes the proportional estimates for each category in each variable
#4) Outputs a data.frame ("nopvo.prop") to be compared against benchmarks

#########################################
#Reading in data and getting n per panel#
#########################################

#change this to the director where the NOPVO .SAV file is located
setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/data/")

#install packages if not already installed
#install.packages(c("Hmisc", "gmodels", "gdata", "reshape"))

#load the libraries
library(Hmisc)
library(gmodels)
library(gdata)
library(reshape)

#Reading in data using function "spss.get" from Hmisc
#I use this instead of "read.spss" from the "foreign" library because spss.get returns a data.frame
nopvo.spss <- spss.get("NOPVO_DATA_english.sav")

nopvo_nonregvars_wtd.est = read.csv("nopvo_nonreg_est_wtd.csv")
nopvo_nonregvars_wtd.se = read.csv("nopvo_nonreg_se_wtd.csv")

##################
#Variables to get#
##################

#Age = 2006 - Birthyear
#Gender = Gender
#Region = NIELS5
#Province = Province
#Nationality = Q48
#Origin of self = Q49.1
#Origin of father = Q49.2
#Origin of mother = Q49.3
#Urbanization = Urbanization
#Number of people in HH = people.in.household
#Number of children in HH = children.in.household
#Education = HighestEducation
#Employment = Q1
#Occupation = Q2
#Transportation = maybe?
#Religious status = Q22
#Religion = Q23


#Subset NOPVO data on these values because the data need to be in a smaller object
reg_variables <- c(
	"BureauID",
	"Birthyear",
	"Gender",
	"NIELS5",
	"Province",
	"Q48",
	"Q49.1",
	"Q49.2",
	"Q49.3",
	#"Urbanization",
	"people.in.household"
)

nonreg_variables <- c(
	"HighestEducation",
	"Q1",
	#"Q2",
	"Q18",
	"Q19",
	#"Q20",
	#"Q21",
	#"Q22",
	"Q25",
	"Q15",
	"Q14",
	"Q24"
)
reg_var_labels <- c(
	"gender",
	"region",
	"province",
	"nationality",
	"originself",
	"originfather",
	"originmother",
#	"urbanization",
	"numpersonshh",
	"agecats"
)

nonreg_var_labels <- c(
	"education",
	"employment",
	"religious_status",
	"religion",
	"move2years",
	"health",
	"lifesat",
	"domicile"
)

####################
#DATA PREPROCESSING#
####################

nopvo <- subset(nopvo.spss, select = c(reg_variables, nonreg_variables))
nopvo <- replace(nopvo, nopvo=="NA", 0)

#Removing bureau 5 because it is missing too many variables
nopvo <- nopvo[which(nopvo$BureauID != 5),]

#Create "age" groups, remove "BureauID" and "Birthyear" from the "variables" object
nopvo$agecats <- cut(2006 - nopvo$Birthyear, c(17,24,34,44,54,66))
reg_variables <- c(reg_variables, "agecats")
reg_variables <- reg_variables[-1:-2]

#Creating a list of all the cross tables for all variables of interest
#This uses the CrossTable function from library "gmodels"
nopvo_reg.crosstables <- list(NA)
nopvo_reg.n <- list(NA)
for (i in 1:length(c(reg_variables))){
	var = eval(parse(text = paste("nopvo$", reg_variables[i], sep = "")))
	nopvo_reg.crosstables[[i]] <- CrossTable(
		nopvo[!is.na(var),]$BureauID,
		na.omit(var))
	nopvo_reg.n[[i]] <- rowSums(nopvo_reg.crosstables[[i]]$t)
}

nopvo_nonreg.crosstables <- list(NA)
nopvo_nonreg.n <- list(NA)
for (i in 1:length(c(nonreg_variables))){
	var = eval(parse(text = paste("nopvo$", nonreg_variables[i], sep = "")))
	nopvo_nonreg.crosstables[[i]] <- CrossTable(
		nopvo[!is.na(var),]$BureauID,
		na.omit(var))
	nopvo_nonreg.n[[i]] <- rowSums(nopvo_nonreg.crosstables[[i]]$t)
}

#TODO: should probably test all NOPVO crosstables against the nopvo.n object
#this is because companies are missing variables and it isn't immediately clear which ones are missing

#Converting the proportional tables from the CrossTables objects into data frames
nopvo_reg.prop_dfs <- list(NA)
nopvo_reg.prop_dfs <- lapply(nopvo_reg.crosstables, function(x){
	as.data.frame(rbind(x$prop.row[1:nrow(x$prop.row),]))
})

nopvo_nonreg.prop_dfs <- list(NA)
nopvo_nonreg.prop_dfs <- lapply(nopvo_nonreg.crosstables, function(x){
	as.data.frame(rbind(x$prop.row[1:nrow(x$prop.row),]))
})

#Labels each data.frame in the list accordingly
names(nopvo_reg.prop_dfs) = reg_var_labels
names(nopvo_nonreg.prop_dfs) = nonreg_var_labels

###################################################################################
#Collapsing categories to create comparable variables between benchmark and sample#
###################################################################################

#REGISTER VARIABLES

#need to merge the origin "don't want to respond" with "others"
#this is pretty hacky
#Origin_of_self, Origin_of_father, Origin_of_mother
nopvo_reg.prop_dfs$Origin_of_self[,7] = nopvo_reg.prop_dfs$Origin_of_self[,7] + nopvo_reg.prop_dfs$Origin_of_self[,8]
nopvo_reg.prop_dfs$Origin_of_self = nopvo_reg.prop_dfs$Origin_of_self[,-8]
nopvo_reg.prop_dfs$Origin_of_father[,7] = nopvo_reg.prop_dfs$Origin_of_father[,7] + nopvo_reg.prop_dfs$Origin_of_father[,8]
nopvo_reg.prop_dfs$Origin_of_father = nopvo_reg.prop_dfs$Origin_of_father[,-8]
nopvo_reg.prop_dfs$Origin_of_mother[,7] = nopvo_reg.prop_dfs$Origin_of_mother[,7] + nopvo_reg.prop_dfs$Origin_of_mother[,8]
nopvo_reg.prop_dfs$Origin_of_mother = nopvo_reg.prop_dfs$Origin_of_mother[,-8]
#nopvo_reg.prop_dfs$Nationality[,1] = nopvo_reg.prop_dfs$Nationality[,1] + nopvo_reg.prop_dfs$Nationality[,3]
#nopvo_reg.prop_dfs$Nationality = nopvo_reg.prop_dfs$Nationality[,-3]
#names(nopvo_reg.prop_dfs$Nationality) = c("Dutch", "non-Dutch")

#NONREGISTER VARIABLES
#TODO:

#collapsing education
#1) no education
#2) lower vocation education/middle level secondary school
#3) intermediate vocational education/highest level secondary school
#4) higher vocational education/university education

orig = nopvo_nonreg.prop_dfs$education
first = orig[,1]
second = orig[,2] + orig[,3]
third = orig[,4] + orig[,5]
fourth = orig[,6] + orig[,7]
education = data.frame(first, second, third, fourth)
rownames(education) = rownames(orig)

nopvo_nonreg.prop_dfs$education = education

rm(orig, first, second, third, fourth, education)

#merging religious status with religion, collapsing religion
#1) none
#2) roman catholic
#3) dutch reform / protestant
#4) islamic
#5) everything else

yesno = nopvo_nonreg.prop_dfs$religious_status
religion = nopvo_nonreg.prop_dfs$religion * yesno[,1]
rc = religion[,1]
prot_dr = religion[,2] + religion[,3]
islam = religion[,5]
rest = rowSums(religion[,6:8])
none = yesno[,2]

relig = data.frame(none, rc, prot_dr, islam, rest)
rownames(relig) = rownames(religion)

nopvo_nonreg.prop_dfs$religion = relig
nopvo_nonreg.prop_dfs$religious_status = NULL
nonreg_var_labels = nonreg_var_labels[-3]

rm(yesno, religion, rc, prot_dr, islam, rest, none, relig)

#collapsing domicile values

domicile = nopvo_nonreg.prop_dfs$domicile
#1 + 2
#3
#5
#4 + 6 + 7

first = domicile[,1] + domicile[,2]
second = domicile[,3]
third = domicile[,5]
fourth = domicile[,4] + domicile[,6] + domicile[,7]
dom = data.frame(first, second, third, fourth)
rownames(dom) = rownames(domicile)
nopvo_nonreg.prop_dfs$domicile = dom

rm(domicile, first, second, third, fourth, dom)

#collapsing employment
#1
#2
#3+6
#4
#5
#6
#7

employment = nopvo_nonreg.prop_dfs$employment
first = employment[,1]
second = employment[,2]
third = employment[,3] + employment[,6]
fourth = employment[,4]
fifth = employment[,5]
sixth = employment[,7]
seventh = employment[,8]
employ = data.frame(first, second, third, fourth, fifth, sixth, seventh)
rownames(employ) = rownames(employment)
nopvo_nonreg.prop_dfs$employment = employ

rm(employment, first, second, third, fourth, fifth, sixth, seventh, employ)


#####################################
#Creating standard error data.frames#
#####################################

#REGISTER VARIABLES

nopvo_reg.se_dfs <- nopvo_reg.prop_dfs
for (n in 1:length(nopvo_reg.prop_dfs)) {
	for (i in 1:length(nopvo_reg.prop_dfs[[n]])) {
		for (j in 1:nrow(nopvo_reg.prop_dfs[[n]])) {
			nopvo_reg.se_dfs[[n]][j, i] <- sqrt((nopvo_reg.prop_dfs[[n]][j,i] * (1 - nopvo_reg.prop_dfs[[n]][j,i])) / as.numeric(nopvo_reg.n[[n]][j]))
		}
	}
}
names(nopvo_reg.se_dfs) = reg_var_labels	

#NONREGISTER VARIABLES

nopvo_nonreg.se_dfs <- nopvo_nonreg.prop_dfs
for (n in 1:length(nopvo_nonreg.prop_dfs)) {
	for (i in 1:length(nopvo_nonreg.prop_dfs[[n]])) {
		for (j in 1:nrow(nopvo_nonreg.prop_dfs[[n]])) {
			nopvo_nonreg.se_dfs[[n]][j, i] <- sqrt((nopvo_nonreg.prop_dfs[[n]][j,i] * (1 - nopvo_nonreg.prop_dfs[[n]][j,i])) / as.numeric(nopvo_nonreg.n[[n]][j]))
		}
	}
}
names(nopvo_nonreg.se_dfs) = nonreg_var_labels

###############################################################
#Comparing register NOPVO variables against GBA benchmark data#
###############################################################

#TODO: replace these horrible manual list creations with loops

#Creates an object for GBA benchmark comparisons
#The order of the objects is important!
nopvo_regvars.est = list(
	nopvo_reg.prop_dfs$gender,
	nopvo_reg.prop_dfs$nationality,
	nopvo_reg.prop_dfs$originself,
	nopvo_reg.prop_dfs$originmother,
	nopvo_reg.prop_dfs$originfather,
	nopvo_reg.prop_dfs$urbanization,
	nopvo_reg.prop_dfs$numpersonshh,
	nopvo_reg.prop_dfs$age,
	nopvo_reg.prop_dfs$region,
	nopvo_reg.prop_dfs$province
)

nopvo_regvars.se = list(
	nopvo_reg.se_dfs$gender,
	nopvo_reg.se_dfs$nationality,
	nopvo_reg.se_dfs$originself,
	nopvo_reg.se_dfs$originmother,
	nopvo_reg.se_dfs$originfather,
	nopvo_reg.se_dfs$urbanization,
	nopvo_reg.se_dfs$numpersonshh,
	nopvo_reg.se_dfs$age,
	nopvo_reg.se_dfs$region,
	nopvo_reg.se_dfs$province
)

names(nopvo_regvars.est) = c(
	"gender",
	"nationality",
	"originself",
	"originmother",
	"originfather",
	"urbanization",
	"numpersonshh",
	"agecats",
	"region",
	"province")

names(nopvo_regvars.se) = c(
	"gender",
	"nationality",
	"originself",
	"originmother",
	"originfather",
	"urbanization",
	"numpersonshh",
	"agecats",
	"region",
	"province")

#TODO: need to prune "nopvo.prop_dfs" according to the two methods
#this is a bit tricky and will require more thinkin

##################################################################
#Comparing nonregister NOPVO variables against CBS benchmark data#
##################################################################

nopvo_nonregvars.est = list(
	nopvo_nonreg.prop_dfs$education,
	nopvo_nonreg.prop_dfs$employment,
	nopvo_nonreg.prop_dfs$religion,
	nopvo_nonreg.prop_dfs$move2years,
	nopvo_nonreg.prop_dfs$health,
	nopvo_nonreg.prop_dfs$lifesat,
	nopvo_nonreg.prop_dfs$domicile
)

nopvo_nonregvars.se = list(
	nopvo_nonreg.se_dfs$education,
	nopvo_nonreg.se_dfs$employment,
	nopvo_nonreg.se_dfs$religion,
	nopvo_nonreg.se_dfs$move2years,
	nopvo_nonreg.se_dfs$health,
	nopvo_nonreg.se_dfs$lifesat,
	nopvo_nonreg.se_dfs$domicile
)

names(nopvo_nonregvars.est) = c(
	"education",
	"employment",
	"religion",
	"move2years",
	"health",
	"lifesat",
	"domicile"
)

names(nopvo_nonregvars.se) = c(
	"education",
	"employment",
	"religion",
	"move2years",
	"health",
	"lifesat",
	"domicile"
)

##########
#WEIGHTED#
##########

setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/data/")

nopvo_reg_est_wtd = read.csv("nopvo_reg_est_wtd.csv")
nopvo_reg_se_wtd = read.csv("nopvo_reg_se_wtd.csv")

reg_var_labels <- c(
	"gender",
	"nationality",
	"originself",
	"originmother",
	"originfather",
	#"urbanization",
	"numpersonshh",
	"agecats",
	"region", #region normally
	"province"
)

#register variables

for (i in 1:length(reg_var_labels)){
	temp_var = paste("nopvo_",reg_var_labels[i],"_est_wtd",sep="")
	temp_df = nopvo_reg_est_wtd[nopvo_reg_est_wtd$variable == reg_var_labels[i],]
	temp_cats = temp_df$X
	temp_df = t(temp_df[,2:19])
	colnames(temp_df) = temp_cats
	assign(temp_var, temp_df)
}

for (i in 1:length(reg_var_labels)){
	temp_var = paste("nopvo_",reg_var_labels[i],"_se_wtd",sep="")
	temp_df = nopvo_reg_se_wtd[nopvo_reg_se_wtd$variable == reg_var_labels[i],]
	temp_cats = temp_df$X
	temp_df = t(temp_df[,2:19])
	#temp_df = factor(temp_df)
	colnames(temp_df) = temp_cats
	assign(temp_var, temp_df)
}

nopvo_regvars_wtd.est = list(NA)
for (i in 1:length(reg_var_labels)){
	nopvo_regvars_wtd.est[[i]] = eval(parse(text = paste("nopvo_",reg_var_labels[i],"_est_wtd",sep="")))
}

nopvo_regvars_wtd.se = list(NA)
for (i in 1:length(reg_var_labels)){
	nopvo_regvars_wtd.se[[i]] = eval(parse(text = paste("nopvo_",reg_var_labels[i],"_se_wtd",sep="")))
}

names(nopvo_regvars_wtd.est) = c(
	"gender",
	"nationality",
	"originself",
	"originmother",
	"originfather",
	#"urbanization",
	"numpersonshh",
	"agecats",
	"region",
	"province")

names(nopvo_regvars_wtd.se) = c(
	"gender",
	"nationality",
	"originself",
	"originmother",
	"originfather",
	#"urbanization",
	"numpersonshh",
	"agecats",
	"region",
	"province")

#nonregister variables

nopvo_nonreg_est_wtd = read.csv("nopvo_nonreg_est_wtd.csv")
nopvo_nonreg_se_wtd = read.csv("nopvo_nonreg_se_wtd.csv")

for (i in 1:length(nonreg_var_labels)){
	temp_var = paste("nopvo_",nonreg_var_labels[i],"_est_wtd",sep="")
	temp_df = nopvo_nonreg_est_wtd[nopvo_nonreg_est_wtd$variable == nonreg_var_labels[i],]
	temp_cats = temp_df$X
	temp_df = t(temp_df[,2:19])
	colnames(temp_df) = temp_cats
	assign(temp_var, temp_df)
}

for (i in 1:length(nonreg_var_labels)){
	temp_var = paste("nopvo_",nonreg_var_labels[i],"_se_wtd",sep="")
	temp_df = nopvo_nonreg_se_wtd[nopvo_nonreg_se_wtd$variable == nonreg_var_labels[i],]
	temp_cats = temp_df$X
	temp_df = t(temp_df[,2:19])
	#temp_df = factor(temp_df)
	colnames(temp_df) = temp_cats
	assign(temp_var, temp_df)
}

nopvo_nonregvars_wtd.est = list(NA)
for (i in 1:length(nonreg_var_labels)){
	nopvo_nonregvars_wtd.est[[i]] = eval(parse(text = paste("nopvo_",nonreg_var_labels[i],"_est_wtd",sep="")))
}

nopvo_nonregvars_wtd.se = list(NA)
for (i in 1:length(nonreg_var_labels)){
	nopvo_nonregvars_wtd.se[[i]] = eval(parse(text = paste("nopvo_",nonreg_var_labels[i],"_se_wtd",sep="")))
}

names(nopvo_nonregvars_wtd.est) = c(
	"education",
	"employment",
	"religion",
	"move2years",
	"health",
	"lifesat",
	"domicile")

names(nopvo_nonregvars_wtd.se) = c(
	"education",
	"employment",
	"religion",
	"move2years",
	"health",
	"lifesat",
	"domicile")


rm(list = setdiff(ls(), c(
	"nopvo_regvars.est",
	"nopvo_regvars_wtd.est",
	"nopvo_regvars.se",
	"nopvo_regvars_wtd.se",
	"nopvo_nonregvars.est",
	"nopvo_nonregvars_wtd.est",
	"nopvo_nonregvars.se",
	"nopvo_nonregvars_wtd.se"
)))

save.image("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/NOPVO_GBA.RData")

rm(list=ls(all=T))
gc()
