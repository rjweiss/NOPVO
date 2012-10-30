#NOPVO PREPROCESSING SCRIPT
#This script takes the NOPVO english .SAV file and creates a data.frame with the following:
#1) Subsets on the variables of interest
#2) Creates crosstables of all the variables
#3) Takes the proportional estimates for each category in each variable
#4) Outputs a data.frame ("nopvo.prop") to be compared against benchmarks

#change this to the director where the NOPVO .SAV file is located
setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/data/")
#setwd('../data')
#install packages if not already installed
#install.packages("Hmisc")
#install.packages("gmodels")

#load the libraries
library(Hmisc)
library(gmodels)
library(reshape)


#Reading in data using function "spss.get" from Hmisc
#I use this instead of "read.spss" from the "foreign" library because spss.get returns a data.frame
nopvo.spss <- spss.get("NOPVO_DATA_english.sav")

#Getting n for each bureau
nopvo.n <- t(apply(table(nopvo.spss$Respone.panel, nopvo.spss$BureauID), 2, max))

#Variables to get:
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
#Interest in politics = Q5
#2003 Vote = Q8
#2006 Intent to Vote = Q9
#2006 Vote = Q10

#Subset NOPVO data on these values because the data need to be in a smaller object
variables <- c(
	"BureauID",
	"Birthyear",
	"Gender",
	"NIELS5",
	"Province",
	"Q48",
	"Q49.1",
	"Q49.2",
	"Q49.3",
	"Urbanization",
	"people.in.household",
	"HighestEducation",
	"Q1",
	"Q2",
	"Q22",
	"Q23",
	"Q25",
	"Q15",
	"Q14"
)

variable_labels <- c(
	"Gender",
	"Region",
	"Province",
	"Nationality",
	"Originself",
	"Originfather",
	"Originmother",
	"Urbanization",
	"People_in_HH",
	"Education",
	"Employment",
	"Occupation",
	"Religious_status",
	"Religion",
	"Move2years",
	"Health",
	"Lifesat",
	"Age"
)

nopvo <- subset(nopvo.spss, select = variables)
nopvo <- nopvo[which(nopvo$BureauID != 5),]
nopvo.n <- nopvo.n[1,-5]

#Create "Age" groups, remove "BureauID" and "Birthyear" from the "variables" object
nopvo$Age <- cut(2006 - nopvo$Birthyear, c(17,24,34,44,54,66))
variables <- c(variables, "Age")
variables <- variables[-1:-2]

#Creating a list of all the cross tables for all variables of interest
#This uses the CrossTable function from library "gmodels"
#TODO: should convert that for loop into an apply function
nopvo.crosstables <- list(NA)
for (i in 1:length(variables)){
	nopvo.crosstables[[i]] <- CrossTable(
		nopvo$BureauID, 
		eval(parse(text = paste(
			"nopvo$", 
			variables[i], 
			sep = "")
			)),
		resid = TRUE
		)
}

#TODO: should probably test all NOPVO crosstables against the nopvo.n object
#this is because companies are missing variables and it isn't immediately clear which ones are missing

#Converting the proportional tables from the CrossTables objects into data frames
nopvo.prop_dfs <- list(NA)
nopvo.prop_dfs <- lapply(nopvo.crosstables, function(x){
	as.data.frame(rbind(x$prop.row[1:nrow(x$prop.row),]))
})

#Labels each data.frame in the list accordingly
names(nopvo.prop_dfs) = variable_labels

#need to merge the origin "don't want to respond" with "others"
#this is pretty hacky
#Origin_of_self, Origin_of_father, Origin_of_mother
nopvo.prop_dfs$Origin_of_self[,7] = nopvo.prop_dfs$Origin_of_self[,7] + nopvo.prop_dfs$Origin_of_self[,8]
nopvo.prop_dfs$Origin_of_self = nopvo.prop_dfs$Origin_of_self[,-8]
nopvo.prop_dfs$Origin_of_father[,7] = nopvo.prop_dfs$Origin_of_father[,7] + nopvo.prop_dfs$Origin_of_father[,8]
nopvo.prop_dfs$Origin_of_father = nopvo.prop_dfs$Origin_of_father[,-8]
nopvo.prop_dfs$Origin_of_mother[,7] = nopvo.prop_dfs$Origin_of_mother[,7] + nopvo.prop_dfs$Origin_of_mother[,8]
nopvo.prop_dfs$Origin_of_mother = nopvo.prop_dfs$Origin_of_mother[,-8]
nopvo.prop_dfs$Nationality[,1] = nopvo.prop_dfs$Nationality[,1] + nopvo.prop_dfs$Nationality[,3]
nopvo.prop_dfs$Nationality = nopvo.prop_dfs$Nationality[,-3]
names(nopvo.prop_dfs$Nationality) = c("Dutch", "non-Dutch")

#creating standard error data.frames
nopvo.se_dfs <- nopvo.prop_dfs
#nopvo.se_dfs <- lapply(nopvo.prop_dfs, function(x){
for (n in 1:length(nopvo.prop_dfs)) {
	for (i in 1:length(nopvo.prop_dfs[[n]])) {
		for (j in 1:nrow(nopvo.prop_dfs[[n]])) {
			nopvo.se_dfs[[n]][j, i] <- sqrt((nopvo.prop_dfs[[n]][j,i] * (1 - nopvo.prop_dfs[[n]][j,i])) / nopvo.n[j])
		}
	}
}
names(nopvo.se_dfs) = variable_labels

#preparing objects to print out .csv in right format

nopvo_est_csv = list(NA)
for(x in 1:length(names(nopvo.prop_dfs))){
	temp.name = names(nopvo.prop_dfs[x])
	temp.df = eval(parse(text = paste("nopvo.prop_dfs$", temp.name, sep = "")))
	temp.df$id = rownames(temp.df)
	temp.df = melt(temp.df, variable_name = "categories")
	#temp.df$var = temp.name
	temp_cast.df = as.data.frame(as.matrix(t(cast(temp.df, id ~ categories))))
	temp_cast.df$variable = temp.name
	nopvo_est_csv[[x]] = temp_cast.df
}

nopvo_se_csv = list(NA)
for(x in 1:length(names(nopvo.se_dfs))){
	temp.name = names(nopvo.se_dfs[x])
	temp.df = eval(parse(text = paste("nopvo.se_dfs$", temp.name, sep = "")))
	temp.df$id = rownames(temp.df)
	temp.df = melt(temp.df, variable_name = "categories")
	temp_cast.df = as.data.frame(as.matrix(t(cast(temp.df, id ~ categories))))
	temp_cast.df$variable = temp.name
	nopvo_se_csv[[x]] = temp_cast.df
}

#Create a gigantic data.frame of all the variables' proportional estimates per categorical response
nopvo_estimates.df <- do.call(rbind, nopvo_est_csv)
nopvo_se.df <- do.call(rbind, nopvo_se_csv)

#Writing comma-separated ASCII files of the giant data.frames
setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/csv output")
write.csv(nopvo_estimates.df, "nopvo_register_estimates.csv")
write.csv(nopvo_se.df, "nopvo_register_ses.csv")

rm(list=ls(all=T))
gc()