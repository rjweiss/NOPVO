#######################################
#NOPVO Script                         #
#Creates estimates and standard errors#
#######################################

rm(list=ls(all=T))
gc()

#change to the directory where the NOPVO .SAV file is located
setwd("/Users/Rebecca/Dropbox/research/NOPVO/analysis/data/")

#load the libraries
library(car)
library(foreign)
library(gmodels)
library(reshape2)

#Reading in data using function "read.spss" from library foreign
nopvo.spss <- read.spss("NOPVO_DATA_english.sav",
                        use.value.labels=FALSE,
                        to.data.frame=TRUE,
                        trim.factor.names = TRUE, 
                        reencode = NA, 
)

#Reading in the weighted data given to me by Sanne
nopvo_regvars_wtd_est = read.csv("nopvo_reg_est_wtd.csv", check.names=F)
nopvo_regvars_wtd_se = read.csv("nopvo_reg_se_wtd.csv", check.names=F)
nopvo_nonregvars_wtd_est = read.csv("nopvo_nonreg_est_wtd.csv", check.names=F)
nopvo_nonregvars_wtd_se = read.csv("nopvo_nonreg_se_wtd.csv", check.names=F)


##################
#Variables to get#
##################

reg_variables <- c(
  "BureauID",
  "Birthyear",
  "Gender",
  "NIELS5",
  "Province",
  "Urbanization",
  "Q48",
  "Q49_1",
  "Q49_2",
  "Q49_3",
  "people_in_household"
)

nonreg_variables <- c(
  "HighestEducation",
  "Q1",
  #   "Q18", #raised in
  #   "Q19", #raised in  #use q20 and q21
  "Q20",
  "Q21",
  #  "Q25",
  "Q15"#,
  #  "Q24"
)

regvar_labels <- c(
  "birthyear",
  "gender",
  "region",
  "province",
  "urbanization",
  "nationality",
  "originself",
  "originfather",
  "originmother",
  "numpersonshh"
)

nonregvar_labels <- c(
  "education",
  "employment",
  "religious_status",
  "religion",
  #  "move2years",
  "health"#,
  #  "domicile"
)

#create 100 simple random samples of nopvo

#Subsetting to variables of interest
nopvo <- subset(nopvo.spss, select = c(reg_variables, nonreg_variables))


#Removing bureaus that are missing too many variables


bureaus = c(3, 4, 5, 9, 10, 11, 13, 15, 17)
# bureaus = c(4, 5, 9, 11, 15, 17)

nopvo = nopvo[!nopvo$BureauID %in% bureaus,]

# nopvo <- nopvo[which(nopvo$BureauID != 3),]#
# nopvo <- nopvo[which(nopvo$BureauID != 4),]
# nopvo <- nopvo[which(nopvo$BureauID != 5),]
# nopvo <- nopvo[which(nopvo$BureauID != 9),]
# nopvo <- nopvo[which(nopvo$BureauID != 10),]#
# nopvo <- nopvo[which(nopvo$BureauID != 11),]
# nopvo <- nopvo[which(nopvo$BureauID != 13),]#
# nopvo <- nopvo[which(nopvo$BureauID != 15),]
# nopvo <- nopvo[which(nopvo$BureauID != 17),]

#Create "agecats" groups
nopvo$agecats <- cut(2006 - nopvo$Birthyear, c(17,24,34,44,54,66))
names(nopvo) = c("BureauID", regvar_labels, nonregvar_labels, "agecats")
regvar_labels = c(regvar_labels, "agecats")


##########
#Recoding#
##########

#some weird namespacing going on...have to manually specify car

#gender
nopvo$gender <- car::recode(nopvo$gender, "'1' = 'Male'; '2' = 'Female'", as.factor.result = TRUE)

#region
nopvo$region <- car::recode(nopvo$region, "'1' = '3 largest cities'; '2' = 'West';'3' = 'North'; '4' = 'East'; '5' = 'South'", as.factor.result = TRUE)

#numpersonshh
nopvo$numpersonshh <- car::recode(nopvo$numpersonshh, "'1' = 'One'; '2' = 'Two'; '3' = 'Three'; '4' = 'Four'; '5' = 'Five'; '6' = 'Six or more'; '99' = NA", as.factor.result = TRUE)

#province
nopvo$province = car::recode(nopvo$province, "'1' = 'Groningen';'2' = 'Friesland';'3' = 'Drenthe';'4' = 'Overijssel';'5' = 'Flevoland';'6' = 'Gelderland';'7' = 'Utrecht';'8' = 'Noord-Holland';'9' = 'Zuid-Holland';'10' = 'Zeeland';'11' = 'Noord-Brabant';'12' = 'Limburg';'99' = NA", as.factor.result = TRUE)

#urbanization
# nopvo$urbanization = car::recode(nopvo$urbanization, "'1' = 'Very strong';'2' = 'Strong';'3' = 'Moderate';'4' = 'Little';'5' = 'None';'99' = NA")

#nationality
nopvo$nationality <- car::recode(nopvo$nationality, "'1' = 'Dutch';'2' = 'not Dutch';'3' = 'Dutch'", as.factor.result = TRUE)

#origins
nopvo$originself <- car::recode(nopvo$originself, "'1' = 'Netherlands'; '2' = 'Other'; '3' = 'Other'; '4' = 'Other'; '5' = 'Other'; '6' = 'Other'; '7' = 'Other'; '8' = 'Other'", as.factor.result = TRUE)
nopvo$originfather <- car::recode(nopvo$originfather, "'1' = 'Netherlands'; '2' = 'Other'; '3' = 'Other'; '4' = 'Other'; '5' = 'Other'; '6' = 'Other'; '7' = 'Other'; '8' = 'Other'", as.factor.result = TRUE)
nopvo$originmother <- car::recode(nopvo$originmother, "'1' = 'Netherlands'; '2' = 'Other'; '3' = 'Other'; '4' = 'Other'; '5' = 'Other'; '6' = 'Other'; '7' = 'Other'; '8' = 'Other'", as.factor.result = TRUE)

#education
nopvo$education = car::recode(nopvo$education, "'1' = 'No education'; '3' = 'Lower';'4' = 'Lower';'5' = 'Intermediate';'6' = 'Intermediate';'7' = 'Highest';'8' = 'Highest'; '99' = NA", as.factor.result = TRUE)

#religion
nopvo[which(nopvo$religious_status==2),]$religion = '0'
nopvo$religious_status = NULL
nopvo$religion = car::recode(nopvo$religion, "'0' = 'None';'1' = 'Roman Catholic';'2' = 'Dutch Reform or Protestant';'3' = 'Dutch Reform or Protestant';'4' = 'Other';'5' = 'Islamic';'6' = 'Other';'7' = 'Other';'8' = 'Other'", as.factor.result = TRUE)

#remove religious status from labels
nonregvar_labels = nonregvar_labels[-3]

# #domicile
# nopvo$domicile = car::recode(nopvo$domicile, "'1' = 'House';'2' = 'House';'3' = 'Apartment';'4' = 'Other';'5' = 'Dorm or Pension';'6' = 'Other';'7' = 'Misc'", as.factor.result = TRUE)

#employment
nopvo$employment = car::recode(nopvo$employment, "'1' = 'Fully employed';'2' = 'Housework';'3' = 'Unemployed';'4' = 'Incapacitated';'5' = 'Retired';'6' = 'Unemployed';'7' = 'Student';'8' = 'Other'", as.factor.result = TRUE)

#health
nopvo$health = car::recode(nopvo$health, "'1' = 'Very good';'2' = 'Good';'3' = 'Okay';'4' = 'Bad';'5' = 'Very bad'", as.factor.result = TRUE)

# #move2years
# nopvo$move2years = car::recode(nopvo$move2years, "'1' = 'Decided not to'; '2' = 'Maybe'; '3' = 'Definitely yes' ; '4' = 'I just moved'", as.factor.result = TRUE)

# cat <- sapply(nopvo, is.factor)
# nopvo[cat] <- lapply(nopvo[cat], factor)

###############################################
#Creating a data.frame for regression analysis#
###############################################
# nopvo_lm <- nopvo
# nopvo_lm$age <- as.numeric(2006-nopvo_lm$birthyear)
# nopvo_lm$birthyear = NULL
# nopvo_lm$agecats = NULL
# 
# #remove birthyear from analysis
nopvo$birthyear = NULL
regvar_labels = regvar_labels[-1]


##############################################
#Creating 100 random samples with replacement#
##############################################
# 
# nopvo.samples = list(NA)
# for (i in 1:100){
#   nopvo.samples[[i]] =  ddply(nopvo, .(BureauID), function(df){
#     rand_ids = sample(1:nrow(df), nrow(df), replace=TRUE)
#     df = df[rand_ids,]
#   })
#   print(i)
# }

###############################
#Creating estimate data.frames#
###############################

#REGISTER
getEstimates = function(x, var_labels){
l <- list(NA)
n <- list(NA)
  
  for (i in 1:length(c(var_labels))){
    var = eval(parse(text = paste("df$", var_labels[i], sep = "")))
    l[[i]] <- CrossTable(df[!is.na(var),]$BureauID, na.omit(var))
    n[[i]] <- rowSums(l[[i]]$t)
  }
    
#Converting the proportional tables from the CrossTables objects into data.frames
est <- list(NA)
est <- lapply(l, function(x){
  as.data.frame(rbind(x$prop.row[1:nrow(x$prop.row),]))
})

names(est) = var_labels
est$n = n[[1]]
return(est)
}

getStandardErrors = function(x, var_labels){
  size = x$n
  x$n = NULL
  vars <- x

  for (n in 1:length(x)) {
    for (i in 1:length(x[[n]])) {
      for (j in 1:nrow(x[[n]])) {
        vars[[n]][j, i] <- sqrt((x[[n]][j,i] * (1 - x[[n]][j,i])) / size[n])
      }
    }
  }
  names(vars) = var_labels
  return(vars)
}

nopvo_reg_est.samples = list(NA)
nopvo_nonreg_est.samples = list(NA)
nopvo_reg_se.samples = list(NA)
nopvo_nonreg_se.samples = list(NA)

for(i in 1:100){

  df =  ddply(nopvo, .(BureauID), function(df){
    rand_ids = sample(1:nrow(df), nrow(df), replace=TRUE)
    df = df[rand_ids,]
    return(df)
  })
  
  nopvo_reg_est.samples[[i]] = getEstimates(df, regvar_labels)
  nopvo_reg_se.samples[[i]] = getStandardErrors(nopvo_reg_est.samples[[i]], regvar_labels)
  nopvo_nonreg_est.samples[[i]] = getEstimates(df, nonregvar_labels)
  nopvo_nonreg_se.samples[[i]] = getStandardErrors(nopvo_nonreg_est.samples[[i]], nonregvar_labels)
}


#############################
#Fixing weighted data.frames#
#############################

nopvo_regvars_wtd_est <- melt(nopvo_regvars_wtd_est)
nopvo_regvars_wtd_se <- melt(nopvo_regvars_wtd_se)
nopvo_nonregvars_wtd_est <- melt(nopvo_nonregvars_wtd_est)
nopvo_nonregvars_wtd_se <- melt(nopvo_nonregvars_wtd_se)


names(nopvo_regvars_wtd_est) = c("category", "variable", "bureau", "value")
names(nopvo_regvars_wtd_se) = c("category", "variable", "bureau", "value")
names(nopvo_nonregvars_wtd_est) = c("category", "variable", "bureau", "value")
names(nopvo_nonregvars_wtd_se) = c("category", "variable", "bureau", "value")

nopvo_regvars_wtd_est <- nopvo_regvars_wtd_est[!nopvo_regvars_wtd_est$bureau %in% bureaus,]
nopvo_regvars_wtd_se <- nopvo_regvars_wtd_se[!nopvo_regvars_wtd_se$bureau %in% bureaus,]
nopvo_nonregvars_wtd_est <- nopvo_nonregvars_wtd_est[!nopvo_nonregvars_wtd_est$bureau %in% bureaus,]
nopvo_nonregvars_wtd_se <- nopvo_nonregvars_wtd_se[!nopvo_nonregvars_wtd_se$bureau %in% bureaus,]


#####################################
#Save Rdata for preprocessing script#
#####################################

save.image("/Users/Rebecca/Dropbox/research/NOPVO/analysis/scripts/NOPVO_no_urb.RData")
