#This script takes the LISS data, combines into one datafile, and creates estimates

#TODO: Get the relevant nonregvars
#TODO: use library foreign instead of Hmisc, so that you can use factors instead of strings for recoding
#TOOD: Fix the recodes to match NOPVO

setwd("C:/Users/Rebecca/Dropbox/research/NOPVO/analysis/data/")

library(Hmisc)

liss = spss.get("LISS_DATA.sav")#, use.value.labels=TRUE)
liss_etc = spss.get("reg_prov_apr2008.sav")#, use.value.labels=TRUE))
liss_etc = liss_etc[liss_etc$doetmee == "yes",]

detach(package:Hmisc, unload=TRUE)
library(car)

liss = subset(liss, select = c(
  geslacht, #gender 378 NA
  gebjaar, #birthyear 378 NA
  aantalhh, #numpersonshh
  cr08a043, #dutch
  cr08a044, #other
# cr08a052, #nationality
  cr08a053, #were you born in the netherlands?
  cr08a054, #in what country were you born? *
  cr08a057, #was your father born in the netherlands?
  cr08a058, #in what country was your father born? *
  cr08a060, #was your mother born in the netherlands?
  cr08a061 #in what country was your mother born?
  ))

#removing rows that are missing gender or nationality
liss = liss[complete.cases(liss$geslacht),]
liss = liss[complete.cases(liss$cr08a043),]

#recoding categories in variables to match benchmarks
#this is kind of ridiculous, fix these recodes
liss$cr08a054 = recode(liss$cr08a054, "NA = liss$cr08a053") #this will throw errors
liss$cr08a058 = recode(liss$cr08a058, "NA = liss$cr08a057")#this will throw errors
liss$cr08a061 = recode(liss$cr08a061, "NA = liss$cr08a060")#this will throw errors

liss$cr08a054 = recode(liss$cr08a054, "1 = 'Netherlands'")
liss$cr08a054 = recode(liss$cr08a054, "'Surinam' = 'Suriname'")
liss$cr08a054 = recode(liss$cr08a054, "'Dutch Antilles' = 'Antilles/Aruba'")
#liss$cr08a054 = recode(liss$cr08a054, "'Turkey' = 'Turkye'")
liss$cr08a054 = recode(liss$cr08a054, "2 = 'Other'")
liss = sqldf(c("update liss set cr08a054 = 'Other' where cr08a054 = 'other non-western country (Africa, Latin America, Asia other than [in wave 1: Indonesia and] Japan)'", "select * from liss"))
liss = sqldf(c("update liss set cr08a054 = 'Other' where cr08a054 = 'other western country (Europe, North America, [in wave 1: Indonesia,] Japan, Oceania)'", "select * from liss"))
liss$cr08a054 = factor(liss$cr08a054)
liss$cr08a058 = recode(liss$cr08a058, "1 = 'Netherlands'")
liss$cr08a058 = recode(liss$cr08a058, "'Surinam' = 'Suriname'")
liss$cr08a058 = recode(liss$cr08a058, "'Dutch Antilles' = 'Antilles/Aruba'")
#liss$cr08a058 = recode(liss$cr08a058, "'Turkey' = 'Turkye'")
liss$cr08a058 = recode(liss$cr08a058, "2 = 'Other'")
liss$cr08a058 = recode(liss$cr08a058, "3 = 'Other'")
liss = sqldf(c("update liss set cr08a058 = 'Other' where cr08a058 = 'I don''t know'", "select * from liss"))
liss = sqldf(c("update liss set cr08a058 = 'Other' where cr08a058 = 'other non-western country (Africa, Latin America, Asia other than [in wave 1: Indonesia and] Japan)'", "select * from liss"))
liss = sqldf(c("update liss set cr08a058 = 'Other' where cr08a058 = 'other western country (Europe, North America, [in wave 1: Indonesia,] Japan, Oceania)'", "select * from liss"))
liss$cr08a058 = factor(liss$cr08a058)
liss$cr08a061 = recode(liss$cr08a061, "1 = 'Netherlands'")
liss$cr08a061 = recode(liss$cr08a061, "'Surinam' = 'Suriname'")
liss$cr08a061 = recode(liss$cr08a061, "'Dutch Antilles' = 'Antilles and Aruba'")
liss$cr08a061 = recode(liss$cr08a061, "'Turkey' = 'Turkye'")
liss$cr08a061 = recode(liss$cr08a061, "2 = 'Other'")
liss$cr08a061 = recode(liss$cr08a061, "3 = 'Other'")
liss = sqldf(c("update liss set cr08a061 = 'Other' where cr08a061 = 'I don''t know'", "select * from liss"))
liss = sqldf(c("update liss set cr08a061 = 'Other' where cr08a061 = 'other non-western country (Africa, Latin America, Asia other than [in wave 1: Indonesia and] Japan)'", "select * from liss"))
liss = sqldf(c("update liss set cr08a061 = 'Other' where cr08a061 = 'other western country (Europe, North America, [in wave 1: Indonesia,] Japan, Oceania)'", "select * from liss"))
liss$cr08a061 = factor(liss$cr08a061)
liss$cr08a043 = recode(liss$cr08a043, "'yes' = 'Only Dutch'")
liss$cr08a043 = recode(liss$cr08a043, "'no' = 'Only non-Dutch'")
liss$aantalhh = recode(liss$aantalhh, "'One person' = 'One'")
liss$aantalhh = recode(liss$aantalhh, "'Two persons' = 'Two'")
liss$aantalhh = recode(liss$aantalhh, "'Three persons' = 'Three'")
liss$aantalhh = recode(liss$aantalhh, "'Four persons' = 'Four'")
liss$aantalhh = recode(liss$aantalhh, "'Five persons' = 'Five'")
liss$aantalhh = recode(liss$aantalhh, "'Six persons' = 'Six or more'")
liss$aantalhh = recode(liss$aantalhh, "'Seven persons' = 'Six or more'")
liss$aantalhh = recode(liss$aantalhh, "'Eight persons' = 'Six or more'")
liss$aantalhh = recode(liss$aantalhh, "'Nine persons or more' = 'Six or more'")

liss$age = 2008-liss$gebjaar
liss = subset(liss, liss$age > 17 & liss$age <66)
liss$agecats = cut(liss$age, c(17,24,34,44,54,66))

liss = subset(liss, select = c(
  geslacht, #gender
  agecats, #age in categories
  aantalhh, #numpersonshh
  cr08a043, #dutch
  cr08a054, #in what country were you born?
  cr08a058, #in what country was your father born?
  cr08a061 #in what country was your mother born?
  ))

freqtables = list(NA)
for (i in 1:length(names(liss))){
	freqtables[[i]] = freq(eval(parse(text = paste("liss$", names(liss)[i], sep = ""))), plot = F)
}

names(freqtables) = c(
  "gender",
  "agecats",
  "numpersonshh",
  "nationality",
  "originself",
  "originfather",
  "originmother")

#remove where region or province is missing
liss_etc = liss_etc[complete.cases(liss_etc$regio),]
liss_etc = liss_etc[complete.cases(liss_etc$prov),]

liss_etc = sqldf(c("update liss_etc set regio = '3 large cities: Amsterdam, Rotterdam, Den Haag + connected cities' where regio = 'Three largest cities'", "select * from liss_etc"))

liss_etc$regio = recode(liss_etc$regio, "'Rest of West' = 'West (Utrecht, Noord-Holland, Zuid-Holland excl. 3 large cities'")
liss_etc$regio = recode(liss_etc$regio, "'East' = 'East (Overijssel, Gelderland, Flevoland)'")
liss_etc$regio = recode(liss_etc$regio, "'North' = 'North (Groningen, Friesland, Drenthe)'")
liss_etc$regio = recode(liss_etc$regio, "'South' = 'South (Zeeland, Noord-Brabant, Limburg)'")

region = freq(liss_etc$regio, plot = F)
prov =  freq(liss_etc$prov, plot = F)

freqtables$region = region
freqtables$province = prov

liss_regvars = melt(freqtables)
names(liss_regvars) = c("category", "type", "value", "variable")

#creating estimates
liss_est = sqldf("select * from liss_regvars lr where lr.type = 'Percent' and lr.category != 'Total'")
liss_est$type = NULL
liss_est$value = liss_est$value/100

#creating standard errors
liss_n = sqldf("select * from liss_regvars lr where lr.type = 'Frequency' and lr.category = 'Total'")
liss_n$type = NULL

liss_se = sqldf("select est.category, est.value, est.variable, n.value as n from liss_est est, liss_n n where est.variable = n.variable")

liss_se$se = sqrt(liss_se$value * (1-liss_se$value) / liss_se$n)
liss_se$value = NULL
liss_se$n = NULL

liss_n = unique(liss_n$value)[1] #hack

setwd("/Users/Rebecca/Dropbox/research/NOPVO/analysis/data")
rm(list=(setdiff(ls(), c(
  "liss_est",
  "liss_se",
  "liss_n"))))
save.image("/Users/Rebecca/Dropbox/research/NOPVO/analysis/scripts/LISS.RData")
