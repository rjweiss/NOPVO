#This script takes the LISS data, combines into one datafile, and creates estimates

#TODO: Get the relevant nonregvars

setwd("/Users/Rebecca/Dropbox/research/NOPVO/analysis/data/")

#This script takes the LISS data, combines into one datafile, and creates estimates

#TODO: Get the relevant nonregvars
#TODO: use library foreign instead of Hmisc, so that you can use factors instead of strings for recoding
#TOOD: Fix the recodes to match NOPVO

setwd("/Users/Rebecca/Dropbox/research/NOPVO/analysis/data/")

liss <- read.spss("LISS_DATA.sav",
                  use.value.labels=FALSE,
                  to.data.frame=TRUE,
                  trim.factor.names = TRUE, 
                  reencode = NA, 
                  )
liss_etc <- read.spss("reg_prov_apr2008.sav",
                      use.value.labels=FALSE,
                      to.data.frame=TRUE,
                      trim.factor.names = TRUE, 
                      reencode = NA, 
                      )
liss_etc = liss_etc[liss_etc$doetmee == 1,]



liss = subset(liss, select = c(
  geslacht, #gender 378 NA
  gebjaar, #birthyear 378 NA
  aantalhh, #numpersonshh
  cr08a043, #dutch
  # cr08a044, #other
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

liss$geslacht = car::recode(liss$geslacht, "'1' = 'Male'; '2' = 'Female'", as.factor.result=TRUE)
liss$aantalhh = car::recode(liss$aantalhh, "'1' = 'One'; '2' = 'Two'; '3' = 'Three'; '4' = 'Four'; '5' = 'Five'; '6' = 'Six or more'; '7' = 'Six or more'; '8' = 'Six or more'; '9' = 'Six or more'", as.factor.result = TRUE)
liss$cr08a043 = car::recode(liss$cr08a043, "'0' = 'not Dutch'; '1' = 'Dutch'")

liss[which(liss$cr08a053==1),]$cr08a054 = '0'
liss$cr08a054 = car::recode(liss$cr08a054, "'0' = 'Netherlands' ; '1' = 'Turkey'; '2' = 'Morocco'; '3' = 'Antilles and Aruba'; '4' = 'Suriname'; '5' = 'Indonesia'; '6' = 'Other'; '7' = 'Other'", as.factor.result=TRUE)

liss[which(liss$cr08a057==1),]$cr08a058 = '0'
liss$cr08a058 = car::recode(liss$cr08a058, "'0' = 'Netherlands' ; '1' = 'Turkey'; '2' = 'Morocco'; '3' = 'Antilles and Aruba'; '4' = 'Suriname'; '5' = 'Indonesia'; '6' = 'Other'; '7' = 'Other'; '8' = 'Other'; '99' = NA", as.factor.result=TRUE)

liss[which(liss$cr08a060==1),]$cr08a061 = '0'
liss$cr08a061 = car::recode(liss$cr08a061, "'0' = 'Netherlands' ; '1' = 'Turkey'; '2' = 'Morocco'; '3' = 'Antilles and Aruba'; '4' = 'Suriname'; '5' = 'Indonesia'; '6' = 'Other'; '7' = 'Other'; '8' = 'Other'; '99' = NA", as.factor.result=TRUE)

#removing rows that are missing originfather and originmother
liss = liss[complete.cases(liss$cr08a058),]
liss = liss[complete.cases(liss$cr08a061),]

liss$age = 2008-liss$gebjaar
liss = subset(liss, liss$age > 17 & liss$age <66)
liss$agecats = cut(liss$age, c(17,24,34,44,54,66))

liss_etc$regio = car::recode(liss_etc$regio, "'1' = '3 largest cities'; '2' = 'West';'3' = 'North'; '4' = 'East'; '5' = 'South'", as.factor.result = TRUE)

liss_etc$prov = car::recode(liss_etc$prov, "'20' = 'Groningen'; '21' = 'Friesland'; '22' = 'Drenthe'; '23' = 'Overijssel'; '24' = 'Flevoland';'25' = 'Gelderland';       '26' = 'Utrecht'; '27' = 'Noord-Holland'; '28' = 'Zuid-Holland'; '29' = 'Zeeland'; '30' = 'Noord-Brabant'; '31' = 'Limburg';", as.factor.result=T)

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

liss_etc = liss_etc[complete.cases(liss_etc$regio),]
liss_etc = liss_etc[complete.cases(liss_etc$prov),]

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
