#This script takes data preprocessed by SPSS and converts to a format readable by Blaise

source("//Srsh1f/sah1/Kwaliteit_Webpanels/R/RBascula.R")

#change this to the directory of the survey
setwd("//Srsh1f/sah1/Kwaliteit_Webpanels/EBB/")

#read in the survey data file
#you can use the read.spss function in the foreign library, but you must convert the list object into a data.frame
#or you can use the spss.get function in the Hmisc library, which is already read in as a data.frame

library(foreign)
library(Hmisc)

#for example, to read in the EBB data
ebb_foreign <- as.data.frame(read.spss("EBB_bascula_startgew.SAV"))
ebb_Hmisc <- spss.get("EBB_bascula_startgew.SAV")

#change FILENAME to name of survey data file
#note that the data MUST BE READ IN AS A DATA.FRAME
#parse must be set to false because we are not using RDCOMCLIENT
write.sambla(ebb_foreign, parse=FALSE) 