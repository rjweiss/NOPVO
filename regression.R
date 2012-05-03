rm(list=ls(all=T))
gc()

library(Hmisc)

nopvo <- spss.get("/Users/Rebecca/Dropbox/Research Projects/NOPVO/current analysis/data/merged.sav")

model1 <- lm(data = nopvo, Q15 ~ 
#  I(Geslachtmanvrouw==1) +
  I(Geslachtmanvrouw==2) + 
#  I(agegroupn1n2n3n4n5==1) +
  I(agegroupn1n2n3n4n5==2) +
  I(agegroupn1n2n3n4n5==3) +
  I(agegroupn1n2n3n4n5==4) +
  I(agegroupn1n2n3n4n5==5)
  )