rm(list=ls(all=T))
gc()

library(Hmisc)

nopvo <- spss.get("/Users/Rebecca/Dropbox/Research Projects/NOPVO/current analysis/data/merged.sav")

#Lisa's SPSS

# RECODE Geslachtmanvrouw (1=0) (2=1) INTO gender_dummy.
# EXECUTE.
# 
# RECODE agegroupn1n2n3n4n5 (1=1) (ELSE=0) INTO age_dummy1.
# EXECUTE.
# RECODE agegroupn1n2n3n4n5 (2=1) (ELSE=0) INTO age_dummy2.
# EXECUTE.
# RECODE agegroupn1n2n3n4n5 (3=1) (ELSE=0) INTO age_dummy3.
# EXECUTE.
# RECODE agegroupn1n2n3n4n5 (4=1) (ELSE=0) INTO age_dummy4.
# EXECUTE.
# RECODE agegroupn1n2n3n4n5 (5=1) (ELSE=0) INTO age_dummy5.

# *UNWEIGHTED==>.
# *Health, gender and age all in one regression, restricted and unrestricted===>.
# REGRESSION
# /MISSING LISTWISE
# /STATISTICS COEFF OUTS R ANOVA
# /CRITERIA=PIN(.05) POUT(.10)
# /NOORIGIN 
# /DEPENDENT Q14
# /METHOD=ENTER Q15 Gender_dummy age_dummy2 age_dummy3 age_dummy4 age_dummy5  .

model1 <- lm(data = nopvo, Q14 ~ 
  #I(Geslachtmanvrouw==1) +
  I(Geslachtmanvrouw==2) + 
  I(agegroupn1n2n3n4n5==1) +
  I(agegroupn1n2n3n4n5==2) +
  I(agegroupn1n2n3n4n5==3) +
  I(agegroupn1n2n3n4n5==4) +
  I(agegroupn1n2n3n4n5==5)
  )

