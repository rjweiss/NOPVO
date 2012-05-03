rm(list=ls(all=T))
gc()

library(Hmisc)

nopvo <- spss.get("/Users/Rebecca/Dropbox/research/NOPVO/analysis/data/merged.sav")

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
  Q15 +
  #I(Geslachtmanvrouw==1) +
  I(Geslachtmanvrouw==2) + 
#  I(agegroupn1n2n3n4n5==1) +
  I(agegroupn1n2n3n4n5==2) +
  I(agegroupn1n2n3n4n5==3) +
  I(agegroupn1n2n3n4n5==4) +
  I(agegroupn1n2n3n4n5==5) 
  )

model1_unres <- lm(data = nopvo, Q14 ~ 
  Q15 +
  #I(Geslachtmanvrouw==1) +
  I(Geslachtmanvrouw==2) + 
  #  I(agegroupn1n2n3n4n5==1) +
  I(agegroupn1n2n3n4n5==2) +
  I(agegroupn1n2n3n4n5==3) +
  I(agegroupn1n2n3n4n5==4) +
  I(agegroupn1n2n3n4n5==5) +
#  I(panelID == 1) +
  I(panelID == 2) +
  I(panelID == 3) +
  I(panelID == 4) +
#   I(panelID == 5) + must keep panel 5 out of analysis due to missing values
  I(panelID == 6) +
  I(panelID == 7) +
  I(panelID == 8) +
  I(panelID == 9) +
  I(panelID == 10) +
  I(panelID == 11) +
  I(panelID == 12) +
  I(panelID == 13) +
  I(panelID == 14) +
  I(panelID == 15) +
  I(panelID == 16) +
  I(panelID == 17) +
  I(panelID == 18) +
  I(panelID == 20) +
  I(Q15) * I(panelID == 2) +
  I(Q15) * I(panelID == 3) +
  I(Q15) * I(panelID == 4) +
  I(Q15) * I(panelID == 6) +
  I(Q15) * I(panelID == 7) +
  I(Q15) * I(panelID == 8) +
  I(Q15) * I(panelID == 9) +
  I(Q15) * I(panelID == 10) +
  I(Q15) * I(panelID == 11) +
  I(Q15) * I(panelID == 12) +
  I(Q15) * I(panelID == 13) +
  I(Q15) * I(panelID == 14) +
  I(Q15) * I(panelID == 15) +
  I(Q15) * I(panelID == 16) +
  I(Q15) * I(panelID == 17) +
  I(Q15) * I(panelID == 18) +
  I(Q15) * I(panelID == 20) + 
  I(Geslachtmanvrouw==2) * I(panelID == 2) +
  I(Geslachtmanvrouw==2) * I(panelID == 3) +
  I(Geslachtmanvrouw==2) * I(panelID == 4) +
  I(Geslachtmanvrouw==2) * I(panelID == 6) +
  I(Geslachtmanvrouw==2) * I(panelID == 7) +
  I(Geslachtmanvrouw==2) * I(panelID == 8) +
  I(Geslachtmanvrouw==2) * I(panelID == 9) +
  I(Geslachtmanvrouw==2) * I(panelID == 10) +
  I(Geslachtmanvrouw==2) * I(panelID == 11) +
  I(Geslachtmanvrouw==2) * I(panelID == 12) +
  I(Geslachtmanvrouw==2) * I(panelID == 13) +
  I(Geslachtmanvrouw==2) * I(panelID == 14) +
  I(Geslachtmanvrouw==2) * I(panelID == 15) +
  I(Geslachtmanvrouw==2) * I(panelID == 16) +
  I(Geslachtmanvrouw==2) * I(panelID == 17) +
  I(Geslachtmanvrouw==2) * I(panelID == 18) +
  I(Geslachtmanvrouw==2) * I(panelID == 20) +
  I(agegroupn1n2n3n4n5==2) * I(panelID == 2) +
  I(agegroupn1n2n3n4n5==2) * I(panelID == 3) +
  I(agegroupn1n2n3n4n5==2) * I(panelID == 4) +
  I(agegroupn1n2n3n4n5==2) * I(panelID == 6) +
  I(agegroupn1n2n3n4n5==2) * I(panelID == 7) +
  I(agegroupn1n2n3n4n5==2) * I(panelID == 8) +
  I(agegroupn1n2n3n4n5==2) * I(panelID == 9) +
  I(agegroupn1n2n3n4n5==2) * I(panelID == 10) +
  I(agegroupn1n2n3n4n5==2) * I(panelID == 11) +
  I(agegroupn1n2n3n4n5==2) * I(panelID == 12) +
  I(agegroupn1n2n3n4n5==2) * I(panelID == 13) +
  I(agegroupn1n2n3n4n5==2) * I(panelID == 14) +
  I(agegroupn1n2n3n4n5==2) * I(panelID == 15) +
  I(agegroupn1n2n3n4n5==2) * I(panelID == 16) +
  I(agegroupn1n2n3n4n5==2) * I(panelID == 17) +
  I(agegroupn1n2n3n4n5==2) * I(panelID == 18) +
  I(agegroupn1n2n3n4n5==2) * I(panelID == 20) +
  I(agegroupn1n2n3n4n5==3) * I(panelID == 2) +
  I(agegroupn1n2n3n4n5==3) * I(panelID == 3) +
  I(agegroupn1n2n3n4n5==3) * I(panelID == 4) +
  I(agegroupn1n2n3n4n5==3) * I(panelID == 6) +
  I(agegroupn1n2n3n4n5==3) * I(panelID == 7) +
  I(agegroupn1n2n3n4n5==3) * I(panelID == 8) +
  I(agegroupn1n2n3n4n5==3) * I(panelID == 9) +
  I(agegroupn1n2n3n4n5==3) * I(panelID == 10) +
  I(agegroupn1n2n3n4n5==3) * I(panelID == 11) +
  I(agegroupn1n2n3n4n5==3) * I(panelID == 12) +
  I(agegroupn1n2n3n4n5==3) * I(panelID == 13) +
  I(agegroupn1n2n3n4n5==3) * I(panelID == 14) +
  I(agegroupn1n2n3n4n5==3) * I(panelID == 15) +
  I(agegroupn1n2n3n4n5==3) * I(panelID == 16) +
  I(agegroupn1n2n3n4n5==3) * I(panelID == 17) +
  I(agegroupn1n2n3n4n5==3) * I(panelID == 18) +
  I(agegroupn1n2n3n4n5==3) * I(panelID == 20) +
  I(agegroupn1n2n3n4n5==4) * I(panelID == 2) +
  I(agegroupn1n2n3n4n5==4) * I(panelID == 3) +
  I(agegroupn1n2n3n4n5==4) * I(panelID == 4) +
  I(agegroupn1n2n3n4n5==4) * I(panelID == 6) +
  I(agegroupn1n2n3n4n5==4) * I(panelID == 7) +
  I(agegroupn1n2n3n4n5==4) * I(panelID == 8) +
  I(agegroupn1n2n3n4n5==4) * I(panelID == 9) +
  I(agegroupn1n2n3n4n5==4) * I(panelID == 10) +
  I(agegroupn1n2n3n4n5==4) * I(panelID == 11) +
  I(agegroupn1n2n3n4n5==4) * I(panelID == 12) +
  I(agegroupn1n2n3n4n5==4) * I(panelID == 13) +
  I(agegroupn1n2n3n4n5==4) * I(panelID == 14) +
  I(agegroupn1n2n3n4n5==4) * I(panelID == 15) +
  I(agegroupn1n2n3n4n5==4) * I(panelID == 16) +
  I(agegroupn1n2n3n4n5==4) * I(panelID == 17) +
  I(agegroupn1n2n3n4n5==4) * I(panelID == 18) +
  I(agegroupn1n2n3n4n5==4) * I(panelID == 20) +
  I(agegroupn1n2n3n4n5==5) * I(panelID == 2) +
  I(agegroupn1n2n3n4n5==5) * I(panelID == 3) +
  I(agegroupn1n2n3n4n5==5) * I(panelID == 4) +
  I(agegroupn1n2n3n4n5==5) * I(panelID == 6) +
  I(agegroupn1n2n3n4n5==5) * I(panelID == 7) +
  I(agegroupn1n2n3n4n5==5) * I(panelID == 8) +
  I(agegroupn1n2n3n4n5==5) * I(panelID == 9) +
  I(agegroupn1n2n3n4n5==5) * I(panelID == 10) +
  I(agegroupn1n2n3n4n5==5) * I(panelID == 11) +
  I(agegroupn1n2n3n4n5==5) * I(panelID == 12) +
  I(agegroupn1n2n3n4n5==5) * I(panelID == 13) +
  I(agegroupn1n2n3n4n5==5) * I(panelID == 14) +
  I(agegroupn1n2n3n4n5==5) * I(panelID == 15) +
  I(agegroupn1n2n3n4n5==5) * I(panelID == 16) +
  I(agegroupn1n2n3n4n5==5) * I(panelID == 17) +
  I(agegroupn1n2n3n4n5==5) * I(panelID == 18) +
  I(agegroupn1n2n3n4n5==5) * I(panelID == 20) 
  )

