rm(list=ls(all=T))
gc()

library(Hmisc)

nopvo <- spss.get("/Users/Rebecca/Dropbox/research/NOPVO/analysis/data/merged.sav")

#unweighted

model1 <- lm(data = nopvo, Q14 ~ 
  Q15 +
  I(Geslachtmanvrouw==2) + 
  I(agegroupn1n2n3n4n5==2) +
  I(agegroupn1n2n3n4n5==3) +
  I(agegroupn1n2n3n4n5==4) +
  I(agegroupn1n2n3n4n5==5) 
  )

model1_unres <- lm(data = nopvo, Q14 ~ 
  Q15 +
  I(Geslachtmanvrouw==2) + 
  I(agegroupn1n2n3n4n5==2) +
  I(agegroupn1n2n3n4n5==3) +
  I(agegroupn1n2n3n4n5==4) +
  I(agegroupn1n2n3n4n5==5) +
  I(panelID == 2) +
  I(panelID == 3) +
  I(panelID == 4) +
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

model2 <- lm(data = nopvo, Q14 ~ Q15)
model2_unres <- lm(data = nopvo, Q14 ~ 
  Q15 +
  I(Geslachtmanvrouw==2) + 
  I(agegroupn1n2n3n4n5==2) +
  I(agegroupn1n2n3n4n5==3) +
  I(agegroupn1n2n3n4n5==4) +
  I(agegroupn1n2n3n4n5==5) +
  I(panelID == 2) +
  I(panelID == 3) +
  I(panelID == 4) +
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
  I(Q15) * I(panelID == 20) 
)

model3 <- lm(data=nopvo, Q14 ~ I(Geslachtmanvrouw==2))
model3_unres <- model1_unres <- lm(data = nopvo, Q14 ~ 
  Q15 +
  I(Geslachtmanvrouw==2) + 
  I(agegroupn1n2n3n4n5==2) +
  I(agegroupn1n2n3n4n5==3) +
  I(agegroupn1n2n3n4n5==4) +
  I(agegroupn1n2n3n4n5==5) +
  I(panelID == 2) +
  I(panelID == 3) +
  I(panelID == 4) +
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
  I(Geslachtmanvrouw==2) * I(panelID == 20)
)

model4 <-
model4_unres <- model1_unres <- lm(data = nopvo, Q14 ~ 
Q15 +
I(Geslachtmanvrouw==2) + 
I(agegroupn1n2n3n4n5==2) +
I(agegroupn1n2n3n4n5==3) +
I(agegroupn1n2n3n4n5==4) +
I(agegroupn1n2n3n4n5==5) +
I(panelID == 2) +
I(panelID == 3) +
I(panelID == 4) +
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

mtable1 <- mtable(model1, model2, model3, model4, model1_unres, model2_unres, model3_unres, model4_unres)

output <- "mtable.csv"
write.csv(write.mtable(mtable1, file=output))
#file.show(output)

anova(model1, model1_unres)
#returns F of 1.7174

