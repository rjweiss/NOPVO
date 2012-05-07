library(tools)
library(memisc)
library(plyr)

load("/Users/Rebecca/Dropbox/research/NOPVO/analysis/scripts/NOPVO.RData")

# regvars <- as.array(regvar_labels)
# regvars <- regvars[-9]
# nonregvars <- as.array(nonregvar_labels)
#
# TODO: return to this!
# test = alply(regvars, 1, function(arr){
#   dlply(nopvo_lm, .(BureauID), lm, formula = age ~ eval(parse(text=paste("nopvo$", arr, sep=""))))
# })

gender <- dlply(nopvo_lm[complete.cases(nopvo_lm$gender),], .(BureauID), lm, formula = age ~ gender)
region <- dlply(nopvo_lm[complete.cases(nopvo_lm$region),], .(BureauID), lm, formula = age ~ region)
#province <- dlply(nopvo_lm[complete.cases(nopvo_lm$province),], .(BureauID), lm, formula = age ~ province)
#nationality <- dlply(nopvo_lm[complete.cases(nopvo_lm$nationality),], .(BureauID), lm, formula = age ~ nationality)
originself <- dlply(nopvo_lm[complete.cases(nopvo_lm$originself),], .(BureauID), lm, formula = age ~ originself)
originfather <- dlply(nopvo_lm[complete.cases(nopvo_lm$originfather),], .(BureauID), lm, formula = age ~ originfather)
originmother <- dlply(nopvo_lm[complete.cases(nopvo_lm$originmother),], .(BureauID), lm, formula = age ~ originmother)
#numpersonshh <- dlply(nopvo_lm[complete.cases(nopvo_lm$numpersonshh),], .(BureauID), lm, formula = age ~ numpersonshh)
education <- dlply(nopvo_lm[complete.cases(nopvo_lm$education),], .(BureauID), lm, formula = age ~ education)
employment <- dlply(nopvo_lm[complete.cases(nopvo_lm$employment),], .(BureauID), lm, formula = age ~ employment)
religion <- dlply(nopvo_lm[complete.cases(nopvo_lm$religion),], .(BureauID), lm, formula = age ~ religion)
move2years <- dlply(nopvo_lm[complete.cases(nopvo_lm$move2years),], .(BureauID), lm, formula = age ~ move2years)
health <- dlply(nopvo_lm[complete.cases(nopvo_lm$health),], .(BureauID), lm, formula = age ~ health)
domicile <- dlply(nopvo_lm[complete.cases(nopvo_lm$domicile),], .(BureauID), lm, formula = age ~ domicile)

gender_output = do.call(mtable, c(gender, list(
  summary.stats=c("R-squared", "adj. R-squared", "sigma", "F", "N")
  )))

region_output = do.call(mtable, c(region, list(
  summary.stats=c("R-squared", "adj. R-squared", "sigma", "F", "N")
  )))

originself_output = do.call(mtable, c(originself, list(
  summary.stats=c("R-squared", "adj. R-squared", "sigma", "F", "N")
  )))

originfather_output = do.call(mtable, c(originfather, list(
  summary.stats=c("R-squared", "adj. R-squared", "sigma", "F", "N")
  )))

originmother_output = do.call(mtable, c(originmother, list(
  summary.stats=c("R-squared", "adj. R-squared", "sigma", "F", "N")
  )))

education_output = do.call(mtable, c(education, list(
  summary.stats=c("R-squared", "adj. R-squared", "sigma", "F", "N")
  )))

employment_output = do.call(mtable, c(employment, list(
  summary.stats=c("R-squared", "adj. R-squared", "sigma", "F", "N")
  )))

religion_output = do.call(mtable, c(religion, list(
  summary.stats=c("R-squared", "adj. R-squared", "sigma", "F", "N")
  )))

move2years_output = do.call(mtable, c(move2years, list(
  summary.stats=c("R-squared", "adj. R-squared", "sigma", "F", "N")
  )))

health_output = do.call(mtable, c(health, list(
  summary.stats=c("R-squared", "adj. R-squared", "sigma", "F", "N")
  )))

domicile_output = do.call(mtable, c(domicile, list(
  summary.stats=c("R-squared", "adj. R-squared", "sigma", "F", "N")
  )))


# test = do.call(mtable, c(gender, region, originself, originfather, originmother, employment, education, religion, move2years, health, domicile, list(
#   summary.stats=c("R-squared", "adj. R-squared", "sigma", "F", "N")
#   )))

gender_output = relabel(gender_output,
                        "(Intercept)" = "(Intercept)",
                        "gender: Male/Female" = "Male")

region_output = relabel(region_output,
                        "(Intercept)" = "(Intercept)",
                        "region: East/3 largest cities" = "East",
                        "region: North/3 largest cities" = "North",
                        "region: South/3 largest cities" = "South",
                        "region: West/3 largest cities" = "West")

employment_output = relabel(employment_output,
                            "employment: Housework/Fully employed" = "Housework",
                            "employment: Incapacitated/Fully employed" = "Incapacitated",
                            "employment: Other/Fully employed" = "Other",
                            "employment: Retired/Fully employed" = "Retired",
                            "employment: Student/Fully employed" = "Student",
                            "employment: Unemployed/Fully employed" = "Unemployed")

education_output = relabel(education_output,
                           "(Intercept)" = "(Intercept)",
                           "education: Intermediate/Highest" = "Intermediate",
                           "education: Lower/Highest" = "Lower",
                           "education: Primary/Highest" = "Primary")

religion_output = relabel(religion_output,
                          "(Intercept)" = "(Intercept)",
                          "religion: Islamic/Dutch Reform or Protestant" = "Islamic",
                          "religion: None/Dutch Reform or Protestant" = "None",
                          "religion: Other/Dutch Reform or Protestant" = "Other",
                          "religion: Roman Catholic/Dutch Reform or Protestant" = "Roman Catholic")

health_output = relabel(health_output,
                       "(Intercept)" = "(Intercept)",
                       "health: Good/Bad" = "Good",
                       "health: Okay/Bad" = "Okay",
                       "health: Very bad/Bad" = "Very bad",
                       "health: Very good/Bad" = "Very good")


setwd("/Users/Rebecca/Dropbox/research/NOPVO/analysis/scripts/output/regression")

write.mtable(gender_output, file="gender_output.csv")
write.mtable(region_output, file="region_output.csv")
write.mtable(employment_output, file="employment_output.csv")
write.mtable(education_output, file="education_output.csv")
write.mtable(religion_output, file="religion_output.csv")
write.mtable(health_output, file="health_output.csv")