library(tools)
library(memisc)
library(plyr)
library(xlsx)
library(ggplot2)

load("/Users/Rebecca/Dropbox/research/NOPVO/analysis/scripts/NOPVO.RData")

###########
#Functions#
###########

getNOPVOLm = function(elem){
  dlply(nopvo_lm[complete.cases(with(nopvo_lm, eval(parse(text=paste(elem))))),], .(BureauID), lm, formula = age ~ eval(parse(text=paste(elem))))
}

getNOPVOMtable = function(elem){
  setCoefTemplate(simple=c(est="($est:#)($p:*)")) 
  do.call(mtable, c(elem, c(list(
    #   coef.style="simple",
    summary.stats=c("R-squared", "adj. R-squared", "sigma", "F", "N")
    ))))
}

#regvars <- as.array(regvar_labels)
# regvars <- regvars[-9]
# nonregvars <- as.array(nonregvar_labels)

#Creating linear model and mtable objects
#figure out what's wrong with province, nationality, and numpersonshh

regvars <- as.array(c(
  "gender",
  "region",
  "originself",
  "originfather",
  "originmother",
  "education",
  "employment",
  "religion",
  "move2years",
  "health",
  "domicile"))#,
#"province",
#"nationality",
#"numpersonshh"))

###################
#NOPVO regressions#
###################

models = alply(regvars, 1, .fun = getNOPVOLm)
names(models) = regvars
output = llply(.data = models, .fun = getNOPVOMtable)
names(output) = regvars

#relabeling output
output$gender = relabel(with(output, gender),
                        "(Intercept)" = "(Intercept)",
                        "eval(parse(text = paste(elem))): Male/Female" = "Male")

output$region = relabel(with(output, region),
                        "(Intercept)" = "(Intercept)",
                        "eval(parse(text = paste(elem))): East/3 largest cities" = "East",
                        "eval(parse(text = paste(elem))): North/3 largest cities" = "North",
                        "eval(parse(text = paste(elem))): South/3 largest cities" = "South",
                        "eval(parse(text = paste(elem))): West/3 largest cities" = "West")

output$employment = relabel(with(output, employment),
                            "eval(parse(text = paste(elem))): Housework/Fully employed" = "Housework",
                            "eval(parse(text = paste(elem))): Incapacitated/Fully employed" = "Incapacitated",
                            "eval(parse(text = paste(elem))): Other/Fully employed" = "Other",
                            "eval(parse(text = paste(elem))): Retired/Fully employed" = "Retired",
                            "eval(parse(text = paste(elem))): Student/Fully employed" = "Student",
                            "eval(parse(text = paste(elem))): Unemployed/Fully employed" = "Unemployed")

output$education = relabel(with(output, education),
                           "(Intercept)" = "(Intercept)",
                           "eval(parse(text = paste(elem))): Intermediate/Highest" = "Intermediate",
                           "eval(parse(text = paste(elem))): Lower/Highest" = "Lower",
                           "eval(parse(text = paste(elem))): Primary/Highest" = "Primary")

output$religion = relabel(with(output, religion),
                          "(Intercept)" = "(Intercept)",
                          "eval(parse(text = paste(elem))): Islamic/Dutch Reform or Protestant" = "Islamic",
                          "eval(parse(text = paste(elem))): None/Dutch Reform or Protestant" = "None",
                          "eval(parse(text = paste(elem))): Other/Dutch Reform or Protestant" = "Other",
                          "eval(parse(text = paste(elem))): Roman Catholic/Dutch Reform or Protestant" = "Roman Catholic")

output$health = relabel(with(output, health),
                        "(Intercept)" = "(Intercept)",
                        "eval(parse(text = paste(elem))): Good/Bad" = "Good",
                        "eval(parse(text = paste(elem))): Okay/Bad" = "Okay",
                        "eval(parse(text = paste(elem))): Very bad/Bad" = "Very bad",
                        "eval(parse(text = paste(elem))): Very good/Bad" = "Very good")

output$domicile = relabel(with(output, domicile),
                        "(Intercept)" = "(Intercept)",
                        "eval(parse(text = paste(elem))): Dorm or Pension/Apartment" = "Dorm or Pension",
                        "eval(parse(text = paste(elem))): House/Apartment" = "House",
                        "eval(parse(text = paste(elem))): Misc/Apartment" = "Misc",
                        "eval(parse(text = paste(elem))): Other" = "Other")

output$move2years = relabel(with(output, move2years),
                          "(Intercept)" = "(Intercept)",
                          "eval(parse(text = paste(elem))): Definitely yes/Decided not to" = "Definitely yes",
                          "eval(parse(text = paste(elem))): I just moved/Decided not to" = "I just moved",
                          "eval(parse(text = paste(elem))): Maybe/Decided not to" = "Maybe")
                          
                


#creating output for ggplots

######
#LISS#
######

