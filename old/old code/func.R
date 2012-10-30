# Common Functions
# 
# Author: Gaurav Sood
###############################################################################

## To ensure reproducibility - 
## Two ways to get the functions to be listed that we are using <- source("abc", verbose=T); 
## or within each function add print(eval(functionname))

# xtable text to numbers
# #anes08_mode2[,c(2,3,4,5)] <- sapply(anes08_mode2[,c(2,3,4,5,6)],as.numeric), allows you to use digits
# #digits = c(0,0,2,2,2,2,0)
## ----------------------------------------------------------------------------
## Author: Jason Morgan (borrowing heavily from code contained in Martin Elff's
##         memisc package).
##
## Notes:  Additional methods for mtable formatting of lme4 model
##         objects. Requires that the memisc package be loaded prior to
##         sourcing these functions.
## ----------------------------------------------------------------------------

## Generate Star
stars <- function(pvalue)
{
	temp <- NA
	pvalue <- as.numeric(pvalue)
	if(pvalue >.10) temp <- ""
	if(pvalue > .05 & pvalue <=.10) temp <- "+"
	if(pvalue <= .05) temp <- "*"
	if(pvalue < .01) temp <- "**"
	if(pvalue < .001) temp <- "***"
temp
}

## Krosnick Number format generator
kros <- function(x){
temp <-	sprintf("%1.2f", as.numeric(x))
# format(c(0.0, 13.1), digits = 2, nsmall = 2)
if(as.numeric(temp)==0) temp <- "0.00"
splits <- strsplit(temp, "\\.")[[1]]
if(splits[1]=="0")	temp <- paste(".",splits[2], sep="")
#grep("\\.+", 4.22)
#format()
if(splits[1]=="-0") temp <- paste("-.", splits[2], sep="")	
temp
}

## Reverse a string split
#strReverse <- function(x)
#        sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")

## Dataframe of vectors and list of functions, each applied to respective one
#applier <- function(dfVec,listFunc)
#{
#out <- NA
#if(length(dfVec) < length(listFunc))
#listFunc <- listFunc[1:length(dfVec)]
#if(length(dfVec)==length(listFunc))
#{
#for(i in 1:length(dfVec)){
#out[i] <- eval(listFunc[[i]])(dfVec[i])
#out[i] <- do.call(listFunc[i], list(dfVec[i]))
#}
#}
#out
#}

#df <- data.frame(x=factor(c(1,1,2,2,2,3,3,3)), y=c(0,2,0,4,8,0,3,6))
#applier2 <- function(df,listFunc){
#library(plyr) 
#newdf <- ddply(df, "x", transform, y.mean=mean(y), y.sd=sd(y))
#detach(package:plyr)
#newdf
#}

# Mean and variance with na.rm
rmean <- function(x, wt=NULL,n=2) {
ans <- round(mean(x, na.rm=T),n)
if(!is.null(wt)) {
	ans <- round(weighted.mean(x, wt, na.rm=T),n)
	}
ans
}

rvar <- function(x) {
round(var(x, na.rm=T),2)
}

# Quick access to directory
#dir <- "C:/Users/Gaurav/Desktop/R/"

# Sweave
sweaver <- function(x, y) {
a <- getwd()
y1 <- paste(y, ".Rnw", sep="")
y2 <- paste(y, ".tex", sep="")
setwd(x)
Sweave(y1, keep.source=TRUE, output=y2)
tools::texi2dvi(y2, pdf = TRUE, clean = FALSE, quiet = TRUE,
         options(texi2dvi="pdflatex.exe"))
setwd(a)
}

###################
# 3 Ways to Rename
#####################
## Rename #Outputs a dataframe with the specific variable renamed
#rename <- function(x, y, data){
#	names(data)[names(data)==x] <- y
#	data
#}
##Memisc Rename version
## pkanestv <- memisc::rename(pkanestv, vcf0004="year", vcf0006a="id", vcf0009 ="wt1")

## Change the entire names vector to your satisfaction and then reassign it to original dataset
# renamev(pkanestv, list(c("year", "year1"),c("id", "id1"),c("wt1", "wt11")))
#renamev <- function(data, x)
#{
#	nv <- names(data)	
#	for(i in 1:length(x)){
#		nv[nv==x[[i]][1]] <- x[[i]][2]
#	}	
#	return(nv)
#}

## Read SPSS
spss <- function(file.name.and.path){
	temp <- foreign::read.spss(file.name.and.path, use.value.labels = FALSE, to.data.frame = FALSE, 
			trim.factor.names = TRUE, reencode = NA, use.missings = TRUE)
	temp <- as.data.frame(temp)
	names(temp) <- tolower(names(temp))
	temp
}

spss2 <- function(file)
{
	a <- memisc::spss.system.file(file)
	b <- as.data.set(a)
	c <- as.data.frame(b)
	names(c) <- tolower(names(c))
	c
}

#############################
##Recode Central#############

# Easy recoder (use with recode in car)
l5r <- "5=1;4=2;3=3;2=4;1=5;NA=NA"

# Center by mean
meanc <- function(x){
x - mean(x, na.rm=T)
}

## Recode 0 to 1
zero1 <- function(x, minx=NA, maxx=NA){
	res <- NA
	if(is.na(minx)) res <- (x - min(x,na.rm=T))/(max(x,na.rm=T) -min(x,na.rm=T))
	if(!is.na(minx)) res <- (x - minx)/(maxx -minx)
	res
}

## Recoder Function
recoder <- function(x, expected.min=0, expected.max){
	y <- eval(parse(text=x))	
	y[y < expected.min | y > expected.max] <- NA # If value exceeds expected max, or is lower than expected min, assign to NA
	name <- paste(x, "r", sep="")
	y01 <- (y - expected.min)/(expected.max - expected.min)
	assign(name, y01, envir = .GlobalEnv)
}

# Assign NAs to 0 in Knowledge Vars
nona <- function(x){
	x[is.na(x)] <- 0; 
	x}

	
re <- function(var, x){
ans <- NA
if(!is.na(match("Hmisc", (.packages()))))
{
detach(package:Hmisc)
ans <- car::recode(var, x)
attach(package:Hmisc)
}
ans <- car::recode(var, x)
ans
}

### Speciality Functions #######

# Generalized Variance
genvar <- function (x) {
	n <- length(x)
	determinant <- sqrt(det(cov(x, use = "pairwise.complete.obs", method =c("pearson")))^2)
	ans <- sqrt((determinant)^(1/n))
	ans
}

## Variance by Group
grpvar <- function(index, group){
	unsplit(lapply(split(index, group), function(a) var (c(a), na.rm=TRUE)), group)
}

### Mean By Group
grpmean <- function(var, group){
	unsplit(lapply(split(var, group), function(a) mean (c(a), na.rm=TRUE)), group)
}

grpsum <- function(var, group){
	unsplit(lapply(split(var, group), function(a) sum (c(a), na.rm=TRUE)), group)
}

##Entropy
entropy <- function(x, n){
	entropy <- NA
	ent1 <- ent2 <- ent3 <- ent4 <- 0
	if(sum(x, na.rm=T)==0 | length(table(round(x,2)))==0){entropy <- NA}
	if(length(table(round(x,2))) > 0)
	{
	p <- table(round(x,2))/length(x)
	if(n ==2) {
		p1 = p[1]
		p2 = 1 - p1
		if(p1==0){ent1 <- 0} else {ent1 <- p1*log2(p1)} 
		if(p2==0){ent2 <- 0} else {ent2 <- p2*log2(p2)}
		entropy = - (ent1 + ent2)
	}
	if(n ==4) {
		if(p[1]==0){ent1 <- 0} else {ent1 <- p[1]*log2(p[1])} 
		if(!is.na(p[2]) & p[2]==0){ent2 <- 0} else {ent2 <- p[2]*log2(p[2])}
		if(!is.na(p[3]) & p[3]==0){ent3 <- 0} else {ent3 <- p[3]*log2(p[3])} 
		if(!is.na(p[4]) & p[4]==0){ent4 <- 0} else {ent4 <- p[4]*log2(p[4])}
		entropy = - (ent1 + ent2 + ent3 + ent4)
 		}
	}
	entropy
}

## Poll Group Maker; Ensures Group has 2 digits, and adds to pollid
pgroup <- function(group, pollid){
	temp <- group
	temp <- ifelse(temp < 10, paste("0", temp, sep=""), temp)
	as.numeric(paste(pollid, temp, sep=""))
}


## Function to Correct for Guessing. Codes to 0 if someone is right at t1 but wrong at t2 or t3
pkcor <- function(t1pk, t2pk, t3pk=NULL)
{
	temp <- t1pk
	temp[t1pk==1 & t2pk==0] <- 0	
	temp[t1pk==1 & t3pk==0] <- 0
	temp
}

## Function for Group Gain
groupgain <- function(knowindex, group, numrow, numitems, grpsize) 
{
	# Initializing data frame
	groupmean <-  as.data.frame(matrix(rep(0, numrow*length(knowindex)), nrow=numrow, ncol=length(knowindex)))
	# Initializing column with data.frame number of rows
	netmean <-c(rep(0, numrow))
	#For all the knowledge items
	for(i in 1:length(knowindex)) 
	{
		#Group's mean score on the item 
		groupmean[,i] <- unsplit(lapply(split(knowindex[,i], group), function(a) mean (c(a), na.rm=TRUE)), group)
		#For all rows
		for(j in 1:numrow) 
		{
			if(!knowindex[j,i]) #if item not answered correctly, group mean of the jth item
				netmean[j] <- netmean[j] + (groupmean[j,i]*grpsize[j]) /(numitems[j]*(grpsize[j] -1))
			else netmean[j] <- netmean[j] + 0
		}
	}
	netmean
}


######summary
## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) {
    require(doBy)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # Collapse the data
    formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
    datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)

    # Rename columns
    names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
    names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
    names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"
    
    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
    
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult
    
    return(datac)
}

## Norms the data within specified groups in a data frame; it normalizes each
## subject (identified by idvar) so that they have the same mean, within each group
## specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL, na.rm=FALSE) {
    require(doBy)
    
    # Measure var on left, idvar + between vars on right of formula.
    subjMeanFormula <- as.formula(paste(measurevar, paste(c(idvar, betweenvars), collapse=" + "), sep=" ~ "))
    data.subjMean   <- summaryBy(subjMeanFormula, data=data, keep.names=TRUE, na.rm=na.rm)
    names(data.subjMean)[names(data.subjMean) == measurevar] <- "subjMean"

    # Put the subject means with original data
    data <- merge(data, data.subjMean)
    
    # Get the normalized data in a new column
    measureNormedVar <- paste(measurevar, "Normed", sep="")
    data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] + mean(data[,measurevar])

    # Remove this subject mean column
    data$subjMean <- NULL
    
    return(data)
}

## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL, idvar=NULL, na.rm=FALSE, conf.interval=.95) {
    # Norm each subject's data    
    data <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm)
    
    # This is the name of the new column
    measureNormedVar <- paste(measurevar, "Normed", sep="")
    
    # Replace the original data column with the normed one
    data[,measurevar] <- data[,measureNormedVar]
    

    # Collapse the normed data - now we can treat between and within vars the same
    datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars), na.rm=na.rm, conf.interval=conf.interval)


    # Apply correction from Morey (2008) to the standard error and confidence interval
    #  Get the product of the number of conditions of within-S variables
    nWithinGroups    <- prod(sapply(datac[,withinvars, drop=FALSE], FUN=nlevels))
    correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )

    # Apply the correction factor
    datac$sd <- datac$sd * correctionFactor
    datac$se <- datac$se * correctionFactor
    datac$ci <- datac$ci * correctionFactor

    return(datac)
}

##FOR 
	
#library("boot")

#sample.wtd.mean <- function(x,w){return(weighted.mean(x = x[d], w = w[d], na.rm=T ))}

################################################################latex tables for lme4
##Summarize lmer model
## Author: Jason Morgan (borrowing heavily from code contained in Martin Elff's
##         memisc package).
##
## Notes:  Additional methods for mtable formatting of lme4 model
##         objects. Requires that the memisc package be loaded prior to
##         sourcing these functions.
## ----------------------------------------------------------------------------
require(memisc)
setSummaryTemplate(mer = c("Log-likelihood" = "($logLik:f#)",
                     "Deviance" = "($deviance:f#)",
                     "AIC" = "($AIC:f#)",
                     "BIC" = "($BIC:f#)",
                     "N" = "($N:d)",
                     "Groups" = "($Groups:d)"))

getSummary.mer <- function (obj, alpha = 0.05, ...) {
  smry <- summary(obj)
  coef <- smry@coefs
  lower <- qnorm(p = alpha/2, mean = coef[, 1], sd = coef[,2])
  upper <- qnorm(p = 1 - alpha/2, mean = coef[, 1], sd = coef[,2])
  if (ncol(smry@coefs) == 3) {
    p <- (1 - pnorm(smry@coefs[, 3])) * 2
    coef <- cbind(coef, p, lower, upper)
  }
  else {
    coef <- cbind(coef, lower, upper)
  }
  RE <- smry@REmat
  ranef <- cbind(as.numeric(RE[,3]), as.numeric(RE[,4]), NA,NA,NA,NA)
  rownames(ranef) <- paste("Ranef", RE[,1], sep = " - ")
  coef <- rbind(coef, ranef)
  colnames(coef) <- c("est", "se", "stat", "p", "lwr", "upr")

  ## Factor levels.
  xlevels <- list()
  Contr <- names(attr(model.matrix(obj), "contrasts"))
  for (c in Contr) xlevels[[c]] <- levels(obj@frame[,c])

  ## Model fit statistics.
  ll <- logLik(obj)[1]
  deviance <- deviance(obj)
  AIC <- AIC(obj)
  BIC <- BIC(obj)
  N <- as.numeric(smry@dims["n"])
  G <- as.numeric(smry@ngrps)
  sumstat <- c(logLik = ll, deviance = deviance, AIC = AIC,
               BIC = BIC, N = N, Groups = G)

  ## Return model summary.
  list(coef = coef, sumstat = sumstat,
       contrasts = attr(model.matrix(obj), "contrasts"),
       xlevels = xlevels, call = obj@call)
}

## ----------------------------------------------------------------------------

removeall <- rm(list=ls())
