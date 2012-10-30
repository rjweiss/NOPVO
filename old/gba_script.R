#GBA PREPROCESSING SCRIPT
#This script takes the GBA SPSS data file and creates the proportions used for benchmark analysis
#1) Outputs an list object "benchmarks" whose elements are the data.frame proportional tables for each variable.

library(Hmisc)
library(gmodels)
library(xtable)
library(descr)

#setwd("//Srsh1f/sah1/Kwaliteit_Webpanels/GBA/")

#Reading in GBA 2007 data using spss.get() from Hmisc
gba.spss <- spss.get("pop20070101_1865.SAV")

#gba.spss$URBANIZATION <- gba.spss$URBANIZATION[,drop=TRUE]
#gba.spss$NUMPERSONSHH <- gba.spss$NUMPERSONSHH[,drop=TRUE]
#gba.spss$NUMCHILDHH <- gba.spss$NUMCHILDHH[,drop=TRUE]

#Create a new dimension that categorizes respondents according to the boundaries in the cut() function
gba.spss$AGECATS <- cut(gba.spss$AGE, c(17,24,34,44,54,65))

#Create a region dimension
gba.spss$REGION <- NA
gba.spss$REGION[gba.spss$PROVINCEPLUS == "Amsterdam"] = "3 large cities: Amsterdam, Rotterdam, Den Haag + connected cities"
gba.spss$REGION[gba.spss$PROVINCEPLUS == "Den Haag"] = "3 large cities: Amsterdam, Rotterdam, Den Haag + connected cities"
gba.spss$REGION[gba.spss$PROVINCEPLUS == "Rotterdam"] = "3 large cities: Amsterdam, Rotterdam, Den Haag + connected cities"
gba.spss$REGION[gba.spss$PROVINCEPLUS == "Groningen"] = "North (Groningen, Friesland, Drenthe)"  
gba.spss$REGION[gba.spss$PROVINCEPLUS == "Drenthe"] = "North (Groningen, Friesland, Drenthe)"  
gba.spss$REGION[gba.spss$PROVINCEPLUS == "Friesland"] = "North (Groningen, Friesland, Drenthe)"  
gba.spss$REGION[gba.spss$PROVINCEPLUS == "Noord-Holland, excl Amsterdam"] = "West (Utrecht, Noord-Holland, Zuid-Holland excl. 3 large cities"  
gba.spss$REGION[gba.spss$PROVINCEPLUS == "Zuid-Holland, excl. Rotterdam and Den Haag"] = "West (Utrecht, Noord-Holland, Zuid-Holland excl. 3 large cities"
gba.spss$REGION[gba.spss$PROVINCEPLUS == "Utrecht"] = "West (Utrecht, Noord-Holland, Zuid-Holland excl. 3 large cities"
gba.spss$REGION[gba.spss$PROVINCEPLUS == "Overijssel"] = "East (Overijssel, Gelderland, Flevoland)"   
gba.spss$REGION[gba.spss$PROVINCEPLUS == "Gelderland"] = "East (Overijssel, Gelderland, Flevoland)"   
gba.spss$REGION[gba.spss$PROVINCEPLUS == "Flevoland"] = "East (Overijssel, Gelderland, Flevoland)"   
gba.spss$REGION[gba.spss$PROVINCEPLUS == "Zeeland"] = "South (Zeeland, Noord-Brabant, Limburg)"  
gba.spss$REGION[gba.spss$PROVINCEPLUS == "Noord-Brabant"] = "South (Zeeland, Noord-Brabant, Limburg)"  
gba.spss$REGION[gba.spss$PROVINCEPLUS == "Limburg"] = "South (Zeeland, Noord-Brabant, Limburg)"  

#Creating a province dimension
gba.spss$PROVINCE <- NA
gba.spss$PROVINCE[gba.spss$PROVINCEPLUS == "Amsterdam"] = "Noord-Holland"
gba.spss$PROVINCE[gba.spss$PROVINCEPLUS == "Den Haag"] = "Zuid-Holland"
gba.spss$PROVINCE[gba.spss$PROVINCEPLUS == "Rotterdam"] = "Zuid-Holland"
gba.spss$PROVINCE[gba.spss$PROVINCEPLUS == "Noord-Holland, excl Amsterdam"] = "Noord-Holland"
gba.spss$PROVINCE[gba.spss$PROVINCEPLUS == "Zuid-Holland, excl. Rotterdam and Den Haag"] = "Zuid-Holland"
gba.spss$PROVINCE[gba.spss$PROVINCEPLUS == "Groningen"] = "Groningen"
gba.spss$PROVINCE[gba.spss$PROVINCEPLUS == "Drenthe"] = "Drenthe"
gba.spss$PROVINCE[gba.spss$PROVINCEPLUS == "Friesland"] = "Friesland"
gba.spss$PROVINCE[gba.spss$PROVINCEPLUS == "Utrecht"] = "Utrecht"
gba.spss$PROVINCE[gba.spss$PROVINCEPLUS == "Overijssel"] = "Overijssel"
gba.spss$PROVINCE[gba.spss$PROVINCEPLUS == "Gelderland"] = "Gelderland"
gba.spss$PROVINCE[gba.spss$PROVINCEPLUS == "Flevoland"] = "Flevoland"
gba.spss$PROVINCE[gba.spss$PROVINCEPLUS == "Zeeland"] = "Zeeland"
gba.spss$PROVINCE[gba.spss$PROVINCEPLUS == "Noord-Brabant"] = "Noord-Brabant"
gba.spss$PROVINCE[gba.spss$PROVINCEPLUS == "Limburg"] = "Limburg"

#Create a reference vector of all the dimensions in gba.spss for recursive functions
#Remove "count" because we will never refer to "count" recursively
#Remove "age" because we have "agecats"
gba_dimnames <- names(gba.spss)
gba_dimnames <- gba_dimnames[c(-2,-11)]

crosstables = list(NA)
index = 1
for (i in 1:length(gba_dimnames)){
	for (j in 1:length(gba_dimnames)){
		crosstables[[index]] = crosstab(
			eval(parse(text = paste("gba.spss$", gba_dimnames[i], sep = ""))),
			eval(parse(text = paste("gba.spss$", gba_dimnames[j], sep = ""))),
			weight=gba.spss$count,
			plot = FALSE)
		pdf(paste("crosstables_", gba_dimnames[i], gba_dimnames[j], ".pdf", sep = ""))
			crosstab(
			eval(parse(text = paste("gba.spss$", gba_dimnames[i], sep = ""))),
			eval(parse(text = paste("gba.spss$", gba_dimnames[j], sep = ""))),
			weight=gba.spss$count,
			plot = TRUE,	
			xlab = paste(gba_dimnames[i]),
			ylab = paste(gba_dimnames[j]))
		dev.off()
		index = index + 1
}}

freqtables = list(NA)
for (i in 1:length(gba_dimnames)){
	pdf(paste("gba.spss$", gba_dimnames[i], ".pdf", sep = ""))	
	freqtables[[i]] = freq(eval(parse(text = paste("gba.spss$", gba_dimnames[i], sep = ""))), w=gba.spss$count, plot = TRUE)
	#freq(eval(parse(text = paste("frequencies_", gba_dimnames[i], sep = ""))), w=gba.spss$count, plot = TRUE)
	dev.off()
}


freqtables <- lapply(freqtables, function(x){
	as.data.frame(x$freqtable)
})

names(freqtables) = gba_dimnames

for (i in 1:length(gba_dimnames)){
	freqtables[[i]] <- eval(parse(text = paste("freqtables$", gba_dimnames[i], sep = "")))[2]/100
}

benchmarks <- lapply(freqtables, function(x){
	as.data.frame(x[-nrow(x),])
})

for (i in 1:length(benchmarks)){
	tmp = row.names(freqtables[[i]])
	tmp = tmp[1:length(tmp)-1] 
	row.names(benchmarks[[i]]) = tmp
	names(benchmarks[[i]]) = c("Percent")
}

#NA Placeholders
benchmarks$REGION[6,] = NA
benchmarks$PROVINCE[13,] = NA



#reordering to match NOPVO orders
benchmarks$ORIGINSELF <- as.data.frame(benchmarks$ORIGINSELF[c(1, 4, 5, 6, 3, 2, 7, 8, 9, 10),])
benchmarks$ORIGINSELF[7,] <- benchmarks$ORIGINSELF[7,] + benchmarks$ORIGINSELF[8,]
benchmarks$ORIGINSELF <- as.data.frame(benchmarks$ORIGINSELF[-8:-9,])
names(benchmarks$ORIGINSELF) = c("Percent")
row.names(benchmarks$ORIGINSELF) = c(
	"Netherlands",
	"Suriname",
	"Antilles and Aruba",
	"Indonesia",
	"Turkye",
	"Morocco",
	"Other",
	"Unknown"
)

benchmarks$ORIGINMOTHER <- as.data.frame(benchmarks$ORIGINMOTHER[c(1, 4, 5, 6, 3, 2, 7, 8, 9, 10),])
benchmarks$ORIGINMOTHER[7,] <- benchmarks$ORIGINMOTHER[7,] + benchmarks$ORIGINMOTHER[8,]
benchmarks$ORIGINMOTHER <- as.data.frame(benchmarks$ORIGINMOTHER[-8:-9,])
names(benchmarks$ORIGINMOTHER) = c("Percent")
row.names(benchmarks$ORIGINMOTHER) = c(
	"Netherlands",
	"Suriname",
	"Antilles and Aruba",
	"Indonesia",
	"Turkye",
	"Morocco",
	"Other",
	"Unknown"
)

benchmarks$ORIGINFATHER <- as.data.frame(benchmarks$ORIGINFATHER[c(1, 4, 5, 6, 3, 2, 7, 8, 9, 10),])
benchmarks$ORIGINFATHER[7,] <- benchmarks$ORIGINFATHER[7,] + benchmarks$ORIGINFATHER[8,]
benchmarks$ORIGINFATHER <- as.data.frame(benchmarks$ORIGINFATHER[-8:-9,])
names(benchmarks$ORIGINFATHER) = c("Percent")
row.names(benchmarks$ORIGINFATHER) = c(
	"Netherlands",
	"Suriname",
	"Antilles and Aruba",
	"Indonesia",
	"Turkye",
	"Morocco",
	"Other",	
	"Unknown"
)

benchmarks$PROVINCEPLUS = NULL

rm(gba.spss)

#Saving the workspace as an .RData file (to send to others easily)
#save.image("\\\\Srsh1f\\sah1\\Kwaliteit_Webpanels\\R\\GBA_tables.RData")