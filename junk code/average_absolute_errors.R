#This is the script that calculates average absolute errors

setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/")
source("nopvo_comparison_script.R")

#AAE for each panel over all categories 

modal.estimates = list(NA)
modal.ses = list(NA)
modal.ppe = list(NA)

for(i in 1:length(benchmark_modes.df)) {
	modal.estimates[[i]] <- nopvo_register_estimates.df[which(colnames(nopvo_register_estimates.df) == benchmark_modes.df[i])]
	modal.ses[[i]] <- nopvo_register_ses.df[which(colnames(nopvo_register_ses.df) == benchmark_modes.df[i])]
	modal.ppe[[i]] <- nopvo_register_errors.df[which(names(nopvo_register_errors.df) == benchmark_modes.df[i])]
}

modal.estimates <- do.call(cbind, modal.estimates)
modal.ses <- do.call(cbind, modal.ses)
modal.ppe <- do.call(cbind, modal.ppe)

byBureau.aae <- apply(modal.ppe, 1, function(x) {
	mean(abs(x))
})

byCategory.aae <- apply(modal.ppe, 2, function(x) {
	mean(abs(x))
})

byBureau.meanPPE <- apply(modal.ppe, 1, function(x) {
	mean(x)
})

byCategory.meanPPE <- apply(modal.ppe, 2, function(x) {
	mean(x)
})
