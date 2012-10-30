tmp <- nopvo_register_estimates.df[which(names(nopvo_register_estimates.df) == names(benchmarks.df)[1])]

for (j in 1:length(nopvo.n)) {
	cat(tmp[j,], " & ", sep = "")
	rm(tmp)
	}
	
tmp <- nopvo_register_errors.df[which(names(nopvo_register_errors.df) == names(benchmarks.df)[1])]

for (j in 1:length(nopvo.n)) {
	cat(tmp[j,], " & ", sep = "")
	}
	
for (i in 1:length(benchmarks)) {
	row.names(benchmarks[[i]])[which()]
}