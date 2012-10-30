rm(list=ls(all=T))
gc()

setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/csv output")

#bureaus = seq(1:18)

#unweighted
nopvo_reg_est = read.csv("nopvo_reg_est.csv")#, col.names = c("categories", bureaus, "variable"))
nopvo_reg_err = read.csv("nopvo_reg_err.csv")#, col.names = c("categories", bureaus, "variable"))
nopvo_reg_se = read.csv("nopvo_reg_se.csv")#, col.names = c("categories", bureaus, "variable"))
nopvo_reg_est$X = NULL
nopvo_reg_err$X = NULL
nopvo_reg_se$X = NULL

#hacky hack hack
nopvo_reg_est = replace(nopvo_reg_est, is.na(nopvo_reg_est), 0)
nopvo_reg_err = replace(nopvo_reg_err, is.na(nopvo_reg_err), 0)
nopvo_reg_se = replace(nopvo_reg_se, is.na(nopvo_reg_se), 0)

#weighted
#nopvo_reg_est_wtd = read.csv("nopvo_reg_est_wtd.csv", col.names = c("categories", bureaus, "variable"))
#nopvo_reg_err_wtd = read.csv("nopvo_reg_err.csv", col.names = c("categories", bureaus, "variable"))
#nopvo_reg_se_wtd = read.csv("nopvo_reg_se_wtd.csv", col.names = c("categories", bureaus, "variable"))

nopvo_n = read.csv("nopvo_n.csv")
nopvo_n$X = NULL
benchmarks = read.csv("benchmarks.csv")
benchmarks$X = NULL


###########
#FUNCTIONS#
###########

#one-sample t-test

onesamp_ttest = function(err, se, n){
	err/(se/sqrt(n))	
}

onesamp_sig = function(t, df){
	1-pt(t, df=df)
}

#welch's t-test

#welch_ttest = function(x1, x2, se1, se2, n1, n2){
#	return (x1 - x2)/(sqrt((se1^2/n1) + (se2^2/n2)))
#}

#welch_sig = function(t, se1, se2, n1, n2){
#	df = ((se1^2/n1) + (se2^2/n2))^2/((se1^2/n1)^2/(n1-1) + (se2^2/n2)^2/(n2-1))
#	return 1-pt(t, df=df)
#}

####################
#REGISTER VARIABLES#
####################

#NOPVO unweighted

reg_var_labels <- c(
	"gender",
	"region",
	"province",
	"nationality",
	"originself",
	"originfather",
	"originmother",
	#"urbanization",
	"numpersonshh",
	"agecats"
)

for (i in 1:length(reg_var_labels)){
	assign(
		paste("",reg_var_labels[i],"_est",sep=""),
		nopvo_reg_est[nopvo_reg_est$variable == reg_var_labels[i],]
)}

for (i in 1:length(reg_var_labels)){
	assign(
		paste("",reg_var_labels[i],"_err",sep=""),
		nopvo_reg_err[nopvo_reg_err$variable == reg_var_labels[i],]
)}

for (i in 1:length(reg_var_labels)){
	assign(
		paste("",reg_var_labels[i],"_se",sep=""),
		nopvo_reg_se[nopvo_reg_se$variable == reg_var_labels[i],]
)}

#########
#T-TESTS#
#########

nopvo_reg_ttest = matrix(0,nrow=length(reg_var_labels), ncol=length(nopvo_reg_err)-2)
nopvo_reg_ttest_p = matrix(0,nrow=length(reg_var_labels), ncol=length(nopvo_reg_err)-2)
nopvo_reg_ttest_star = matrix(0,nrow=length(reg_var_labels), ncol=length(nopvo_reg_err)-2)

rownames(nopvo_reg_ttest) = reg_var_labels
rownames(nopvo_reg_ttest_p) = reg_var_labels
rownames(nopvo_reg_ttest_star) = reg_var_labels

for (i in 1:length(reg_var_labels)){
	temp_err = eval(parse(text = paste("",reg_var_labels[i],"_err",sep="")))
	temp_se = eval(parse(text = paste("",reg_var_labels[i],"_se",sep="")))
	temp_err$categories = NULL
	temp_err$variable = NULL
	for (j in 1:length(temp_err)){
		diff = as.matrix(temp_err[,j])
		#for (k in 1:length(diff)){if(diff[k]=="NA") diff[k]=1e-14}
		df = nopvo_n[i,]-1
		result = as.numeric(tryCatch(ttest(diff, mat), error = function(e) print("0")))
		nopvo_reg_mahal[i,j] = result
		nopvo_reg_mahal_p[i,j] = mahal_sig(result, df)
		nopvo_reg_mahal_star[i,j] = star(mahal_sig(result, df))
	}
}