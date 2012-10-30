rm(list=ls(all=T))
gc()

setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/csv output")

##############
#Loading data#
##############

#NOPVO


#weighted
nopvo_reg_est_wtd = read.csv("nopvo_reg_est_wtd.csv")#, col.names = c("categories", bureaus, "variable"))
nopvo_reg_err_wtd = read.csv("nopvo_reg_err_wtd.csv")#, col.names = c("categories", bureaus, "variable"))
nopvo_reg_se_wtd = read.csv("nopvo_reg_se_wtd.csv")#, col.names = c("categories", bureaus, "variable"))

nopvo_n = read.csv("nopvo_n.csv")
nopvo_n$X = NULL
benchmarks = read.csv("benchmarks.csv")
benchmarks$X = NULL

###########
#FUNCTIONS#
###########

mahal_dist = function(diff, mat){
	distance = t(diff) %*% solve(mat, tol=1e-21) %*% diff
	return(distance)
}

mahal_sig = function(dist, dof){
	round(1 - pchisq(dist, df = dof), digits=3)
}

star = function(x){
	as.character(symnum(x, corr=FALSE,
               cutpoints = c(0,  .001,.01,.05, .1, 1),
               symbols = c("***","**","*","+"," ")))
}

####################
#REGISTER VARIABLES#
####################

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


#NOPVO weighted

for (i in 1:length(reg_var_labels)){
	assign(
		paste("",reg_var_labels[i],"_est_wtd",sep=""),
		nopvo_reg_est_wtd[nopvo_reg_est_wtd$variable == reg_var_labels[i],]
)}

for (i in 1:length(reg_var_labels)){
	assign(
		paste("",reg_var_labels[i],"_err_wtd",sep=""),
		nopvo_reg_err_wtd[nopvo_reg_err_wtd$variable == reg_var_labels[i],]
)}

for (i in 1:length(reg_var_labels)){
	assign(
		paste("",reg_var_labels[i],"_se_wtd",sep=""),
		nopvo_reg_se_wtd[nopvo_reg_se_wtd$variable == reg_var_labels[i],]
)}

#finding variance-covariance matrix
#returns a list of list: variables by bureau
nopvo_reg_varcovs_wtd = list(NA)
for (i in 1:length(reg_var_labels)){
	temp_est = eval(parse(text = paste("",reg_var_labels[i],"_est_wtd",sep="")))
	temp_se = eval(parse(text = paste("",reg_var_labels[i],"_se_wtd",sep="")))
	temp_categories = temp_est$X
	temp_est$X = NULL
	temp_est$variable = NULL
	temp_est$categories = NULL
	temp_se$X = NULL
	temp_se$variable = NULL
	temp_se$categories = NULL
	covs = list(NA)
	for (j in 1:length(temp_est)){
		mat = -1 * (temp_est[,j] %*% t(temp_est[,j])) / nopvo_n$x[j]
		sigma_sq = as.numeric(temp_se[,j])^2
		diag(mat) = sigma_sq
		covs[[j]] = mat
	}
	nopvo_reg_varcovs_wtd[[i]] = covs
}

#EBB


#######################
#MAHALANOBIS DISTANCES#
#######################

#EBB

#NOPVO


nopvo_reg_wtd_mahal = matrix(0,nrow=length(reg_var_labels), ncol=length(nopvo_reg_err_wtd)-3)
nopvo_reg_wtd_mahal_p = matrix(0,nrow=length(reg_var_labels), ncol=length(nopvo_reg_err_wtd)-3)
nopvo_reg_wtd_mahal_star = matrix(0,nrow=length(reg_var_labels), ncol=length(nopvo_reg_err_wtd)-3)

rownames(nopvo_reg_wtd_mahal) = reg_var_labels
rownames(nopvo_reg_wtd_mahal_p) = reg_var_labels
rownames(nopvo_reg_wtd_mahal_star) = reg_var_labels

for (i in 1:length(reg_var_labels)){
	temp_err = eval(parse(text = paste("",reg_var_labels[i],"_err_wtd",sep="")))
	temp_err$X = NULL
	temp_err$categories = NULL
	temp_err$variable = NULL
	for (j in 1:length(temp_err)){
		diff = as.matrix(temp_err[,j])
		#for (k in 1:length(diff)){if(diff[k]=="NA") diff[k]=1e-14}
		df = length(diff)-1
		mat = nopvo_reg_varcovs_wtd[[i]][[j]]
		result = as.numeric(tryCatch(mahal_dist(diff, mat), error = function(e) print("0")))
		nopvo_reg_wtd_mahal[i,j] = result
		nopvo_reg_wtd_mahal_p[i,j] = mahal_sig(result, df)
		nopvo_reg_wtd_mahal_star[i,j] = star(mahal_sig(result, df))
	}
}

setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/csv output")
write.csv(nopvo_reg_wtd_mahal, "nopvo_reg_wtd_mahal.csv")
write.csv(nopvo_reg_wtd_mahal_p, "nopvo_reg_wtd_mahal_p.csv")
write.csv(nopvo_reg_wtd_mahal_star, "nopvo_reg_wtd_mahal_star.csv")

#weighted

#######################
#NONREGISTER VARIABLES#
#######################
