#This script contains all of the mahalanobis distance code
#This script *must* be sourced from within analysis.R

###########
#FUNCTIONS#
###########

mahalDistance = function(diffVector, varCovMatrix){
  #TODO: fix the tryCatch here
  #dist = as.numeric(tryCatch(t(diff) %*% solve(mat, tol=1e-21) %*% diff, error = function(e) return("0")))
  distanceMatrix = t(diffVector) %*% solve(varCovMatrix, tol=1e-21) %*% diffVector
  return(distanceMatrix)
}

varCovMatrix = function(propEstimateVector, varianceVector, sampleSize){
  outputMatrix = -1 * (propEstimateVector %*% t(propEstimateVector)) / sampleSize
  diag(outputMatrix) = varianceVector
  return(outputMatrix)
}



#TODO: fix the mahal distance function
getMahalDistance = function(dat, n, regvars = T, cbs = F, nopvo = F, liss = F){
  if(liss){
    dat$n = n
    ddply(dat, .(variable), function(x){
      #variance-covariance matrix
      mat = -1 * ((x$est %*% t(x$est)) / x$n)
      diag(mat) = x$se^2      
      for (k in 1:length(mat)){if(mat[k]==0) mat[k]=1e-14}      
      #difference column vector
      diff = x$err
      for (i in 1:length(diff)){if(diff[i]==0) diff[i]=1e-14}
      dof = length(diff) - 1
      #mahalanobis distance    
      distance = mahalDistance(diff, mat)
      sig = round(1 - pchisq(distance, df = dof), digits=3)
      return(c("dist" = distance, "sig" = sig))  
    })
  }
  #   if(cbs){
  #     
  #   }
  else { #if(nopvo){
    if(regvars){
      data = join(dat, n)
      ddply(data, .(bureau), function(df){
        ddply(df, .(id), function(x){
          #variance-covariance matrix
          mat = -1 * ((x$est %*% t(x$est)) / x$n)
          diag(mat) = x$se^2      
          for (k in 1:length(mat)){if(mat[k]==0) mat[k]=1e-14}      
          #difference column vector
          diff = x$err
          for (i in 1:length(diff)){if(diff[i]==0) diff[i]=1e-14}
          dof = length(diff) - 1
          #mahalanobis distance    
          distance = mahalDistance(diff, mat)
          sig = round(1 - pchisq(distance, df = dof), digits=3)
          return(c("dist" = distance, "sig" = sig))
        })
      })
    } else {
      data = join(dat, n)
      ddply(data, .(bureau), function(df){
        ddply(df, .(id), function(x){
          #variance-covariance matrix
          mat = -1 * ((x$est %*% t(x$est)) / x$n)
          diag(mat) = x$se^2      
          for (k in 1:length(mat)){if(mat[k]==0) mat[k]=1e-14}      
          #difference column vector
          diff = x$err
          for (i in 1:length(diff)){if(diff[i]==0) diff[i]=1e-14}
          dof = length(diff) - 1
          #mahalanobis distance    
          distance = mahalDistance(diff, mat)
          sig = round(1 - pchisq(distance, df = dof), digits=3)
          return(c("dist" = distance, "sig" = sig))
        })
      })  
    }
  }
  }



###############################
#FINDING MAHALANOBIS DISTANCES#
###############################

#NOPVO

colnames(nopvo_regvars_se)[4] <- "se"
colnames(nopvo_regvars_est)[4] <- "est"
nopvo_regvars_err = sqldf("select variable, bureau, category, err from nopvo_regvars_err")

regvars = sqldf("select * from nopvo_regvars_se natural join nopvo_regvars_est natural join nopvo_regvars_err")
#regvars = sqldf("select * from nopvo_regvars_se  join nopvo_regvars_est join nopvo_regvars_err")

regvars = join(nopvo_n, regvars, by = 'bureau')

regvars_wtd = sqldf("select * from nopvo_regvars_wtd_se natural join nopvo_regvars_wtd_est natural join nopvo_regvars_wtd_err")

regvars_wtd = sqldf("select * from nopvo_regvars_wtd_est natural join nopvo_regvars_wtd_err")

regvars_wtd$abs_err = NULL

#regvars$se = as.numeric(format(signif(regvars$se, digits = 2), scientific = FALSE))
#regvars$err = as.numeric(format(signif(regvars$err, digits = 2), scientific = FALSE))
#regvars$est = as.numeric(format(signif(regvars$est, digits = 2), scientific = FALSE))

regvarsDistances = 
  ddply(regvars, .(bureau), function(df){
    ddply(df, .(variable), function(x){
    if(0 %in% x$est){
        tmpEstimate = subset(reg_benchmarks, variable == x$variable)$value
        imputedVariance = tmpEstimate*(1-tmpEstimate)/x$n
        mat = varCovMatrix(tmpEstimate, imputedVariance, x$n)
        print(tmpEstimate)
        print(imputedVariance)
        distance = mahalDistance(x$err, mat)
        print(distance)
    }
#     else {
#       print(c(x$bureau, x$variable))
#       mat = varCovMatrix(x$est, x$se^2, x$n)
#       distance = format(signif(mahalDistance(x$err, mat), digits = 2), scientific=FALSE)    
#     }
#     return(distance)
  })
})
    #variance-covariance matrix
    mat = -1 * ((x$est %*% t(x$est)) / x$n)
    diag(mat) = x$se^2      
    for (k in 1:length(mat)){if(mat[k]==0) mat[k]=1e-14}      
    #difference column vector
    diff = x$err
    for (i in 1:length(diff)){if(diff[i]==0) diff[i]=1e-14}
    dof = length(diff) - 1
    #mahalanobis distance    
    distance = mahalDistance(diff, mat)
    sig = round(1 - pchisq(distance, df = dof), digits=3)
    return(c("dist" = distance, "sig" = sig))
  })
})


regvars_dist = getMahalDistance(regvars, nopvo_n, regvars = T, cbs = F, nopvo = T, liss = F)
regvars_dist$star = star(regvars_dist$sig)

regvars_wtd_dist = getMahalDistance(regvars_wtd, nopvo_n, regvars = T, cbs = F, nopvo = T, liss = F)
regvars_wtd_dist$star = star(regvars_wtd_dist$sig)

# nonregvars = sqldf("select * from nopvo_nonregvars_wtd_se natural join nopvo_nonregvars_wtd_est natural join nopvo_nonregvars_wtd_err")
# 
# names(nonregvars) = c("id", "bureau", "variable", "se", "est", "err")

#LISS
#getMahalDistance = function(dat, n, regvars = T, cbs = F, nopvo = T, liss = F){

liss_regvars_dist = getMahalDistance(liss_err, liss_n, regvars = T, cbs = F, nopvo = F, liss = T)
liss_regvars_dist$star = star(liss_regvars_dist$sig)
liss_regvars_dist$bureau = c("liss")
names(liss_regvars_dist) = c("id", "dist", "sig", "star", "bureau")

#regvars_dist = rbind(liss_regvars_dist, regvars_dist)