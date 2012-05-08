#This script contains all of the mahalanobis distance code
#This script *must* be sourced from within analysis.R

###########
#FUNCTIONS#
###########

mahal_dist = function(diff, mat){
  #TODO: Fix Dirty Hack
  dist = as.numeric(tryCatch(t(diff) %*% solve(mat, tol=1e-21) %*% diff, error = function(e) return("0")))
  return(dist)
}

get_mahal_dist = function(dat, n, regvars = T, cbs = F, nopvo = F, liss = F){
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
      distance = mahal_dist(diff, mat)
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
          distance = mahal_dist(diff, mat)
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
          distance = mahal_dist(diff, mat)
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
nopvo_regvars_err = sqldf("select _id, _bureau, variable, adjusted as err from nopvo_regvars_err")
colnames(nopvo_regvars_err) <- c(".id", ".bureau", "variable", "err")

regvars = sqldf("select * from nopvo_regvars_se natural join nopvo_regvars_est natural join nopvo_regvars_err")
names(regvars) = c("id", "bureau", "variable", "se", "est", "err")

regvars_wtd = sqldf("select * from nopvo_regvars_wtd_se natural join nopvo_regvars_wtd_est natural join nopvo_regvars_wtd_err")

regvars_wtd = sqldf("select * from nopvo_regvars_wtd_est natural join nopvo_regvars_wtd_err")

names(regvars_wtd) = c("id", "bureau", "variable", "est", "err", "abs_err", "se")
regvars_wtd$abs_err = NULL

regvars_dist = get_mahal_dist(regvars, nopvo_n, regvars = T, cbs = F, nopvo = T, liss = F)
regvars_dist$star = star(regvars_dist$sig)

regvars_wtd_dist = get_mahal_dist(regvars_wtd, nopvo_n, regvars = T, cbs = F, nopvo = T, liss = F)
regvars_wtd_dist$star = star(regvars_wtd_dist$sig)

# nonregvars = sqldf("select * from nopvo_nonregvars_wtd_se natural join nopvo_nonregvars_wtd_est natural join nopvo_nonregvars_wtd_err")
# 
# names(nonregvars) = c("id", "bureau", "variable", "se", "est", "err")

#LISS
#get_mahal_dist = function(dat, n, regvars = T, cbs = F, nopvo = T, liss = F){

liss_regvars_dist = get_mahal_dist(liss_err, liss_n, regvars = T, cbs = F, nopvo = F, liss = T)
liss_regvars_dist$star = star(liss_regvars_dist$sig)
liss_regvars_dist$bureau = c("liss")
names(liss_regvars_dist) = c("id", "dist", "sig", "star", "bureau")

#regvars_dist = rbind(liss_regvars_dist, regvars_dist)

liss_regvars_mahal_stars = dcast(liss_regvars_dist, id ~ ., value.var = c("star"))

liss_mahal_stars_count = melt(ddply(liss_regvars_dist, .(id), summarise, 
                                    three = sum(star %in% "***"),
                                    two = sum(star %in% "**"),
                                    one = sum(star %in% "*"),
                                    cross = sum(star %in% "+")))

#table of stars
regvars_mahal_stars = arrange(dcast(regvars_dist, bureau ~ id, value.var = c("star")), desc(as.numeric(bureau)))
regvars_wtd_mahal_stars = arrange(dcast(regvars_wtd_dist, bureau ~ id, value.var = c("star")), desc(as.numeric(bureau)))

regvars_mahal = arrange(dcast(regvars_dist, bureau ~ id, value.var = c("dist")), desc(as.numeric(bureau)))

#distribution of distance significances
regvars_mahal_stars_count = melt(ddply(regvars_dist, .(bureau), summarise, 
                                       three = sum(star %in% "***"),
                                       two = sum(star %in% "**"),
                                       one = sum(star %in% "*"),
                                       cross = sum(star %in% "+")))

regvars_wtd_mahal_stars_count = melt(ddply(regvars_wtd_dist, .(bureau), summarise, 
                                           three = sum(star %in% "***"),
                                           two = sum(star %in% "**"),
                                           one = sum(star %in% "*"),
                                           cross = sum(star %in% "+")))