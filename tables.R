#This script creates all tables
#This script must be run from within analysis.R


setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts/")

source("analysis.R")


star = function(x){
  as.character(symnum(x, corr=FALSE,
                      cutpoints = c(0,  .001,.01,.05, .1, 1),
                      symbols = c("***","**","*","+"," ")))
}



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