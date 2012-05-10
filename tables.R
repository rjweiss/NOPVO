#This script creates all tables
#This script must be run from within analysis.R

setwd("/Users/Rebecca/Dropbox/research/NOPVO/analysis/tables")
#TODO: Fix absolute column hack for rounding to 2 digits

###########
#ESTIMATES#
###########
liss_est$bureau = "LISS"

regvars_estimate_data = rbind(liss_est, nopvo_regvars_est)
regvars_estimate_table = dcast(regvars_estimate_data, variable + category ~ bureau, value.var=c("value"))
regvars_estimate_table[,3:21] = round(regvars_estimate_table[,3:21]*100, digits=2)

#nonregvars_estimate_data = rbind(liss_est, nopvo_regvars_est)
nonregvars_estimate_data = nopvo_nonregvars_est
nonregvars_estimate_table = dcast(nonregvars_estimate_data, variable + category ~ bureau, value.var=c("value"))
nonregvars_estimate_table[,3:20] = round(nonregvars_estimate_table[,3:20]*100, digits=2)
#error_table = arrange(error_table, desc(as.numeric(bureau)))

write.csv(regvars_estimate_table, "regvars_estimate_table.csv")
write.csv(nonregvars_estimate_table, "nonregvars_estimate_table.csv")


#################
#ESTIMATE ERRORS#
#################

liss_err$bureau = "LISS"
ebb_cali_err$bureau = "EBB"
pols_cali_err$bureau = "POLS"

nopvo_regvars_err$abs_err = NULL

regvars_error_data = rbind(liss_err, nopvo_regvars_err)
regvars_error_table = dcast(regvars_error_data, variable + category ~ bureau, value.var=c("err"))
regvars_error_table[,3:21] = round(regvars_error_table[,3:21]*100, digits=2)

#nonregvars_error_data = rbind(liss_err, nopvo_regvars_err)
nonregvars_error_data = nopvo_nonregvars_err
nonregvars_error_table = dcast(nopvo_nonregvars_err, variable + category ~ bureau, value.var=c("err"))
nonregvars_error_table[,3:20] = round(nonregvars_error_table[,3:20]*100, digits=2)

write.csv(nonregvars_error_table, "nonregvars_error_table.csv")
write.csv(regvars_error_table, "regvars_error_table.csv")

#########################
#AVERAGE ABSOLUTE ERRORS#
#########################


######################
#MODE CATEGORY T-TEST#
######################

#TODO: relabel t test result tables with "regvars"
nopvo_t$abs_err = NULL
nopvo_t$n = NULL

liss_t$bureau = c('liss')
ebb_t$bureau = c('ebb')
pols_t$bureau = c('pols')

regvars_t_data = rbind(liss_t, ebb_t, pols_t, nopvo_t)
regvars_t_data$V1 = round(as.numeric(regvars_t_data$V1), digits=3)

regvars_t_values = dcast(regvars_t_data, bureau ~ variable, value.var = c('V1'))
regvars_t_stars = dcast(regvars_t_data, bureau ~ variable, value.var = c('V2'))
regvars_t_values_table = arrange(regvars_t_values, desc(as.numeric(bureau)))
regvars_t_stars_table = arrange(regvars_t_stars, desc(as.numeric(bureau)))

regvars_t_stars_count = melt(ddply(regvars_t_data, .(bureau), summarise, 
                                    three = sum(V2 %in% "***"),
                                    two = sum(V2 %in% "**"),
                                    one = sum(V2 %in% "*"),
                                    cross = sum(V2 %in% "+")))

#for Sweave, print(xtable(t_stars), include.rownames=F)

write.csv(regvars_t_stars_table, "regvars_t_tests_stars.csv")
write.csv(regvars_t_values_table, "regvars_t_tests_values.csv")

#######################
#MAHALANOBIS DISTANCES#
#######################

# liss_regvars_mahal_stars = dcast(liss_regvars_dist, id ~ ., value.var = c("star"))
# 
# liss_mahal_stars_count = melt(ddply(liss_regvars_dist, .(id), summarise, 
#                                     three = sum(star %in% "***"),
#                                     two = sum(star %in% "**"),
#                                     one = sum(star %in% "*"),
#                                     cross = sum(star %in% "+")))
# 
# #table of stars
# regvars_mahal_stars = arrange(dcast(regvars_dist, bureau ~ id, value.var = c("star")), desc(as.numeric(bureau)))
# regvars_wtd_mahal_stars = arrange(dcast(regvars_wtd_dist, bureau ~ id, value.var = c("star")), desc(as.numeric(bureau)))
# 
# regvars_mahal = arrange(dcast(regvars_dist, bureau ~ id, value.var = c("dist")), desc(as.numeric(bureau)))
# 
# #distribution of distance significances
# regvars_mahal_stars_count = melt(ddply(regvars_dist, .(bureau), summarise, 
#                                        three = sum(star %in% "***"),
#                                        two = sum(star %in% "**"),
#                                        one = sum(star %in% "*"),
#                                        cross = sum(star %in% "+")))
# 
# regvars_wtd_mahal_stars_count = melt(ddply(regvars_wtd_dist, .(bureau), summarise, 
#                                            three = sum(star %in% "***"),
#                                            two = sum(star %in% "**"),
#                                            one = sum(star %in% "*"),
#                                            cross = sum(star %in% "+")))

#####################
#REGRESSION ANALYSES#
#####################

l_ply(names(output),function(name,...) {
  df <- output[[name]]
  write.mtable(df, file=paste("age_", name, "_regression.csv", sep=""))  
  })
