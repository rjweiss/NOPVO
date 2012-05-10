#This script generates all the plots from the analysis

#plotting all estimates, estimate errors, and standard errors for each NOPVO panel
setwd("/Users/Rebecca/Dropbox/research/NOPVO/analysis/plots/summary_by_category")


#TODO: add benchmark values as a different color bar 

benchmarks = reg_benchmarks
benchmarks$bureau = c("CBS")
benchmarks$var = c("Benchmark")

nopvo_regvars = nopvo_regvars_est
nopvo_regvars$var = c("NOPVO")

plot_data = rbind(nopvo_regvars, benchmarks)

plotter = function(df, upper, lower){
  var = unique(df$variable)
  parameter = unique(df$parameter)
  filename <- function(y){
    paste(var, "_", df$category, y, ".pdf", sep = "")
  }
  p = ggplot(df, aes(x = reorder(bureau, value), y = value*100, fill=var)) + geom_bar(stat = "identity") +
    scale_x_discrete("Bureaus") +
    scale_y_continuous(parameter, limits=c(upper, lower)) +
    opts(
      title = paste(df$variable, ": ", df$category, sep = ""))
  suppressMessages(ggsave(filename(paste(parameter)), height = 3, width = 9, p, dpi = 100))
}

d_ply(plot_data, .variables = c('variable', 'category'), .progress ="text", function(df){
  plotter(df, 0, 100)
})

###########
#AAE plots#
###########

#TODO: fix widths of bars

#NOPVO

#plotting all average absolute errors, unweighted and weighted, for each NOPVO panel
setwd("/Users/Rebecca/Dropbox/research/NOPVO/analysis/plots/summary_by_aae")

nopvo_regvars_avg_abs_err$group = c("register")
nopvo_regvars_wtd_avg_abs_err$group = c("register")
nopvo_nonregvars_avg_abs_err$group = c("nonregister") 
nopvo_nonregvars_wtd_avg_abs_err$group = c("nonregister")

aae_wtd_data = rbind(
  nopvo_regvars_avg_abs_err, 
  nopvo_regvars_wtd_avg_abs_err,
  nopvo_nonregvars_avg_abs_err, 
  nopvo_nonregvars_wtd_avg_abs_err)

aae_delta = ddply(aae_wtd_data, .(group), function(df){
  ddply(df, .(bureau), function(x){
    diff(x$value)
  })
})

#accuracy plots

d_ply(nopvo_regvars_avg_abs_err, .(group), .progress = "text", function(df){
  g = unique(df$group)
  b = as.character(unique(df$variable))
  df$variable = factor(df$variable)
  
  p = ggplot(df, aes(x = reorder(bureau, value), y = value)) +
    geom_bar(aes(width=0.75),stat = "identity", position = "dodge") +
    scale_x_discrete("Samples") +
    scale_y_continuous("") +
    opts(
      title = paste("NOPVO Average Absolute Error over ", g, " variables", sep = "")
      )
  suppressMessages(ggsave(paste(g, "_accuracy.pdf", sep = ""), p, height = 5, dpi = 300))
})

d_ply(nopvo_nonregvars_avg_abs_err, .(group), .progress = "text", function(df){
  g = unique(df$group)
  b = as.character(unique(df$variable))
  df$variable = factor(df$variable)
  
  p = ggplot(df, aes(x = reorder(bureau, value), y = value)) +
    geom_bar(aes(width=0.75), stat = "identity", position = "dodge") +
    scale_x_discrete("Samples") +
    scale_y_continuous("") +
    opts(
      title = paste("NOPVO Average Absolute Error over ", g, " variables", sep = "")
      )
  suppressMessages(ggsave(paste(g, "_accuracy.pdf", sep = ""), p, height = 5, dpi = 300))
})


#CBS

cbs_regvars = data.frame(ebb_regvar_abs_err$abs_err, pols_regvar_abs_err$abs_err)
names(cbs_regvars) = c("ebb", "pols")
cbs_regvars = melt(cbs_regvars)

p = ggplot(cbs_regvars, aes(x = variable, y = value*100)) + 
  geom_bar(aes(width=0.25), stat="identity") +
  scale_x_discrete("Samples") +
  scale_y_continuous("Percent error", limits = c(0, 10)) +
  opts(
    title = "CBS: Average Absolute Error over Register Variables"
    )
suppressMessages(ggsave("cbs_accuracy.pdf", p, height = 5, dpi = 300))

cbs_wtd_regvars = data.frame(ebb_regvar_wtd_abs_err$abs_err, pols_regvar_wtd_abs_err$abs_err)
names(cbs_wtd_regvars) = c("ebb", "pols")
cbs_wtd_regvars = melt(cbs_wtd_regvars)

p = ggplot(cbs_wtd_regvars, aes(x = variable, y = value*100)) + 
  geom_bar(aes(width=0.25), stat="identity") +
  scale_x_discrete("Samples") +
  scale_y_continuous("Percent", limits = c(0, 10)) +
  opts(
    title = "CBS: Average Absolute Error over Register Variables"
    ) 
suppressMessages(ggsave("cbs_wtd_accuracy.pdf", p, height = 5, dpi = 300))

#CBS + LISS
liss_regvars = liss_regvars_abs_errs$abs_err
names(liss_regvars) = c("liss")
liss_regvars = melt(liss_regvars)

cbs_regvars$group = c("CBS")
liss_regvars$group = c("LISS")

cbs_liss_regvars = rbind(cbs_regvars, liss_regvars)
names(cbs_liss_regvars) = c("bureau","value","group")

p = ggplot(cbs_liss_regvars, aes(x = bureau, y = value*100, fill = group)) + 
  geom_bar(aes(width=0.25), stat="identity") +
  scale_x_discrete("Samples") +
  scale_y_continuous("Percent", limits = c(0, 10)) +
  scale_fill_discrete(name = "Source") +
  scale_fill_brewer(palette="Dark2") +
  opts(
    title = "CBS and LISS: Average Absolute Error over Register Variables"
    )
suppressMessages(ggsave("cbs_liss_accuracy.pdf", p, height = 5, dpi = 300))

#CBS + NOPVO + LISS

nopvo_regvars = nopvo_regvars_avg_abs_err
nopvo_regvars$variable = NULL
nopvo_regvars$group = c("NOPVO")

cbs_liss_nopvo_regvars = rbind(cbs_liss_regvars, nopvo_regvars)

p = ggplot(cbs_liss_nopvo_regvars, aes(x = reorder(bureau, value, max), y = value, fill = group)) + 
  geom_bar(aes(width=0.75), stat="identity") +
  scale_x_discrete("Samples") +
  scale_y_continuous("Percent", limits = c(0, 0.11)) +
  scale_fill_discrete(name = "Source") +
  scale_fill_brewer(palette="Dark2") +
  opts(
    title = "CBS, LISS, and NOPVO: Average Absolute Error over Register Variables"
    )
suppressMessages(ggsave("cbs_liss_nopvo_accuracy.pdf", p, height = 5, dpi = 300))

#weighted comparison
aae_data = subset(nopvo_regvars_avg_abs_err, select = c(bureau, value))
aae_data$group = c("NOPVO")
names(cbs_regvars) = c("bureau", "value", "group")
#cbs_regvars$group = c("CBS")

aae_data = melt(rbind(aae_data, cbs_regvars))

p = ggplot(aae_data, aes(x = reorder(bureau, value), y = value, fill = group)) + 
  geom_bar(aes(width=0.75), stat="identity") +
  scale_x_discrete("Samples") +
  scale_y_continuous("Percent", limits = c(0, 0.11)) +
  scale_fill_discrete(name = "Source") +
  opts(
    title = "Comparing Average Absolute Errors of register variables"
    )
suppressMessages(ggsave("reg_accuracy_comparison.pdf", p, height = 5, dpi = 300))

#comparison plot
d_ply(aae_wtd_data, .(group), .progress = "text", function(df){
  g = unique(df$group)
  b = as.character(unique(df$variable))
  df$variable = factor(df$variable)
  
  p = ggplot(df, aes(x = reorder(bureau, value), y = value, fill = variable)) +
    geom_bar(aes(width=0.75), stat = "identity", position = "dodge") +
    scale_fill_discrete(name = "Condition", breaks = b, labels=c("Unweighted", "Weighted")) +
    scale_x_discrete("Samples") +
    scale_y_continuous("") +
    opts(
      title = paste("Average Absolute Error over ", g, " variables", sep = "")
      )
  suppressMessages(ggsave(paste(g, "_comparison.pdf", sep = ""), p, height = 5, dpi = 300))
})

#delta plot
d_ply(aae_delta, .(group), .progress = "text", function(df){
  g = unique(df$group)
  
  p = ggplot(df, aes(reorder(bureau, V1), V1)) +
    geom_bar(aes(width=0.75), stat = "identity") +
    scale_x_discrete("Samples") +
    scale_y_continuous("Delta") +
    opts(
      title = paste("Change in Average Absolute Error over ", g, " variables", sep = "")
      )
  suppressMessages(ggsave(paste(g, "_delta.pdf", sep=""), p, height = 5, dpi = 300))
})

##############
#T-test plots#
##############
setwd("/Users/Rebecca/Dropbox/research/NOPVO/analysis/plots/summary_by_mode_cat")

levels(regvars_t_stars_count$variable)[levels(regvars_t_stars_count$variable)=="three"] = "***" 
levels(regvars_t_stars_count$variable)[levels(regvars_t_stars_count$variable)=="two"] = "**" 
levels(regvars_t_stars_count$variable)[levels(regvars_t_stars_count$variable)=="one"] = "*" 
levels(regvars_t_stars_count$variable)[levels(regvars_t_stars_count$variable)=="cross"] = "+" 

regvars_t_stars_count = arrange(regvars_t_stars_count, desc(as.numeric(bureau)))
regvars_t_stars_count$bureau = factor(regvars_t_stars_count$bureau, as.character(regvars_t_stars_count$bureau)) #throws a warning

p = ggplot(regvars_t_stars_count, aes(bureau, value)) + 
  geom_bar(stat="identity", position = "dodge") + facet_wrap(~ variable) +
  scale_x_discrete("Bureaus") +
  scale_y_continuous("Count", limits = c(0, 10)) +
  opts(title = "Total significant mode category estimate differences", strip.text.x = theme_text(size=18), axis.text.x=theme_text(angle=-90))
suppressMessages(ggsave("t_signif_totals.pdf", p, height = 5.5, dpi = 300))

###################
#Mahalanobis plots#
###################
#TODO: Fix Mahalanobis plots
# setwd("/Users/Rebecca/Dropbox/research/NOPVO/analysis/plots/summary_by_mahal")
# 
# # tmp = melt(regvars_dist)
# # d_ply(tmp, .(id), .progress = "text", function(dat){
# #   d_ply(dat, .(variable), function(df){
# #     id = unique(df$id)
# #     var = unique(df$variable)
# #     p = ggplot(df, aes(bureau, value)) + geom_bar(stat = "identity")
# #     suppressMessages(ggsave(paste(id, "_", var, ".pdf", sep=""), p))
# #   })    
# # })
# 
# #barplots of number of significant distances
# 
# #relabeling levels for facet_wrap() panel titles
# levels(regvars_mahal_stars_count$variable)[levels(regvars_mahal_stars_count$variable)=="three"] = "***" 
# levels(regvars_mahal_stars_count$variable)[levels(regvars_mahal_stars_count$variable)=="two"] = "**" 
# levels(regvars_mahal_stars_count$variable)[levels(regvars_mahal_stars_count$variable)=="one"] = "*" 
# levels(regvars_mahal_stars_count$variable)[levels(regvars_mahal_stars_count$variable)=="cross"] = "+" 
# 
# levels(regvars_wtd_mahal_stars_count$variable)[levels(regvars_wtd_mahal_stars_count$variable)=="three"] = "***" 
# levels(regvars_wtd_mahal_stars_count$variable)[levels(regvars_wtd_mahal_stars_count$variable)=="two"] = "**" 
# levels(regvars_wtd_mahal_stars_count$variable)[levels(regvars_wtd_mahal_stars_count$variable)=="one"] = "*" 
# levels(regvars_wtd_mahal_stars_count$variable)[levels(regvars_wtd_mahal_stars_count$variable)=="cross"] = "+" 
# 
# levels(liss_mahal_stars_count$variable)[levels(liss_mahal_stars_count$variable)=="cross"] = "+" 
# levels(liss_mahal_stars_count$variable)[levels(liss_mahal_stars_count$variable)=="cross"] = "+" 
# levels(liss_mahal_stars_count$variable)[levels(liss_mahal_stars_count$variable)=="cross"] = "+" 
# levels(liss_mahal_stars_count$variable)[levels(liss_mahal_stars_count$variable)=="cross"] = "+" 
# 
# 
# #rearranging in numeric descending order for facet_wrap() panel titles
# regvars_mahal_stars_count = arrange(regvars_mahal_stars_count, desc(as.numeric(bureau)))
# regvars_mahal_stars_count$bureau = factor(regvars_mahal_stars_count$bureau, as.character(regvars_mahal_stars_count$bureau)) #throws a warning
# 
# regvars_wtd_mahal_stars_count = arrange(regvars_wtd_mahal_stars_count, desc(as.numeric(bureau)))
# regvars_wtd_mahal_stars_count$bureau = factor(regvars_wtd_mahal_stars_count$bureau, as.character(regvars_wtd_mahal_stars_count$bureau)) #throws a warning
# 
# p = ggplot(regvars_mahal_stars_count, aes(bureau, value)) + 
#   geom_bar(stat="identity", position = "dodge") + facet_wrap(~ variable) +
#   scale_x_discrete("Bureaus") +
#   scale_y_continuous("Count", limits = c(0, 10)) +
#   opts(title = "Total significant mahalanobis distances over unweighted register variables", strip.text.x = theme_text(size=18))
# suppressMessages(ggsave("nopov_mahal_totals.pdf", p, height = 5.5, dpi = 300))
# 
# p = ggplot(regvars_wtd_mahal_stars_count, aes(bureau, value)) + 
#   geom_bar(stat="identity", position = "dodge") + facet_wrap(~ variable) +
#   scale_x_discrete("Bureaus") +
#   scale_y_continuous("Count", limits = c(0, 10)) +
#   opts(title = "Total significant mahalanobis distances over weighted register variables", strip.text.x = theme_text(size=18))
# suppressMessages(ggsave("nopvo_mahal_wtd_totals.pdf", p, height = 5.5, dpi = 300))
# 
