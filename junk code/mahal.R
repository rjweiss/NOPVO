rm(list=ls(all=T))
gc()

setwd("/Users/Rebecca/Dropbox/NOPVO/current analysis/scripts")
source("nopvo_comparison_script.R")

rm(
benchmark_modes,
benchmark_modes.df,
benchmarks.df,
benchmark_modes.temp,
i,
nopvo_estimates.df,
nopvo.prop_dfs,
nopvo_register_errors.df,
nopvo_register_estimates.df,
nopvo_register_ses.df,
nopvo.se_dfs,
tmp
)