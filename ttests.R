#This script performs t-tests on the modal categories of each variable for each panel
#This script *must* be sourced from within analysis.R
#TODO: two-sample t-tests for nonregister variables

#NOPVO
#register variables

reg_benchmark_modes$bench = sqldf("SELECT * FROM reg_benchmarks, reg_benchmark_modes WHERE reg_benchmarks.category = reg_benchmark_modes.value AND reg_benchmarks.variable = reg_benchmark_modes.L1")$value

nopvo_df = join(nopvo_regvars_err_modes, nopvo_n)

nopvo_t = ddply(nopvo_df, .(bureau), function(df){
  ddply(df, .variables=c('category', 'variable'), function(x){
    #print((reg_benchmark_modes[reg_benchmark_modes$value==x$category,]$bench - x$est)/x$se)
    benchmark = sqldf("SELECT * FROM reg_benchmark_modes, x WHERE reg_benchmark_modes.value = x.category AND reg_benchmark_modes.L1 = x.variable")
    out = (benchmark$bench - x$est)/x$se
    p = dt(out, x$n-1)
    c(p, star(p))
  })
})

nopvo_t = join(nopvo_t, nopvo_df)

#nonregister variables

#LISS
#register variables
liss_t = ddply(liss_regvars_err_modes, .variables=c('category', 'variable'), function(x){
  benchmark = sqldf("SELECT * FROM reg_benchmark_modes, x WHERE reg_benchmark_modes.value = x.category AND reg_benchmark_modes.L1 = x.variable")
  out = (benchmark$bench - x$est)/x$se
  p = dt(out, liss_n-1)
  c(p, star(p))
})

liss_t = join(liss_t, liss_regvars_err_modes)

#nonregister variables

#CBS

#Register

ebb_t = ddply(ebb_regvars_err_modes, .variables=c('category', 'variable'), function(x){
  benchmark = sqldf("SELECT * FROM reg_benchmark_modes, x WHERE reg_benchmark_modes.value = x.category AND reg_benchmark_modes.L1 = x.variable")
  out = (benchmark$bench - x$est)/x$se
  p = dt(out, 10589)
  c(p, star(p))
})

ebb_t = join(ebb_t, ebb_regvars_err_modes)

pols_t = ddply(pols_regvars_err_modes, .variables=c('category', 'variable'), function(x){
  benchmark = sqldf("SELECT * FROM reg_benchmark_modes, x WHERE reg_benchmark_modes.value = x.category AND reg_benchmark_modes.L1 = x.variable")
  out = (benchmark$bench - x$est)/x$se
  p = dt(out, 9607)
  c(p, star(p))
})

pols_t = join(pols_t, pols_regvars_err_modes)

