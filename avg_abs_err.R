#This script contains all the code for finding the average absolute errors
#This script *must* be sourced from within analysis.R

#################################
#FINDING AVERAGE ABSOLUTE ERRORS#
#################################

#####
#CBS#
#####

#creating structured data for EBB, POLS
#for now, let's just look at at 2006

#EBB
ebb_cali_err = subset(ebb_reg_err, select = c(ebb_cali_err6, variable, categories))
ebb_cali_est = subset(ebb_reg_est, select = c(calibrated2006, variable, categories))
ebb_cali_se = subset(ebb_reg_se, select = c(calibrated_se2006, variable, categories))

ebb_cali = sqldf("select * from ebb_cali_err natural join ebb_cali_est natural join ebb_cali_se")
names(ebb_cali) = c("err", "variable", "category", "est", "se")

ebb_incl_err = subset(ebb_reg_err, select = c(ebb_incl_err6, variable, categories))
ebb_incl_est = subset(ebb_reg_est, select = c(inclusion2006, variable, categories))
ebb_incl_se = subset(ebb_reg_se, select = c(inclusion_se2006, variable, categories))

ebb_incl = sqldf("select * from ebb_incl_err natural join ebb_incl_est natural join ebb_incl_se")
names(ebb_incl) = c("err", "variable", "category", "est", "se")

#POLS
pols_cali_err = subset(pols_reg_err, select = c(pols_cali_err6, variable, categories))
pols_cali_est = subset(pols_reg_est, select = c(calibrated2006, variable, categories))
pols_cali_se = subset(pols_reg_se, select = c(calibrated_se2006, variable, categories))

pols_cali = sqldf("select * from pols_cali_err natural join pols_cali_est natural join pols_cali_se")
names(pols_cali) = c("err", "variable", "category", "est", "se")

pols_incl_err = subset(pols_reg_err, select = c(pols_incl_err6, variable, categories))
pols_incl_est = subset(pols_reg_est, select = c(inclusion2006, variable, categories))
pols_incl_se = subset(pols_reg_se, select = c(inclusion_se2006, variable, categories))

pols_incl = sqldf("select * from pols_incl_err natural join pols_incl_est natural join pols_incl_se")
names(pols_incl) = c("err", "variable", "category", "est", "se")

#calibration weighted register variables

#EBB

ebb_regvars_err_modes = sqldf("select ebb.variable, ebb.category, ebb.est, ebb.se, ebb.err from ebb_cali ebb, reg_benchmark_modes rbm where ebb.category = rbm.value and ebb.variable = rbm.L1")

ebb_nvars = sqldf("select count(distinct variable) from ebb_regvars_err_modes")
names(ebb_nvars) = c("nvars")

ebb_regvar_abs_err = sqldf("select *, sum(abs(err)) from ebb_regvars_err_modes")
names(ebb_regvar_abs_err)[6] = c("sum_abs_err")
ebb_regvar_abs_err$abs_err = ebb_regvar_abs_err$sum_abs_err/ebb_nvars

#POLS

pols_regvars_err_modes = sqldf("select pols.variable, pols.category, pols.est, pols.se, pols.err from pols_cali pols, reg_benchmark_modes rbm where pols.category = rbm.value and pols.variable = rbm.L1")

pols_nvars = sqldf("select count(distinct variable) from pols_regvars_err_modes")
names(pols_nvars) = c("nvars")

pols_regvar_abs_err = sqldf("select *, sum(abs(err)) from pols_regvars_err_modes")
names(pols_regvar_abs_err)[6] = c("sum_abs_err")
pols_regvar_abs_err$abs_err = pols_regvar_abs_err$sum_abs_err/pols_nvars

#inclusion weighted register variables

#EBB

ebb_regvars_wtd_err_modes = sqldf("select ebb.variable, ebb.category, ebb.est, ebb.se, ebb.err from ebb_incl ebb, reg_benchmark_modes rbm where ebb.category = rbm.value and ebb.variable = rbm.L1")

ebb_regvar_wtd_abs_err = sqldf("select *, sum(abs(err)) from ebb_regvars_wtd_err_modes")
names(ebb_regvar_wtd_abs_err)[6] = c("sum_abs_err")
ebb_regvar_wtd_abs_err$abs_err = ebb_regvar_wtd_abs_err$sum_abs_err/ebb_nvars

#POLS

pols_regvars_wtd_err_modes = sqldf("select pols.variable, pols.category, pols.est, pols.se, pols.err from pols_incl pols, reg_benchmark_modes rbm where pols.category = rbm.value and pols.variable = rbm.L1")

pols_regvar_wtd_abs_err = sqldf("select *, sum(abs(err)) from pols_regvars_wtd_err_modes")
names(pols_regvar_wtd_abs_err)[6] = c("sum_abs_err")
pols_regvar_wtd_abs_err$abs_err = pols_regvar_wtd_abs_err$sum_abs_err/pols_nvars


######
#LISS#
######

liss_err = sqldf("select le.category, le.value, le.variable, le.value - rb.value as err from liss_est le, reg_benchmarks rb where le.category = rb._categories and le.variable = rb._id")

liss_err = sqldf("select distinct * from liss_err natural join liss_se")
names(liss_err) = c("category","est", "variable","err","se")

liss_regvars_err_modes = sqldf("select le.category, le.est, le.variable, le.err, le.se from liss_err le, reg_benchmark_modes rbm where le.category = rbm.value and le.variable = rbm.L1")

liss_nvars = sqldf("select count(distinct variable) from liss_regvars_err_modes")
names(liss_nvars) = c("nvars")

liss_regvar_abs_errs = sqldf("select *, sum(abs(err)) from (select distinct * from liss_regvars_err_modes)")
names(liss_regvar_abs_errs) = c("category","est", "variable","err","se","sum_abs_err")
liss_regvar_abs_errs$avg_abs_err_regvar = liss_regvar_abs_errs$sum_abs_err/liss_nvars

#######
#NOPVO#
#######

#unweighted register variables 

#TODO: where's region in nopvo modes?
nopvo_regvars_err_modes = sqldf("select nre.variable, nre.category, nre.bureau, nre.est, nre.err, nre.abs_err, nre.se from nopvo_regvars_err nre, reg_benchmark_modes rbm where nre.category = rbm.value and nre.variable = rbm.L1")
nopvo_regvars_n = sqldf("select count(distinct variable) from nopvo_regvars_err_modes group by bureau")
names(nopvo_regvars_n) = c("nvars")
nopvo_regvars_sum_abs_errs = sqldf("select *, sum(abs_err) as sum_abs_err from (select distinct * from nopvo_regvars_err_modes) group by bureau")
nopvo_regvars_sum_abs_errs$avg_abs_err_regvar = nopvo_regvars_sum_abs_errs$sum_abs_err/nopvo_regvars_n
nopvo_regvars_avg_abs_err = melt(subset(nopvo_regvars_sum_abs_errs, select = c(bureau, avg_abs_err_regvar)))
#TODO: In plots.R, change to "regvars" from "regvar"

#unweighted nonregister variables
nopvo_nonregvars_err_modes = sqldf("select nre.variable, nre.category, nre.bureau, nre.est, nre.err, nre.abs_err, nre.se from nopvo_nonregvars_err nre, nonreg_benchmark_modes rbm where nre.category = rbm.value and nre.variable = rbm.L1")
nopvo_nonregvars_n = sqldf("select count(distinct variable) from nopvo_nonregvars_err_modes group by bureau")
names(nopvo_nonregvars_n) = c("nvars")
nopvo_nonregvars_sum_abs_errs = sqldf("select *, sum(abs_err) as sum_abs_err from (select distinct * from nopvo_nonregvars_err_modes) group by bureau")
nopvo_nonregvars_sum_abs_errs$avg_abs_err_nonregvar = nopvo_nonregvars_sum_abs_errs$sum_abs_err/nopvo_nonregvars_n
nopvo_nonregvars_avg_abs_err = melt(subset(nopvo_nonregvars_sum_abs_errs, select = c(bureau, avg_abs_err_nonregvar)))

#weighted register variables
nopvo_regvars_wtd_err_modes = sqldf("select nre.variable, nre.category, nre.bureau, nre.est, nre.err, nre.abs_err, nre.se from nopvo_regvars_wtd_err nre, reg_benchmark_modes rbm where nre.category = rbm.value and nre.variable = rbm.L1")
nopvo_wtd_regvars_n = sqldf("select count(distinct variable) from nopvo_regvars_wtd_err_modes group by bureau")
names(nopvo_wtd_regvars_n) = c("nvars")
nopvo_regvars_wtd_sum_abs_errs = sqldf("select *, sum(abs_err) as sum_abs_err from (select distinct * from nopvo_regvars_wtd_err_modes) group by bureau")
nopvo_regvars_wtd_sum_abs_errs$avg_abs_wtd_err_regvar = nopvo_regvars_wtd_sum_abs_errs$sum_abs_err/nopvo_wtd_regvars_n
nopvo_regvars_wtd_avg_abs_err = melt(subset(nopvo_regvars_wtd_sum_abs_errs, select = c(bureau, avg_abs_wtd_err_regvar)))
#TODO: In plots.R, change to "regvars" from "regvar"


#weighted nonregister variables
nopvo_nonregvars_wtd_err_modes = sqldf("select nre.variable, nre.category, nre.bureau, nre.est, nre.err, nre.abs_err, nre.se from nopvo_nonregvars_wtd_err nre, nonreg_benchmark_modes rbm where nre.category = rbm.value and nre.variable = rbm.L1")
nopvo_wtd_nonregvars_n = sqldf("select count(distinct variable) from nopvo_nonregvars_wtd_err_modes group by bureau")
names(nopvo_wtd_nonregvars_n) = c("nvars")
nopvo_nonregvars_wtd_sum_abs_errs = sqldf("select *, sum(abs_err) as sum_abs_err from (select distinct * from nopvo_nonregvars_wtd_err_modes) group by bureau")
nopvo_nonregvars_wtd_sum_abs_errs$avg_abs_wtd_err_nonregvar = nopvo_nonregvars_wtd_sum_abs_errs$sum_abs_err/nopvo_wtd_nonregvars_n
nopvo_nonregvars_wtd_avg_abs_err = melt(subset(nopvo_nonregvars_wtd_sum_abs_errs, select = c(bureau, avg_abs_wtd_err_nonregvar)))
#TODO: In plots.R, change to "nonregvars" from "nonregvar"
