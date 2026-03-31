rm(list = ls())

library(brms)
library(data.table)

# data --------------------------------------------------------------------

# REASONS RESULTS
oos_res_file = '04a_llm_oos_results-TR80-SPLT80-N5000.rds'
oos_results = readRDS(paste0('04_results/rds_res/', oos_res_file))

oos_accuarcy = lapply(oos_results, function(x) {
  do.call(rbind, lapply(x, function(y) y$accuracy))
})

xx = data.frame(sapply(oos_accuarcy, colMeans))

xx$subject_id = 1:86
xl = data.frame( melt(data.table(xx),
                      id.vars = 'subject_id',
                      value.name = 'accuracy',
                      variable.factor = F)
)
xl[,c('set')] = sapply(strsplit(xl$variable, '\\.'), '[', 1)
xl[,c('conditional')] = sapply(strsplit(xl$variable, '\\.'), '[', 2)
xl$variable = NULL

# EXCLUDE THE FORMAL-notVERBAL SET
xl = xl[xl$set != 'PREF_NLLM', ]

# brms models -------------------------------------------------------------

# conditionals to loop over
conds = c('marginal', 'subject_id', 'problemID', 'pr_class')
names(conds) = conds

# contrasts
xl$set = factor(xl$set)
contrasts(xl$set) = contr.sum(2)

mms = lapply(conds, function(p)  {
               
               dl = xl[xl$conditional == p ,]
               
               m = brm(bf(accuracy ~ set + (1|subject_id)),
                       data = dl,
                       chains = 5,
                       cores = 5,
                       warmup = 1e3,
                       iter = 3e3,
                       family = Beta(link = 'cauchit'),
                       thin = 5,
                       seed = 13)
               
             })

# save
saveRDS(mms, '04_results/rds_res/04c_brms_mods.rds')

# results -----------------------------------------------------------------

# POSTERIOR DIFFERECNE ESTIMATEs
cx = sapply(mms, function(m) fixef(m, summary = F)[,2])

#
t(round(
  rbind(posterior_above_zero = apply(cx, 2, function(x) sum(x > 0)/length(x)),
        apply(cx, 2, quantile, p = c(.5, .025, .975))), 3
))