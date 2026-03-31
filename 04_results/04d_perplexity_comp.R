rm(list = ls())

library(brms)
library(data.table)

# data --------------------------------------------------------------------

# REASONS RESULTS
oos_res_file = '04a_llm_oos_results-TR80-SPLT80-N5000.rds'
oos_results = readRDS(paste0('04_results/rds_res/', oos_res_file))

# PERPLEXITY
oos_perplexity = lapply(oos_results, function(x) {
  do.call(rbind, lapply(x, function(y) y$effective_reasons[,'perplexity']))
  })

# select results for comparison
oos_perplexity = oos_perplexity[!grepl('NLLM', names(oos_perplexity))]

# only results for both verbal and formal reason sets
oos_perplexity = oos_perplexity[!grepl('clust|raw|iCls', names(oos_perplexity))]

# oos means into long data ------------------------------------------------

oos_perp_m = data.frame(perplexity = unlist(lapply(oos_perplexity, colMeans)))

# set and conditional indicators
oos_perp_m$var = rownames(oos_perp_m)
oos_perp_m[,c('set', 'cond')] = t( sapply(strsplit(oos_perp_m$var, '-'), '[') )
oos_perp_m$var = 'NULL'
rownames(oos_perp_m) = NULL

# sets
setz = c('LLM', 'PREFERENCE')

# group conditionals
condz = c('marginal', 'subject_id', 'problemID', 'pr_class')
oos_perp_m$cond_g = 'NA'
for(i in condz) {
  oos_perp_m$cond_g[grepl(i, oos_perp_m$cond)] = i
}

#
aggregate(perplexity ~ set,
          FUN = mean,
          data = oos_perp_m)

# brms model --------------------------------------------------------------

oos_perp_m$set = factor(oos_perp_m$set)
contrasts(oos_perp_m$set) = contr.sum(2)/2

oos_perp_m$cond_g = factor(oos_perp_m$cond_g)
contrasts(oos_perp_m$cond_g) = contr.sum(nlevels(oos_perp_m$cond_g))

#
perp_m = brm(bf(perplexity ~ set * cond_g,
                sigma  ~ set,
                alpha  ~ set),
             data = oos_perp_m,
             family = skew_normal(),
             cores = 4,
             chains = 4,
             iter = 3e3,
             thin = 2,
             warmup = 1e3)

perp_m
plot(perp_m, ask = F)
pp_check(perp_m, ndraws = 1e2)

# results -----------------------------------------------------------------

# prediction grid
newdata = expand.grid(
  set = setz,
  cond_g = condz
)

# posterior predictions
epred = posterior_epred(perp_m, newdata = newdata)
#
colnames(epred) = apply(newdata, 1, function(x) {
  paste0(x["set"], "-", x["cond_g"])
})

# estimates
round( t(apply(epred, 2, quantile, p = c(.5, .025, .975))), 2)

# differences by conditional
t(sapply(condz, function(x) {
  
  nn = grep(x, colnames(epred), value = TRUE)
  
  diff = epred[, nn[1]] - epred[, nn[2]]
  
  round(c(p_below_zero = mean(diff < 0),
          quantile(diff, p = c(.5, .025, .975))), 3)
  
}))

# set differences
mar_epred_llm = rowMeans(epred[, grepl('LLM', colnames(epred))])
mar_epred_pref = rowMeans(epred[, grepl('PREFERENCE', colnames(epred))])

diff = mar_epred_llm - mar_epred_pref

round(
  c(
    p_below_zero = mean(diff < 0),
    quantile(diff, p = c(.5, .025, .975))
  ),
  3)
