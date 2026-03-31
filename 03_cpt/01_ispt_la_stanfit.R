rm(list = ls(all.names = T))

library(rstan)
library(loo)
library(bayesplot)

# modeling data -----------------------------------------------------------

stan_dat = readRDS("00_data/rds_dat/stan_d.rds")
md = readRDS("00_data/rds_dat/md.rds")

# sampler settings --------------------------------------------------------

# sampler parameters
n_chains = 4
n_iter = 6e3
n_burn = 2e3
n_thin = 4
n_cores = n_chains

# parameters to monitor
plp_pars = c( 'alpha', 'beta', 'gam', 'phi', 'id_sig')
ind_pars = c('ialpha', 'ibeta', 'igam', 'iphi')

# compile stan model ------------------------------------------------------

trans = stanc(file = '03_cpt/stan_m/ispt_la.stan')
compiled = stan_model(stanc_ret = trans)

# estimate  ---------------------------------------------------------------

source('03_cpt/functions/stan_diags.R')
source('03_cpt/functions/binary_accuracy_loo.R')

# sample from posterior
stanfit = sampling(object = compiled,
                   data = stan_dat,
                   pars = c(ind_pars, plp_pars, 'log_lik'),
                   init = 0,
                   chains = n_chains,
                   iter = n_iter,
                   warmup = n_burn,
                   thin = n_thin,
                   cores = n_chains,
                   control = NULL)

# model performance -------------------------------------------------------

# approximate loo 
looE = rstan::loo(stanfit,
                   moment_match = F)

# loo balanced accuracy
ba = lapply(list(h = .5, opt ='optimal'), function(x) {
  
  binary_accuracy_loo(stanfit,
                      stan_dat$choice,
                      binary_cutoff = x)
  
})

# loo ind accuracy
ind_ba_loo = ID_binary_accuracy_loo(stanfit,
                                    y = stan_dat$choice,
                                    N = 86,
                                    ncp = 20)

# output ------------------------------------------------------------------

# summary table
fit_summary = summary(stanfit,
                      pars = c(plp_pars, 'lp__'))[[1]]
(fit_summary = round(fit_summary, 3))


# posterior samples
p_pars = extract(stanfit)[plp_pars]
i_pars = extract(stanfit)[ind_pars]

plp_diag = c(plp_pars[ plp_pars != 'id_sig' ],
             paste0('id_sig[', 1:4, ']'))

try(stan_diags(stanfit = stanfit,
               N = stan_dat$N,
               ind_p = ind_pars,
               group_p = plp_diag,
               pairs_p = list(p1 = plp_diag[1:4],
                              p2 = plp_diag[5:8]),
               write_path = '03_cpt/stan_diags/ispt'))

# sampling info
sampling_info = list(
  data = stan_dat,
  pars = c(plp_pars, ind_pars),
  chains = n_chains,
  iter = n_iter,
  warmup = n_burn,
  thin = n_thin,
  control = NULL,
  model = compiled
)

# list with results
ispt_la = list(pars = list(p_pars = p_pars,
                           i_pars = i_pars),
               fit_summary = fit_summary,
               looE = looE,
               balanced_acc = ba,
               ind_ba_loo = ind_ba_loo,
               sampling_info = sampling_info)

# rm(list = setdiff(ls(), 'ispt_la'))
saveRDS(ispt_la, '03_cpt/posterior/ispt_la.rds')