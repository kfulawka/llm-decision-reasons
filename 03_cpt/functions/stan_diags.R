stan_diags = function(stanfit,
                      N,
                      ind_p,
                      group_p,
                      pairs_p,
                      write_path = 'analyses/modeling/diagnostics') {
  
  
  # function to print plots to specified directory
  print_jpg = function(x, 
                        w = 20,
                        h = 10, 
                        pe = 'diag') {
    
    # open jpg
    jpeg(paste(write_path, pe, sep = '_'), 
         width = w, 
         height = h, 
         units = 'cm', 
         res = 200,
         quality = 100)
    
    print(x) # print in the plot
    
    dev.off() # close jpg
    
  }
  
  # diagnostics -------------------------------------------------------------
  
  # posterior draws as array
  post_draws = as.array(stanfit)
  
  # remove log-liks to save memory
  post_draws = post_draws[,,-grep('log_lik', dimnames(post_draws)[[3]])]
  
  # ind pars names as returned by stan ( e.g., theta[1] )
  ind_pars = paste(rep(ind_p, each = N), '[', 1:N, ']', sep = '') 

  # parameters for diagnostic
  p_diag = c(group_p, 'lp__')
  
  # nuts parameters
  nuts_pars = nuts_params(stanfit)
  
  # log posterior
  lp = log_posterior(stanfit)
  
  # # parallel coordinates plot
  # color_scheme_set("brightblue")
  # print_jpg(x = mcmc_parcoord(post_draws,
  #                             pars = group_p,
  #                             np = nuts_pars),
  #           pe = 'paral_p.jpg')
  
  # pairs plots
  lapply(names(pairs_p), function(x) {
    
    print_jpg(x = mcmc_pairs(post_draws,
                             np = nuts_pars,
                             pars = pairs_p[[x]],
                             off_diag_args = list(size = 0.75)),
              h = 20,
              w = 40,
              pe = paste0(x, '_pairs.jpg'))
    
  })

  
  # trace plots
  color_scheme_set("mix-brightblue-gray")
  print_jpg(
    x = mcmc_trace(post_draws,
                   pars = p_diag,
                   np = nuts_pars),
    pe = 'chains.jpg')
  
  
  # nuts accptance
  color_scheme_set("brightblue")
  print_jpg(x = mcmc_nuts_divergence(nuts_pars, lp),
            pe = 'nuts_ac.jpg')

  # nuts energy
  print_jpg(x = mcmc_nuts_energy(nuts_pars),
            pe = 'nuts_energy.jpg')
  
  # r-hats
  rhats = rhat(stanfit)[c(p_diag, ind_pars)]
  print_jpg(x =  mcmc_rhat(rhats),
            pe = 'r_hats.jpg')
  
  # effective sample sizes
  ratios_nss = neff_ratio(stanfit)[c(p_diag, ind_pars)]
  print_jpg(x = mcmc_neff(ratios_nss,
                          size = 2),
            pe = 'ess.jpg')
  

  # autocorrelations
  print_jpg(x = mcmc_acf(post_draws,
                         pars = p_diag,
                         lags = 20),
            h = 20,
            w = 40,
            pe = 'acf.jpg')
  
}