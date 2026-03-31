rm(list = ls())
library(data.table)
library(future.apply)

source('04_results/functions/prediction_funs.R')
# reason names
dr_as = readRDS('00_decisionReasons/dr_names.rds')

# LOOP OVER LLMS ----------------------------------------------------------

llms = list.files('02_llm_analyses/02_reports_raw_dat', full.names = F)

for(llm in llms) {
  
  # data --------------------------------------------------------------------
  
  dw = readRDS(paste0('02_llm_analyses/02_reports_extracted/',
                      'conf_choice_dat_wide_', llm, '.rds'))
  
  # choice data
  choice = dw$choice
  
  # filter decision reasons
  reason_p = dw$reason[,dr_as]
  asmt = dw$asmt[,dr_as]
  
  # 
  sub = unique(choice[,'subject_id'])
  
  # generate alignments ----------------------------------------------------
  
  # for each threshold level
  tr_lvls = seq(0, 100, 10)
  
  plan(multisession)
  XP = future_lapply(tr_lvls, function(t) {
    
    # print(t)
    
    X = lapply(sub, function(s) {
      
      # generate predictions ------------------------------------------------
      
      # assessment data for subject s
      as = asmt[choice[,'subject_id'] == s, ]
      
      # threshold apply
      asT = tr_fun(as, tr = t)
      
      # majority pred
      mP = majority_pred(asT, reason_p)
      
      # filter assessments based on threshold
      asF = as * asT
      
      # max pred
      maxP = majority_pred(maxA_fun(asF), reason_p)
      
      # weighted pred
      wP = weighted_pred(w_fun(asF), reason_p)
      
      # gather the linear predictors
      LP = cbind(mP, maxP, wP)
      
      colnames(LP) = paste0(colnames(LP), '_', t)
      
      # evaluate predictions ----------------------------------------------------
      
      # change LP into predictions
      Y_hat = choice_pred(LP)
      
      # observed choice
      y = choice[choice[,'subject_id'] == s, 'y']
      
      acc = apply(Y_hat, 2, function(y_hat) {
        
        yp = y_hat == y
        
        # substitute lack of pred 0 with .5
        # print(paste0('NO PRED # = ', length(yp[y_hat == 0])))
        yp[y_hat == 0] = .5
        
        return(mean(yp))
        
      })
      
      return(list(LP = data.frame(LP),
                  acc = acc))
      
    })
    
    # data with trial-wise predictions
    XY = lapply(X, function(x) x$LP)
    XY = data.frame( rbindlist(XY) )
    
    # evaluated predictions
    ep = t(sapply(X, function(x) x$acc))
    
    #
    return(list(XY = XY, ep = ep))
    
  })
  plan(sequential)
  
  # extract evaluated accuracies
  xp_acc = lapply(XP, function(x) x$ep)
  xp_acc = do.call(cbind, xp_acc)
  
  saveRDS(xp_acc, paste0('04_results/rds_res/02b_id-mean-alignment_', llm, '.rds'))
  
  # identified reasons stats ------------------------------------------------
  
  tr_pr_nr = sapply(tr_lvls, function(t) {
    
    # apply treshold
    xx = tr_fun(asmt, t)
    
    # no of reasons per trial
    y = rowSums(xx)
    
    # 
    r = c(me = median(y), q1 = quantile(y, .25), q3 = quantile(y, .75),
          min = min(y), max = max(y), m = mean(y), sd = sd(y), 
          prop_zero = mean(y == 0))
    
  });
  colnames(tr_pr_nr) = tr_lvls
  print(llm)
  print(
    rbind(round(tr_pr_nr, 3),
          round(colMeans(xp_acc[,grep('mP', colnames(xp_acc))]), 3)
          ))
  
}