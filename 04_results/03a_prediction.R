rm(list = ls())
library(data.table)
library(future.apply)

source('04_results/prediction_funs.R')

# data --------------------------------------------------------------------

dw = readRDS('02_llms_hpc/02_verbal_reports/conf_choice_dat_wide.rds')

# reason names for processing
dr_as = readRDS('04_results/dr_fin.rds')

choice = dw$choice

# filter decision reasons
reason_p = dw$reason[,dr_as]
asmt = dw$asmt[,dr_as]

# 
sub = unique(choice[,'subject_id'])

# generate predictions ----------------------------------------------------

# for each threshold level
tr_lvls = seq(0, 100, 10)

plan(multisession)
XP = future_lapply(tr_lvls, function(t) {
  
  print(t)
  
  X = lapply(sub, function(s) {
    
    # generate predictions ------------------------------------------------
    
    # assessment data for subject s
    as = asmt[choice[,'subject_id'] == s, ]
    
    # threshold apply
    asT = tr_fun(as, tr = t)
    
    # majority pred
    mP = majority_pred(asT, reason_p)
    
    # filter assessments based on treshold
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

saveRDS(xp_acc, '04_results/id_accs.rds')

# extract trial-wise predictions
LP = lapply(XP, function(x) x$XY)
LP = do.call(cbind, LP)

#
saveRDS(LP, '04_results/LP_trial-lvl.rds')

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
round(tr_pr_nr, 2)