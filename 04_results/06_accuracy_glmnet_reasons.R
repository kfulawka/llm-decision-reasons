rm(list = ls())

set.seed(13)

library(glmnet)
library(arrow)
library(future.apply)

# choice and reason data -------------------------------------------------------------

# Load the embeddings matrix (X) and the metadata
dw = readRDS(paste0('02_llm_analyses/02_reports_extracted/',
                    'conf_choice_dat_wide_qwen3-235b.rds'))

# data prep ---------------------------------------------------------------

# recode choice to 1 and 0 (from 1 and -1)
y = ifelse(dw$choice[,'y']==1, 1, 0)

XX = list(X1 = as.matrix(dw$asmt), # raw LLM confidence scores
         X2 = as.matrix(ifelse(dw$asmt >= 80, 1, 0)) # reasons identified at T=80
         )

# ridge regression --------------------------------------------------------

n_reps <- 100

# loop over two trial-level reason representations
res = lapply(XX, function(X) {
  
  # future plan
  plan('multisession', workers = 8)
  
  oos_embd_results = future_sapply(1:n_reps, function(r) {
    
    foldid <- ave(
      dw$choice[,'subject_id'],
      dw$choice[,'subject_id'],
      FUN = function(x) sample(rep(1:5, length.out = length(x)))
    )
    
    cv_fit <- cv.glmnet(
      x = X,
      y = y,
      alpha = 0,
      # nfolds = 5,
      foldid = foldid,
      family = "binomial",
      type.measure = "class"
    )
    
    # accuracy
    acc <- 1 - min(cv_fit$cvm)
    
    #
    return(acc)
    
  }, future.seed=TRUE)
  
  #
  plan('sequential')
  
  return(oos_embd_results)
  
})

# print accuracy
lapply(res, function(x) { round(c(mean(x), sd(x)), 4) })