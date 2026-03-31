rm(list = ls())
library(data.table)
library(viridis)


# LOOP OVER LLMS ----------------------------------------------------------

llms = list.files('02_llm_analyses/02_reports_raw_dat', full.names = F)

for(llm in llms) {
  
  # data --------------------------------------------------------------------
  
  dd = readRDS(paste0('02_llm_analyses/02_reports_extracted/',
                      'conf_choice_dat_long_', llm, '.rds'))
  
  # aggregate and order
  da = aggregate(assessment ~ reason + correct, 
                 FUN = mean,
                 na.rm = T,
                 dd)
  # into wide
  da = dcast(data.table(da), reason ~ correct,
             value.var = 'assessment')
  da = data.frame(da)
  
  da = da[order(da$C, da$X, decreasing = T), ]
  
  # ordered reasons
  reasons = as.character(da$reason)
  names(reasons) = reasons
  
  dd$reason = factor(dd$reason,
                     levels = reasons,
                     ordered = T)
  
  # tables for plotting -----------------------------------------------------
  
  plt_tabs = lapply(reasons, function(x) {
    
    y = dd[dd$reason == x, c('correct', 'assessment')]
    
  })
  
  #
  saveRDS(plt_tabs, paste0('04_results/rds_res/02a_assesment_tabs_', llm, '.rds'))
  
}