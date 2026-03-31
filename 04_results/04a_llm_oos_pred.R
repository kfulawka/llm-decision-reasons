rm(list = ls())

set.seed(13)

source('04_results/functions/oos_funs.R')

library(rsample)
library(future.apply)
library(data.table)

# LLM-REASON IDENTIFICATION TRESHOLD
LLM_TRESHOLD = 80
LLM_RESULTS = 'conf_choice_dat_long_qwen3-235b.rds'

# choice data -------------------------------------------------------------

dy = readRDS("00_data/rds_dat/md.rds")
# select only the y and indicators
choices = dy[, c('subject_id', 'problemID', 'y')]

# reason preferences ------------------------------------------------------

# reason names
reasons_names = readRDS('00_decisionReasons/dr_names.rds')

# matrix with problem x reason preferences
reasons_pref = read.csv('00_decisionReasons/dr_fun_valid.csv')
reasons_pref = reasons_pref[c('id', reasons_names)]
colnames(reasons_pref)[1] = 'problemID'

# identified reasons -----------------------------------------------------

# clusters of problems and individuals 
hClusts = readRDS('04_results/rds_res/03c_llm_clusters.rds')
# add domain+type problem class
hClusts$problems$pr_class = paste(hClusts$problems$domain,
                                  hClusts$problems$type, sep = '_')

# LLM ANNOTATIONS AND REASON ALIGNMENTS
llm_annot_reasons = readRDS(paste0('02_llm_analyses/02_reports_extracted/', LLM_RESULTS))
llm_annot_reasons = llm_annot_reasons[llm_annot_reasons$reason %in% reasons_names,]

# IDENTIFY REASONS ABOVE TRESHOLD
llm_annot_reasons$reason_llm = ifelse(llm_annot_reasons$assessment >= LLM_TRESHOLD, 1, 0)
  
# IDENTIFY CHOICE-MATCHING REASONS
llm_annot_reasons$reason_pref = ifelse(llm_annot_reasons$correct == 'C', 1, 0)

# IDENTIFY CHOICE-MATCHING BUT LLM-MISALIGNED REASONS
llm_annot_reasons$reason_pref_nllm = ifelse(llm_annot_reasons$reason_pref & !(llm_annot_reasons$reason_llm), 
                                            1, 0)

# gather llm and pref wide-data for oos pred
llm_pref_reasons = lapply(list(LLM = 'reason_llm', 
                               PREFERENCE = 'reason_pref',
                               PREF_NLLM = 'reason_pref_nllm'),
                          function(x) {
                            dd = dcast(data.table(llm_annot_reasons),
                                       formula = 'subject_id + problemID ~ reason',
                                       value.var = x)
                            dd = data.frame(dd)
                            # add individual and problem clusters
                            dd = merge(dd, hClusts$individuals)
                            dd = merge(dd, hClusts$problems[c('problemID', 'pr_clust', 'pr_class')])
                            dd = dd[order(dd$subject_id, dd$problemID),]
                            rownames(dd) = NULL
                            #
                            dd$iCls_prID = paste0(dd$i_clust, '-', dd$problemID)
                            dd$iCls_prCls = paste0(dd$i_clust, '-', dd$pr_clust)
                            #
                            return(dd)
                          })

# RUN OOS ANALYSES --------------------------------------------------------

# number of oos runs per split
n_oos = 5e3

# OOS TRAINING DATA SPLIT
SPLIT = .8

# LIST FOR RESULTS
oos_results = list()

# future plan
plan('multisession')
for(xn in names(llm_pref_reasons)) {
  
  # SET VARIABLES TO BE USED AS CONDITIONALS 
  if(xn == 'PREFERENCE') {
    cond_vars = c('raw', 'marginal', 
                  'subject_id', 'pr_class', 'problemID')
  } else if (xn == 'PREF_NLLM'){ 
    cond_vars = c('marginal', 'subject_id', 'pr_class', 
                  'problemID')
  } else {
    cond_vars = c('marginal', 'i_clust', 'subject_id', 
                  'pr_class', 'pr_clust', 'problemID', 
                  'iCls_prID', 'iCls_prCls')
  }
  
  for(v in cond_vars) {
    #
    vn = paste0(xn, '-', v)
    print(paste0('REASON SET: ', xn,
                 ' --CONDITIONAL: ', v,
                 ' --SPLIT: ', SPLIT*100, '%'))
    
    oos_results[[vn]] = future_lapply(1:n_oos, function(i) {
      oos_reas_pred(reasons = llm_pref_reasons[[xn]], 
                    choices = choices,
                    reasons_pref = reasons_pref,
                    reasons_names = reasons_names,
                    oos_split_var = 'subject_id',
                    oos_conditional_var = v,
                    P = SPLIT,
                    equal_weight = F)
    }, future.seed=TRUE)
  
  }
}
plan('sequential')

# save results ------------------------------------------------------------

# # ACCURACY
# oos_accuarcy = lapply(oos_results, function(x) {
#   do.call(rbind, lapply(x, function(y) y$accuracy))
# })
# 
# # PERPLEXITY
# oos_perplexity = lapply(oos_results, function(x) {
#   do.call(rbind, lapply(x, function(y) y$effective_reasons[,'perplexity']))
#   })

# save
saveRDS(oos_results, paste0('04_results/rds_res/04a_llm_oos_results-TR',
                            LLM_TRESHOLD, '-SPLT', SPLIT*1e2, '-N', n_oos,
                            '.rds'))