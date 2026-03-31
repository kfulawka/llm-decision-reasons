rm(list = ls())

set.seed(13)

# library(rsample)
library(future.apply)
library(data.table)
library(brms)

# LLM-REASON IDENTIFICATION TRESHOLD
LLM_TRESHOLD = 80
LLM_RESULTS = 'conf_choice_dat_long_qwen3-235b.rds'

# reasons -----------------------------------------------------------------

# reason names
reasons_names = readRDS('00_decisionReasons/dr_names.rds')

# LLM ANNOTATIONS AND REASON ALIGNMENTS
llm_annot_reasons = readRDS(paste0('02_llm_analyses/02_reports_extracted/', LLM_RESULTS))
llm_annot_reasons = llm_annot_reasons[llm_annot_reasons$reason %in% reasons_names,]

#
llm_annot_reasons$llm_output = NULL

# IDENTIFY REASONS ABOVE TRESHOLD
llm_annot_reasons$reason_llm = ifelse(llm_annot_reasons$assessment >= LLM_TRESHOLD, 1, 0)
#
llm_annot_reasons$reason_pref = ifelse(llm_annot_reasons$correct=='C', 1, 0)

# FREQUENCY MATRICES
reasons_ids <- readRDS("04_results/rds_res/03a_reasons-x-individuals.rds")
reasons_ids = as.matrix(reasons_ids[,reasons_names])
reasons_problems <- readRDS("04_results/rds_res/03a_reasons-x-problems.rds")
reasons_problems = as.matrix(reasons_problems[,reasons_names])

# simple metrics ----------------------------------------------------------

# Calculate correlation matrices
cor_prob <- cor(t(reasons_problems))
cor_part <- cor(t(reasons_ids))

# Extract the average pairwise shared variance (ignoring the diagonal 1s)
round( summary(cor_prob[lower.tri(cor_prob)]), 2)
round( summary( cor_prob[lower.tri(cor_prob)]^2), 2)
#
round( summary( cor_part[lower.tri(cor_part)]), 2)
round( summary( cor_part[lower.tri(cor_part)]^2), 2)

# reason variability sources ----------------------------------------------

### FULL ANALYSIS WITH BAYESIAN MODEL

# formula to fit
bf_form = bf(reason_llm ~ 1 + (1|reason) + (1|subject_id) + (1|problemID) +
               (1|reason:subject_id) + (1|reason:problemID))

# estimate the model
m_reason_var = brm(bf_form,
                   data = llm_annot_reasons,
                   family = bernoulli(),
                   cores = 4,
                   chains = 4,
                   iter = 2e3,
                   warmup = 1e3,
                   thin = 2)

# save model
saveRDS(m_reason_var, '04_results/rds_res/03d_m_reason_var.rds')

# ll = loo(m_reason_var)

vc <- VarCorr(m_reason_var, summary = FALSE)

# extract variances per grouping factor
var_draws <- lapply(vc, function(x) {
  variance = unlist(x)^2  # sd → variance
})

# combine into matrix: rows = draws, cols = components
var_mat <- do.call(cbind, lapply(var_draws, as.vector))
colnames(var_mat) <- names(var_draws)

# add residual variance (logistic)
res_var <- pi^2 / 3

# total variance per draw
total_var <- rowSums(var_mat) + res_var

# compute shares
shares <- sweep(var_mat, 1, total_var, "/")

# 'summarize '%' 
t(apply(shares, 2, function(x) {
  round( quantile(x, c(.5, .025, .975)), 3 )
}))


# variances
t(sapply(vc, function(x) {
  
  variance = unlist(x)^2
  round(quantile(variance, c(.5, .025, .975)), 3)
  
}))