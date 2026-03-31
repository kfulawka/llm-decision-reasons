rm(list = ls())

source('04_results/functions/prediction_funs.R')

TRESHOLD = 80
LLM_RES = 'conf_choice_dat_wide_qwen3-235b.rds'

# data --------------------------------------------------------------------

# reason names
dr_as = readRDS('00_decisionReasons/dr_names.rds')

# confidence assessments from qwen3-235b
daw = readRDS(paste0('02_llm_analyses/02_reports_extracted/', LLM_RES))
da = cbind(daw$choice[,1:2], daw$asmt)

# select only the DR that we have assessments for
da = da[, c('subject_id', 'problemID', dr_as)]

# choice problems
xp = read.csv('00_data/csv_dat/input.csv')
xp$headline = NULL; xp[is.na(xp)] = 0

colnames(xp)[1] = 'problemID'

# identify reasons --------------------------------------------------------

# APPLY TRESHOLD
daF = da
daF[,dr_as] = tr_fun(da[,dr_as], TRESHOLD)

# MARGINAL FREQUENCY
reasons_mar = sort( colSums(daF[,dr_as]), decreasing = F )

# reasons X choice problems -----------------------------------------------

# matrix with reasons identified per choice problem
re_dp = aggregate(. ~ problemID, 
                  data = daF[,-1],
                  FUN = function(x) sum(x))

write.csv(re_dp, '04_results/03a_reasons-x-problems.csv', row.names = F)

# add problem groupings (for figures and clustering)
re_dp = merge(re_dp, xp[,c('problemID', 'domain', 'type')])

saveRDS(re_dp, '04_results/rds_res/03a_reasons-x-problems.rds')

# reasons X individuals ---------------------------------------------------

# matrix with reasons identified per subject
re_id = aggregate(. ~ subject_id, 
                  data = daF[,-2],
                  FUN = function(x) sum(x))

write.csv(re_id, '04_results/03a_reasons-x-individuals.csv', row.names = F)
saveRDS(re_id, '04_results/rds_res/03a_reasons-x-individuals.rds')

# Fig04 -------------------------------------------------------------------

library(viridis)

# marginal distribution of identified reasons
reasons_mar_prop = reasons_mar / nrow(daF)
reasons_mar_prop = sort(reasons_mar_prop, decreasing = F)
#
outcome_only = c('minimum_outcome', 'maximum_outcome', 'mean_outcome',
                 'outcome_sensitivity', 'reference_point',
                 'regret', 'disappointment', 'zero_outcome_presence',
                 'zero_outcome_absence', 'sum_of_outcomes', 'large_outcome_range', 
                 'small_outcome_range', 'outcomes_better_than_average', 
                 'consequence_count', 'segregation', 'importance_sampling', 
                 'aspiration_level', 'loss_aversion')
#
prob_only = c('higher_maximum_probability', 'lower_maximum_probability', 
              'higher_minimum_probability', 'lower_minimum_probability', 
              'large_probability_range', 'small_probability_range',
              'sure_outcome_presence', 'sure_outcome_absence')
#
both = setdiff(dr_as, c(outcome_only, prob_only))

reas_cols = mako(3, 1, .1, .7)
reas_cols_v = rep(NA, length(dr_as))
names(reas_cols_v) = dr_as
reas_cols_v[outcome_only] = reas_cols[3]
reas_cols_v[prob_only] = reas_cols[2]
reas_cols_v[both] = reas_cols[1]

# THE FIGURE
cairo_pdf('05_figures/Fig04.pdf',
          width = (8/2.54),
          height = (9/2.54),
          pointsize = 7)

par(mar = c(2, 11, 0, 0))

bp = barplot(reasons_mar_prop, xaxt = 'n', yaxt = 'n',
             border = NA, 
             col = reas_cols_v[names(reasons_mar_prop)],
             xlim = c(0, .55),
             horiz = T)
axis(1, at = seq(0, .5, .1), 
     line = -1,
     labels = F
     )
axis(1, at = seq(0, .5, .1), 
     line = -1.5,
     cex.axis = .7,
     tick = F
)
title(xlab = 'Proportion of trials with the reason identified', 
      line = .5,
      cex.lab = .8)
axis(2, at = bp,
     # labels = NA
     labels = gsub('_', ' ', names(reasons_mar_prop)),
     las = 2,
     cex.axis = .7
)

#
legend(.22, 10,
       # 'topleft', 
       # inset = c(-0.25, .15),
       legend = c('Outcomes & probabilities',
                  'Probabilities only',
                  'Outcomes only'),
       title = 'Reason considers:',
       col = reas_cols,
       pch = 15,
       bty = 'n',
       cex = .8)


dev.off()

# marginal reason dist ~ session ------------------------------------------

# session info
d_text = readRDS("00_data/rds_dat/verbal_reports.rds")
d_text$report_l = nchar(d_text$response)

# 
dd = merge(d_text[,c('subject_id', 'problemID', 'session', 'report_l')], daF)

# REASONS DISTIRBUTION BY SESSION
reasons_session = aggregate(. ~ session,
                            data = dd[,-c(1:2, 4)],
                            FUN = sum)
#
reas_prop_s1 = unlist(reasons_session[1,dr_as])/sum(reasons_session[1,dr_as])
reas_prop_s2 = unlist(reasons_session[2,dr_as])/sum(reasons_session[2,dr_as])

# scatter plot and correlation
plot(reas_prop_s1, reas_prop_s2)
abline(0, 1, col = 'red')
#
round( cor(reas_prop_s1, reas_prop_s2), 2)
round( summary(abs(reas_prop_s1-reas_prop_s2)), 4)
round( sd(abs(reas_prop_s1-reas_prop_s2)), 4)

# # REASON COMPLEXITY AND IDENTIFICATION ----------------------------------

dec_resons_v = read.csv2('00_decisionReasons/decision_reasons.csv')
colnames(dec_resons_v) = c('name', 'description')
dec_resons_v$name = gsub(' ', '_', dec_resons_v$name)
#
reasons_mar = data.frame(freq = reasons_mar,
                         name = names(reasons_mar))

dec_resons_v = merge(dec_resons_v, reasons_mar, by = 'name')
dec_resons_v$n_char = nchar(dec_resons_v$description)

plot(dec_resons_v$n_char, dec_resons_v$freq,
     xlab = 'no of characters in reason description',
     ylab = 'frequency of identification',
     main = paste('r =', round(cor(dec_resons_v$n_char,
                                   dec_resons_v$freq,
                                   method = 's'),
                               2))
)
