rm(list = ls())

source('04_results/prediction_funs.R')

treshold = 80

# data --------------------------------------------------------------------

# reason names for processing
dr_as = readRDS('04_results/dr_fin.rds')

# confidence assessments
# confidence assessments
daw = readRDS('02_llms_hpc/02_verbal_reports/conf_choice_dat_wide.rds')
da = cbind(daw$choice[,1:2], daw$asmt)

# select only the DR that we have assessments for
da = da[, c('subject_id', 'problemID', dr_as)]

# choice problems
xp = read.csv('00_data/input.csv')
xp$headline = NULL; xp[is.na(xp)] = 0

colnames(xp)[1] = 'problemID'

# identify reasons --------------------------------------------------------

# APPLY TRESHOLD
daF = da
daF[,dr_as] = tr_fun(da[,dr_as], treshold)

# MARGINAL FREQUENCY
reasons_mar = sort( colSums(daF[,dr_as]), decreasing = F )

# per trial dist
# par(mfrow = c(1,2))
# plot(table(rowSums(daF[,-(1:2)]))); boxplot(rowSums(daF[,-(1:2)]))
quantile(rowSums(daF[,-(1:2)]))
summary(rowSums(daF[,-(1:2)]))

# reasons X choice problems -----------------------------------------------

# matrix with reasons identified per choice problem
re_dp = aggregate(. ~ problemID, 
                  data = daF[,-1],
                  FUN = function(x) sum(x))

write.csv(re_dp, '04_results/reasonsXproblems.csv', row.names = F)

# add problem groupings (for figures and clustering)
re_dp = merge(re_dp, xp[,c('problemID', 'domain', 'type')])

saveRDS(re_dp, '04_results/reasXcp.rds')

# reasons X individuals ---------------------------------------------------

# matrix with reasons identified per subject
re_id = aggregate(. ~ subject_id, 
                  data = daF[,-2],
                  FUN = function(x) sum(x))

write.csv(re_id, '04_results/individualsXproblems.csv', row.names = F)
saveRDS(re_id, '04_results/reasXid.rds')

# Fig04 -------------------------------------------------------------------

library(viridis)

# marginal distribution of identified reasons
reasons_mar_prop = reasons_mar / nrow(daF)
reasons_mar_prop = sort(reasons_mar_prop, decreasing = F)
#
outcome_only = c('minimum_outcome', 'maximum_outcome', 'mean_outcome',
                 'outcome_sensitivity', 'outcome_sensitivity', 'reference_point',
                 'regret', 'disappointment', 'zero_outcome_presence',
                 'zero_outcome_absence', 'sure_outcome_presence', 'sure_outcome_absence',
                 'sum_of_outcomes', 'large_outcome_range', 'small_outcome_range',
                 'outcomes_better_than_average', 'consequence_count', 'segregation',
                 'importance_sampling', 'aspiration_level', 'loss_aversion')
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
          width = (16/2.54),
          height = (12/2.54),
          pointsize = 8)

par(mar = c(4, 12, 1, 0))

bp = barplot(reasons_mar_prop, xaxt = 'n', yaxt = 'n',
             border = NA, 
             col = reas_cols_v[names(reasons_mar_prop)],
             xlab = 'Proportion of trials',
             xlim = c(0, .55),
             horiz = T)
axis(1, at = seq(0, .5, .1))
axis(2, at = bp,
     # labels = NA
     labels = gsub('_', ' ', names(reasons_mar_prop)),
     las = 2,
     cex.axis = .8
     )

#
legend(.2, 25,
       # 'topleft', 
       # inset = c(-0.25, .15),
       legend = c('Outcomes & probabilities',
                  'Probabilities only',
                  'Outcomes only'),
       title = 'Reason considers:',
       col = reas_cols,
       pch = 15,
       bty = 'n',
       cex = 1)


dev.off()

# saveRDS(names(reasons_mar_prop[which(reasons_mar_prop > .3)]),
#         '04_results/dr_top.rds')


# marginal reason dist ~ session ------------------------------------------

# session info
d_text = readRDS("00_data/rds_dat/verbal_reports.rds")
d_text$report_l = nchar(d_text$response)

# 
dd = merge(d_text[,c('subject_id', 'problemID', 'session', 'report_l')], daF)

# REASONS DISTIRBUTION BY SESSION
reasons_session = aggregate(. ~ session,
                            data = dd[,-c(1:2, 4)],
                            FUN = mean)

# scatter plot and correlation
plot(unlist(reasons_session[1,-1]), 
     unlist(reasons_session[2,-1]))
abline(0, 1, col = 'red')
cor(unlist(reasons_session[1,-1]),
    unlist(reasons_session[2,-1]))

# frequency distribution
reasons_session_sum = aggregate(. ~ session,
                                data = dd[,-c(1:2)],
                                FUN = sum)
reasons_session_sum = as.matrix(reasons_session_sum[,-1])
chisq.test(reasons_session_sum[1,],
           reasons_session_sum[2,],
           correct = F,
           B = 1e5,
           simulate.p.value = T)

# VERBAL REPORT LENGTH AND NO OF REASONS
# dd$reasons_n = rowSums(dd[,dr_as])
#
# cr = sapply(1:86, function(i) cor(dd[dd$subject ==i, c('report_l', 'reasons_n')],
#                                   method = 's')[1,2])
# plot(dd$report_l, dd$reasons_n+1, log = 'xy');
# cor(dd$report_l, dd$reasons_n, method = 's')
# library(brms)
# dd$reasons_n_ln = log(dd$reasons_n+1)
# dd$report_l_z = scale(log(dd$report_l))
# mm = brm(reasons_n_ln ~ report_l_z + (report_l_z|subject_id),
#          data = dd,
#          chains = 4,
#          cores = 4,
#          warmup = 1e3,
#          thin = 2,
#          iter = 2e3)
# summary(mm)

# REASON COMPLEXITY AND IDENTIFICATION
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