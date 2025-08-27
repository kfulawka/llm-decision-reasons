
rm(list = ls())

library(future.apply)
library(data.table)
library(stringi)

# data --------------------------------------------------------------------

d1 = read.csv('00_data/raw_data/results_s1.csv'); d1$session = 1
d2 = read.csv('00_data/raw_data/results_s2.csv'); d2$session = 2

dd = rbind(d1, d2)

# remove empty columns
dd = dd[,-(1:5)]

# set subject ids to 1:N
dd$subject_id = as.numeric(factor(dd$subject_id,
                                  levels = unique(dd$subject_id),
                                  ordered = T)) # preserves the original ordering of subjects

# presentation sides
dd$left_lottery = substr(dd$order, 11, 11)

# lotteries
xp = read.csv('00_data/input.csv')
xp$headline = NULL
colnames(xp)[1] = 'problemID'
xp[is.na(xp)] = 0

# colnames with outcomes and probs
X = grep('X', colnames(xp), value = T)
P = grep('P', colnames(xp), value = T)


# demographics ------------------------------------------------------------

d_demo = dd[dd$trial_type == 'survey' & dd$stimulus == '', ]

# Extract the fields using regular expressions
d_demo$sex = sub('.*"q_sex":"?([^",]*)"?,"q_age".*', '\\1', d_demo$response)
d_demo$education = sub('.*"q_education":"?([^"}]*)"?}.*', '\\1', d_demo$response)
d_demo$age = sub('.*"q_age":"?([^",]*)"?,"q_education".*', '\\1', d_demo$response)
d_demo$age = as.numeric(d_demo$age)
summary(d_demo$age); sd(d_demo$age)

# Optional: clean up encoding issues
d_demo$sex = gsub("\\\\u00e4", "ä", d_demo$sex)
d_demo$sex = gsub("\\\\u00f6", "ö", d_demo$sex)
d_demo$sex = gsub("\\\\u00fc", "ü", d_demo$sex)

# desc stats
table(d_demo$sex) / nrow(d_demo)

table(d_demo$education)

write.csv(d_demo, '00_data/demographics.csv', row.names = F)

# choices -----------------------------------------------------------------

# select relevant columns
d_choice = dd[dd$trial_type == 'rdm-llm', c('subject_id', 'problemID', 'left_lottery', 'session', 'choice')]

# add lotteries parameters
# this also drops the practice trial data
d_choice = merge(d_choice, xp, by = 'problemID')

# match choice to the lotteries
d_choice$y = d_choice$choice
d_choice$y[d_choice$left_lottery == 'b'] = ifelse(d_choice$choice[d_choice$left_lottery == 'b'] == 'A', 'B', 'A')
# table(d_choice$choice, d_choice$y, d_choice$left_lottery) # sanity check
d_choice$y = ifelse(d_choice$y == 'A', 1, 0)

# modeling data -----------------------------------------------------------

# modeling data
md = d_choice[,c('y', X, P, 'problemID', 'subject_id', 'left_lottery', 'domain', 'type', 'session')]
md = md[order(md$subject_id, md$problemID), ]

saveRDS(md, '00_data/rds_dat/md.rds')
write.csv(md, '00_data/choice_data.csv', row.names = F)

# data for stan
stan_dat = list(XA = md[,X[1:3]],
                XB = md[,X[4:6]],
                PA = md[,P[1:3]] / 1e2, # probabilities to 0-1 range
                PB = md[,P[4:6]] / 1e2,
                choice = md$y,
                sub = md$subject_id,
                XAs = sign(md[,X[1:3]]),
                XBs = sign(md[,X[4:6]]),
                mixed = ifelse(md$domain == 'mixed', 1, 0),
                N = max(md$subject_id),
                n = nrow(md),
                ses = md$session
                )

saveRDS(stan_dat, '00_data/rds_dat/stan_d.rds')

# text --------------------------------------------------------------------

source('00_data/00_functions/text_extract.R')

# select relevant rows
d_text = dd[dd$trial_type == 'survey-text', ]

plan(multisession)
d_text_l = future_lapply(unique(d_text$subject_id), function(s) {
  
  # print(s)
  
  x = d_text[d_text$subject_id == s, ]
  
  r = text_extract(x)
  
  return(r)
  
})
plan(sequential)

d_text = data.frame( rbindlist(d_text_l) )

# add info on left lottery
# also removes problemID = 999 (practice) from the d_text dataset
d_text = merge(d_text, d_choice[,c('subject_id', 'problemID', 'left_lottery')],
               by = c('subject_id', 'problemID'))

d_text$problemID = as.numeric(d_text$problemID)
d_text = d_text[order(d_text$subject_id, d_text$problemID), ]

saveRDS(d_text, '00_data/rds_dat/verbal_reports.rds')
write.csv(d_text, '00_data/verbal_reports.csv', row.names = F)

# decision contexts -------------------------------------------------------

source('00_data/00_functions/decision_context.R')

d_context = md[, c('subject_id', 'problemID')]
d_context$context = NA

# colnames in proper order for the decision_context function
dcc = c(paste0('aX', 1:3), paste0('aP', 1:3), 
        paste0('bX', 1:3), paste0('bP', 1:3))

for(i in 1:nrow(md)) {
  
  d_context$context[i] = decision_context(x = md[i, dcc], 
                                          dc = NULL,
                                          left_lottery = md$left_lottery[i])
  
}

# merge with ids
d_context = merge(d_context, d_text[,c('subject_id', 'problemID')],
                  by = c('subject_id', 'problemID'))

d_context = d_context[order(d_context$subject_id, d_context$problemID), ]

saveRDS(d_context, '00_data/rds_dat/id_decision_problems.rds')
write.csv(d_context, '00_data/id_decision_problems.csv', row.names = F)

# choice problems as text
xp_txt = sapply(1:20, function(i) {
  
  decision_context(x = xp[i, c(X[1:3], P[1:3], X[4:6], P[4:6]) ],
                   dc = NULL,
                   left_lottery = 'a')
  
})

# into data frame
xp_txt = data.frame( id = 1:20, choice_problem = xp_txt )

write.table(xp_txt, '00_data/choice_problems_txt.csv',
            row.names = F,
            sep = ',',
            quote = T)