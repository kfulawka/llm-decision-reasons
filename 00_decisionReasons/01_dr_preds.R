rm(list = ls())


# decision reasons functions  ---------------------------------------------

source('00_decisionReasons/decision_reasons_funs.R')
dr_names = ls()

dec_resons_funs = lapply(dr_names, get)
names(dec_resons_funs) = dr_names

# clean up
rm(list = setdiff(ls(), c('dec_resons_funs', 'dr_names')) )

# wrapper function to run the deicsion reasons on relevant lot pars
source('00_decisionReasons/dr_execute.R')

# choice problems  --------------------------------------------------------

xp = read.csv('00_data/input.csv')
xp$headline = NULL; xp$domain = NULL; xp$type = NULL
colnames(xp)[1] = 'id'
xp[is.na(xp)] = 0

# colnames with outcomes and probs
xps = lapply(c(aX = 'aX', aP = 'aP', bX = 'bX', bP = 'bP'),
             function(x) grep(x, colnames(xp), value = T))

# probabilities to 0--1 range
xp[,c(xps$aP, xps$bP)] = xp[,c(xps$aP, xps$bP)] / 100

xp = as.matrix(xp)

# reasons' preferences ----------------------------------------------------

# for each decision reason
for(i in dr_names) {
  
  pr = sapply(1:nrow(xp), function(j) {

    decision_reason_choice(xA = xp[j, xps$aX], pA = xp[j, xps$aP],
                           xB = xp[j, xps$bX], pB = xp[j, xps$bP],
                           reason = dec_resons_funs[[i]])

  })
  
  # add chocies to the decision problem set
  xp = cbind(xp, pr)
  
  # set column name
  colnames(xp)[ncol(xp)] = i

}

# into file
write.csv(xp, file = '00_decisionReasons/dr_fun_valid.csv',
          row.names = F)

saveRDS(data.frame(xp[,-(2:13)]), '00_decisionReasons/dr_co.rds')