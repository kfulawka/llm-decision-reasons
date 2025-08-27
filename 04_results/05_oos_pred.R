rm(list = ls())

source('04_results/prediction_funs.R')

library(rsample)
library(future.apply)
library(viridis)

# data --------------------------------------------------------------------

dy = readRDS("00_data/rds_dat/md.rds")
# select only the y and indicators
dy = dy[, c('subject_id', 'problemID', 'y')]
# colnames(dy)[1] = 'subject_id'

# reason names for processing
dr_as = readRDS('04_results/dr_fin.rds')

# confidence assessments
daw = readRDS('02_llms_hpc/02_verbal_reports/conf_choice_dat_wide.rds')
da = cbind(daw$choice[,1:2], daw$asmt)

# select only the DR that we have assessments for
da = da[, c('subject_id', 'problemID', dr_as)]

# choice problems
xp = read.csv('00_data/input.csv')
xp$headline = NULL; xp[is.na(xp)] = 0

colnames(xp)[1] = 'problemID'

# add extra groupings
xp$dom_typ = paste(xp$domain, xp$type, sep = '_')

# cluster problems based on pacmap
pmr = read.csv('04_results/reasonsXproblems_pacmap.csv')
xx = as.matrix(pmr[,c('dim1', 'dim2')]) # matrix for clustering
#
# pmr = readRDS('04_results/reasXcp.rds')
# xx = as.matrix(pmr[,dr_as]) # matrix for clustering
#
for(i in 1:ncol(xx)) xx[,i] = scale(xx[,i])
xx_d <- dist(xx, method = "euclidean")
hc_ward <- hclust(xx_d, method = "ward.D2")
pmr$pr_clust = cutree(hc_ward, k = 3);
# plot(pmr[,1:2], col = pmr[,4], pch = 19)
xp = merge(xp, pmr[,c('problemID', 'pr_clust')],
           by = 'problemID')

# cluster individuals based on pacmap
pmi = read.csv('04_results/individualsXproblems_pacmap.csv')
xx = as.matrix(pmi[,c('dim1', 'dim2')]) # matrix for clustering
for(i in 1:2) xx[,i] = scale(xx[,i])
xx_d <- dist(xx, method = "euclidean")
hc_ward <- hclust(xx_d, method = "ward.D2")
pmi$i_clust = cutree(hc_ward, k = 3); rm(xx, xx_d, hc_ward)

# reasons' preferences ----------------------------------------------------

# get the results of function validation
dr_f = read.csv('00_decisionReasons/dr_fun_valid.csv')
dr_f = dr_f[c('id', dr_as)]
colnames(dr_f)[1] = 'problemID'

# identify reasons --------------------------------------------------------

TRESHOLD = 80

# APPLY TRESHOLD
daF = da
daF[,dr_as] = tr_fun(da[,dr_as], TRESHOLD)
daF = daF[,c('subject_id', 'problemID', dr_as)]

# add extra groupings
daF = merge(daF, xp[,c('problemID', 'dom_typ', 'pr_clust')],
            by = 'problemID')

#
daF = merge(daF, pmi[,c('subject_id', 'i_clust')])

# functions ---------------------------------------------------------------

split_by_strata = function(df, strata, P = .8) {
  
  # strata is a list of length N
  # where each element is a data.frame column with a grouping var
  
  # Function to split within each (subject, problem)
  split_within_group <- function(df, P) {
    n <- nrow(df)
    train_indices <- sample(seq_len(n), size = floor(P * n), replace = FALSE)
    list(train = df[train_indices, ], test = df[-train_indices, ])
  }
  
  # Split data by (subject, problem)
  split_result <- lapply(split(df, df[,strata], drop = TRUE), 
                         split_within_group, P = P)
  
  # Combine train and test sets
  train_set <- do.call(rbind, lapply(split_result, `[[`, "train"))
  test_set <- do.call(rbind, lapply(split_result, `[[`, "test"))
  
  return(list(train = train_set, test = test_set))
  
}

# function for running the oos 
oos_reas_pred = function(daF, 
                         dy,
                         split_var,
                         reasons_var = split_var,
                         P = .8) {

  # set up variables
  daF$split_var = daF[,split_var]
  # 
  if(!reasons_var %in% c('marginal', 'raw') ) {
    daF$reasons_var = daF[,reasons_var]
  } else {
    daF$reasons_var = 1 
  }

  # Perform split by strata (EITHER SUB OR PROBLEMID)
  split = split_by_strata(daF, strata = 'split_var', P = P)
  
  # Extract train and test sets
  train <- split$train
  test <- split$test
  
  # get the reasons distribution from training data
  # CONDITINONED ON REASONS_VAR !!!!
  re_d = aggregate(. ~ reasons_var, 
                   data = train[,c('reasons_var', dr_as)],
                   FUN = sum)

  # set all to 1 (ie 'raw' reasons pred)
  if(reasons_var == 'raw') re_d[,dr_as] = 1

  # for storing the predictions
  test$pr = NA

  # get the predictions
  for(i in 1:nrow(test)) {
    
    # get the reasons dist
    re_di = re_d[re_d$reasons_var == test$reasons_var[i], dr_as]
    # normalize
    re_di = re_di/sum(re_di)
    
    # prediction
    test$pr[i] = sum(dr_f[dr_f$problemID == test$problemID[i], dr_as] * re_di)
    
  }
  
  # map to 0-1 predicted choice
  test$choice_pr = ifelse(test$pr > 0, 1, 0)

  # combine with choice data
  test = merge(test, dy)
  test$cor_pred = as.numeric(test$choice_pr == test$y)
  #
  # substitute incorrect pred resulting from indifference with .5
  test$cor_pred[test$pr == 0] = .5
  
  # accuracy --- overall
  # acc = mean(test$cor_pred)
  # return(acc)
  
  # accuracy --- per individual
  acc_i = aggregate(cor_pred ~ subject_id,
                    data = test,
                    FUN = mean)


  return(acc_i[,2])
  
}

# results: sub or problem ID ----------------------------------------------

# number of oos runs per split
n = 1e3

# splits
splits = .8
split_vars = c('raw', 'marginal', 
                'i_clust', 'subject_id',
               'dom_typ', 'pr_clust', 'problemID')

# for the results
acc_list = list()

# loop over splits
plan('multisession')
for(v in split_vars) {
  for(i in 1:length(splits)) {
    
    nn = paste(v, sep = '_s', splits[i])
    acc_list[[nn]] = future_replicate(n,
                                      oos_reas_pred(daF, 
                                                    dy, 
                                                    split_var = 'subject_id', #
                                                    reasons_var = v,
                                                    P = splits[i]))
  }
}
plan('sequential')

# save results ------------------------------------------------------------
round(sapply(acc_list, mean, na.rm = T), 3)
saveRDS(acc_list, paste0('04_results/oos_accuracy_t', TRESHOLD, '.rds'))