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

# shanon entropy
shanon_entropy = function(p,
                          log_base = 2,
                          perplexity = F) {
  #
  p = p[p > 0]
  H = -sum(p*log(p, log_base))
  if(perplexity) H = log_base^H
  #
  return(H)
}

# function for running the oos
oos_reas_pred = function(reasons,
                         choices,
                         reasons_pref,
                         reasons_names,
                         oos_split_var,
                         oos_conditional_var,
                         P = .8,
                         equal_weight = F) {

  # set up variables
  reasons$oos_split_var = reasons[,oos_split_var]
  #
  if(!oos_conditional_var %in% c('marginal', 'raw') ) {
    reasons$oos_conditional_var = reasons[,oos_conditional_var]
  } else {
    reasons$oos_conditional_var = 1
  }

  # Perform split by strata (EITHER SUB OR PROBLEMID)
  split = split_by_strata(reasons, strata = 'oos_split_var', P = P)

  # Extract train and test sets
  train = split$train
  test = split$test

  # get the reasons distribution from training data
  # CONDITINONED ON REASONS_VAR !!!!
  re_d = aggregate(. ~ oos_conditional_var,
                   data = train[,c('oos_conditional_var', reasons_names)],
                   FUN = sum)
  
  # matrix for perplexity and non-zero outcomes compute
  re_xx = as.matrix(re_d[,reasons_names])
  re_xx = re_xx / rowSums(re_xx)
  #
  effective_reasons = cbind(
    perplexity = apply(re_xx, 1, shanon_entropy, perplexity = T),
    n_non_zero = apply(re_xx, 1, function(x) sum(x > 0))
  )

  # set all to 1 (ie 'raw' reasons pred)
  if(oos_conditional_var == 'raw') re_d[,reasons_names] = 1

  # for storing the predictions
  test$pr = NA

  # get the predictions
  for(i in 1:nrow(test)) {

    # get the reasons dist
    re_di = as.numeric(re_d[re_d$oos_conditional_var == test$oos_conditional_var[i], reasons_names])
    # normalize
    re_di = re_di/sum(re_di)
    #
    # reason preference vector
    pref_vec = as.numeric(reasons_pref[reasons_pref$problemID == test$problemID[i], reasons_names])
    #
    # prediction
    if(!equal_weight) {
      # weighted by reason frequencies
      test$pr[i] = sum(pref_vec * re_di)
    } else {
      test$pr[i] = sum(pref_vec)/length(reasons_names)
    }
  }

  # map to 0-1 predicted choice
  test$choice_pr = ifelse(test$pr > 0, 1, 0)

  # combine with choice data
  test = merge(test, choices)
  test$cor_pred = as.numeric(test$choice_pr == test$y)
  #
  # substitute incorrect pred resulting from indifference with .5
  test$cor_pred[test$pr == 0] = .5

  # # accuracy --- overall
  # acc = mean(test$cor_pred)
  # return(acc)

  # accuracy --- per individual
  acc_i = aggregate(cor_pred ~ subject_id,
                    data = test,
                    FUN = mean)

  # return subject-ordered mean accuracy
  return(list(accuracy = acc_i[order(acc_i$subject_id), 2], 
              effective_reasons = effective_reasons))

}