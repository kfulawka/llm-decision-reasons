split_by_strata = function(df, strata, P = .2) {
  
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