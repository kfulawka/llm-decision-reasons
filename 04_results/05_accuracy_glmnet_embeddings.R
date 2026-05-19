rm(list = ls())

set.seed(13)

library(glmnet)
library(arrow)
library(future.apply)

# choice data -------------------------------------------------------------

dy = readRDS("00_data/rds_dat/md.rds")
# select only the y and indicators
choices = dy[, c('subject_id', 'problemID', 'y')]


# embedding --------------------------------------------------------------

# Load the embeddings matrix (X) and the metadata
embeddings_df = read_parquet("02_llm_analyses/03_reports_embeddings/verbal_reports_embeddings.parquet.gzip")
embeddings_df = embeddings_df[order(embeddings_df$subject_id, embeddings_df$problemID),]
embeddings_df$response = NULL

# merge wich choice data
embeddings_df = merge(choices, embeddings_df)

# data prep ---------------------------------------------------------------

y = embeddings_df$y
X = as.matrix(embeddings_df[,paste0('emb_', 0:1023)])

# ridge regression --------------------------------------------------------

n_reps <- 100

# future plan
plan('multisession', workers = 8)

oos_embd_results = future_sapply(1:n_reps, function(r) {
  
  foldid <- ave(
    embeddings_df$subject_id,
    embeddings_df$subject_id,
    FUN = function(x) sample(rep(1:5, length.out = length(x)))
  )
  
  cv_fit <- cv.glmnet(
    x = X,
    y = y,
    alpha = 0,
    # nfolds = 5,
    foldid = foldid,
    family = "binomial",
    type.measure = "class"
    )
  
  # accuracy
  acc <- 1 - min(cv_fit$cvm)
  
  #
  return(acc)
  
}, future.seed=TRUE)

#
plan('sequential')

hist(oos_embd_results, breaks = 'scott')
mean(oos_embd_results); sd(oos_embd_results)