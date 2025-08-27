rm(list = ls())

library(ggplot2)

# clean data --------------------------------------------------------------

d_text = readRDS("00_data/rds_dat/verbal_reports.rds")

xp = read.csv('00_data/input.csv'); colnames(xp)[1] = 'problemID'

d_text = merge(d_text, xp[,c('problemID', 'domain', 'type')])

# no of spaces
d_text$space_no = sapply(d_text$response, function(x) length(gregexpr(" ", x)[[1]]) )

# no of characters
d_text$char_no = nchar(d_text$response)

summary(d_text$space_no-1); quantile(d_text$space_no-1, c(.1, .2))
summary(d_text$char_no); quantile(d_text$char_no, c(.1, .2))