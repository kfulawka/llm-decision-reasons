rm(list = ls())

library(stringr)

# clean data --------------------------------------------------------------

d_text = readRDS("00_data/rds_dat/verbal_reports.rds")

xp = read.csv('00_data/csv_dat/input.csv'); colnames(xp)[1] = 'problemID'

d_text = merge(d_text, xp[,c('problemID', 'domain', 'type')])

# no of words
d_text$word_no <- str_count(str_squish(d_text$response), "\\S+")

# no of characters
d_text$char_no = nchar(d_text$response)

summary(d_text$word_no); quantile(d_text$word_no, c(.1, .2))
summary(d_text$char_no); quantile(d_text$char_no, c(.1, .2))