rm(list = ls())

library(ggplot2)
library(data.table)

# replication proportions -------------------------------------------------

# contains choice proportions from the recent replication
# Ruggeri, K., Alí, S., Berge, M.L. et al. 
# Replicating patterns of prospect theory for decision under risk. 
# Nat Hum Behav 4, 622–633 (2020). 
# https://doi.org/10.1038/s41562-020-0886-x
d_rep = data.frame( fread('01_descr_a/rep_props.csv') )
d_rep$rep_id = d_rep$Item; d_rep$Item = NULL

# our problems
xp = read.csv('00_data/input.csv')
xp$headline = NULL
colnames(xp)[1] = 'problemID'

# add problem id referring to the replication
xp$rep_id = NA
xp$rep_id[1:10] = 1:10
xp$rep_id[11:16]= 12:17

# 
xp = merge(xp, d_rep[c('prop', 'rep_id')], by = 'rep_id',
           all.x = T)
colnames(xp)[ncol(xp)] = 'rep_prop'

# choices -----------------------------------------------------------------

d = readRDS("00_data/rds_dat/md.rds")

da = aggregate(y ~ problemID,
               data = d,
               FUN = mean)
colnames(da)[2] = 'obs_prop'
da = merge(da, xp)

# 
plot(da$obs_prop, da$rep_prop, xlim = 0:1, ylim = 0:1)
abline(h = .5, v = .5, col = 'red')
cor.test(da$obs_prop, da$rep_prop)
round( summary(abs(da$obs_prop-da$rep_prop)), 2) 
round( sd(abs(da$obs_prop-da$rep_prop), na.rm = T), 2)