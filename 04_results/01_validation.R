library(data.table)
library(viridis)

# validation --------------------------------------------------------------

rm(list = ls())

# plotting functions
source('04_results/plt_funs.R')

# read all results
valid_res_f = list.files(full.names = T,
                         path = paste0('02_llms_hpc/01_valid/extracted'))
# select only those with _eval
valid_res_f = valid_res_f[grepl('_eval', valid_res_f)]

# model names
valid_res_n = gsub('02_llms_hpc/01_valid/extracted/|_eval.rds', '', valid_res_f) 

# read the results into a list
valid_res = lapply(valid_res_f, readRDS); names(valid_res) = valid_res_n

# remove llama 3.1-8b
# valid_res$`llama31-8b` = NULL

#
valid_agr = sapply(valid_res, mean, na.rm = T)

# order by best average perf
valid_agr = valid_agr[order(valid_agr)]

#
valid_res = valid_res[names(valid_agr)]

# FULL FIGURE -------------------------------------------------------------

names(valid_res)[which(names(valid_res) == 'olmo2-32b')] = 'olmo-2-32b'
names(valid_res)[which(names(valid_res) == 'qwen25-32b')] = 'qwen-2.5-32b'
names(valid_res)[which(names(valid_res) == 'mistral31-24b')] = 'mistral-3.1-24b'
names(valid_res)[which(names(valid_res) == 'phi4')] = 'phi-4'
names(valid_res)[which(names(valid_res) == 'gpt4om')] = 'gpt-4o-mini'
names(valid_res)[which(names(valid_res) == 'gpt4o')] = 'gpt-4o'
names(valid_res)[which(names(valid_res) == 'llama31-8b')] = 'llama-3.1-8b'
names(valid_res)[which(names(valid_res) == 'llama33-70b')] = 'llama-3.3-70b'


# THE FIGURE
cairo_pdf('05_figures/Fig02.pdf',
          width = (16/2.54),
          height = (6/2.54),
          pointsize = 5)

# set the layout
layout(matrix(1:8,
              ncol = 8, nrow = 1,
              byrow = T),
       widths = c(2.2, rep(1, 7)))

for(i in names(valid_res)) {

  d_comp = valid_res[[i]]
  
  colnames(d_comp) = gsub('number_of_', '', colnames(d_comp))

  if(i %in% names(valid_res)[1])  { par(mar = c(2, 15.5, 3, .5))
    } else { par(mar = c(2, 1, 3, .5)) }

  #
  mean_p = round(mean(d_comp, na.rm = T), 3);
  # sd_p = round(sd(colMeans(d_comp)), 2)

  # tile plot reason X prediction
  tile_plts(t(d_comp),
            cols = mako(2, 1, .5, .7)[2:1],
            xlab = '',
            ylab = '',
            axes = F,
            main = paste0(i, '\nM = ', mean_p*100, '%'))

  title(xlab = 'Problem',
        line = .5)
  
  # axis(1, 1:nrow(d_comp),
  #      lwd = .5,
  #      las = 1, padj = .5)

  if(i %in% names(valid_res)[1]) {

    axis(2, 1:ncol(d_comp),
         gsub("_", ' ', colnames(d_comp)),
         lwd = .5,
         las = 2, padj = .5)

  }

}

# # horizontal legend
# par(mar = c(0, 20, 0, 20))
# 
# # add legend
# plot(0:1, xlim = 0:1, ylim = 0:1,
#      type = 'n', bty = 'n', axes = F, xlab = '', ylab = '')
# legend(.5, 1,
#        pch = 15,
#        bty = 'n',
#        title = 'Choice',
#        title.adj = 0,
#        legend = c('Incorrect', 'Correct'),
#        col = cividis(2, .5),
#        cex = 1,
#        horiz = T)

dev.off()