rm(list = ls())
library(data.table)
library(viridis)

# data --------------------------------------------------------------------

dd = readRDS('02_llms_hpc/02_verbal_reports/conf_choice_dat_long.rds')

# aggregate and order
da = aggregate(assessment ~ reason + correct, 
               FUN = mean,
               na.rm = T,
               dd)
# into wide
da = dcast(data.table(da), reason ~ correct,
           value.var = 'assessment')
da = data.frame(da)

da = da[order(da$C, da$X, decreasing = T), ]

# ordered reasons
reasons = as.character(da$reason)
names(reasons) = reasons

dd$reason = factor(dd$reason,
                   levels = reasons,
                   ordered = T)

# no of reasons
n = length(reasons)

# no of cols
nc = 3

# tables for plotting -----------------------------------------------------

plt_tabs = lapply(reasons, function(x) {
  
  y = dd[dd$reason == x, c('correct', 'assessment')]
  
})

#
saveRDS(plt_tabs, '04_results/assesment_tabs.rds')

# single panel function ---------------------------------------------------

replace_second_whitespace <- function(text) {
  # Find positions of whitespaces
  space_positions <- gregexpr(" ", text)[[1]]
  
  # If there are at least two whitespaces, replace the second one with a newline
  if (length(space_positions) >= 2) {
    substr(text, space_positions[2], space_positions[2]) <- "\n"
  }
  
  return(text)
}

plt = function(y, 
               main = '', 
               reason = '',
               bx_cols = viridis(2, 1)) {
  
  boxplot(
    y$assessment[y$correct=='NA'],
    y$assessment[y$correct=='X'],
    y$assessment[y$correct=='C'],
    horizontal = T,
    staplewex = 0,
    outline = F,
    varwidth = F,
    col = bx_cols,
    xaxt = 'n',
    yaxt = 'n',
    ylab = '',
    xlab = '',
    axes = F,
    ylim = c(0, 100),
    pars = list(medcol = 'black'),
    border = rgb(1,1,1,0)
    )
  abline(v = 50, col = 'black', lty = 2, lwd = 1)
  abline(v = c(0, 100), col = 'black', lty = 2, lwd = 1)

  axis(2, at = 1.5, 
       labels = replace_second_whitespace(reas_labs[reason]), 
       las = 2)
}

# 03 A ------------------------------------------------------------------

# layout matrix
layout_mat = matrix(c(1:16, 49,
                      17:32, 50,
                      33:48, 51),
                    ncol = 3,
                    byrow = F)

# reason names cleaned up a bit
reas_labs = as.character(gsub('_', ' ', reasons))
reas_labs = gsub('minimum', 'min', reas_labs)
reas_labs = gsub('maximum', 'max', reas_labs)
reas_labs = gsub('number of ', '', reas_labs)
reas_labs = gsub('probability', 'prob', reas_labs)
# reas_labs = gsub('number', 'no', reas_labs)

names(reas_labs) = reasons

# THE FIGURE
cairo_pdf('05_figures/Fig03a_na.pdf',
          width = (16/2.54),
          height = (11/2.54),
          pointsize = 9)

# plot layout
layout(layout_mat,
       heights = c(rep(1, 16), 2.2),
       widths = c(1, 1, 1))

# REASONS
par(mar = c(0, 8.5, .5, 1))
lapply(reasons, function(r) {

  plt(y = plt_tabs[[r]],
      reason = r,
      # bx_cols = mako(2, 1, .5, .7)[2:1]
      bx_cols = c('lightgrey', mako(2, 1, .5, .7)[2:1])
      )

})

# add panel w/ legend
plot(0:1, 0:1, axes = F, type = 'n',
     ylab = '', xlab = '')

legend(0, 1, 
       title = 'Choice alignment',
       legend = c('Correct', 'Incorrect', 'NA'),
       col = c(mako(2, 1, .5, .7)[2:1], 'lightgrey'),
       lwd = 3,
       bty = 'n'
       )

#
par(mar = c(5, 8.5, 0, 1))
for(i in 1:3) {
  
  plot(0:1, 0:1, axes = F, type = 'n',
       ylab = '', 
       xlab = '',
       xlim = c(0, 100))
  title(xlab = 'Reason in verbal report?', line = 2.5)
  axis(1, at = c(0, 50, 100), 
       labels = c('NO', "don't know", 'YES'))
  
}

dev.off()

# pass to next stage ------------------------------------------------------

dr_fin = sort(as.character(reasons))
dr_fin = dr_fin[which(!dr_fin %in% c('lower_minimum_probability',
                                     'lower_minimum_outcome_probability',
                                     'lower_zero_outcome_probability'))]

saveRDS(dr_fin, '04_results/dr_fin.rds')