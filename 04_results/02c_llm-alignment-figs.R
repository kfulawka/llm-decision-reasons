# source('04_results/03a_prediction.R')

rm(list = ls())
library(data.table)
library(viridis)

TRESHOLD = 80

# reason names
reasons = readRDS('00_decisionReasons/dr_names.rds')


# descriptive stats  ------------------------------------------------------

ds = readRDS('02_llm_analyses/02_reports_extracted/conf_choice_dat_long_qwen3-235b.rds')

# reasons above threshold 50
ds$tr50 = ifelse(ds$assessment > 50, 1, 0)

#
prop.table(table(ds$tr50, ds$correct),2)


# LOOP OVER LLMS ----------------------------------------------------------

# llm names
llms = list.files('02_llm_analyses/02_reports_raw_dat', full.names = F)

for(llm in llms) {
  
  # data --------------------------------------------------------------------
  
  # assessment tabs for 3A
  plt_tabs = readRDS(paste0('04_results/rds_res/02a_assesment_tabs_', llm, '.rds'))
  
  dd = readRDS(paste0('04_results/rds_res/02b_id-mean-alignment_', llm, '.rds'))
  
  # thresholds
  tt = unique( sub('.*_', '', colnames(dd) ) )
  nt = length(tt)
  
  # methods
  mp = unique( sub('_.*', '', colnames(dd) ) )
  
  # method names
  mp_n = c('Treshold \u2014 majority',
           'Max conf \u2014 majority',
           'Conf weighted majority')
  names(mp_n) = mp
  
  #
  mp_cols = viridis::viridis(3)
  names(mp_cols) = mp
  
  print(llm)
  print(round(apply(dd[,grepl('mP_', colnames(dd))], 2, mean), 3))
  
  # boxplots per threshold, majority prediction
  Fig03b = dd[, grepl('mP', colnames(dd)) ]

  # 03A funs ----------------------------------------------------------------
  
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
      # whisklty = 1, whisklwd = .5
    )
    abline(v = 50, col = 'black', lty = 2, lwd = 1)
    abline(v = c(0, 100), col = 'black', lty = 2, lwd = 1)
    
    axis(2, at = 1.5, 
         labels = replace_second_whitespace(reas_labs[reason]), 
         las = 2)
  }
  
  # reason names cleaned up a bit
  reas_labs = as.character(gsub('_', ' ', names(plt_tabs)))
  reas_labs = gsub('minimum', 'min', reas_labs)
  reas_labs = gsub('maximum', 'max', reas_labs)
  reas_labs = gsub('number of ', '', reas_labs)
  reas_labs = gsub('probability', 'prob', reas_labs)
  # reas_labs = gsub('number', 'no', reas_labs)
  
  names(reas_labs) = names(plt_tabs)
  
  
  # panel setup -------------------------------------------------------------
  # layout matrix
  layout_mat = matrix(c(1:16, 49, 52,
                        17:32, 50, 52,
                        33:48, 51, 52),
                      ncol = 3,
                      byrow = F)
  
  hh = c(rep(1, 16), 2, 4.5)
  
  # 03A FIG -----------------------------------------------------------------
  
  # THE FIGURE
  file_name = ifelse(grepl('235b', llm), 
                     '05_figures/Fig03.pdf',
                     paste0('05_figures/Fig03_', llm, '.pdf'))
  cairo_pdf(file_name,
            width = (16/2.54),
            height = (15/2.54),
            pointsize = 9)
  
  # plot layout
  layout(layout_mat,
         heights = hh,
         widths = c(1, 1, 1))
  
  # REASONS
  par(mar = c(0, 8.5, .5, 1))
  lapply(names(plt_tabs), function(r) {
    
    plt(y = plt_tabs[[r]],
        reason = r,
        # bx_cols = mako(2, 1, .5, .7)[2:1]
        bx_cols = c('lightgrey', mako(2, 1, .5, .7)[2:1])
    )
    
  })
  
  # add empty panel
  plot(0:1, 0:1, axes = F, type = 'n',
       ylab = '', xlab = '')
  #
  legend('topleft',
         inset = c(-.7, .65),
         xpd = NA,
         title = 'Choice-reason:',
         title.adj = 0.5,
         legend = c('Aligned', 'Misaligned', 'NA'),
         col = c(mako(2, 1, .5, .7), 'lightgrey'),
         box.lwd = .5,
         title.font = 1,
         pch = 15
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
  
  # dev.off()
  
  # 03 B --------------------------------------------------------------------
  
  nt = 11; tt = c(0, seq(10, 100, 10))
  
  par(mar = c(4, 8.5, .5, 1))
  
  plot(0,
       type = 'n',
       xaxt = 'n',
       yaxt = 'n',
       ylim = c(.35, 1),
       xlim = c(.7, 11.3),
       ylab = '',
       xlab = 'Reason in verbal report?')
  title(ylab = 'Choice-reason\nalignment', line = 2)
  
  polygon(x = c(2.5, 9.5, 9.5, 2.5, 2.5),
          y = c(.32, .32, 1.03, 1.03, .32),
          col = rgb(.5, .5, .5, .2),
          border = NA)
  
  boxplot(Fig03b,
          at = 1:11,
          staplewex = 0,
          outline = F,
          varwidth = F,
          col = c( rep(mako(1, 1, .5), TRESHOLD/10),
                   'white',
                   rep(mako(1, 1, .5), 10 - TRESHOLD/10)),
          pars = list(whisklty = 1),
          border = mako(1, 1, .5),
          medcol = c(rep('white', TRESHOLD/10),
                     mako(1, 1, .5),
                     rep('white', 10 - TRESHOLD/10)),
          xaxt = 'n',
          yaxt = 'n',
          add = T)
  
  # x axis
  axis(1, at = 1:nt, labels = F, line = 0, tick = T)
  axis(1, at = 1:nt, labels = tt, line = -.5, tick = F)
  axis(1, at = c(1, 6, 11), labels = c('NO', "don't know", 'YES'),
       line = .75,
       tick = F)
  
  # y axis
  axis(2, at = seq(.4, 1, .1), line = -.3, tick = F)
  axis(2, at = seq(.4, 1, .1), labels = F, tick = T)
  
  # guessing line
  abline(h = .5, lty = 3)
  
  # fig marks
  mtext("a", side = 3, 
        line = -1.5, adj = .005, 
        cex = 1.25, font = 2,
        outer = T)
  #
  mtext("b", side = 3, 
        line = -49, adj = .005, 
        cex = 1.25, font = 2,
        outer = T)
  
  dev.off()
  
}