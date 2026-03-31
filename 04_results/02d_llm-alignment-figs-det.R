rm(list = ls())

library(ggplot2)
library(patchwork)
library(viridis)

# data --------------------------------------------------------------------

# treshold values to evaluate at
Trs = seq(0, 100, 10)

# llm names 
llms = list.files('02_llm_analyses/02_reports_raw_dat', full.names = F)

# read and process data
apn = lapply(llms, function(llm) {
  
  # data in long format
  dd = readRDS(paste0('02_llm_analyses/02_reports_extracted/',
                      'conf_choice_dat_long_', llm, '.rds'))
  
  # alignment
  dd$alignment = dd$reason_choice == dd$y
  dd$llm_output = NULL
  
  # stats
  ap = lapply(Trs, function(t) {
    
    ds = dd[dd$assessment >= t, ]
    
    # basic stats
    a = data.frame(alignment = mean(ds$alignment),
                   li = mean(ds$alignment) - sd(ds$alignment)/sqrt(nrow(ds)),
                   ui = mean(ds$alignment) + sd(ds$alignment)/sqrt(nrow(ds)),
                   n = nrow(unique(ds[,c('subject_id', 'problemID')]))
    )
    
    # av no. of reasons / trial
    ar = aggregate(reason ~ problemID + subject_id,
                   data = ds,
                   FUN = function(x) length(unique(x)))
    #
    a$reas.m = mean(ar$reason)
    a[,c( 'reas.mn', 'reas.q1', 'reas.me', 'reas.q3', 'reas.mx')] = quantile(ar$reason)
    
    a$Trs = t
    
    return(a)
  })
  ap = do.call(rbind, ap)
  ap$n = ap$n/1720
  ap$mod = llm
  
  return(ap)
  
})
apn = do.call(rbind, apn)


# figures -----------------------------------------------------------------

# alignment
plt_alignment = ggplot(data = apn,
                       mapping = aes(x = factor(Trs),
                                     group = mod,
                                     fill = mod,
                                     color = mod)) +
  # geom_vline(xintercept = c(9, 10), lty = 3) +
  geom_line(mapping = aes(y = alignment)) +
  geom_ribbon(mapping = aes(ymin = li,
                            ymax = ui),
              alpha = .5,
              lwd = 0) +
  xlab('LLM confidence threshold') +
  scale_y_continuous('choice-reason alignment',
                     breaks = seq(0, 1, .1),
                     # limits = c(.4, 1)
  ) +
  scale_color_manual('LLM', values = turbo(4, 1, 0, .85)) +
  scale_fill_manual('LLM', values = turbo(4, 1, 0, .85)) +
  theme_light() +
  theme(legend.position = 'right') +
  coord_cartesian(ylim = c(.4, 1))

plt_nTrials = ggplot(data = apn,
                     mapping = aes(x = factor(Trs),
                                   group = mod,
                                   fill = mod,
                                   color = mod)) +
  # geom_vline(xintercept = c(9, 10), lty = 3) +
  geom_line(mapping = aes(y = n),
            lty = 1, 
            lwd = 1,
            alpha = .5,
            show.legend = F) +
  xlab('LLM confidence threshold') +
  scale_y_continuous('proportion of choice trialas',
                     breaks = seq(0, 1, .1)
                     # limits = c(.4, 1)
  ) +
  scale_color_manual('LLM', values = turbo(4, 1, 0, .85)) +
  scale_fill_manual('LLM', values = turbo(4, 1, 0, .85)) +
  theme_light() +
  theme(legend.position = 'none') +
  coord_cartesian(ylim = c(.4, 1))

#
plt_nReasons = ggplot(data = apn,
                      mapping = aes(x = factor(Trs),
                                    group = mod,
                                    fill = mod,
                                    color = mod))  +
  # geom_vline(xintercept = c(9, 10), lty = 3) +
  geom_line(mapping = aes(y = reas.me),
            lwd = 1,
            alpha = .7,
            show.legend = F) +
  geom_ribbon(mapping = aes(ymin = reas.q1,
                            ymax = reas.q3),
              alpha = .3,
              lwd = 0,
              show.legend = F) +
  xlab('LLM confidence threshold') +
  scale_y_continuous('# reasons / report',
                     # transform = 'log2',
                     breaks = seq(1, 30, 2)) +
  scale_color_manual('LLM', values = turbo(4, 1, 0, .85)) +
  scale_fill_manual('LLM', values = turbo(4, 1, 0, .85)) +
  theme_light() +
  theme(legend.position = 'none') +
  coord_cartesian(ylim = c(1, 30)) 

# panels together
fig3a_alt = wrap_plots(plotlist = list(plt_alignment, 
                                       plt_nTrials, 
                                       plt_nReasons,
                                       guide_area()),
                       ncol = 4, 
                       widths = c(3,3,3,1),
                       axis_titles = 'collect_x',
                       guides = 'collect') +
  plot_annotation(tag_levels = 'a')
# 
ggsave('05_figures/Fig03-SI-rev.pdf', 
       width = 16,
       height = 4,
       units = 'cm',
       scale = 2)