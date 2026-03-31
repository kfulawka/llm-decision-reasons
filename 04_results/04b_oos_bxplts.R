rm(list = ls())

library(ggplot2)
library(data.table)


# data --------------------------------------------------------------------

# PT RESULTS
ispt_la = readRDS("03_cpt/posterior/ispt_la.rds")
ispt = ispt_la$ind_ba_loo$ba
spt92 = readRDS('03_cpt/spt92.rds')

# REASONS RESULTS
oos_res_file = '04a_llm_oos_results-TR80-SPLT80-N5000.rds'
oos_results = readRDS(paste0('04_results/rds_res/', oos_res_file))

# accuracies
oos_accuarcy = lapply(oos_results, function(x) {
  do.call(rbind, lapply(x, function(y) y$accuracy))
})

# perplexity
oos_perplexity = lapply(oos_results, function(x) {
  do.call(rbind, lapply(x, function(y) y$effective_reasons[,'perplexity'])) #n_non_zero
})
#
perplexity = data.frame(t(sapply(oos_perplexity, 
                                 function(x) c(perp_m = mean(x), perp_sd = sd(x)))
))

cond_levels = c('raw', 'marginal', 'subject_id', 'i_clust',
                'pr_class', 'pr_clust', 'iCls_prCls', 'problemID',
                'iCls_prID', 'IPT', 'TK92')
cond_labels = c('Uniform', 'Marginal', 'Participant', 'Participant\ncluster',
                'Problem\nclass', 'Problem\ncluster', 'PartCl\nPrbCl',
                'Problem', 'Problem x\nparticipant cluster', 
                'Estimated', 'TK 1992')

# perplexity dat prep -----------------------------------------------------

#
perplexity$variable = rownames(perplexity)
perplexity[,c('set')] = sapply(strsplit(perplexity$variable, '-'), '[', 1)
perplexity[,c('conditional')] = sapply(strsplit(perplexity$variable, '-'), '[', 2)
perplexity$variable = NULL

perplexity$conditional = factor(perplexity$conditional,
                                levels = cond_levels, 
                                labels = cond_labels,
                                ordered = T)

#
perplexity = perplexity[perplexity$conditional != 'PartCl\nPrbCl', ]
perplexity$set[perplexity$conditional == 'Uniform'] = 'None'

perplexity$set = factor(perplexity$set, 
                        levels = c('LLM', 'PREFERENCE', 'PREF_NLLM', 'None'),
                        labels = c('Verbal', 'Formal', 'Not LLM', 'None'),
                        ordered = T)
perplexity = perplexity[perplexity$set != 'Not LLM',]

# accuracy dat prep -------------------------------------------------------

xx = data.frame(sapply(oos_accuarcy, colMeans))
# COMBINE WITH PT
xx = cbind(xx, PREFERENCE.IPT = ispt)
xx = cbind(xx, PREFERENCE.TK92 = spt92)
#
xx$subject_id = 1:86
xl = data.frame( melt(data.table(xx),
                      id.vars = 'subject_id',
                      value.name = 'accuracy',
                      variable.factor = F)
)
xl[,c('set')] = sapply(strsplit(xl$variable, '\\.'), '[', 1)
xl[,c('conditional')] = sapply(strsplit(xl$variable, '\\.'), '[', 2)
xl$variable = NULL

xl$conditional = factor(xl$conditional,
                        levels = cond_levels, 
                        labels = cond_labels,
                        ordered = T)
#
xl = xl[xl$conditional != 'PartCl\nPrbCl', ]
xl$set[xl$conditional == 'Uniform'] = 'None'

xl$set = factor(xl$set, 
                levels = c('LLM', 'PREFERENCE', 'PREF_NLLM', 'None'),
                labels = c('Verbal', 'Formal', 'Not LLM', 'None'),
                ordered = T)
xl = xl[xl$set != 'Not LLM',]

xl$meta_set = NA
xl$meta_set[xl$conditional %in% c('Uniform', 'Marginal')] = 'Baseline profiles'
xl$meta_set[xl$conditional %in% c('Estimated', 'TK 1992')] = 'Prospect theory'
xl$meta_set[is.na(xl$meta_set)] = 'Conditional profiles'

# labels ------------------------------------------------------------------

# mean acc
xa = aggregate(accuracy ~ set + conditional + meta_set,
               FUN = function(x) c(m = mean(x), 
                                   q = quantile(x, .25)),
               data = xl)
xa = do.call(data.frame, xa)
colnames(xa)[4:5] = c('accuracy', 'q1')
# 
xa = merge(xa, perplexity, all.x = T)
xa$l1 = xa$q1 - .02
xa$l2 = xa$q1 - .045

xa$l1[xa$conditional=='Uniform'] = .645
xa$l2[xa$conditional=='Uniform'] = .62

# LABELS
xa$lab1 = paste0(round(xa$accuracy, 2))
xa$lab2 = paste0('(', round(xa$perp_m, 1), ')')
xa$lab2[grepl('(NA)', xa$lab2)] = ''

# # uncomment to view the exact accuracy and perplexity values
# xa_view = xa[c('set','conditional','accuracy','perp_m')]
# xa_view[c('accuracy','perp_m')] = round(xa_view[c('accuracy','perp_m')],3)
# xa_view

# figure ------------------------------------------------------------------

#
oos_plt = ggplot(xl,
                 mapping = aes(x = conditional,
                               y = accuracy,
                               fill = set,
                               col = set)) +
  # geom_point(position = position_jitterdodge(.1),
  #            alpha = .2,
  #            shape = 16) +
  geom_boxplot(alpha = .65,
               col = NA,
               varwidth = T) +
  geom_text(data  = xa,
            mapping = aes(label = lab1,
                          y = l1),
            position = position_dodge2(.75),
            size = 2) +
  geom_text(data  = xa,
            mapping = aes(label = lab2,
                          y = l2),
            position = position_dodge2(.75),
            size = 1.9) +
  geom_point(data = xa,
             position = position_dodge2(.75),
             size = 1) +
  facet_grid(cols = vars(meta_set),
             scales = 'free_x',
             space = 'free_x') +
  scale_y_continuous('Accuracy',
                     breaks = seq(0, 1, .1),
                     minor_breaks = seq(0, 1, .01)) +
  scale_fill_manual('Reason set',
                    values = viridis::mako(3, 0, .6, 
                                           direction = -1,
                                           alpha = .7)
                    ) +
  scale_color_manual('Reason set',
                     values = viridis::mako(3, 0, .6, 
                                            direction = -1,
                                            alpha = 1)
                     ) +
  theme_bw() +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 10, face = 'bold'),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        panel.grid.major.y = element_line(colour = "grey", linewidth = .2),
        panel.grid.major.x = element_line(linewidth = .2),
        legend.position = 'right') +
  coord_cartesian(ylim = c(.45, .86))
oos_plt

ggsave('05_figures/Fig06.pdf',
       units = 'cm',
       width = 16,
       height = 5,
       device = 'pdf',
       scale = 1.2
)