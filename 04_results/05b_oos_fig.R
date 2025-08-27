library(viridis)

# Fig06 -------------------------------------------------------------------

# ispt = readRDS('03_cpt/spt_oos.rds')
# ispt = rowMeans(ispt, na.rm = T)
ispt_la = readRDS("03_cpt/posterior/ispt_la.rds")
ispt = ispt_la$ind_ba_loo$ba
spt92 = readRDS('03_cpt/spt92.rds')
pt = cbind(ispt, spt92)

# get individual means for the 80/20 split
acc_list = readRDS(paste0('04_results/oos_accuracy_t80.rds'))
oos_80 = sapply(acc_list, rowMeans)

#
Fig06 = cbind(oos_80[,c(1:2, 4, 3, 7, 6, 5)], pt)

# colors
box_cols = c(rgb(.7,.7,.7,.75),
             mako(1, .75, 0, .0),
             mako(2, .75, .2, .2),
             mako(3, .75, .4, .4),
             mako(ncol(pt), .75, .8, .8))

# THE FIGURE
cairo_pdf('05_figures/Fig06.pdf',
          width = (16/2.54),
          height = (5/2.54),
          pointsize = 7)

par(mar = c(3, 3, 3, 1))
plot(0, type = 'n',
     ylim = c(.3, 1),
     xlim = c(.6, ncol(Fig06)+.6),
     xlab = '',
     ylab = '',
     xaxt = 'n',
     yaxt = 'n'
)
title(ylab = 'Out-of-sample accuracy', line = 2)
# abline(h = seq(.2, 1, .1), lwd = c(.5, 1), col = 'black')

xac = c(1:2, 3:7 + .2, 8:9 + .4)

boxplot(Fig06,
        at = xac,
        boxwex = .7,
        staplewex = 0,
        outline = F,
        varwidth = F,
        notch = F,
        col = box_cols,
        pars = list(whisklty = 1),
        border = box_cols,
        medcol = 'white',
        xaxt = 'n',
        yaxt = 'n',
        add = T)

points(xac, colMeans(Fig06), col = 'black', pch = 23)

axis(1, at = xac, labels = F)
axis(1, at = xac[1:7], tick = F, line = -1,
     labels = c('Uniform', 'Marginal', 'Participant',
                'Participant\ncluster', 'Problem', 'Problem\ncluster',
                'Problem\nclass'),
     padj = 1)

axis(1, tail(xac,2), c('Estimated', 'TK 1992'), tick = F, line = -1,
     padj = 1)
axis(3, 1.4, 'Baseline profiles', 
     tick = F, line = 0, font = 2)
axis(3, 5.2, 'Conditional profiles', 
     tick = F, line = 0, font = 2)
axis(3, 8.9, 'Prospect theory', 
     tick = F, line = 0, font = 2)

# y axis
axis(2, at = seq(.4, 1, .1), line = -.3, tick = F)
axis(2, at = seq(.4, 1, .1), labels = F, tick = T)

#
abline(h = .5, lty = 3)
abline(v = c(2.6, 7.8))

dev.off()

#
round(colMeans(Fig06), 3)