rm(list = ls())

source('04_results/99_substitute_underscores.R')
source('04_results/99_cp_text.R')

source('04_results/plt_funs.R')

library(data.table)
library(viridis)
# library(cstab)

# data --------------------------------------------------------------------

# reason names for processing
dr_as = readRDS('04_results/dr_fin.rds')

re_dp = readRDS('04_results/reasXcp.rds')
re_id = readRDS('04_results/reasXid.rds')

# sort by marginal
dr_as = names( sort(colSums(re_dp[,dr_as])) )
re_dp = cbind(re_dp[,dr_as], re_dp[,c('problemID', 'type', 'domain')])
re_id = re_id[,c('subject_id', dr_as)]

#
xp = read.csv('00_data/input.csv')
xp[is.na(xp)] = 0; colnames(xp)[1] = 'problemID'

# add decision problem
xp$dec_prob = paste0(apply(xp[,2:7], 1, cp_text), '\n',
                     apply(xp[,8:13], 1, cp_text, lot = 'B') )

# reasons data ------------------------------------------------------------

re_dp = merge(re_dp, xp[,c('problemID', 'dec_prob')])
re_dp[,dr_as] = re_dp[,dr_as]/86 # into proportions

# order decision problems
# re_dp = re_dp[order(re_dp$domain, re_dp$type, re_dp$prob_d), ]

# pacmap reasons
pmr = read.csv('04_results/reasonsXproblems_pacmap.csv')
pmr = merge(pmr, re_dp[,c('dec_prob', 'type', 'domain', 'problemID')])

# 
pmr$shape = NA
pmr$shape[pmr$domain == 'gain' & pmr$type == 'risk_safe'] = 1 
pmr$shape[pmr$domain == 'gain' & pmr$type == 'risk_risk'] = 19
pmr$shape[pmr$domain == 'loss' & pmr$type == 'risk_safe'] = 2 
pmr$shape[pmr$domain == 'loss' & pmr$type == 'risk_risk'] = 17
pmr$shape[pmr$domain == 'mixed'] = 0

# CLUSTER
xx = as.matrix(pmr[,c('dim1', 'dim2')]) # matrix for clustering
# cStability(xx, 2:10, method = 'hierarchical', linkage = 'ward.D2')

for(i in 1:ncol(xx)) xx[,i] = scale(xx[,i])
xx_d <- dist(xx, method = "euclidean")
hc_ward <- hclust(xx_d, method = "ward.D2"); #plot(hc_ward)
n_cl = 3
pmr$pr_clust = cutree(hc_ward, k = n_cl); rm(xx, xx_d, hc_ward)
#
pmr$pr_clust = ifelse(pmr$pr_clust == 2, 2,
                      ifelse(pmr$pr_clust == 1, 3, 1))

# reorder
re_dp = merge(re_dp, pmr[,c('problemID', 'pr_clust')])
re_dp = re_dp[order(re_dp$pr_clust, re_dp$domain),]; re_dp$id2 = 1:20
clust_cols = inferno(n_cl, 1, .3, .85)[3:1]

# PROBLEM COLORS FOR THE 2D MAP
pmr$col = NA; 
for(i in 1:n_cl) pmr$col[pmr$pr_clust == i] = clust_cols[i]


# jitter the mixed points
pmr[pmr$domain=='mixed', c('dim1', 'dim2')] = pmr[pmr$domain=='mixed', c('dim1', 'dim2')] + runif(8, -.1, .1)


# ind data ----------------------------------------------------------------

# individual pacmap
# cluster individuals based on pacmap
pmi = read.csv('04_results/individualsXproblems_pacmap.csv')
xx = as.matrix(pmi[,c('dim1', 'dim2')]) # matrix for clustering
# cDistance(xx, 2:10, method = 'hierarchical', linkage = 'ward.D2')

for(i in 1:ncol(xx)) xx[,i] = scale(xx[,i])
xx_d <- dist(xx, method = "euclidean")
hc_ward <- hclust(xx_d, method = "ward.D2"); #plot(hc_ward)
n_cl_i = 3
pmi$i_clust = cutree(hc_ward, k = n_cl_i); rm(xx, xx_d, hc_ward)
#
pmi$i_clust = ifelse(pmi$i_clust == 1, 1,
                     ifelse(pmi$i_clust == 2, 3, 2))

# color clusters
id_cols = turbo(n_cl_i)
pmi$col = id_cols[pmi$i_clust]

#
re_id = merge(re_id, pmi[,c('subject_id', 'i_clust')])
re_id[,dr_as] = re_id[,dr_as]/20 # into proportions

re_id = re_id[order(re_id$i_clust), ]; re_id$subject_id2 = 1:86

dxr = gsub('_', ' ', dr_as)
dxr = gsub('number of ', '', dxr)

# pdf device --------------------------------------------------------------

cairo_pdf('05_figures/Fig05.pdf',
          width = (16/2.54),
          height = (12/2.54),
          pointsize = 6)

mat = matrix(1:6,
             nrow = 2, ncol = 3,
             byrow = T)

layout(mat,
       widths = c(13.8, 10.2, 1.2),
       heights = c(2, 1))

# reasons by individuals --------------------------------------------------

# cairo_pdf('05_figures/Fig05c.pdf',
#           width = (16/2.54),
#           height = (12/2.54),
#           pointsize = 6)

# plot margins
# par(mar = c(1, 2, 14, 1))
par(mar = c(1, 15, 12, 1))


# tile plot reason X prediction
tile_plts(t(re_id[,dr_as]),
          cols = mako(100, 1),
          xlab = '',
          ylab = '',
          axes = F,
          grid = F)

# REASONS NAMES
axis(2, 1:length(dr_as),
     dxr,
     las = 2, padj = .5)
# axis(2, at = 1:length(dr_as), labels = F)
axis(3, at = 1:86, labels = T, tick = F)
for(i in 1:n_cl_i) {
  
  axis(3, at = re_id$subject_id2[re_id$i_clust == i], 
       col = id_cols[i], 
       labels = F,
       col.ticks = id_cols[i])
  
}

mtext('Participant', line = 3,)

# dev.off()

# reasons by problems -----------------------------------------------------

# cairo_pdf('05_figures/Fig05a.pdf',
#           width = (16/2.54),
#           height = (12/2.54),
#           pointsize = 6)

# plot margins
# par(mar = c(1, 15, 14, 1))
par(mar = c(1, 2, 12, 1))

# tile plot reason X prediction
tile_plts(t(re_dp[,dr_as]),
          cols = mako(100, 1),
          xlab = '',
          ylab = '',
          axes = F,
          grid = F)

# REASONS NAMES
# axis(2, 1:length(dr_as), 
#      dxr,
#      las = 2, padj = .5)
axis(2, at = 1:length(dr_as), labels = F)

for(i in 1:n_cl) {
  axis(3, 
       at = re_dp$id2[re_dp$pr_clust == i],
       labels = re_dp$dec_prob[re_dp$pr_clust == i],
       col.axis = clust_cols[i],
       col = clust_cols[i],
       cex.axis = .8,
       las = 2)
}

# add legend
par(mar = c(10, 1, 25, 3))
legend_values = seq(0, 1, length.out = 100)  # Values for the gradient
image(z = t(matrix(legend_values, ncol = 1)),
      col = mako(100, 1), axes = FALSE)

# Add axis labels to the legend
axis(4, at = seq(0, 1, .2),
     labels = seq(0, 1, .2),
     tick = F, las = 2)

# dev.off()

# individuals 2d map --------------------------------------------------

par(mar = c(2, 15, 1, 1))

plot(pmi$dim1, pmi$dim2, type = 'p',
     xlab = '', ylab = '', pch = 19,
     col = pmi$col,
     cex = 2,
     xaxt = 'n', yaxt = 'n',
     bty = 'n',
     panel.first = {
       usr <- par("usr")
       rect(usr[1], usr[3], usr[2], usr[4], 
            col = rgb(.9, .9, .9, .5), border = NA)
     })

# problems 2d map ---------------------------------------------------------

# cairo_pdf('05_figures/Fig05b.pdf',
#           width = (16/2.54),
#           height = (10/2.54),
#           pointsize = 5)

par(mar = c(2, 2, 1, 1))

plot(pmr$dim1, pmr$dim2, type = 'p',
     pch = pmr$shape, col = pmr$col,
     xlab = '', ylab = '',
     cex  = 2,
     xaxt = 'n', yaxt = 'n',
     bty = 'n',
     panel.first = {
       usr <- par("usr")
       rect(usr[1], usr[3], usr[2], usr[4], 
            col = rgb(.9, .9, .9, .5), border = NA)
     })

# par(xpd = T)
legend(.65, .85,
       # 'topleft', 
       # inset = c(-0.25, .15),
       legend = c('Gains risk-safe', 'Gains risk-risk',
                  'Losses risk-safe', 'Losses risk-risk',
                  'Mixed risk-safe'),
       title= 'Problem type',
       pch = c(1, 19, 2, 17, 0),
       bty = 'n',
       cex = 1.5)

# par(xpd = F)
# dev.off()


# figure marks ------------------------------------------------------------

# fig marks
mtext("a", side = 3, 
      line = -8, adj = .13, 
      cex = 1.25, font = 2,
      outer = T)
#
mtext("b", side = 3, 
      line = -50, adj = .13, 
      cex = 1.25, font = 2,
      outer = T)

# fig marks
mtext("c", side = 3, 
      line = -8, adj = .97, 
      cex = 1.25, font = 2,
      outer = T)
#
mtext("d", side = 3, 
      line = -50, adj = .97, 
      cex = 1.25, font = 2,
      outer = T)

# save the figure ---------------------------------------------------------

dev.off()

# variability comp --------------------------------------------------------

round(var( unlist(re_dp[,dr_as]/86)), 3)
round(var( unlist(re_id[,dr_as]/20)), 3)
