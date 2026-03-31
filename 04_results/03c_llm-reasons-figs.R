rm(list = ls())

source('04_results/functions/99_cp_text.R')
source('04_results/functions/plt_funs.R')

library(data.table)
library(viridis)

N_IND_CL = 4
N_PR_CL = 3

# data --------------------------------------------------------------------

# reason names for processing
dr_as = readRDS('00_decisionReasons/dr_names.rds')

# frequency matrices
re_dp = readRDS('04_results/rds_res/03a_reasons-x-problems.rds')
re_id = readRDS('04_results/rds_res/03a_reasons-x-individuals.rds')

# sort by marginal
dr_as = names( sort(colSums(re_dp[,dr_as])) )
re_dp = cbind(re_dp[,dr_as], re_dp[,c('problemID', 'type', 'domain')])
re_id = re_id[,c('subject_id', dr_as)]

#
xp = read.csv('00_data/csv_dat/input.csv')
xp[is.na(xp)] = 0; colnames(xp)[1] = 'problemID'

# add decision problem
xp$dec_prob = paste0(apply(xp[,2:7], 1, cp_text), '\n',
                     apply(xp[,8:13], 1, cp_text, lot = 'B') )

# problems ----------------------------------------------------------------

# reason frequencies for plotting
re_dp = merge(re_dp, xp[,c('problemID', 'dec_prob')])
re_dp[,dr_as] = re_dp[,dr_as]/86 # into proportions

# pacmap problems x reasons
pcmp_problems = read.csv('04_results/03b_reasons-x-problems_pacmap.csv')
pcmp_problems = merge(pcmp_problems, re_dp[,c('dec_prob', 'type', 'domain', 'problemID')])

# 
pcmp_problems$shape = NA
pcmp_problems$shape[pcmp_problems$domain=='gain'] = 1 
pcmp_problems$shape[pcmp_problems$domain == 'gain' & pcmp_problems$type == 'risk_risk'] = 19
pcmp_problems$shape[pcmp_problems$domain == 'loss' & pcmp_problems$type == 'risk_safe'] = 2 
pcmp_problems$shape[pcmp_problems$domain == 'loss' & pcmp_problems$type == 'risk_risk'] = 17
pcmp_problems$shape[pcmp_problems$domain == 'mixed'] = 0

# matrix for clustering
pcmp_problems_xx = as.matrix(pcmp_problems[,c('dim1', 'dim2')])

# distance matrix
pcmp_problems_xx_dist = dist(pcmp_problems_xx, method = "euclidean")
hc_problems = hclust(pcmp_problems_xx_dist, method = "ward.D2");
# plot(hc_problems)
# number of problem clusters
n_problem_clusters = N_PR_CL
pcmp_problems$pr_clust = cutree(hc_problems, k = n_problem_clusters); 
#
# reorder proportion matrix
re_dp = merge(re_dp, pcmp_problems[,c('problemID', 'pr_clust')])
re_dp = re_dp[hc_problems$order,]; 
# move prblm 15 to a different position
re_dp = re_dp[c(1:7,11,8:10,12:20),]
re_dp$id2 = 1:20
pr_clust_cols = inferno(n_problem_clusters, 1, .3, .85)[n_problem_clusters:1]

# PROBLEM COLORS FOR THE 2D MAP
pcmp_problems$col = NA; 
for(i in 1:n_problem_clusters) {
  pcmp_problems$col[pcmp_problems$pr_clust == i] = pr_clust_cols[i]
} 

rm(hc_problems, pcmp_problems_xx, pcmp_problems_xx_dist)

# shorten problem 1 description
re_dp$dec_prob = gsub('; 0€, .01', '', re_dp$dec_prob)

# individuals -------------------------------------------------------------

# cluster individuals based on pacmap
pcmp_inds = read.csv('04_results/03b_reasons-x-individuals_pacmap.csv')
pcmp_inds_xx = as.matrix(pcmp_inds[,c('dim1', 'dim2')]) # matrix for clustering

pcmp_inds_xx_dist = dist(pcmp_inds_xx, method = "euclidean")
hc_inds = hclust(pcmp_inds_xx_dist, method = "ward.D2"); 
# plot(hc_inds)
# number of individual clusters
n_ind_clusters = N_IND_CL
pcmp_inds$i_clust_r = cutree(hc_inds, k = n_ind_clusters)
#
# renumber clusters
pcmp_inds$i_clust = pcmp_inds$i_clust_r
pcmp_inds$i_clust[pcmp_inds$i_clust_r==3] = 2
pcmp_inds$i_clust[pcmp_inds$i_clust_r==2] = 3

# color clusters
id_clust_cols = turbo(n_ind_clusters)
pcmp_inds$col = id_clust_cols[pcmp_inds$i_clust]

# reasons x individuals frequencies 
re_id = merge(re_id, pcmp_inds[,c('subject_id', 'i_clust')])
re_id[,dr_as] = re_id[,dr_as]/20 

# reorder proportion matrix
re_id = re_id[hc_inds$order, ]; 
re_id = re_id[order(re_id$i_clust), ]; 
#
re_id$subject_id2 = 1:86
#
re_id$subject_id2[re_id$i_clust==3] = sort(re_id$subject_id2[re_id$i_clust==3],
                                           decreasing = T)
re_id = re_id[order(re_id$subject_id2),]

rm(hc_inds, pcmp_inds_xx, pcmp_inds_xx_dist)

dxr = gsub('_', ' ', dr_as)
dxr = gsub('number of ', '', dxr)

# save clusters for oos analyses ------------------------------------------

hClusters = list(problems = re_dp[c('problemID', 'pr_clust', 'type', 'domain')],
                 individuals = re_id[c('subject_id', 'i_clust')])
hClusters = lapply(hClusters, function(x) x[order(x[,1]), ] )

saveRDS(hClusters, '04_results/rds_res/03c_llm_clusters.rds')


# reason names acronyms  --------------------------------------------------

reason_acronym = function(x) {
  x = tolower(x)
  x = gsub("_", " ", x)
  x = gsub("-", " ", x)
  x = gsub("[^a-z ]", "", x)      # keep only letters and spaces
  x = trimws(x)
  
  sapply(strsplit(x, "\\s+"), function(words) {
    paste0(substr(words, 1, 1), ".", collapse = "")
  })
}

dxr_a = reason_acronym(dxr)

# pdf device --------------------------------------------------------------

cairo_pdf('05_figures/Fig05.pdf',
          width = (16/2.54),
          height = (8/2.54),
          pointsize = 6)

mat = matrix(1:3,
             nrow = 1, ncol = 3,
             byrow = T)

layout(mat,
       widths = c(13, 10.2, 1.2))

# reasons by individuals --------------------------------------------------

# plot margins
# par(mar = c(1, 2, 14, 1))
par(mar = c(1, 15, 10, 1))

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
for(i in 1:n_ind_clusters) {
  
  axis(3, at = re_id$subject_id2[re_id$i_clust == i], 
       col = id_clust_cols[i], 
       labels = F,
       col.ticks = id_clust_cols[i])
  
}

mtext('Participant', line = 3,)

# reasons by problems -----------------------------------------------------

par(mar = c(1, 4, 10, 1))

# tile plot reason X prediction
tile_plts(t(re_dp[,dr_as]),
          cols = mako(100, 1),
          xlab = '',
          ylab = '',
          axes = F,
          grid = F)

# REASONS NAMES
axis(2, 1:length(dr_as),
     dxr_a,
     las = 2, padj = .5)
axis(2, at = 1:length(dr_as), labels = F)

for(i in 1:n_problem_clusters) {
  axis(3, 
       at = re_dp$id2[re_dp$pr_clust == i],
       labels = re_dp$dec_prob[re_dp$pr_clust == i],
       col.axis = pr_clust_cols[i],
       col = pr_clust_cols[i],
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


# individuals 2d map --------------------------------------------------

par(fig = c(0.01, 0.32, 0, .25), new = T) 
par(mar = c(2, 15, 1, 1))

plot(pcmp_inds$dim1, 
     pcmp_inds$dim2, 
     type = 'p',
     xlab = '', ylab = '', 
     pch = 16,
     col = pcmp_inds$col,
     cex = 1,
     xaxt = 'n', yaxt = 'n',
     bty = 'n',
     panel.first = {
       usr = par("usr")
       rect(usr[1], usr[3], usr[2], usr[4], 
            col = rgb(1, 1, 1, 1), border = NA)
     })

# problems 2d map ---------------------------------------------------------

par(fig = c(0.56, .74, 0, .25), new = T) 
par(mar = c(2, 2, 1, 1))

plot(pcmp_problems$dim1, 
     pcmp_problems$dim2, 
     type = 'p',
     pch = pcmp_problems$shape, col = pcmp_problems$col,
     xlab = '', ylab = '',
     cex  = 1,
     xaxt = 'n', yaxt = 'n',
     bty = 'n',
     panel.first = {
       usr = par("usr")
       rect(usr[1], usr[3], usr[2], usr[4], 
            col = rgb(1, 1, 1, 1), border = NA)
     })

# problem class legend
legend(-.3, -.1,
       # 'topleft', 
       # inset = c(-0.25, .15),
       legend = c('Gains risk-safe', 'Gains risk-risk',
                  'Losses risk-safe', 'Losses risk-risk',
                  'Mixed risk-safe'),
       # title= 'Problem type',
       pch = c(1, 19, 2, 17, 0),
       bty = 'n',
       cex = .9)

# figure marks ------------------------------------------------------------

# fig marks
mtext("a", side = 3, 
      line = -6, 
      adj = .13, 
      cex = 1.25, 
      font = 2,
      outer = T)

# fig marks
mtext("b", side = 3, 
      line = -6, 
      adj = .55, 
      cex = 1.25, 
      font = 2,
      outer = T)

# save the figure ---------------------------------------------------------

dev.off()
