rm(list = ls(all.names = T))

# ispt
source('03_cpt/functions/spt.R')

# modeling data -----------------------------------------------------------

md = readRDS("00_data/rds_dat/md.rds")

aX = grep('aX', colnames(md), value = T)
aP = grep('aP', colnames(md), value = T)
bX = grep('bX', colnames(md), value = T)
bP = grep('bP', colnames(md), value = T)

# 1992 CPT parameters -----------------------------------------------------

md$pt = NA

for(i in 1:nrow(md)) {
  
  #
  va = spt(x = md[i,aX],p = md[i,aP]/100, 
           a = .88, l = 2.25, g = .65)
  vb = spt(x = md[i,bX],p = md[i,bP]/100, 
           a = .88, l = 2.25, g = .65)
  
  md$pt[i] = ifelse(sign(va - vb) == 1, 1, 0)
  
}


# pred --------------------------------------------------------------------

md$pt_c = ifelse(md$y == md$pt, 1, 0)

spt92 = aggregate(pt_c ~ subject_id,
                  FUN = mean, 
                  data = md)

saveRDS(spt92$pt_c, '03_cpt/spt92.rds')