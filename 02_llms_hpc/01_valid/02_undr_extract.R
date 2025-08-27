rm(list = ls())

# decision reasons functions  ---------------------------------------------

source('00_decisionReasons/decision_reasons_funs.R')
dr_names = ls()

dec_resons_funs = lapply(dr_names, get)
names(dec_resons_funs) = dr_names

# clean up
rm(list = setdiff(ls(), c('dec_resons_funs', 'dr_names')) )

# folder with results
res_f = '02_llms_hpc/01_valid/res/'

# model -------------------------------------------------------------------

for(mod_res in list.files(res_f)) {
  
  # choice problems  --------------------------------------------------------
  
  xp = read.csv('00_data/input.csv')
  xp$headline = NULL
  colnames(xp)[1] = 'id'
  xp[is.na(xp)] = 0
  
  # data --------------------------------------------------------------------
  
  # get the results of function validation
  dr_f = read.csv('00_decisionReasons/dr_fun_valid.csv')
  dr_f = dr_f[c('id', dr_names)]
  
  #
  llm_r_csvs = list.files(full.names = T,
                          path = paste0(res_f, mod_res)
  )
  
  llm_r_raw = lapply(llm_r_csvs, function(xx) {
    
    # get reason name
    rn = gsub(paste0(res_f, mod_res, "/|\\.csv"), '', xx)
    rn = gsub(' ', '_', rn)
    # read in the results
    x = read.csv(xx)
    # remove index column
    if(dim(x)[2] == 2) x = as.data.frame(x[,2])
    colnames(x)[1] = rn
    
    return(x)
    
  })
  
  # function to extract decisions -----------------------------------------
  
  # (y = llm_r_raw[[1]][1,])
  
  extract_llm_dec = function(x) {
    
    # reason name
    rn = colnames(x); print(rn)
    
    # extract decisions
    xx = sapply(1:20, function(ii) {
      
      y = x[ii,]
      
      #
      y <- gsub("[\\*\\_]", "", y)  # remove asterisks and underscores
      
      #
      pattern <- "FINAL ANSWER\\*?\\*?:?\\s*(A|B|INDIFFERENT)"
      
      match <- regexec(pattern, y, ignore.case = TRUE)
      capture <- regmatches(y, match)
      
      if (length(capture[[1]]) < 2) {
        final_answer <- NA
        print(paste0('regex failed for ', ii))
      } else {
        final_answer <- toupper(capture[[1]][2])
      }
      
      return(final_answer)
      
    }); xx = matrix(xx, 20, 1)
    
    colnames(xx) = rn
    
    return(xx)
    
  }
  
  # extract -----------------------------------------------------------------
  
  # (y = llm_r_raw[[1]][1,])
  
  # set up problem IDs
  dr_llm = matrix(1:20, nrow = 20, ncol = 1,
                  dimnames = list(NULL, 'id'))
  
  # extract llm decisions
  for(i in 1:length(llm_r_raw)) { 
    
    print(i) 
    # if(! i  %in% c(43) )
    dr_llm = cbind(dr_llm, extract_llm_dec(llm_r_raw[[i]]))
    
  }
  
  dr_llm[dr_llm == 'A'] = 1
  dr_llm[dr_llm == 'B'] = -1
  dr_llm[dr_llm == c('INDIFFERENT')] = 0
  
  dr_llm = data.frame(dr_llm)
  # View(dr_llm)
  
  # save --------------------------------------------------------------------
  
  # into rds
  saveRDS(dr_llm, paste0('02_llms_hpc/01_valid/extracted/', 
                         mod_res, '_co.rds'))
  
  # now correct
  xx = which(colnames(dr_llm) %in% colnames(dr_f))
  
  dr_f = dr_f[,colnames(dr_llm)[xx]]
  dr_llm = dr_llm[,colnames(dr_llm)[xx]]
  
  # data frame with piece wise comparisons
  d_comp = dr_f[,-1] == dr_llm[,-1]
  saveRDS(d_comp, paste0('02_llms_hpc/01_valid/extracted/', 
                         mod_res, '_eval.rds'))
  
  mean(d_comp, na.rm = T); sd(d_comp, na.rm = T)
  
}