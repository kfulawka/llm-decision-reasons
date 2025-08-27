rm(list = ls())

library(future.apply)
library(data.table)

# === Load function to extract final assessment ===
source('02_llms_hpc/02_verbal_reports/extract_confidence.R')

# === Base folder with subject subfolders ===
base_folder = "02_llms_hpc/02_verbal_reports/llama33-70b"

# === Function to process all subjects ===
process_files_long_format = function(base_folder, n = 86) {
  
  plan(multisession)
  
  all_outputs = future_lapply(1:n, function(i) {
    folder_name = sprintf("S%02d", i) 
    folder_path = file.path(base_folder, folder_name)
    
    files = list.files(folder_path, full.names = TRUE)
    
    subject_entries = lapply(files, function(file) {
      reason_name = gsub(".csv", "", basename(file))
      df = read.csv(file, stringsAsFactors = FALSE)
      
      if (nrow(df) == 0) return(NULL)
      
      data.frame(
        subject_id = i,
        problemID = df$problemID,
        reason = reason_name,
        llm_output = df$response,
        stringsAsFactors = FALSE
      )
    })
    
    do.call(rbind, subject_entries)
  })
  
  plan(sequential)
  
  df_all = rbindlist(all_outputs)
  return(df_all)
}

# === Run and combine all responses ===
df_all = process_files_long_format(base_folder)

# === Extract numeric final assessments ===
df_all$llm_output = gsub("\\*+", "", df_all$llm_output)  # remove bolding
df_all$assessment = suppressWarnings(
  as.numeric(
    sub(".*FINAL ASSESSMENT:\\s*([0-9\\.]+).*", "\\1", df_all$llm_output, ignore.case = TRUE)
  )
)

# === Identify rows where assessment is NA ===
missing_entries = df_all[is.na(df_all$assessment),]

# write for re-analysis
write.table(missing_entries, '02_llms_hpc/02_verbal_reports/reanalysis.csv',
            row.names = F, sep = ';')

# for now (25 entries...)
df_all$assessment[is.na(df_all$assessment)] = 50

# data for analyses -------------------------------------------------------

# decision reason choices
d_dr = readRDS( '00_decisionReasons/dr_co.rds')

# to long
d_dr = melt(data.table(d_dr),
            id.vars = c('id'),
            value.name = 'reason_choice',
            variable.name = 'reason')
d_dr = data.frame(d_dr); colnames(d_dr)[1] = 'problemID'
d_dr = d_dr[d_dr$reason %in% unique(df_all$reason), ]

# merge reason pref with llm assessment
dd = merge(df_all, d_dr, by = c('reason', 'problemID'))

# read observed choices
d_co = readRDS('00_data/rds_dat/md.rds')
d_co = d_co[,c('subject_id', 'problemID', 'y')]
d_co$y[d_co$y == 0] = -1

# merge
dd = merge(dd, d_co, by = c('subject_id', 'problemID'))

# group rows
dd$correct = NA
dd$correct = ifelse(dd$reason_choice == dd$y, 'C', 'X')
dd$correct[dd$reason_choice == 0] = 'NA'
dd$correct = factor(dd$correct, 
                    levels = c('C', 'X', 'NA'),
                    ordered = T)

# save
saveRDS(dd, '02_llms_hpc/02_verbal_reports/conf_choice_dat_long.rds')

# wide format -------------------------------------------------------------

# THIS IS BETTER FOR PREDICTION TASKS

# vector with choices
d_co = d_co[order(d_co$subject_id, d_co$problemID), ]
dc = as.matrix(d_co)

#
asmt = dcast(data.table(df_all), 
                      formula = subject_id + problemID ~ reason,
                      value.var = 'assessment')
asmt = data.frame(asmt)
asmt = asmt[order(asmt$subject_id, asmt$problemID), -(1:2)]

# reasons' preferences in wide
d_dr = readRDS( '00_decisionReasons/dr_co.rds')
colnames(d_dr)[1] = 'problemID'
d_dr = as.matrix(d_dr[,colnames(asmt)]) 

#
dw = list(choice = dc,
          asmt = asmt,
          reason = d_dr)

saveRDS(dw, '02_llms_hpc/02_verbal_reports/conf_choice_dat_wide.rds')