library(stringr)

extract_final_assessment <- function(xx) {
  # Step 1: Remove all * characters (used for bold formatting)
  cleaned <- gsub("\\*", "", xx)
  
  # Step 2: Extract lines containing "FINAL ASSESSMENT:"
  final_lines <- regmatches(cleaned, gregexpr("(?i)FINAL ASSESSMENT:\\s*.*", cleaned, perl = TRUE))
  final_lines <- unlist(final_lines)
  
  if(length(final_lines) == 0) return(NA)
  
  # Step 3: Extract the numeric value after "FINAL ASSESSMENT:"
  final_scores <- sub("(?i).*FINAL ASSESSMENT:\\s*", "", final_lines, perl = TRUE)
  final_scores <- as.numeric(sub("[^0-9.]+.*", "", final_scores))  # extract first number only
  
  if(length(final_scores) > 1) final_scores = final_scores[!is.na(final_scores)]
  
  return(final_scores)
}



extract_assessment_values_sub = function(x) {
  
  y = matrix(NA, nrow = 20, ncol = 1)
  
  for(i in 1:20) {
    # print(i)
    r = extract_final_assessment(x[i])
    if(is.na(r)) print(paste0('---trial-', i, '---NA assessment!!!') )
    y[i,1] = r
  } 

  return(y)
  
}

# r = extract_assessment_values_sub(file_content[,2])