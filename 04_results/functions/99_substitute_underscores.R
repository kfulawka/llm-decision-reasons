substitute_underscores <- function(strings) {
  
  sapply(strings, function(string) {
    # Split the string into parts based on underscores
    parts <- unlist(strsplit(string, "_"))
    
    if (length(parts) > 2) {
      
      mid = round( length(parts)/2 )
      
      part1 = parts[1:mid]
      part2 = parts[(mid+1):length(parts)]
      
      # Join the parts with space and newline for the middle underscore
      r = paste(paste(part1, collapse = " "),
                paste(part2, collapse = " "), sep = "\n")

    } else {
      # Join the parts with space
      r = paste(parts, collapse = " ")
    }
    
  })
  
}