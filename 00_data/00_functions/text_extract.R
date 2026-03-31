library(data.table)
library(stringi)

text_extract = function(x) {
  
  # for each trial
  xx = lapply(1:nrow(x), function(i) {
    
    # take the row with the data
    xy = x[i,]
    
    # check if response contains both '{' and '}'
    if(grepl('\\{&\\}', xy$response)) {
      
      r = xy[,c('session', 'subject_id', 'problemID', 'response')]

    } else {
      
      # respons col no
      rno = which(grepl('response', colnames(xy)))
      
      # find the closing bracket
      cb = which(grepl('}', xy))
      
      # put all the text from other columns into response
      xy$response = paste(xy[rno:cb], collapse = ' ')
      
      # recover the problemID (cb-rno is the no of coumns shiften from problemID)
      xy$problemID = as.numeric( xy[,which(colnames(xy) == 'problemID') + (cb-rno)] )
      
      r = xy[,c('session', 'subject_id', 'problemID', 'response')]
      
    }
    
    # clean up the text responses
    r$response = gsub('\\{\"HOW_WHY\":\"|\"\\}', '', r$response)
    
    # this is a very specific fix for a single weird case...
    r$response = gsub('\\\\0', '0', r$response)
    
    # get the German letters back
    r$response = try(stri_unescape_unicode(r$response))
    
    # REMOVE the \n and \r from the text data!!
    r$response = gsub('\n|\r', '', r$response)
    
    # remove double spaces
    r$response = gsub('  ', ' ', r$response)
    
    # OUTPUT
    return(r)
    
  })
  
  # into a data frame
  xx = data.frame( data.table::rbindlist(xx) )
  
  # FUNCTION OUTPUT
  return(xx)
  
}