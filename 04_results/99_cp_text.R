cp_text = function(x, lot = 'A') {
  
  xx = x[1:3] / 1000
  p = x[4:6] / 100
  
  # remove zero prob outcomes
  xx = xx[p > 0]; p = p[p > 0]
  
  #
  xp = sapply(1:length(xx), function(i) paste0(xx[i], '\u20AC, ', p[i]))
  xp = paste(xp, collapse = '; ')
  
  # substitue 0. with .
  xp = gsub('0\\.', '.', xp)
  
  r = paste0(lot, ' = [', xp, ']')
  
  return(r)
  
}