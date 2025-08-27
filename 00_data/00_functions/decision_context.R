decision_context = function(x, 
                            dc = 'Decision context: ',
                            left_lottery = 'a') {
  
  # input is a data frame with a single row and 12 columns
  # aX1, aX2, aX3, aP1, aP2, aP3, bX1, bX2, bX3, bP1, bP2, bP3
  
  # 

  if(left_lottery == 'a') { LR_lot = c('A', 'B') } else { LR_lot = c('B', 'A')}

  L_lot = paste0('Lottery ', LR_lot[1], ':\n')
  R_lot = paste0('Lottery ', LR_lot[2], ':\n')
  
  # lottery into text
  lot_text = function(xo, po, lot, end = '') {
    
    n = length(po[po > 0])
    
    r = paste0(lot,
               paste0(xo[1:n], ' Euros with ', po[1:n], '% probability', collapse = '\n'), 
               end)
    
    return(r)
    
  }
  
  dec_context = paste0(lot_text(x[1:3], x[4:6], L_lot),
                      '\n\n',
                      lot_text(x[7:9], x[10:12], R_lot))
  
  # results
  return(paste0(dc, dec_context))
  
}