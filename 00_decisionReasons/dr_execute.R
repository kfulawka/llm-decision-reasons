decision_reason_choice = function(xA, pA, xB, pB, reason) {
  
  # first, remove outcomes with non-zero probabilities and corresponding zero probabilities
  xA = xA[which(pA > 0)]; pA = pA[which(pA > 0)]
  xB = xB[which(pB > 0)]; pB = pB[which(pB > 0)]
  
  # # get the probabilities to 0--1 range
  # if(any(pA > 1) | any(pB > 1)) {
  #   
  #   pA = pA/100; pB = pB/100
  #   
  # }
  
  # apply decision reason
  choice = reason(xA = xA, pA = pA, xB = xB, pB = pB)
  
  return(choice)
  
}