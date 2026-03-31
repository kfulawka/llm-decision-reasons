# assessments transformation ----------------------------------------------

# treshold function
tr_fun = function(x, tr = 70) {
  
  apply(x, 1:2, function(y) {
    
    if(y >= tr) r = 1 else r = 0
    
    return(r)
    
  })
}

# assessment-as-weight function
w_fun = function(x) x / 100

# equal-to-max
maxA_fun = function(x) {
  
  y = x
  
  for(i in 1:nrow(y) ) {
    for(j in 1:ncol(y)) {
      
      y[i,j] = as.numeric(x[i,j] == max(x[i,]))
      
    }
  }
  
  return(y)
  
}

# prediction functions ----------------------------------------------------

majority_pred = function(aaT, drp) {
  
  X = aaT * reason_p # element-wise multiplication
  
  apply(X, 1, function(x) {
    
    y = x[ x != 0 ]
    
    if(length(y) == 0) r = 0 else r = mean(y)
    
    return(r)
    
    })
}

weighted_pred = function(W, drp) {
  
  X = c()
  
  for(i in 1:nrow(W)) {
    
    S = sum(W[i,])
    
    X[i] = sum(W[i,] * drp[i,]) / ifelse(S == 0, 1, S)
    
  }
  return(X)
  
}


# prediction
choice_pred = function(lin_pred) {
  
  p = sign(lin_pred) # -1, 0, 1
  
  return(p)
  
}


