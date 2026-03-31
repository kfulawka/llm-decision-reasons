# SPT components

# value function
v_fun = function(x, a, l) {
  
  sv = sapply(x, function(y) {
    
    L = ifelse(y < 0, l, 1)
    
    sign(y) * L * abs(y)^a
    
  })
  
  return(sv)
  
}

# pwf
pwf = function(p, g, d = 1) { 
  
  wp = sapply(p, function(pp) {
    
    if(pp > 0) { exp( -d*(-log(pp))^g) } else { wp = 0}
    
  })

  return(wp)
  
}

# spt
spt = function(x, p, a, l, g) {
  
  # wp
  wp = pwf(p, g)
  
  # vx
  vx = v_fun(x, a, l) 
  
  # U
  U = sum(vx * wp)
  
  #
  return(U)
  
}

# vax
# vax_rb = function(X = contr.sum(8), b) crossprod( t(X), b)

# inv-logit
softmax = function(x, phi) { 1 / (1 + exp(-phi * x)) }