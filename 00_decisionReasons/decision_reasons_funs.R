minimum_outcome = function(xA, pA, xB, pB) {
  
  # lottery A subjective value according to the minimum outcome reason: minimum of xA
  svA = min(xA)
  
  # lottery B subjective value according to the minimum outcome reason: minimum of xB
  svB = min(xB)
  
  # to obtain 1 as preference for A, -1 as preference for B, and 0 as indifference
  # we can take the sign of equation svA - svB:
  choice = sign( round(svA - svB, 5) )
  
  #
  return(choice)
}

maximum_outcome = function(xA, pA, xB, pB) {
  # lottery A subjective value according to the maximum outcome reason: maximum of xA
  svA = max(xA)
  
  # lottery B subjective value according to the maximum outcome reason: maximum of xB
  svB = max(xB)
  
  # to obtain 1 as preference for A, -1 as preference for B, and 0 as indifference
  # we can take the sign of equation svA - svB:
  choice = sign( round(svA - svB, 5) )
  
  return(choice)
}

higher_maximum_probability = function(xA, pA, xB, pB) {
  # Prefer the lottery with the higher maximum probability
  svA = max(pA)
  svB = max(pB)
  choice = sign( round(svA - svB, 5) )
  return(choice)
}

lower_maximum_probability = function(xA, pA, xB, pB) {
  # Prefer the lottery with the lower maximum probability
  svA = max(pA)
  svB = max(pB)
  choice = sign( round(svB - svA, 5) )
  return(choice)
  }

higher_minimum_probability = function(xA, pA, xB, pB) {
  # Prefer the lottery with the higher minimum probability
  svA = min(pA)
  svB = min(pB)
  choice = sign( round(svA - svB, 5) )
  return(choice)
}

lower_minimum_probability = function(xA, pA, xB, pB) {
  # Prefer the lottery with the lower minimum probability
  svA = min(pA)
  svB = min(pB)
  choice = sign( round(svB - svA, 5) )
  return(choice)
}

zero_outcome_presence = function(xA, pA, xB, pB) {
  # Prefer the lottery with a zero outcome
  svA = ifelse(any(xA == 0), 1, 0)
  svB = ifelse(any(xB == 0), 1, 0)
  choice = sign( round(svA - svB, 5) )
  return(choice)
}

zero_outcome_absence = function(xA, pA, xB, pB) {
  # Prefer the lottery without a zero outcome
  svA = ifelse(any(xA == 0), 0, 1)
  svB = ifelse(any(xB == 0), 0, 1)
  choice = sign( round(svA - svB, 5) )
  return(choice)
}

sure_outcome_presence = function(xA, pA, xB, pB) {
  # Prefer the lottery with a sure outcome
  svA = ifelse(any(pA == 1), 1, 0)
  svB = ifelse(any(pB == 1), 1, 0)
  choice = sign( round(svA - svB, 5) )
  return(choice)
}

sure_outcome_absence = function(xA, pA, xB, pB) {
  # Prefer the lottery without a sure outcome
  svA = ifelse(any(pA == 1), 0, 1)
  svB = ifelse(any(pB == 1), 0, 1)
  choice = sign( round(svA - svB, 5) )
  return(choice)
}

higher_minimum_outcome_probability = function(xA, pA, xB, pB) {
  # Prefer the lottery with the higher probability of the minimum outcome
  indexA = which.min(xA)
  indexB = which.min(xB)
  svA = pA[indexA]
  svB = pB[indexB]
  choice = sign( round(svA - svB, 5) )
  return(choice)
}

lower_minimum_outcome_probability = function(xA, pA, xB, pB) {
  # Prefer the lottery with the lower probability of the minimum outcome
  indexA = which.min(xA)
  indexB = which.min(xB)
  svA = pA[indexA]
  svB = pB[indexB]
  choice = sign( round(svB - svA, 5) )
  return(choice)
}

higher_maximum_outcome_probability = function(xA, pA, xB, pB) {
  
  # Prefer the lottery with the higher probability of the maximum outcome
  svA = pA[which.max(xA)]
  svB = pB[which.max(xB)]
  
  choice = sign( round(svA - svB, 5) )

  return(choice)
}

lower_maximum_outcome_probability = function(xA, pA, xB, pB) {
  # Prefer the lottery with the lower probability of the maximum outcome
  indexA = which.max(xA)
  indexB = which.max(xB)
  svA = pA[indexA]
  svB = pB[indexB]
  choice = sign( round(svB - svA, 5) )
  return(choice)
}

higher_zero_outcome_probability = function(xA, pA, xB, pB) {
  # Prefer the lottery with the higher probability of a zero outcome
  svA = ifelse(any(xA == 0), pA[xA == 0], 0)
  svB = ifelse(any(xB == 0), pB[xB == 0], 0)
  choice = sign( round(svA - svB, 5) )
  return(choice)
}

lower_zero_outcome_probability = function(xA, pA, xB, pB) {
  # Prefer the lottery with the lower probability of a zero outcome
  svA = ifelse(any(xA == 0), pA[xA == 0], 0)
  svB = ifelse(any(xB == 0), pB[xB == 0], 0)
  choice = sign( round(svB - svA, 5) )
  return(choice)
}

most_likely_outcome = function(xA, pA, xB, pB) {
  
  # Determine the most probable outcome for each lottery
  max_pA = sapply(pA, function(x) x == max(pA) )
  max_pB = sapply(pB, function(x) x == max(pB) )
  
  svB = max( xB[max_pB] )
  svA = max( xA[max_pA] )
  
  choice = sign( round(svA - svB, 5) )
  
  # if(svA != svB ) { # if the outcomes are different make a choice
  #   
  #   choice = sign( round(svA - svB, 5) )
  #   
  # } else {
  #   
  #   pvA = pA[xA == svA]
  #   pvB = pB[xB == svB]
  #   
  #   if(any(xA > 0) & any(xB > 0)) {
  #     
  #     if(svA == 0) choice = sign( round(pvB - pvA, 5) )
  #     if(svA != 0) choice = sign( round(pvA - pvB, 5) )
  #     
  #   } else {
  #     
  #     if(svA == 0) choice = sign( round(pvA - pvB, 5) )
  #     if(svA != 0) choice = sign( round(pvB - pvA, 5) )
  #     
  #   }
  # 
  # }
  
  # Prefer the lottery with the more favorable most probable outcome
  return(choice)
}

least_likely_outcome = function(xA, pA, xB, pB) {
  
  # Determine the least probable outcome for each lottery
  min_pA = sapply(pA, function(x) x == min(pA) )
  min_pB = sapply(pB, function(x) x == min(pB) )
  
  # take the most favorable ones
  svA = max( xA[min_pA] )
  svB = max( xB[min_pB] )
  
  choice = sign( round(svA - svB, 5) )
  
  # Prefer the lottery with the more favorable most probable outcome
  return(choice)
}


large_outcome_range = function(xA, pA, xB, pB) {
  # Calculate the range (difference between max and min) of outcomes for each lottery
  svA = max(xA) - min(xA)
  svB = max(xB) - min(xB)
  # Prefer the lottery with the larger outcome range
  choice = sign( round(svA - svB, 5) )
  return(choice)
}

small_outcome_range = function(xA, pA, xB, pB) {
  # Calculate the range (difference between max and min) of outcomes for each lottery
  svA = max(xA) - min(xA)
  svB = max(xB) - min(xB)
  # Prefer the lottery with the smaller outcome range
  choice = sign( round(svB - svA, 5) )
  return(choice)
}

large_probability_range = function(xA, pA, xB, pB) {
  # Calculate the range (difference between max and min) of probabilities for each lottery
  svA = max(pA) - min(pA)
  svB = max(pB) - min(pB)
  # Prefer the lottery with the larger probability range
  choice = sign( round(svA - svB, 5) )
  return(choice)
}

small_probability_range = function(xA, pA, xB, pB) {
  # Calculate the range (difference between max and min) of probabilities for each lottery
  svA = max(pA) - min(pA)
  svB = max(pB) - min(pB)
  # Prefer the lottery with the smaller probability range
  choice = sign( round(svB - svA, 5) )
  return(choice)
}

expected_value = function(xA, pA, xB, pB) {
  # Calculate the expected value for each lottery
  svA = sum(xA * pA)
  svB = sum(xB * pB)
  # Prefer the lottery with the higher expected value
  choice = sign( round(svA - svB, 5) )
  return(choice)
}

large_variance = function(xA, pA, xB, pB) {
  # Calculate the variance for each lottery
  svA = sum(pA * (xA - sum(xA * pA))^2)
  svB = sum(pB * (xB - sum(xB * pB))^2)
  # Prefer the lottery with the larger variance
  choice = sign( round(svA - svB, 5) )
  return(choice)
}

small_variance = function(xA, pA, xB, pB) {
  # Calculate the variance for each lottery
  svA = sum(pA * (xA - sum(xA * pA))^2)
  svB = sum(pB * (xB - sum(xB * pB))^2)
  # Prefer the lottery with the smaller variance
  choice = sign( round(svB - svA, 5) )
  return(choice)
}

sum_of_outcomes = function(xA, pA, xB, pB) {
  # Calculate the sum of all outcomes for each lottery
  svA = sum(xA)
  svB = sum(xB)
  # Prefer the lottery with the higher sum of outcomes
  choice = sign( round(svA - svB, 5) )
  return(choice)
}

mean_outcome = function(xA, pA, xB, pB) {
  # Calculate the mean of all outcomes for each lottery
  svA = mean(xA)
  svB = mean(xB)
  # Prefer the lottery with the higher mean outcome
  choice = sign( round(svA - svB, 5) )
  return(choice)
}

### MODELS ###

outcome_neglect = function(xA, pA, xB, pB) {
  # Lottery A and B subjective value according to outcome neglect reason:
  # The highest probability of the maximum outcome
  svA = pA[which.max(xA)]
  svB = pB[which.max(xB)]
  
  if( sign(max(xA)) == sign(max(xB)) ) {
    
    if(svA == svB) {
      
      svA = sum(pA[xA != 0])
      svB = sum(pB[xB != 0])

    }
    
    # Decision based on the highest probability of maximum outcome
    choice = sign( round(svA - svB, 5) )
    
  } else {
    
    choice = sign(max(xA) - max(xB))
    
  }
  
  return(choice)
}

outcome_sensitivity = function(xA, pA, xB, pB) {
  # Lottery A and B subjective value according to outcome sensitivity reason:
  
  # Maximum and minimum outcomes
  svA = c(max(xA), min(xA))
  svB = c(max(xB), min(xB))
  
  # Decision based on more favorable outcomes
  if(svA[1] != svB[1]) { 
    
    # prefer higgher maximum outcome if they differ
    choice = sign( round(svA[1] - svB[1], 5) ) 
    
  } else {
    
    # prefer higgher minimum outcome if they differ
    choice = sign( round(svA[2] - svB[2], 5) ) 
    
  }
  
  return(choice)
}

probability_neglect = function(xA, pA, xB, pB) {
  # Lottery A and B subjective value according to probability neglect reason:
  # The more favorable outcomes, ignoring differences in probabilities
  svA = c(max(xA), min(xA))
  svB = c(max(xB), min(xB))
  
  # Decision based on more favorable outcomes
  if(svA[1] != svB[1]) { 
    
    choice = sign( round(svA[1] - svB[1], 5) ) 
    
  } else {
    
    choice = sign( round(svA[2] - svB[2], 5) ) 
    
  }
  
  return(choice)
  
}

probability_sensitivity = function(xA, pA, xB, pB) {
  # Lottery A and B subjective value according to probability sensitivity reason:
  # higher chances of obtaining zero or positive outcome
  if(any(xA < 0) | any(xB < 0)) {
    
    svA = sum(pA[which(xA >= 0)])
    svB = sum(pB[which(xB >= 0)])
    
  } else {
    
    svA = sum(pA[which(xA > 0)])
    svB = sum(pB[which(xB > 0)])
    
  }
  
  # Decision based on expected values
  choice = sign( round(svA - svB, 5) )
  
  return(choice)
}

small_probability_overweighting = function(xA, pA, xB, pB) {
  
  # Identify the smallest probabilities and overweight them
  pA = sapply(pA, function(x) ifelse(x < .25, x + .1, x) )
  pB = sapply(pB, function(x) ifelse(x < .25, x + .1, x) )
  
  svA = sum(pA * xA)
  svB = sum(pB * xB)
  
  # Prefer the lottery with the more favorable outcome for the smallest probability
  choice = sign( round(svA - svB, 5) )
  
  return(choice)
}

loss_avoidance = function(xA, pA, xB, pB) {

  # lower loss probability
  svA = pA[which.min(xA)]
  svB = pB[which.min(xB)]

  if(sign(min(xA)) == sign(min(xB))) {

    if(svA == svB) {

      # lower loss probability
      svA = sum(pA[xA < 0])
      svB = sum(pB[xB < 0])

    }

    # lower loss prob is better: pB - pA !!
    choice = sign( round(svB - svA, 5) )

  } else {

    choice = sign( min(xA) - min(xB) )

  }

  return(choice)
}

loss_aversion = function(xA, pA, xB, pB) {
  
  # higher minimum outcome
  svA = min(xA)
  svB = min(xB)
  
  choice = sign( round(svA - svB, 5) )
  
  return(choice)
}

left_skewness = function(xA, pA, xB, pB) {
  # Compute skewness for each lottery
  muA = sum(xA * pA)
  sigmaA = sqrt(sum(pA * (xA - muA)^2))
  thirdMomentA = sum(pA * (xA - muA)^3)
  svA = thirdMomentA / sigmaA^3
  
  if(length(xB) > 1) {
    
    muB = sum(xB * pB)
    sigmaB = sqrt(sum(pB * (xB - muB)^2))
    thirdMomentB = sum(pB * (xB - muB)^3)
    svB = thirdMomentB / sigmaB^3
    
  } else {
    
    svB = 0
    
  }
  
  # Prefer the lottery with more left-skewed (more negative skewness value) distribution
  choice = sign( round(svB - svA, 5) )  # Note the reversed sign for preferring more negative (left-skew)
  
  return(choice)
}

right_skewness = function(xA, pA, xB, pB) {
  # Compute skewness for each lottery
  muA = sum(xA * pA)
  sigmaA = sqrt(sum(pA * (xA - muA)^2))
  thirdMomentA = sum(pA * (xA - muA)^3)
  svA = thirdMomentA / sigmaA^3
  
  if(length(xB) > 1) {
    
    muB = sum(xB * pB)
    sigmaB = sqrt(sum(pB * (xB - muB)^2))
    thirdMomentB = sum(pB * (xB - muB)^3)
    svB = thirdMomentB / sigmaB^3
    
  } else {
    
    svB = 0
    
  }
  
  # Prefer the lottery with more right-skewed (more positive skewness value) distribution
  choice = sign( round(svA - svB, 5) )
  
  return(choice)
}

reference_point = function(xA, pA, xB, pB) {
  
  # the reference is either the sure outcome
  # or zero 
  if(any(pB == 1)) { reference = xB} else { reference = 0}
  
  # Calculate differences to the subjective reference point, weighted by their probabilities
  svA = sum((xA - reference) * pA)
  svB = sum((xB - reference) * pB)
  
  # Prefer the lottery with the more favorable differences
  choice = sign( round(svA - svB, 5) )
  
  return(choice)
}

segregation = function(xA, pA, xB, pB) {
  # Identify the minimum magnitude (sure) outcomes for each lottery
  minMA = xA[which.min( abs(xA) )]
  minMB = xB[which.min( abs(xB) )]
  
  # Calculate the remaining differences from the minimum, weighted by probabilities
  differencesA = xA - minMA
  differencesB = xB - minMB
  
  # Calculate subjective values for each lottery
  svA = minMA + sum(differencesA * pA)
  svB = minMB + sum(differencesB * pB)
  
  # Prefer the lottery with the more favorable combination of sure outcome and weighted differences
  choice = sign( round(svA - svB, 5) )
  
  return(choice)
}

importance_sampling = function(xA, pA, xB, pB) {
  
  # Identify the index of outcomes of the highest magnitude of each lottery
  maxMagnitudeAindex = which.max(abs(xA))
  maxMagnitudeBindex = which.max(abs(xB))
  
  # Expectations of the max magnitude outcome-probability pairs
  
  # Calculate the weighted sum of probabilities for the important outcomes
  svA = xA[maxMagnitudeAindex] * pA[maxMagnitudeAindex]
  svB = xB[maxMagnitudeBindex] * pB[maxMagnitudeBindex]
  
  # Decision based on the weighted sum of probabilities of the highest magnitude outcomes
  choice = sign( round(svA - svB, 5) )
  
  return(choice)
}

aspiration_level = function(xA, pA, xB, pB) {
  
  aspiration = mean(c(xA, xB))
  
  # Calculate probability of positive differences to the aspiration level
  svA = sum(pA[which(xA > aspiration)])
  svB = sum(pB[which(xB > aspiration)])
  
  # Prefer the lottery with the higher probability of exceeding the aspiration level
  choice = sign( round(svA - svB, 5) )
  
  return(choice)
}

regret = function(xA, pA, xB, pB) {
  # Calculate the sum of pairwise differences between outcomes of A and B
  pairwise_differences = outer(xA, xB, "-")
  svA = sum(pairwise_differences)
  
  # Determine preference: 1 for A if sum is positive (A is less regretful),
  # -1 for B if sum is negative, 0 if equal
  choice = sign( round(svA, 5) )
  
  return(choice)
}

disappointment = function(xA, pA, xB, pB) {
  # Calculate the differences between maximum and minimum outcomes within each lottery
  svA = max(xA) - min(xA)
  svB = max(xB) - min(xB)
  
  # Prefer the lottery with the smaller difference between max and min outcomes
  choice = sign( round(svB - svA, 5) )  # Note the reversed sign for preferring smaller differences
  
  return(choice)
}

outcomes_better_than_average = function(xA, pA, xB, pB) {
  # Calculate average of all outcomes across lotteries
  average = mean(c(xA, xB))
  
  # Count number of outcomes above average for each lottery
  svA = sum(xA > average)
  svB = sum(xB > average)
  
  # Prefer the lottery with more outcomes better than average
  choice = sign( round(svA - svB, 5) )
  
  return(choice)
}

consequence_count = function(xA, pA, xB, pB) {
  # Compare maximum and minimum outcomes between lotteries
  svA = sum(c(max(xA), min(xA)) > c(max(xB), min(xB)))
  svB = sum(c(max(xB), min(xB)) > c(max(xA), min(xA)))
  
  # Prefer the lottery with a higher number of favorable comparisons
  choice = sign( round(svA - svB, 5) )
  
  return(choice)
}

maximum_outcome_expectation = function(xA, pA, xB, pB) {
  # Calculate products of the maximum outcome and its probability for each lottery
  svA = max(xA) * pA[which.max(xA)]
  svB = max(xB) * pB[which.max(xB)]
  
  # Prefer the lottery with the higher product
  choice = sign( round(svA - svB, 5) )
  
  return(choice)
}

minimum_outcome_expectation = function(xA, pA, xB, pB) {
  # Calculate products of the minimum outcome and its probability for each lottery
  svA = min(xA) * pA[which.min(xA)]
  svB = min(xB) * pB[which.min(xB)]
  
  # Prefer the lottery with the higher product
  choice = sign( round(svA - svB, 5) )
  
  return(choice)
}

mean_of_probable_outcomes = function(xA, pA, xB, pB) {
  # Define probable outcomes as those with probability higher than 1/n, where n is the number of outcomes
  thresholdA = 1/length(xA)
  thresholdB = 1/length(xB)
  probableA = xA[pA >= thresholdA]
  probableB = xB[pB >= thresholdB]
  
  # Calculate mean of probable outcomes
  svA = mean(probableA)
  svB = mean(probableB)
  
  # Prefer the lottery with a higher mean of probable outcomes
  choice = sign( round(svA - svB, 5) )
  
  return(choice)
}