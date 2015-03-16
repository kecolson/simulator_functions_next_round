# an_gcomp.R

# Function: an_gcomp
# Input: A (indicator of treated unit)
#        q (numeric vector, 3x # obs as an_data--1st n obs are predictions for observed data, 2nd n obs are predictions when A_1=1 for everyone and 3rd n obs are predictions when A_1=0 for everyone)
#        samp_weights (numeric vector, same # obs as an_data)
#        estimand ("ate" or "att")
#        metric (string, "rr", "rd"  or "rr")
# Output: est (numeric scalar)

# No need for match_weights*samp_weights here as these are incorporated in the estimation of the Q model
# But we do need the sampling weights to re-weight the predictions that come out of the Q

an_gcomp <- function(A, q, samp_weights, estimand, metric) {
  
  n <- nrow(q)/3
  tx.indices <- which(an_data$A_1[1:n] == 1)
  ct.indices <- which(an_data$A_1[1:n] == 0)
  
  if (estimand == "ate") {
    EY1 <- weighted.mean(q[(n+1):(2*n)],   w = samp_weights) 
    EY0 <- weighted.mean(q[(2*n+1):(3*n)], w = samp_weights)
    
  } else if (estimand == "att") {
    q1 <- q[(n+1):(2*n)]
    q0 <- q[(2*n+1):(3*n)]
    EY1 <- weighted.mean(q1[tx.indices], w = samp_weights[tx.indices])  
    EY0 <- weighted.mean(q0[tx.indices], w = samp_weights[tx.indices])  
  }
  return(return_est(EY1, EY0, metric))
}


