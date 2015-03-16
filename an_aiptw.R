# an_aiptw.R

# NOTE: an_tmle is assuming Y is continuous for now. Need to figure out how to transfer information from pop gen phase on dist to here. 

# Function: an_tmle
# Input: an_data (format as pop)
#        samp_weights (numeric vector, same # obs as an_data, if no weights, will be a vector of NA's)
#        covs (string vector of covariate names)
#        estimand ("ate" or "att")
#        metric (string, "rr", "rd"  or "rr")
#        g (numeric vector, 3x # obs as an_data--1st n obs are predictions for observed data, 
#            2nd n obs are predictions when A_1=1 for everyone and 3rd n obs are predictions when A_1=0 for everyone)
#        q (numeric vector, 3x # obs as an_data--1st n obs are predictions for observed data, 2nd n obs are predictions 
#            when A_1=1 for everyone and 3rd n obs are predictions when A_1=0 for everyone)
# Output: est (numeric scalar)

an_aiptw <- function(an_data, samp_weights, covs, estimand, metric, g, q) {
  
  n <- nrow(q)/3
  tx.indices <- (an_data$A_1 == 1)
  ct.indices <- (an_data$A_1 == 0)
  
  weights <- samp_weights # Weights will be a vector of 1's if no sampling weights are necessary

  # ATE
  if (estimand == "ate") { 
    weight1 <- ifelse(an_data$A_1 == 1, (1/g), 0)     * weights
    weight0 <- ifelse(an_data$A_1 == 0, (1/(1-g)), 0) * weights  
    q1 <- q[(n+1):(2*n)]
    q0 <- q[(2*n+1):(3*n)]

    EY1 <- mean(mean((an_data$outcome - q1) *weight1) + q1)
    EY0 <- mean(mean((an_data$outcome - q0) *weight0) + q0)

  # ATT
  } else if (estimand == "att") {
    return(NA)
  }
  
  return(return_est(EY1,EY0,metric))
}