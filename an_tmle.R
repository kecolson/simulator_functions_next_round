# an_tmle.R

# NOTE: an_tmle is assuming Y is continuous for now. Need to figure out how to transfer information from pop gen phase on dist to here. 

# Function: an_tmle
# Input: an_data (format as pop)
#        match_weights (numeric vector, same # obs as an_data, if no weights, will be a vector of NA's)
#        samp_weights (numeric vector, same # obs as an_data, if no weights, will be a vector of NA's)
#        covs (string vector of covariate names)
#        estimand ("ate" or "att")
#        metric (string, "rr", "rd"  or "rr")
#        g (numeric vector, 3x # obs as an_data--1st n obs are predictions for observed data, 
#            2nd n obs are predictions when A_1=1 for everyone and 3rd n obs are predictions when A_1=0 for everyone)
#        q (numeric vector, 3x # obs as an_data--1st n obs are predictions for observed data, 2nd n obs are predictions 
#            when A_1=1 for everyone and 3rd n obs are predictions when A_1=0 for everyone)
# Output: est (numeric scalar)

an_tmle <- function(an_data, match_weights, samp_weights, covs, estimand, metric, g, q) {
  
  n <- nrow(q)/3
  tx.indices <- (an_data$A_1 == 1)
  ct.indices <- (an_data$A_1 == 0)
  
  # Combine weights. This will be a vector of 1's if no weighting is necessary.
  weights <- match_weights * samp_weights

  # ATE
  if (estimand == "ate") {  
    tmle.out<- tmle(Y = an_data$outcome, A = an_data$A_1, W = an_data[,covs], family = "gaussian",
                    Delta = rep(1,nrow(an_data)), pDelta1 = cbind(1/weights,1/weights),
                    Q = cbind(q[(2*n+1):(3*n)], q[(n+1):(2*n)]),
                    g1W = g)
    # Re-weighting after estimation is just for sampling weights:
    EY1 <- weighted.mean(tmle.out$Qstar[,2], w = samp_weights)
    EY0 <- weighted.mean(tmle.out$Qstar[,1], w = samp_weights)
   
  # ATT
  } else if (estimand == "att") {
    outcome.scaled <- (an_data$outcome - min(an_data$outcome)) / (max(an_data$outcome) - min(an_data$outcome)) 
        # Scale outcome to be between 0 and 1 so it will play nice with tmle.att
    tmle.out<- tmle.att2(Y = outcome.scaled, A = an_data$A_1, W = an_data[,covs], family = "gaussian", 
                         # Using tmle.att2 here, which is my adjusted function to make weights work properly
                         Delta = rep(1,nrow(an_data)), gDelta.method = "user", gDelta.1 = 1/weights,
                         g.method = "user", g.A1 = g,
                         Q.method = "user", Q.A1 = q[(n+1):(2*n)], Q.A0 = q[(2*n+1):(3*n)])
    # Re-weighting after estimation is just for sampling weights:
    EY1 <- weighted.mean(tmle.out$Qstar[tx.indices,2], w = samp_weights[tx.indices]) * (max(an_data$outcome)-min(an_data$outcome))
    EY0 <- weighted.mean(tmle.out$Qstar[tx.indices,1], w = samp_weights[tx.indices]) * (max(an_data$outcome)-min(an_data$outcome))
  }
  
  return(return_est(a,b,metric))
}