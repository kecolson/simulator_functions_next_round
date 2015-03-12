# an_bcm.R

# Function: an_bcm
# Input: an_data (format as pop)
#        match_input (a single row of matches, as a list)
#        estimand ("ate" or "att")
#        metric (string, "rr", "rd"  or "rr")
#        q (numeric vector, 3x # obs as an_data--1st n obs are predictions for observed data, 2nd n obs are predictions when A_1=1 for everyone and 3rd n obs are predictions when A_1=0 for everyone)
# Output: est (numeric scalar)

# No need for match_weights*samp_weights here as these are incorporated in the estimation of the Q model

an_bcm <- function(an_data, match_input, estimand, metric, q) {
  
  n <- nrow(q)/3
  Q1 <- q[(n+1):(2*n)]
  Q0 <- q[(2*n+1):(3*n)]
  
  # If the matching method is not exact, nearest neighbor, or optimal, drop it
  if (!match_input$match_method %in% c("exact","nn","opt")) est <- NA; return(est)
  
  if (estimand == "ate") {
    # Match TREATED units to controls
    m.out <- match(an_data, match_input)[[5]]
   
        # Calculate EY0 counterfactuals for treated units
        mn <-  summary(m.out)$nn[2,2]
        EY0.tx <- rep(NA, mn)
        for (i in 1:mn) {
          EY0.tx[i] <- mean(an_data$outcome[as.numeric(m.out$match.matrix[i,])] + Q0[as.numeric(row.names(m.out$match.matrix)[i])] - Q0[as.numeric(m.out$match.matrix[i,])], na.rm = T)
        }
        
        # Calculate EY1 counterfactuals for treated units
        EY1.tx <- an_data$outcome[as.numeric(row.names(m.out$match.matrix))]
    
    # Match CONTROL units to treated units
    an_data2 <- an_data
    an_data2$A_1 <- ifelse(an_data2$A_1==1, 0, 1)
    m.out <- match(an_data2, match_input)[[5]]
    
        # Calculate EY0 counterfactuals for control units
        EY0.ct <- an_data$outcome[as.numeric(row.names(m.out$match.matrix))]
    
        # Calculate EY1 counterfactuals for control units
        mn <-  summary(m.out)$nn[2,2]
        EY1.ct <- rep(NA, mn)
        for (i in 1:mn) {
          EY1.ct[i] <- mean(an_data$outcome[as.numeric(m.out$match.matrix[i,])] + Q1[as.numeric(row.names(m.out$match.matrix)[i])] - Q1[as.numeric(m.out$match.matrix[i,])], na.rm = T)
        }
    
    EY1 <- mean(c(EY1.tx, EY1.ct))
    EY0 <- mean(c(EY0.tx, EY0.ct))
    
  } else if (estimand == "att") {
    # Match treated units to controls
    m.out <- match(an_data, match_input)[[5]]
    
    # Calculate EY0 counterfactuals for treated units
    ntx <-  summary(m.out)$nn[2,2]
    EY0 <- rep(NA, ntx)
    for (i in 1:ntx) {
      EY0[i] <- mean(an_data$outcome[as.numeric(m.out$match.matrix[i,])] + Q0[as.numeric(row.names(m.out$match.matrix)[i])] - Q0[as.numeric(m.out$match.matrix[i,])], na.rm = T)
    }
    EY0 <- mean(EY0)
    
    # Calculate EY1 counterfactuals for treated units
    EY1 <- mean(an_data$outcome[as.numeric(row.names(m.out$match.matrix))])
  }
  
  return(return_est(EY1, EY0, metric))
}


