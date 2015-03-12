# an_pweight.R

# Function: an_pweight
# Input: an_data (format as pop)
#        match_weights (numeric vector, same # obs as an_data)
#        samp_weights (numeric vector, same # obs as an_data)
#        estimand ("ate" or "att")
#        metric (string, "rr", "rd"  or "rr")
#        pweight_type (string, e.g. "pweight_ht")
#        g (numeric vector, 3x # obs as an_data--1st n obs are predictions for observed data, 2nd n obs are predictions when A_1=1 for everyone and 3rd n obs are predictions when A_1=0 for everyone)
# Output: est (numeric scalar)

an_pweight <- function(an_data, match_weights, samp_weights, estimand, metric, pweight_type, g) {
  
  # Combine weights. This will be a vector of 1's if no weighting is necessary.
  weights <- match_weights * samp_weights
  
  # Regular IPTW
  if (pweight_type == "pweight_ht") {
      # ATE
      if (estimand == "ate") {  
          weight1 <- ifelse(an_data$A_1 == 1, (1/g), 0) * weights
          weight0 <- ifelse(an_data$A_1 == 0, (1/(1-g)), 0) * weights  
          EY1 <- mean(weight1 * an_data$outcome)
          EY0 <- mean(weight0 * an_data$outcome)
      # ATT
      } else if (estimand == "att") {
          an_data$weight <- ifelse(an_data$A_1 == 1, 1, g/(1-g)) * weights
          mod <- glm(outcome ~ A_1, weights = weight, data = an_data)
          txt <- control <- an_data
          txt$A_1 <- 1; control$A_1 <- 0
          EY1 <- mean(predict(mod, newdata = txt, type='response')) 
          EY0 <- mean(predict(mod, newdata = control, type='response')) 
      }
  }
  
  # Modified IPTW
  if (pweight_type == "pweight_modht") {
      # ATE
      if (estimand == "ate") {  
        p <- sum(an_data$A_1) / nrow(an_data)
        an_data$weight <- ifelse(an_data$A_1==1, p/g, ((1-p)/(1-g)) ) * weights
        mod <- glm(outcome ~ A_1, weights = weight, data = an_data)
        txt <- control <- an_data
        txt$A_1 <- 1; control$A_1 <- 0
        EY1 <- mean(predict(mod, newdata = txt, type='response')) 
        EY0 <- mean(predict(mod, newdata = control, type='response')) 
        
        # ATT
      } else if (estimand == "att") {
        p <- sum(an_data$A_1) / nrow(an_data)
        an_data$weight <- ifelse(an_data$A_1 == 1, p, (1-p)*g/(1-g)) * weights
        mod <- glm(outcome ~ A_1, weights = weight, data = an_data)
        txt <- control <- an_data
        txt$A_1 <- 1; control$A_1 <- 0
        EY1 <- mean(predict(mod, newdata = txt, type='response')) 
        EY0 <- mean(predict(mod, newdata = control, type='response')) 
      }
  }
  
  return(return_est(EY1, EY0, metric))
}

