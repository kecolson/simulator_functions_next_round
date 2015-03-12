# analyze.R

# Function: analyze
# Input: sample (format as pop. This is the unmatched sample even if matching was conducted-- will use this for bias-corrected matching analysis in which matching happens in the analysis phase)
#        an_data (format as pop),
#        subclass (numeric vector, same # obs as an_data),
#        match_weights (numeric vector, same # obs as an_data),
#        samp_weights (numeric vector, same # obs as an_data),
#        match_input (single row of matches, as a list)
#        a single row of analyses (as a list)
#        g (data frame of all formulated estimates of the probability of tx-- n rows per method),
#        q (data fame of all formulated estimates of the outcome model-- n*3 rows per method-- 1st n obs are predictions for observed data, 2nd n obs are predictions when A_1=1 for everyone and 3rd n obs are predictions when A_1=0 for everyone)
# Output: est (ATE or ATT estimate, a numeric scalar)

analyze <- function(sample, an_data, subclass, match_weights, samp_weights, match_input, analyses, g, q) {
  
  # Identify outcome
  if (analyses$diff == 1) an_data$outcome <- an_data$Y_1 - an_data$Y_0
  if (analyses$diff == 0) an_data$outcome <- an_data$Y_1 
  
  # If data was matched, don't do IPTW 
  if ((match_input$match_method != NA) & analyses$method == "pweight") est <- NA; return(est)
  
  # If match method is not exact, nearest neighbor, or optimal, and methods is BCM, don't estimate
  if (!match_input$match_method %in% c("exact","nn","opt") & analyses$method == "bcm") est <- NA; return(est)
  
  # If subclassification was used, need to conduct analyses within each subgroup and then combine. 
  # Otherwise, run analyses just once
  
  # Non- subclassification methods ----------
  if (is.na(subclass[1])) {
    
    # Run the appropriate analysis
    # If the analysis method is bcm, pass the full sample, otherwise pass an_data
    if (analyses$method == "bcm") { data <- sample } else { data <- an_data }
    est <- analyze_sub(an_data = data, match_weights = match_weights, samp_weights = samp_weights, match_input = match_input, analyses = analyses, g = g, q = q) 
  
  # Subclassification --------------------------
  } else {
    nsubclass <- length(unique(subclass))
    
    # Loop through subclasses and generate estimates
    ests <- rep(NA, nsubclass)
    for (s in 1:nsubclass) {
      sub_data <- an_data[subclass == s,]
      sub_match_weights <- match_weights[subclass == s]
      sub_samp_weights <- samp_weights[subclass == s]
      sub_g <- g[g$subclass == s,] 
      sub_q <- q[q$subclass == s,] 
      # Run the appropriate analysis
      ests[s] <- analyze_sub(an_data = sub_data, match_weights = rep(1,nrow(sub_data)), samp_weights = sub_samp_weights, match_input = match_input, analyses = analyses, g = sub_g, q = sub_q) 
    }
    
    if (analyses$estimand == "ate") est <- weighted.mean(ests, w = as.numeric(table(subclass)), na.rm = T)
    if (analyses$estimand == "att") est <- weighted.mean(ests, w = as.numeric(table(subclass[an_data$A_1 == 1])), na.rm = T)
  }
  return(est)
}

