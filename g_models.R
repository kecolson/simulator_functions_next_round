# g_models.R

# Function: g_models
# Inputs: an_data (format same as pop),
#         match_weights (numeric vector, same # obs as an_data),
#         samp_weights  (numeric vector, same # obs as an_data),
#         analyses (data frame of all analysis specifications )
# Output: g (data frame of all formulated estimates of the probability of tx-- n rows per method)
#          
# This function estimates the probability of treatment given covariates

g_models <- function(an_data, subclass, match_weights, samp_weights, analyses) {
  
  # Collapse analyses to all unique combinations of g estimates, for analyses that require them
  runs <- analyses[,c("method","covs","pscore_para","pscore_link","pscore_covform","pscore_slfull","pscore_sldefault","pscore_sllibs")]
  runs <- runs[runs$method %in% c("pweight","aiptw","tmle"),]
  
  # IPTW and AIPTW will use the same estimates of g, so we can collapse them
  runs$method[runs$method %in% c("pweight","aiptw")] <- "pweight_aiptw"

  runs <- runs[!duplicated(runs),]
  
  # If there are weights, and the method is tmle, then the g model estimation should include those weights.
  # If there are weights, but the method is iptw or aiptw, then the g model does not need to take these into account-- these are taken into account in the analysis phase. In this case, set all weights to 1. 
  
  # For datasets that were not pre-processed with subclassification:
  if (is.na(subclass[1])) {
    
      for (i in 1:nrow(runs)) {
        temp_run <- run[i,]
        
        if (temp_run$method == "pweight_aiptw" { 
          temp_g <- est_g(an_data = an_data, match_weights = rep(1,nrow(an_data)), samp_weights = rep(1,nrow(an_data)), 
                          covs = temp_run$covs[[1]], # for iptw and aiptw, don't need weights in estimation of propensity score; only in estimation phase
                          para = temp_run$pscore_para, link = temp_run$pscore_link, 
                          covform = temp_run$pscore_covform, slfull = temp_run$pscore_slfull, 
                          sldefault = temp_run$pscore_sldefault, sllibs = temp_run$pscore_sllibs[[1]])
          
        } else if (temp_run$method == "tmle") { 
          temp_g <- est_g(an_data = an_data, match_weights = match_weights, samp_weights = samp_weights,
                          covs = temp_run$covs[[1]], 
                          para = temp_run$pscore_para, link = temp_run$pscore_link,
                          covform = temp_run$pscore_covform, slfull = temp_run$pscore_slfull, 
                          sldefault = temp_run$pscore_sldefault, sllibs = temp_run$pscore_sllibs[[1]])
        }
        g_df <- data.frame(temp_g, temp_run, row.names=NULL) 
        if (i ==1) { g <- g_df
        } else { g <- rbind(g, g_df)  }
      }
      g$subclass <- NA
  
  # For datasets pre-processed with subclassification-- need to estimate q within each subclass
  } else {
      nsubclass <- length(unique(subclass))
      
      for (i in 1:nrow(runs)) {
        temp_run <- run[i,]
        
        # Loop through subclasses and generate estimates
        for (s in 1:nsubclass) {
          sub_data <- an_data[subclass == s,]
          sub_match_weights <- match_weights[subclass == s]
          sub_samp_weights <- samp_weights[subclass == s]
          if (temp_run$method == "pweight_aiptw") { 
            temp_est <- est_g(an_data = sub_data, match_weights = rep(1,nrow(an_data)), samp_weights = rep(1,nrow(an_data)),
                              covs = temp_run$covs[[1]], 
                              para = temp_run$pscore_para, link = temp_run$pscore_link, 
                              covform = temp_run$pscore_covform, slfull = temp_run$pscore_slfull, 
                              sldefault = temp_run$pscore_sldefault, sllibs = temp_run$pscore_sllibs[[1]])
          } else if (temp_run$method == "tmle")    { 
            temp_est <- est_g(an_data = sub_data, match_weights = sub_match_weights, samp_weights = sub_samp_weights,
                              covs = temp_run$covs[[1]], 
                              para = temp_run$pscore_para, link = temp_run$pscore_link,
                              covform = temp_run$pscore_covform, slfull = temp_run$pscore_slfull, 
                              sldefault = temp_run$pscore_sldefault, sllibs = temp_run$pscore_sllibs[[1]])
          }
          temp_est$subclass <- s
          if (s ==1 ) { temp_g <- temp_est } else { temp_g <- c(temp_g, temp_est) }
        }
        
        g_df <- data.frame(temp_g, temp_run, row.names=NULL) 
        if (i ==1) { g <- g_df
        } else { g <- rbind(g, g_df)  } 
      }
  }
  
  names(g)[1] <- "g"
  return(g)
}


