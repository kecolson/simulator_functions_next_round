# est_g.R

# Function: est_g
# Inputs: an_data (format same as pop)
#         match_weights (numeric vector, same the number obs as an_data)
#         samp_weights (numeric vector, same the number obs as an_data)
#         covs (list with string vector inside, e.g. c("W1 ","W2","W3"))
#         para (T/F)
#         link (string, e.g. "logit")
#         covform (string, e.g. "mainterm")
#         slfull (0/1)
#         sldefault (0/1)
#         sllibs (list with string vector inside, e.g. c("SL.glm ","SL.step"))
# Output: g (numeric vector, same # obs as an_data)

est_g <- function(an_data, match_weights, samp_weights, covs, para, link, covform, slfull, sldefault, sllibs) {
  
  covs.formula <- paste(covs, collapse = "+")
  ncovs <- length(covs)
  
  n <- nrow(an_data)
  
  # Combine weights. This will be a vector of 1's if no weighting is necessary.
  weights <- match_weights * samp_weights
  
  # Parametric GLM
  if (para == T) {
    
    # Create covariate formulas and transform the data as needed
    # Mainterm covariates
    if (covform == "mainterm") {
      final.covs.formula <- covs.formula
      
    # Mainterm + interactions  
    } else if (covform == "mainterm_inter") {
      final.covs.formula <- paste0("(", covs.formula,")*(",covs.formula,")")
      
    # Quintile covariates  
    } else if (covform == "quintile") {
      for (i in covs) {
        quintiles <- quantile(an_data[[i]], probs = seq(0.2, 1, 0.2))
        for (j in 1:4) {
          q <- as.numeric( an_data[[i]] > quintiles[j] & an_data[[i]] <= quintiles[j+1])
          an_data <- cbind(an_data, q)
        }
        names(an_data)[names(an_data)=="q"] <- paste0(i,"q",2:5)
      }
      final.covs.formula <- paste0(rep(covs,each=4),"q",rep(2:5,ncovs), collapse = " + ")
    
    # Custom covariate formula
    } else {
      final.covs.formula <- covform
    }
    
    # Fit model
    model <- glm(as.formula(paste("A_1~",final.covs.formula)), family = binomial(link = link), data = an_data,
                 weights = weights)
    
    # Make predictions
    g <- predict(model, newdata = an_data, type = 'response')
    
    return(g)

  # Super Learner
  } else {
    
    final.covs.formula <- covs.formula
    
    # Create data frame with default covariate transformations
    
    # First order interactions
    final.covs.formula <- paste0("(", covs.formula,")*(",covs.formula,")")
    Xtransf <- model.matrix(as.formula(paste("~",final.covs.formula)), data = an_data)
    Xtransf <- Xtransf[,colnames(Xtransf)!="(Intercept)"]
    Xtransf <- as.data.frame(Xtransf)
    
    # Quintile dummies
    for (i in covs) {
      quintiles <- quantile(an_data[[i]], probs = seq(0.2,1,0.2) )
      for (k in 1:4) {
        t <- as.numeric(an_data[[i]] > quintiles[k] & an_data[[i]] <= quintiles[k+1] )
        Xtransf <- cbind(Xtransf, t)
      }
    }
    
    # Estimate propensity score
    SL.out <- SuperLearner(Y = an_data$A_1, X = Xtransf, SL.library = sllibs, newX = Xtransf,
                           family='binomial', cvControl=list(V=10), obsWeights = weights)
    g <- ifelse(rep(slfull == 1, nrow(an_data)), SL.out$SL.predict, 
                       SL.out$library.predict[,which.min(SL.out$cvRisk)] )
    return(g)
  }
}

