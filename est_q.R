# est_q.R

# NOTE: est_q is assuming Y is continuous for now. Need to figure out how to transfer information from pop gen phase on dist to here. 

# Function: est_q
# Inputs: an_data (this is the full sample or the matched data set, depending on the method)
#         match_weights (numeric vector, same the number obs as an_data)
#         samp_weights (numeric vector, same the number obs as an_data)
#         covs (list with string vector inside, e.g. c("W1 ","W2","W3"))
#         para (T/F)
#         dist (string, e.g. "gaus")
#         link (string, e.g. "logit")
#         covform (string, e.g. "mainterm")
#         slfull (0/1)
#         sldefault (0/1)
#         sllibs (list with string vector inside, e.g. c("SL.glm ","SL.step"))
# Output: q (numeric vector, 3x # obs as an_data--1st n obs are predictions for observed data, 2nd n obs are predictions when A_1=1 for everyone and 3rd n obs are predictions when A_1=0 for everyone)

est_q <- function(an_data, match_weights, samp_weights, covs, para, dist, link, covform, slfull, sldefault, sllibs) {
  
  covs <- c("A_1", covs)
  covs.formula <- paste(covs, collapse = "+")
  ncovs <- length(covs)
  
  # Create dataset with 3*n observations to make predictions for the observed data, the data when A_1 = 1 for everyone, and the data when A_1 = 0 for everyone
  n <- nrow(an_data)
  tx <- ct <- an_data
  tx$A_1 <- 1
  ct$A_1 <- 0
  an_data <- rbind(an_data, tx, ct)
  
  # Combine weights. This will be a vector of 1's if no weighting is necessary.
  weights <- match_weights * samp_weights
  
  # Parametric
  if (para == T) {
    
    # Distribution
    if        (family == "gaus") { mod.family <- gaussian(link = link) 
    } else if (family == "bin")  { mod.family <- binomial(link = link) 
    } else if (family == "gam")  { mod.family <- Gamma(link = link)
    } else if (family == "pois") { mod.family <- poisson(link = link) }
   
    # Create covariate formulas and transform data as needed
    # Mainterm covariates
    if (covform == "mainterm") {
      final.covs.formula <- covs.formula
      
    # Mainterm + interactions  
    } else if (covform == "mainterm_inter") {
      covs <- covs[covs!="A_1"] # Do not square the exposure, but be sure to add it to the covs formula at the end
      ncovs <- length(covs)
      covs.formula <- paste(covs, collapse = "+")
      final.covs.formula <- paste0("A_1 + A_1*(",covs.formula,") + (", covs.formula,")*(",covs.formula,")")
      
    # Quintile covariates  
    } else if (covform == "quintile") {
      covs <- covs[covs!="A_1"] # If covariates are being quintiled, do not quintile the exposure, but be sure to add it to the covs formula at the end
      ncovs <- length(covs)
      for (i in covs) {
        quintiles <- quantile(an_data[[i]], probs = seq(0.2, 1, 0.2))
        for (j in 1:4) {
          q <- as.numeric( an_data[[i]] > quintiles[j] & an_data[[i]] <= quintiles[j+1])
          an_data <- cbind(an_data, q)
        }
        names(an_data)[names(an_data)=="q"] <- paste0(i,"q",2:5)
      }
      final.covs.formula <- paste0(rep(covs,each=4),"q",rep(2:5,ncovs), collapse = " + ")
      final.covs.formula <- paste("A_1 +",final.covs.formula)
    
    # Custom covariate form
    } else {
      final.covs.formula <- covform # This custom provided formula from the user is assumed to include A_1
    }
    
    # Fit model just on observed data
    model <- glm(as.formula(paste("outcome~",final.covs.formula)), family = mod.family, data = an_data[1:n,], weights = weights)
    
    # Make predictions for observed data, if A_1 == 1 for everyone, and if A_1 == 0 for everyone
    q <- predict(model, newdata = an_data, type = 'response')
    
    return(q)
    
    # Super Learner
  } else {
    
    # Create data frame with default covariate transformations

    # First order interactions (included with A_1)
    covs <- covs[covs!="A_1"] 
    ncovs <- length(covs)
    covs.formula <- paste(covs, collapse = "+")
    final.covs.formula <- paste0("A_1 + A_1*(",covs.formula,") + (", covs.formula,")*(",covs.formula,")")
    Xtransf <- model.matrix(as.formula(paste("~",final.covs.formula)), data = an_data)
    Xtransf <- Xtransf[,colnames(Xtransf)!="(Intercept)"]
    
    # Quintile dummies
    for (i in covs) {
      quintiles <- quantile(an_data[[i]], probs = seq(0.2,1,0.2) )
      for (k in 1:4) {
        t <- as.numeric(an_data[[i]] > quintiles[k] & an_data[[i]] <= quintiles[k+1] )
        Xtransf <- cbind(Xtransf, t)
      }
    }
    Xtransf <- as.data.frame(Xtransf)
    
    # Estimate propensity score
    SL.out <- SuperLearner(Y = an_data$outcome[1:n], X = Xtransf[1:n,], SL.library = sllibs, newX = Xtransf,
                           family = 'gaussian', cvControl=list(V=10), obsWeights = weights)
    q <- ifelse(rep(slfull == 1, nrow(an_data)), SL.out$SL.predict, 
                SL.out$library.predict[,which.min(SL.out$cvRisk)] )
    return(q)
  }
}
