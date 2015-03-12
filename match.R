# match.R

# Function: match
# Input: sample (format as pop)
#        match_input: a single row of matches (as a list)
# Output: a list containing:
#   an_data (format as pop)
#   subclass (numeric vector, same # obs as an_data),
#   weights (numeric vector, same # obs as an_data),
#   match_input: a single row of matches (as a list)
#   m_out: the results of the matching procedure


# Test input
# results <- NULL
# W1_0 <- rnorm(100,0,1)
# W2_0 <- rbinom(100,1,0.5)
# W3_0 <- rbinom(100,1,1)
# A_1 <- rbinom(100,1,0.4)
# Y_0 <- rnorm(100,1,1)
# Y_1 <- Y_0 + rnorm(100,0.3,0.5)
# sample <- data.frame(W1_0,W2_0,W3_0,A_1,Y_0,Y_1)
# match_input <- expand.grid(estimand = "att", match = 1, 
#                            match_covs = list(c("W1_0","W2_0","W3_0")), match_exact = F, match_distance = "pscore", match_method = "opt", 
#                            match_nonexact_ratio = NA, match_replace = NA, match_firstexact_covs = list(NA),
#                            match_sub_pscore_classes = NA,
#                            match_discard = 0, match_discard_reest = NA,
#                            match_pscore_para = F, match_pscore_link = NA, match_pscore_backtrans = NA, match_pscore_covform = NA,
#                            match_pscore_slfull = 1, match_sldefault = 1, match_pscore_sllibs = list(c("SL.glm", "SL.step")) )
# match_input <- as.list(match_input)

match <- function(sample, match_input) {
  
  # Create information on covariates that will be used for all methods
  covs.formula <- paste(match_input$match_covs[[1]], collapse = "+")
  ncovs <- length(match_input$match_covs[[1]])
  
  # Exact matching
  if (match_input$match_exact == T) {
    print("Exact")
    m.out <- matchit(as.formula(paste("A_1 ~",covs.formula)), data = sample, method = "exact")  
    an_data <- match.data(m.out)
    return(list(an_data = an_data, subclass = rep(NA,nrow(an_data)), weights = an_data$weights, match_input = match_input, m_out = m.out))
  
  # Non-exact matching
  } else if (match_input$match_exact == F) {
    print("Nonexact")
    
    # Define inputs for MatchIt
    
    # 1. Discard and discard reestimate
    discard <- c("none","hull.both","both","hull.control","control","hull.treat","treat")[as.numeric(match_input$match_discard) + 1]
    if (is.na(match_input$match_discard)) discard <- "none"
    discard_reestimate <- ifelse(is.na(match_input$match_discard_reest), FALSE, match_input$match_discard_reest)
      
    # 2. The covariate form
        # 2a. For non-propensity score methods  
        if (match_input$match_distance != "pscore") {
          final.covs.formula <- covs.formula
        
        # 2b. For propensity score methods
        } else {
            # 2b.i.  For SuperLearner propensity score models
            if (match_input$match_pscore_para == F) {
                final.covs.formula <- covs.formula # This will be ignored because we are providing it with SL pscore estimates in the "distance" option
              
            # 2b.ii. For parametric propensity score models  
            } else {
                if (match_input$match_pscore_covform == "mainterm") {
                  final.covs.formula <- covs.formula
                } else if (match_input$match_pscore_covform == "mainterm_inter") {
                  final.covs.formula <- paste(paste0("I(",rep(match_input$match_covs[[1]], ncovs), "*", 
                                                    rep(match_input$match_covs[[1]], each = ncovs),")"), collapse = " + ")
                  final.covs.formula <- paste0(covs.formula," + ", final.covs.formula)
                } else if (match_input$match_pscore_covform == "quintile") {
                  for (i in match_input$match_covs[[1]]) {
                    quintiles <- quantile(sample[[i]], probs = seq(0.2, 1, 0.2))
                    for (j in 1:4) {
                      q <- as.numeric( sample[[i]] > quintiles[j] & sample[[i]] <= quintiles[j+1])
                      sample <- cbind(sample, q)
                    }
                    names(sample)[names(sample)=="q"] <- paste0(i,"q",2:5)
                  }
                  final.covs.formula <- paste0(rep(match_input$match_covs[[1]],each=4),"q",rep(2:5,ncovs), collapse = " + ")
                } else { # this is custom input covariate formula
                  final.covs.formula <- match_input$match_pscore_covform
                }
            }
        }
          
          
      # 3. The distance metric 
          # 3a. For non-propensity score methods
          if (match_input$match_distance != "pscore") {
            if        (match_input$match_distance == "mahal") { distance <- "mahalanobis"
            } else if (match_input$match_distance == "class") { distance <- "rpart"
            } else if (match_input$match_distance == "nnet" ) { distance <- "nnet" }
          
          # 3b. For propensity score methods
          } else {
              # 3b.i.  For SuperLearner propensity score models
              if (match_input$match_pscore_para == F) {           
                  # Create data frame with default covariate transformations
                  Xtransf <- sample[,match_input$match_covs[[1]]] 
                  for (i in 1:length(match_input$match_covs[[1]])) {
                    # First order interactions
                    for (j in 1:length(match_input$match_covs[[1]])) {
                      t <- sample[,match_input$match_covs[[1]][i]] * sample[,match_input$match_covs[[1]][j]] 
                      Xtransf <- cbind(Xtransf, t)
                    }
                    # Quintile dummies
                    quintiles <- quantile(sample[,match_input$match_covs[[1]][i]], probs = seq(0.2,1,0.2) )
                    for (k in 1:4) {
                      t <- as.numeric(sample[,match_input$match_covs[[1]][i]] > quintiles[k] & sample[,match_input$match_covs[[1]][i]] <= quintiles[k+1] )
                      Xtransf <- cbind(Xtransf, t)
                    }
                  }
                  # Estimate propensity score
                  SL.out <- SuperLearner(Y = sample$A_1, X = Xtransf, SL.library = match_input$match_pscore_sllibs[[1]], 
                                         family='binomial', cvControl=list(V=10))
                  distance <- ifelse(rep(match_input$match_pscore_slfull == 1, nrow(sample)), SL.out$SL.predict, 
                                     SL.out$library.predict[,which.min(SL.out$cvRisk)] )
              
              # 3b.ii. For parametric propensity score models
              } else {
                  if (match_input$match_pscore_backtrans == F) { distance <- match_input$match_pscore_link
                  } else { distance <- paste0("linear.",match_input$match_pscore_link) }
              }
          }
          
    # Conduct matching with options specific to each method
    
    # Nearest neighbor
    if (match_input$match_method == "nn") {
      exact <- NULL; replace <- F; ratio <- 1
      if (!is.null(match_input$match_firstexact_covs[[1]]) && !is.na(match_input$match_firstexact_covs[[1]])) exact   <- match_input$match_firstexact_covs[[1]]
      if (!is.null(match_input$match_replace[[1]])         && !is.na(match_input$match_replace))              replace <- match_input$match_replace
      if (!is.null(match_input$match_nonexact_ratio[[1]])  && !is.na(match_input$match_nonexact_ratio))       ratio   <- match_input$match_nonexact_ratio
      
      m.out <- matchit(as.formula(paste("A_1 ~",final.covs.formula)), data = sample, method = "nearest",
                       distance = distance,
                       discard = discard,
                       reestimate = discard_reestimate,
                       m.order = "random",
                       replace = replace,
                       ratio = ratio,
                       exact = exact)
      an_data <- match.data(m.out)
      return(list(an_data = an_data, subclass = rep(NA,nrow(an_data)), weights = an_data$weights, match_input = match_input, m_out = m.out))
      
    # Optimal
    } else if (match_input$match_method == "opt") {
      # Can only handle cases with more control units than treated right now. If >=50% treated, just return the sample
      if (sum(sample$A_1)/nrow(sample) >=0.5) {
        return(list(an_data = sample, subclass = rep(NA,nrow(sample)), weights = rep(NA,nrow(sample)), match_input = match_input, m_out = NA))
      }
      m.out <- matchit(as.formula(paste("A_1 ~",final.covs.formula)), data = sample, method = "optimal",
                       distance = distance,
                       discard = discard,
                       reestimate = discard_reestimate)
      an_data <- match.data(m.out)
      return(list(an_data = an_data, subclass = rep(NA,nrow(an_data)), weights = an_data$weights, match_input = match_input, m_out = m.out))
    
    # Genetic
    } else if (match_input$match_method == "gen") {
      ratio <- 1
      if (!is.null(match_input$match_nonexact_ratio) && !is.na(match_input$match_nonexact_ratio)) ratio <- match_input$match_nonexact_ratio
      m.out <- matchit(as.formula(paste("A_1 ~",final.covs.formula)), data = sample, method = "genetic",
                       distance = distance,
                       discard = discard,
                       reestimate = discard_reestimate,
                       ratio = ratio)
      an_data <- match.data(m.out)
      return(list(an_data = an_data, subclass = rep(NA,nrow(an_data)), weights = an_data$weights, match_input = match_input, m_out = m.out))
    
    # Subclassification  
    } else if (match_input$match_method == "sub") {
      sub.by <- ifelse(match_input$estimand == "att", "treat", "all") # Use "treat" for the ATT and "all" for the ATE
      m.out <- matchit(as.formula(paste("A_1 ~",final.covs.formula)), data = sample, method = "subclass",
                       subclass = as.numeric(match_input$match_sub_pscore_classes),
                       sub.by = sub.by,
                       distance = distance,
                       discard = discard,
                       reestimate = discard_reestimate)
      an_data <- match.data(m.out)
      return(list(an_data = an_data, subclass = an_data$subclass, weights = an_data$weights, match_input = match_input, m_out = m.out))
    
    # Full matching  
    } else if (match_input$match_method == "full") {
      m.out <- matchit(as.formula(paste("A_1 ~",final.covs.formula)), data = sample, method = "full",
                       distance = distance,
                       discard = discard,
                       reestimate = discard_reestimate)
      an_data <- match.data(m.out)
      return(list(an_data = an_data, subclass = rep(NA,nrow(an_data)), weights = an_data$weights, match_input = match_input, m_out = m.out))
        # Not passing subclass term because we will not use it in the analysis phase
    } else {
      return(list(an_data = sample, subclass = rep(NA,nrow(sample)), weights = rep(NA,nrow(sample)), match_input = match_input, m_out = NA))
    }
  }
}

# results <- match(sample, match_input)
# head(results[[1]])
# summary(results[[1]])
# dim(results[[1]])
# head(results[[2]])
# head(results[[3]])
# results[[4]]

