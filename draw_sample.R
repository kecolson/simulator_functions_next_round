# draw_sample. R

# Function: draw_sample
# Input: pop (Data frame containing variables named: W*_0, W*_1, A_0, A_1, Y_0, Y_1)
# a single row of designs
# Output: sample (format as pop, except includes new variable for sampling weights)

# Test data
# W1_0 <- rnorm(100,0,1)
# W2_0 <- rbinom(100,1,0.5)
# W3_0 <- rbinom(100,1,1)
# A_1 <- rbinom(100,1,0.4)
# Y_0 <- rnorm(100,1,1)
# Y_1 <- Y_0 + rnorm(100,0.3,0.5)
# pop <- data.frame(W1_0,W2_0,W3_0,A_1,Y_0,Y_1)
# design <- list(sample_size = 80, all_exposed = 0, all_controls = 0)

draw_sample <- function(pop, design) {
  
  # Cohort/cross sectional sampling
  if (design$samp_cohort == T) {
    if (design$all_exposed == 1 & design$all_controls==1) {
      sample <- pop
      sample$samp_weights <- 1
      return(sample)
      
    } else if (design$all_exposed == 0 & design$all_controls == 0) {
      sample <- pop[sample(row.names(pop), design$sample_size),]
      sample$samp_weights <- 1
      return(sample)
      
    } else if (design$all_exposed == 1 & design$all_controls == 0) {
      interv <- pop[pop$A_1==1,]
      ncontrol <- max(design$sample_size - nrow(interv), 0) # Number of controls cannot be negative (i.e. if # treated units is >= sample size)
      all.control <- pop[pop$A_1==0,]
      ncontrol <- min(ncontrol, nrow(all.control) ) # Number of controls to be drawn cannot be more than the number of controls available
      control <- all.control[sample(row.names(all.control), ncontrol),]
      
      interv$samp_weights <- 1
      control$samp_weights <- nrow(all.control) / nrow(control)
      sample <- rbind(interv, control)
      return(sample)
      
    } else if (design$all_exposed == 0 & design$all_controls == 1) {
      control <- pop[pop$A_1==0,]
      ninterv <- max(design$sample_size - nrow(control), 0) # Number of treated units cannot be negative (i.e. if # control units is >= sample size)
      all.interv <- pop[pop$A_1==1,]
      ninterv <- min(ninterv, nrow(all.interv) ) # Number of treated units to be drawn cannot be more than the number of treated units available
      interv <- all.interv[sample(row.names(all.interv), ninterv),]
      
      interv$samp_weights <- nrow(all.interv) / nrow(interv)
      control$samp_weights <- 1
      sample <- rbind(interv, control)
      return(sample)
    }
  }
  
  if (design$samp_cc == T) {
    all.cases    <- pop[pop$Y_1 == 1,]
    all.controls <- pop[pop$Y_1 == 0,]
    cases    <- all.cases   [sample(row.names(all.cases),    design$samp_n_cases),]
    controls <- all.controls[sample(row.names(all.controls), design$samp_n_controls),]
    cases$samp_weights    <- nrow(all.cases)/nrow(cases)
    controls$samp_weights <- nrow(all.controls)/nrow(controls)
    sample <- rbind(cases, controls)
    return(sample)
  }
}


