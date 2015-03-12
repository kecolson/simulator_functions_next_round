# study.R

# Function: study
# Input: pop (Data frame containing variables named: W*_0, W*_1, A_0, A_1, Y_0, Y_1)
#     a single row of the data frame designs (as a list)
#     a single row of the data frame matches (as a list)
# Output: a list containing:
#           sample (format as pop. This is the unmatched sample even if matching was conducted-- will use this for bias-corrected matching analysis in which matching happens in the analysis phase)
#           an_data (format as pop), 
#           subclass (numeric vector, same # obs as an_data),
#           match_weights (numeric vector, same # obs as an_data),
#           samp_weights (numeric vector, same # obs as an_data),
#           match_input (single row of matches, as a list)

study <- function(pop, design, match_input) {
  
  # Draw sample 
  sample <- draw_sample(pop, design)
  samp_weights <- sample$samp_weights # This will be vector of 1's if the sample was representative
  
  # Match if desired, otherwise return sample
  if (match_input$match == 1) {
    match_results <- match(sample, match_input)
    return(list(sample = sample, 
                an_data = match_results[[1]], 
                subclass = match_results[[2]],
                match_weights = match_results[[3]], 
                samp_weights = samp_weights, 
                match_input = match_results[[4]]))
  } else {
    return(list(sample = sample, 
                an_data = sample, 
                subclass = rep(NA,nrow(sample)),
                match_weights = rep(1,nrow(sample)),
                samp_weights = samp_weights,
                match_input = NA))
  }
}
