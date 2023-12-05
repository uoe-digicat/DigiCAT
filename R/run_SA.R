#### Sensitivity Analysis ####
run_SA <- function(PS_object, balanced_data, missing_method, outcome_variable, SA_type,...){ # sensitivity analysis type
  switch(SA_type,
         rosenbaum_SA = {
           SA_results = perform_rosenbaum_SA(PS_object, balanced_data, missing_method, outcome_variable,...)
         },
         PS_calibration = {
           
         },
         VW_A_formula = {
           
         },
         E_value = {
           
         },
         stop("Need a valid method to run the sensitivity analysis")
  )
  return(SA_results)
  
}


perform_rosenbaum_SA <- function(PS_object, balanced_data, missing_method, outcome_variable, ...) {
  if (missing_method == "complete") {
    mpairs <- cbind(
      PS_object$missingness_treated_dataset[row.names(balanced_data$match.matrix), outcome_variable, drop = FALSE],
      PS_object$missingness_treated_dataset[balanced_data$match.matrix, outcome_variable, drop = FALSE]
    )
    SA_result <- rbounds::psens(x = mpairs[, 1], y = mpairs[, 2])
  } else if (missing_method == "mi") {
    matchit_list <- balanced_data$models
    
    imputed_data <- PS_object$missingness_treated_dataset
    
    avg_upper_bounds <- numeric(length = PS_object$missingness_treated_dataset$m)  
    
    for (i in seq_along(matchit_list)) {
      match_matrix <- matchit_list[[i]]$match.matrix
      
      comp <- complete(imputed_data, "all")
      
      mpairs <- cbind(
        comp[[i]][row.names(match_matrix), outcome_variable, drop = FALSE],
        comp[[i]][match_matrix, outcome_variable, drop = FALSE]
      )
      
      SA_result <- rbounds::psens(x = mpairs[, 1], y = mpairs[, 2])
      
      # Average Upper bound values across rows for each set
      avg_upper_bounds <- avg_upper_bounds + SA_result$bounds[, "Upper bound", drop = FALSE]
    }

    # Create a summary with the pooled values
    avg_upper_bounds <- avg_upper_bounds / length(matchit_list)

    # Replace Upper bound values with pooled values
    SA_result$bounds$`Upper bound` <- avg_upper_bounds
  }
  
  return(SA_result)
}




















