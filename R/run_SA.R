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
    SA_results <- rbounds::psens(x = mpairs[, 1], y = mpairs[, 2])
  } else if (missing_method == "mi") {
    matchit_list <- balanced_data$models
    
    imputed_data <- PS_object$missingness_treated_dataset
    
    SA_results <- vector("list", length = length(matchit_list))
    
    for (i in seq_along(matchit_list)) {
      match_matrix <- matchit_list[[i]]$match.matrix
      
      comp <- complete(imputed_data, "all")
      
      mpairs <- cbind(
        comp[[i]][row.names(match_matrix), outcome_variable, drop = FALSE],
        comp[[i]][match_matrix, outcome_variable, drop = FALSE]
      )
      
      SA_results[[i]] <- rbounds::psens(x = mpairs[, 1], y = mpairs[, 2])
    }
    # SA_results <- mean(sapply(SA_results, function(sa_result) sa_result$bounds[[3]])) # average
    # preferably iterate to provide output averaged across Gamma rows to mirror that of CCA 
    
  }
  
  return(SA_results)
}




