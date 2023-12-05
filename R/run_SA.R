#### Sensitivity Analysis ####
run_SA <- function(PS_object, balanced_data, missing_method, outcome_variable, SA_type,...){ # sensitivity analysis type
  switch(SA_type,
         rosenbaum_SA = { # currently for continuous outcome
           SA_results = perform_rosenbaum_SA(PS_object, balanced_data, missing_method, outcome_variable,...)
         },
         PS_calibration = { # may require further upload
           SA_results = perform_PS_calibration(PS_object, unmeasured_confounder,...)
           
         },
        # VW_A_formula = {
        #   
        # },
         VW_Evalue = { # typically for binary outcome
           SA_results = perform_VW_Evalue(outcome_results, outcome_type, missing_method,...)
           
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
    SA_result <- rbounds::psens(x = mpairs[, 1], y = mpairs[, 2]) # default Gamma and GammaInc = 6 & 1
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
      
      SA_result <- rbounds::psens(x = mpairs[, 1], y = mpairs[, 2]) # default Gamma and GammaInc = 6 & 1
      
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


VW_Evalue <- function(outcome_results, outcome_type, missing_method,...){
  if(outcome_type == "continuous"){
  estimate <- outcome_results$standardised_format$`Coefficient Estimate`
  sd_est <- outcome_results$standardised_format$`Standard Error`
  upper_bound <- outcome_results$standardised_format$`Upper CI (97.5%)`
  lower_bound <- outcome_results$standardised_format$`Lower CI (2.5%)`
  
  SA_result <- evalue(OLS(estimate, sd_est))
  }
  else if(outcome_type == "binary"){
    estimate <- outcome_results$standardised_format$`Coefficient Estimate`
    upper_bound <- outcome_results$standardised_format$`Upper CI (97.5%)`
    lower_bound <- outcome_results$standardised_format$`Lower CI (2.5%)`
    
    SA_result <- evalue(RR(estimate), lo = lower_bound, hi = upper_bound)
  }
  return(SA_result)
}















