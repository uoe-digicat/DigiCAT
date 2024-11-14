#' Sensitivity Analysis
#' @import rbounds
#' @import EValue

run_sensitivity <- function(PS_object, balanced_data, missing_method, outcome_variable, sensitivity_type, 
                            outcome_results, outcome_type,...) { 
  # sensitivity analysis type
  switch(sensitivity_type,
         rosenbaum_sensitivity = { #
           sensitivity_results = perform_rosenbaum_sensitivity(PS_object, balanced_data, missing_method, outcome_variable, ...)
         },
         PS_calibration = { # may require further upload
           sensitivity_results = perform_PS_calibration(PS_object, unmeasured_confounder, ...)
         },
         VW_A_formula = { # may require further tuning/checks
           sensitivity_results = perform_VW_A_formula(PS_object, balanced_data, missing_method, sensitivity_parameter, ...)
         },
         VW_Evalue = { # 
           sensitivity_results = perform_VW_Evalue(outcome_results, outcome_type, outcome_variable, missing_method, ...)
         },
         stop("Need a valid method to run the sensitivity analysis")
  )
  return(sensitivity_results)
}

perform_rosenbaum_sensitivity <- function(PS_object, balanced_data, missing_method, outcome_variable, ...) {
  if (missing_method == "complete") {
    mpairs <- cbind(
      PS_object$missingness_treated_dataset[row.names(balanced_data$match.matrix), outcome_variable, drop = FALSE],
      PS_object$missingness_treated_dataset[balanced_data$match.matrix, outcome_variable, drop = FALSE]
    )
    
    mpairs <- na.omit(mpairs)
    if (nrow(mpairs) == 0) {
      warning("No valid pairs after removing NA values.")
      return(NULL)
    }
    
    sensitivity_result <- rbounds::psens(x = mpairs[, 1], y = mpairs[, 2]) # default Gamma and GammaInc = 6 & 1
  } else if (missing_method == "mi") {
   # browser()
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
      
      sensitivity_result <- rbounds::psens(x = mpairs[, 1], y = mpairs[, 2]) # default Gamma and GammaInc = 6 & 1
      
      # Average Upper bound values across rows for each set
      avg_upper_bounds <- avg_upper_bounds + sensitivity_result$bounds[, "Upper bound", drop = FALSE]
    }
    
    # Create a summary with the pooled values
    avg_upper_bounds <- avg_upper_bounds / length(matchit_list)
    
    # Replace Upper bound values with pooled values
    sensitivity_result$bounds$`Upper bound` <- avg_upper_bounds
    
  } else if (missing_method == "weighting") {
    mpairs <- cbind(
      PS_object$missingness_treated_dataset$variables[row.names(balanced_data$match.matrix), outcome_variable, drop = FALSE],
      PS_object$missingness_treated_dataset$variables[balanced_data$match.matrix, outcome_variable, drop = FALSE]
    )
    
    # Remove rows with NA values
    mpairs <- mpairs[complete.cases(mpairs), ]
    
    # Check if there are still valid pairs after removing NAs
    if (nrow(mpairs) > 0) {
      sensitivity_result <- rbounds::psens(x = mpairs[, 1], y = mpairs[, 2])
    } else {
      warning("No valid pairs for sensitivity analysis after removing NAs.")
      return(NULL)
    }
  }
  
  return(sensitivity_result)
}

perform_VW_Evalue <- function(outcome_results, outcome_type, outcome_variable, missing_method, use_ci = FALSE, ...) {
  #browser()
  # Check if confidence intervals should be used
  if (use_ci) {
    estimate <- outcome_results$standardised_format$`Coefficient Estimate`
    upper_bound <- outcome_results$standardised_format$`Upper CI (97.5%)`
    lower_bound <- outcome_results$standardised_format$`Lower CI (2.5%)`
    se <- outcome_results$standardised_format$`Standard Error`
    sd <- sd(outcome_results$extracted_balanced_data[[1]][[outcome_variable]])
    
    if (outcome_type == "continuous") {
       sensitivity_result <- evalues.OLS(estimate, sd = sd, se = se, lo = lower_bound, hi = upper_bound)  # for continuous outcome
      
      } else if (outcome_type == "binary") {
      sensitivity_result <- evalues.OR(estimate, lo = lower_bound, hi = upper_bound)  # for binary outcome
    }
    
  } else {
    # Calculate E-value using the point estimate
    estimate <- outcome_results$standardised_format$`Coefficient Estimate`
    sd <- sd(outcome_results$extracted_balanced_data[[1]][[outcome_variable]])
    
    if (outcome_type == "continuous") {
      sensitivity_result <- evalues.OLS(estimate, sd = sd)  # for continuous outcome
    } else if (outcome_type == "binary") {
      sensitivity_result <- evalues.OR(estimate)  # for binary outcome
    }
  }
  return(sensitivity_result)
}
