extract_outcome_results <- function(fitted_model, missing_method, outcome_type, ...){
  if("comparisons" %in% class(fitted_model) & missing_method == "weighting"){ # weighting ME
    
    return(list(extracted_outcome_results = fitted_model, process = "weighting"))
    
  }else if("comparisons" %in% class(fitted_model) & missing_method == "complete"){ # complete ME

    return(list(extracted_outcome_results = fitted_model, process = "cc"))

  }
  else if("mipo" %in% class(fitted_model) & missing_method == "mi"){ # MI ME
    extracted_outcome_results = summary(fitted_model, conf.int = TRUE)
    return(list(extracted_outcome_results, process = "mi"))
    
  } else if("list" %in% class(fitted_model) & missing_method == "mi"){
    
    if(outcome_type == 'categorical'){
      df_residuals <- sapply(fitted_model, function(model) {
             if (!is.null(df.residual(model$fit))) {
                   return(df.residual(model$fit))
               } else {
                     warning("Degrees of freedom for residuals not available.")
                     return(NA)
                 }
         })
    } else {
      
      df_residuals <- sapply(fitted_model, function(model) {
        if (!is.null(model$df.residual)) {
          return(model$df.residual)
        } else {
          warning("Degrees of freedom for residuals not available.")
          return(NA)
        }
      })
      
    }
    
   
    mean_df <- mean(df_residuals, na.rm = TRUE)
    total_variance <- var(df_residuals, na.rm = TRUE)
    between_imputation_variance <- mean((df_residuals - mean_df)^2, na.rm = TRUE)
    within_imputation_variance <- total_variance - between_imputation_variance
    df_combined <- mean_df + (between_imputation_variance / (within_imputation_variance + 1))
    combined_results <- MIcombine(fitted_model)
    standard_errors <- sqrt(diag(vcov(combined_results)))
    t_stat <- coef(combined_results) / standard_errors
    p_value <- 2 * (1 - pt(abs(t_stat), df = df_combined))
    coefficients <- coef(combined_results)
    conf_int <- confint(combined_results)
    
    # Create a data frame with the extracted information
    extracted_outcome_results <- data.frame(
      Term = names(coefficients),
      Coefficient = coefficients,
      StandardError = standard_errors,
      PValue = p_value,
      LowerCI = conf_int[, 1],
      UpperCI = conf_int[, 2]
    )
    
    return(list(extracted_outcome_results, process = "mi"))
    
  }
  else if(("lm" %in% class(fitted_model) | "svy_vglm" %in% class(fitted_model)) & missing_method == "complete"){ # LM no ME, complete
    
    if ("lm" %in% class(fitted_model)){
      extracted_outcome_results =dplyr::bind_cols(data.frame(coef(summary(fitted_model))), data.frame(confint(fitted_model)))
    } else { # Categorical outcome case
      extracted_outcome_results =dplyr::bind_cols(data.frame(summary(fitted_model)$coeftable), data.frame(confint(fitted_model)))
    }
    return(list(extracted_outcome_results, process = "cc"))
    
  }else if("svyglm" %in% class(fitted_model) & missing_method == "weighting"){
    extracted_outcome_results = summary(fitted_model)
    return(list(extracted_outcome_results, process = "weighting"))
    
  } else if("comparisons" %in% class(fitted_model[[1]]) & missing_method == "mi"){
    return(summary(fitted_model, conf.int = T))
  }
} 



























