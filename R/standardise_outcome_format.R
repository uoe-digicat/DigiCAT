standardise_outcome_format <- function(extracted_outcome_results, counterfactual_method, outcome_formula, fitted_model,outcome_type,treatment_variable, ...){
  if(extracted_outcome_results[[2]] == "mi" & outcome_formula == "marginal_effects"){ # ME MI with/without covs
    results_dataframe = extracted_outcome_results[[1]]
    results_dataframe <- results_dataframe[,c("term", "estimate", "std.error", "p.value", "2.5 %", "97.5 %")]
    
    colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    rownames(results_dataframe) <- results_dataframe[,1]  
    return(results_dataframe)
    
  } else if(extracted_outcome_results[[2]] == "mi" & outcome_formula == "unadjusted"){ # unadjusted MI with/without covs
    results_dataframe = as.data.frame(extracted_outcome_results[[1]])
    colnames(results_dataframe) <- c("Term", "Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
   # rownames(results_dataframe) <- results_dataframe[,1]  
    
    if (outcome_type == 'Categorical'){
      results_dataframe <- results_dataframe %>% filter(grepl(treatment_variable, Term, ignore.case = TRUE))
    } else {
      results_dataframe <- results_dataframe[2,]
    }
    
    
    return(results_dataframe)
    
  } else if(extracted_outcome_results[[2]] == "mi" & outcome_formula == "with_matching_variables"){ # adjusted for matching variables with MI with/without covs
    results_dataframe = as.data.frame(extracted_outcome_results[[1]])
    colnames(results_dataframe) <- c("Term", "Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    if (outcome_type == 'Categorical'){
      results_dataframe <- results_dataframe %>% filter(grepl(treatment_variable, Term, ignore.case = TRUE))
    } else {
      results_dataframe <- results_dataframe[2,]
    }
    
    return(results_dataframe)
    
    
  } else if(extracted_outcome_results[[2]] == "cc" & counterfactual_method != "nbp" & outcome_formula == "marginal_effects"){ # ME CCA with/without covs
    results_dataframe = as.data.frame(extracted_outcome_results[[1]])
    if (counterfactual_method != 'cbps'){
      results_dataframe <- results_dataframe[,-c(2,5,7,10)]
      colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
      rownames(results_dataframe) <- results_dataframe[,1]  
      results_dataframe <- results_dataframe[,-1]
    } else {
      results_dataframe <- results_dataframe[,-c(2,5,7,10, 11, 12)]
      colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    }
    return(results_dataframe)
  } else if(extracted_outcome_results[[2]] == "cc" & counterfactual_method != "nbp" & outcome_formula == "unadjusted"){ # unadjusted CCA with/without covs
    results_dataframe = as.data.frame(extracted_outcome_results[[1]])
    results_dataframe <- results_dataframe[,-3]
    colnames(results_dataframe) <- c("Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    if (outcome_type == 'Categorical'){
      results_dataframe <- results_dataframe %>% filter(grepl(treatment_variable, rownames(results_dataframe), ignore.case = TRUE))
      results_dataframe$Term <- rownames(results_dataframe)
    } else {
      results_dataframe <- results_dataframe[2,]
    }
    return(results_dataframe)
    
    
  } else if(extracted_outcome_results[[2]] == "cc" & counterfactual_method != "nbp" & outcome_formula == "with_matching_variables"){# adjusted for matching variables with CCA with/without covs
    results_dataframe = as.data.frame(extracted_outcome_results[[1]])
    results_dataframe <- results_dataframe[,-3]
    colnames(results_dataframe) <- c("Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    
    if (outcome_type == 'Categorical'){
      results_dataframe <- results_dataframe %>% filter(grepl(treatment_variable, rownames(results_dataframe), ignore.case = TRUE))
      results_dataframe$Term <- rownames(results_dataframe)
    } else {
      results_dataframe <- results_dataframe[2,]
    }
    
    return(results_dataframe)
    
    
  } else if(extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "psm" & outcome_formula == "marginal_effects"){
    results_dataframe = as.data.frame(extracted_outcome_results)
    results_dataframe <- results_dataframe[,-c(2,5,7,10)]
    colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    rownames(results_dataframe) <- results_dataframe[,1]  
    results_dataframe <- results_dataframe[,-1]
    return(results_dataframe)
  }
  else if(extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "iptw" & outcome_formula == "marginal_effects"){
    results_dataframe = as.data.frame(extracted_outcome_results)
    results_dataframe <- results_dataframe[,-c(2,5,7,10)]
    colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    rownames(results_dataframe) <- results_dataframe[,1]  
    results_dataframe <- results_dataframe[,-1]
    return(results_dataframe)
  }
  else if(extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "psm" & outcome_formula == "unadjusted"){
    results_dataframe = as.data.frame(extracted_outcome_results[[1]]$coefficients)
    results_dataframe <- results_dataframe[,-3]
    Cis <- confint(fitted_model)
    results_dataframe <- cbind(results_dataframe, Cis)
    colnames(results_dataframe) <- c("Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    results_dataframe <- results_dataframe[2,]
    return(results_dataframe)
  }
  else if(extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "iptw" & outcome_formula == "unadjusted"){
    results_dataframe = as.data.frame(extracted_outcome_results[[1]]$coefficients)
    results_dataframe <- results_dataframe[,-3]
    Cis <- confint(fitted_model)
    results_dataframe <- cbind(results_dataframe, Cis)
    colnames(results_dataframe) <- c("Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    results_dataframe <- results_dataframe[2,]
    return(results_dataframe)
  }
  else if(extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "iptw" & outcome_formula == "with_matching_variables"){
    browser()
    results_dataframe = as.data.frame(extracted_outcome_results[[1]]$coefficients)
    results_dataframe <- results_dataframe[,-3]
    Cis <- confint(fitted_model)
    results_dataframe <- cbind(results_dataframe, Cis)
    colnames(results_dataframe) <- c("Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    results_dataframe <- results_dataframe[2,]
    return(results_dataframe)
  }
  else if(extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "psm" & outcome_formula == "with_matching_variables"){
    browser()
    results_dataframe = as.data.frame(extracted_outcome_results[[1]]$coefficients)
    results_dataframe <- results_dataframe[,-3]
    Cis <- confint(fitted_model)
    results_dataframe <- cbind(results_dataframe, Cis)
    colnames(results_dataframe) <- c("Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    results_dataframe <- results_dataframe[2,]
    return(results_dataframe)
  }
  else if(extracted_outcome_results[[2]] == "cc" & counterfactual_method == "nbp" & outcome_formula == "unadjusted"){
    results_dataframe = extracted_outcome_results[[1]]
    results_dataframe = results_dataframe[,-3]
    colnames(results_dataframe) <- c("Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    results_dataframe <- results_dataframe[-1,]
    return(results_dataframe)
  } else if(extracted_outcome_results[[2]] == "cc" & counterfactual_method == "nbp" & outcome_formula == "with_matching_variables"){
    results_dataframe = extracted_outcome_results[[1]]
    results_dataframe = results_dataframe[,-3]
    colnames(results_dataframe) <- c("Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    results_dataframe <- results_dataframe[2,]
    return(results_dataframe)
  } else if(extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "nbp" & outcome_formula == "unadjusted"){
    results_dataframe = extracted_outcome_results[[1]]
    results_dataframe = results_dataframe[13]
    results_dataframe <- as.data.frame(results_dataframe)
    results_dataframe <- results_dataframe[, -3]
    Cis <- confint(fitted_model)
    results_dataframe <- cbind(results_dataframe, Cis)
    colnames(results_dataframe) <- c("Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    results_dataframe <- results_dataframe[2,]
    return(results_dataframe)
  } else if(extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "nbp" & outcome_formula == "with_matching_variables"){
    results_dataframe = extracted_outcome_results[[1]]
    results_dataframe = results_dataframe[13]
    results_dataframe <- as.data.frame(results_dataframe)
    results_dataframe <- results_dataframe[, -3]
    Cis <- confint(fitted_model)
    results_dataframe <- cbind(results_dataframe, Cis)
    colnames(results_dataframe) <- c("Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    results_dataframe <- results_dataframe[2,]
    return(results_dataframe)
  } 
}
























