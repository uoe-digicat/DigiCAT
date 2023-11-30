standardise_outcome_format <- function(extracted_outcome_results, counterfactual_method, outcome_formula){
  if(extracted_outcome_results[[2]] == "mi" & outcome_formula == "marginal_effects"){ # may need to specify if ME or not
    results_dataframe = extracted_outcome_results[[1]]
    results_dataframe <- results_dataframe[,-c(2, 5, 6)]
    colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    rownames(results_dataframe) <- results_dataframe[,1]  
    results_dataframe <- results_dataframe[,-1]
    
  } else if(extracted_outcome_results[[2]] == "mi" & outcome_formula == "unadjusted"){ # unadjusted
    results_dataframe = as.data.frame(extracted_outcome_results[[1]])
    results_dataframe <- results_dataframe[,-c(4,5)]
    colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    rownames(results_dataframe) <- results_dataframe[,1]  
    results_dataframe <- results_dataframe[,-1]
    results_dataframe <- results_dataframe[2,]
    
    
  } else if(extracted_outcome_results[[2]] == "mi" & outcome_formula == "with_matching_variables"){ # matching variables
    results_dataframe = as.data.frame(extracted_outcome_results[[1]])
    results_dataframe <- results_dataframe[,-c(4,5)]
    colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    rownames(results_dataframe) <- results_dataframe[,1]  
    results_dataframe <- results_dataframe[,-1]
    results_dataframe <- results_dataframe[2,]
    
    
    
  } else if(extracted_outcome_results[[2]] == "cc" & counterfactual_method != "nbp" & outcome_formula == "marginal_effects"){ # and ME
    results_dataframe = as.data.frame(extracted_outcome_results)
    results_dataframe <- results_dataframe[,-c(2,5,7,10)]
    colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    rownames(results_dataframe) <- results_dataframe[,1]  
    results_dataframe <- results_dataframe[,-1]
    
  } else if(extracted_outcome_results[[2]] == "cc" & counterfactual_method != "nbp" & outcome_formula == "unadjusted"){ # 
    results_dataframe = as.data.frame(extracted_outcome_results[[1]])
    results_dataframe <- results_dataframe[,-3]
    colnames(results_dataframe) <- c("Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    results_dataframe <- results_dataframe[2,]
    
    
  } else if(extracted_outcome_results[[2]] == "cc" & counterfactual_method != "nbp" & outcome_formula == "with_matching_variables"){ # 
    results_dataframe = as.data.frame(extracted_outcome_results[[1]])
    results_dataframe <- results_dataframe[,-3]
    colnames(results_dataframe) <- c("Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    results_dataframe <- results_dataframe[2,]
    
    
  } else if(extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "psm" & outcome_formula == "marginal_effects"){
    results_dataframe = as.data.frame(extracted_outcome_results)
    results_dataframe <- results_dataframe[,-c(2,5,7,10)]
    colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    rownames(results_dataframe) <- results_dataframe[,1]  
    results_dataframe <- results_dataframe[,-1]
  }
  else if(extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "iptw" & outcome_formula == "marginal_effects"){
    results_dataframe = as.data.frame(extracted_outcome_results)
    results_dataframe <- results_dataframe[,-c(2,5,7,10)]
    colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    rownames(results_dataframe) <- results_dataframe[,1]  
    results_dataframe <- results_dataframe[,-1]
  }
  else if(extracted_outcome_results[[2]] == "cc" & counterfactual_method == "nbp"){
    results_dataframe = as.data.frame(extracted_outcome_results[[1]][[4]])
    colnames(results_dataframe) <- c("Coefficient Estimate", "Standard Error", "T statistic", "P-value")
    results_dataframe <- results_dataframe[c(1,2),]
    return(results_dataframe)
  }
}
























