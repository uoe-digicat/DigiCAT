standardise_outcome_format <- function(extracted_outcome_results, counterfactual_method, fitted_model,...){
  if(extracted_outcome_results$process == "mi"){
    results_dataframe = as.data.frame(extracted_outcome_results)
    results_dataframe <- results_dataframe[,-c(2,5,6,10)]
    colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    rownames(results_dataframe) <- results_dataframe[,1]  
    results_dataframe <- results_dataframe[,-1]
    
  } else if(extracted_outcome_results$process == "cc" & counterfactual_method != "nbp"){
    results_dataframe = as.data.frame(extracted_outcome_results)
    results_dataframe <- results_dataframe[,-c(2,5,7,10)]
    colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    rownames(results_dataframe) <- results_dataframe[,1]  
    results_dataframe <- results_dataframe[,-1]
    
  } else if(extracted_outcome_results$process == "weighting" & counterfactual_method == "psm"){
    results_dataframe = as.data.frame(extracted_outcome_results)
    results_dataframe <- results_dataframe[,-c(2,5,7,10)]
    colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    rownames(results_dataframe) <- results_dataframe[,1]  
    results_dataframe <- results_dataframe[,-1]
  }
  else if(extracted_outcome_results$process == "weighting" & counterfactual_method == "iptw"){
    results_dataframe = as.data.frame(extracted_outcome_results)
    results_dataframe <- results_dataframe[,-c(2,5,7,10)]
    colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower CI (2.5%)", "Upper CI (97.5%)")
    rownames(results_dataframe) <- results_dataframe[,1]  
    results_dataframe <- results_dataframe[,-1]
  }
  else if(extracted_outcome_results$process == "cc" & counterfactual_method == "nbp"){
    results_dataframe = as.data.frame(extracted_outcome_results[[1]][[4]])
    colnames(results_dataframe) <- c("Coefficient Estimate", "Standard Error", "T statistic", "P-value")
    results_dataframe <- results_dataframe[c(1,2),]
    return(results_dataframe)
  }
  else if(extracted_outcome_results$process == "weighting" & counterfactual_method == "nbp"){
    results_dataframe = as.data.frame(extracted_outcome_results[[1]][[13]])
    CIs <- confint(fitted_model)
    results_dataframe <- cbind(results_dataframe, CIs)
    colnames(results_dataframe) <- c("Coefficient Estimate", "Standard Error", "T statistic", "P-value", "Lower CI (2.5%)",
                                     "Upper CI (97.5%)")
    results_dataframe <- results_dataframe[c(1,2),]
    
    return(results_dataframe)
  }
}

























