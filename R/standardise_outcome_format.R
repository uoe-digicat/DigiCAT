standardise_outcome_format <- function(extracted_outcome_results){
  if(extracted_outcome_results$process == "mi"){
    results_dataframe = as.data.frame(extracted_outcome_results)
    results_dataframe <- results_dataframe[,-c(5,7)]
    colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "T statistic", "P-value")
    rownames(results_dataframe) <- results_dataframe[,1]  
    results_dataframe <- results_dataframe[,-1]
    return(results_dataframe)
  } else if(extracted_outcome_results$process == "cc"){
    results_dataframe = as.data.frame(extracted_outcome_results[[1]][[4]])
    colnames(results_dataframe) <- c("Coefficient Estimate", "Standard Error", "T statistic", "P-value")
    return(results_dataframe)
    
  }
}
