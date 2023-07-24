extract_balanced_data <- function(balanced_data, psmodel_obj){
  
  if( class(balanced_data)=="mimids") { 
   extracted_balanced_data = MatchThem::complete(balanced_data, "all", all = FALSE) 
   return(list(extracted_balanced_data, process = "mi_psm"))
   
  } else if ( class(balanced_data)=="wimids"){
    extracted_balanced_data = MatchThem::complete(balanced_data, "all", all = FALSE) 
    return(list(extracted_balanced_data, process = "mi_iptw"))
    
  } else if ( class(balanced_data)=="matchit"){
    extracted_balanced_data = match.data(balanced_data)
    return(list(extracted_balanced_data, process = "cc_psm"))
    
  } else if ( class(balanced_data)=="weightit"){
    psmodel_obj$missingness_treated_dataset = cbind(psmodel_obj$missingness_treated_dataset,balanced_data$weights)
    colnames(psmodel_obj$missingness_treated_dataset)[colnames(psmodel_obj$missingness_treated_dataset) == "balanced_data$weights"] <- "weights"
    return(list(psmodel_obj$missingness_treated_dataset, process = "cc_iptw"))

  }
}