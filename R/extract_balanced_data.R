extract_balanced_data <- function(balanced_data, psmodel_obj, missing_method = NULL,
                                  weighting_variable = NULL, counterfactual_method, treatment_variable,
                                  cluster_variable = NULL, strata_variable = NULL,...){
  

  if( "mimids" %in% class(balanced_data)) { 
    extracted_balanced_data = MatchThem::complete(balanced_data, "all", all = FALSE) 
    return(list(extracted_balanced_data, process = "mi_psm"))
    
  } else if ( "wimids" %in% class(balanced_data) & counterfactual_method == "iptw"){
    extracted_balanced_data = MatchThem::complete(balanced_data, "all", all = FALSE) 
    return(list(extracted_balanced_data, process = "mi_iptw"))
    
  } else if ( "wimids" %in% class(balanced_data) & counterfactual_method == "cbps"){
    extracted_balanced_data = MatchThem::complete(balanced_data, "all", all = FALSE) 
    return(list(extracted_balanced_data, process = "mi_cbps"))
    
   
  }else if ( "matchit" %in% class(balanced_data) & missing_method == "complete"){
    extracted_balanced_data = match.data(balanced_data)
    return(list(extracted_balanced_data, process = "cc_psm"))
    
    
  } else if(missing_method =="weighting" & "matchit" %in% class(balanced_data)){
    extracted_balanced_data = match.data(balanced_data)
    
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
    } else {
      cluster_formula <- as.formula("~1")
    }
    
    if (!is.null(weighting_variable)) {
      weighting_formula <- as.formula(paste("~ as.numeric(as.character(", weighting_variable, "))* weights"))
    } else {
      weighting_formula <- as.formula("~ weights")  
    }
    
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~ as.numeric(as.character(", strata_variable, "))"))
    } else {
      strata_formula <- NULL
    }
    
    extracted_balanced_design <- svydesign(ids = ~subclass,
                                weights = weighting_formula,
                                strata = strata_formula,
                                data = extracted_balanced_data)
    
    
    extracted_balanced_data = extracted_balanced_design
    return(list(extracted_balanced_data, process = "weighting_psm"))
    
  } else if ( "weightit" %in% class(balanced_data) & missing_method == "complete" & counterfactual_method == "iptw"){
    psmodel_obj$missingness_treated_dataset = cbind(psmodel_obj$missingness_treated_dataset,balanced_data$weights)
    colnames(psmodel_obj$missingness_treated_dataset)[colnames(psmodel_obj$missingness_treated_dataset) == "balanced_data$weights"] <- "weights"
    return(list(psmodel_obj$missingness_treated_dataset, process = "cc_iptw"))
    
    
  } else if(missing_method=="weighting" & "weightit" %in% class(balanced_data)){
    survey_data = psmodel_obj$estimated_propensity_model$survey.design$variables
    survey_data = cbind(survey_data,balanced_data$weights)
    colnames(survey_data)[colnames(survey_data) == "balanced_data$weights"] <- "weights"
    
    
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~ as.numeric(as.character(", cluster_variable, "))"))
    } else {
      cluster_formula <- as.formula("~1")
    }
    
    if (!is.null(weighting_variable)) {
      weighting_formula <- as.formula(paste("~ as.numeric(as.character(", weighting_variable, ")) * weights"))
    } else {
      weighting_formula <- as.formula("~ weights")  # Replace "weights" with the appropriate variable
    }
    
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~ as.numeric(as.character(", strata_variable, "))"))
    } else {
      strata_formula <- NULL
    }
    
    extracted_balanced_design <- svydesign(ids = cluster_formula,
                                           weights = weighting_formula,
                                           strata = strata_formula,
                                           data = survey_data)
    
    
    extracted_balanced_data = extracted_balanced_design
    
    return(list(extracted_balanced_data, process = "weighting_iptw"))
    
  } else if(counterfactual_method == "nbp" & missing_method == "complete"){
    extracted_balanced_data = balanced_data
    return(list(extracted_balanced_data, process = "cc_nbp"))
  }
  else if(counterfactual_method == "nbp" & missing_method == "weighting"){
    extracted_balanced_data = balanced_data
    return(list(extracted_balanced_data, process = "weighting_nbp"))
  }
  else if(counterfactual_method == "nbp" & missing_method == "mi"){
    extracted_balanced_data = balanced_data
    return(list(extracted_balanced_data, process = "mi_nbp"))
  }
  else if(counterfactual_method == "cbps" & missing_method == "complete"){
    extracted_balanced_data = cbind(psmodel_obj$missingness_treated_dataset, 
                                    psmodel_obj$propensity_scores,
                                    balanced_data$weights)
    colnames(extracted_balanced_data)[colnames(extracted_balanced_data) == "psmodel_obj$propensity_scores"] <- "propensity_scores"
    colnames(extracted_balanced_data)[colnames(extracted_balanced_data) == "balanced_data$weights"] <- "weights"
   return(list(extracted_balanced_data, process = "cc_cbps")) 
  }
  
}



















