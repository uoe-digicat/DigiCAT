extract_balanced_data <- function(balanced_data, psmodel_obj, missing_method = NULL,
                                  weighting_variable, counterfactual_method, treatment_variable,...){
  
  if( "mimids" %in% class(balanced_data)) { 
   extracted_balanced_data = MatchThem::complete(balanced_data, "all", all = FALSE) 
   return(list(extracted_balanced_data, process = "mi_psm"))
   
  } else if ( "wimids" %in% class(balanced_data)){
    extracted_balanced_data = MatchThem::complete(balanced_data, "all", all = FALSE) 
    return(list(extracted_balanced_data, process = "mi_iptw"))
    
  } else if ( "matchit" %in% class(balanced_data) & missing_method == "complete"){
    extracted_balanced_data = match.data(balanced_data)
    return(list(extracted_balanced_data, process = "cc_psm"))
    
    
    ## to do: change below design obj - ids = subclass, strata as is first entered?
    
  } else if(missing_method =="weighting" & "matchit" %in% class(balanced_data)){
    extracted_balanced_data = match.data(balanced_data)
    extracted_balanced_design = svydesign(ids=~subclass, weights = (extracted_balanced_data[[weighting_variable]]*extracted_balanced_data$weights), 
                                                  data = extracted_balanced_data)
    extracted_balanced_data = extracted_balanced_design
    return(list(extracted_balanced_data, process = "weighting_psm"))

  } else if ( "weightit" %in% class(balanced_data) & missing_method == "complete"){
    psmodel_obj$missingness_treated_dataset = cbind(psmodel_obj$missingness_treated_dataset,balanced_data$weights)
    colnames(psmodel_obj$missingness_treated_dataset)[colnames(psmodel_obj$missingness_treated_dataset) == "balanced_data$weights"] <- "weights"
    return(list(psmodel_obj$missingness_treated_dataset, process = "cc_iptw"))
    
    
    ## to do: change below design obj - ids = subclass, strata as is first entered?
    
  } else if(missing_method=="weighting" & "weightit" %in% class(balanced_data)){
    survey_data = psmodel_obj$survey_design_object$variables
    survey_data = cbind(survey_data,balanced_data$weights)
    colnames(survey_data)[colnames(survey_data) == "balanced_data$weights"] <- "weights"
    extracted_balanced_data = svydesign(ids=~1, weights = (survey_data[[weighting_variable]]*survey_data$weights), 
                                          data = survey_data)
    extracted_balanced_data = extracted_balanced_data
    return(list(extracted_balanced_data, process = "weighting_iptw"))
    
  } else if(counterfactual_method == "nbp" & missing_method == "complete"){
    extracted_balanced_data = balanced_data
    # replace treatment variable with dose
    return(list(extracted_balanced_data, process = "cc_nbp"))
  }
  
}



















