extract_balanced_data <- function(balanced_data, psmodel_obj, missing_method = NULL,
                                  weighting_variable,...){
  
  if( class(balanced_data)=="mimids") { 
   extracted_balanced_data = MatchThem::complete(balanced_data, "all", all = FALSE) 
   return(list(extracted_balanced_data, process = "mi_psm"))
   
  } else if ( class(balanced_data)=="wimids"){
    extracted_balanced_data = MatchThem::complete(balanced_data, "all", all = FALSE) 
    return(list(extracted_balanced_data, process = "mi_iptw"))
    
  } else if ( class(balanced_data)=="matchit" & missing_method == "complete"){
    extracted_balanced_data = match.data(balanced_data)
    return(list(extracted_balanced_data, process = "cc_psm"))
    
    
    ## to do: change below design obj - ids = subclass, strata as is first entered?
    
  } else if(missing_method =="weighting" & class(balanced_data)=="matchit"){
    extracted_balanced_data = match.data(balanced_data)
    extracted_balanced_design = svydesign(ids=~1, weights = (extracted_balanced_data[[weighting_variable]]*extracted_balanced_data$weights), 
                                                  data = extracted_balanced_data)
    extracted_balanced_data = extracted_balanced_design
    return(list(extracted_balanced_data, process = "weighting_psm"))

  } else if ( class(balanced_data)=="weightit" & missing_method == "complete"){
    psmodel_obj$missingness_treated_dataset = cbind(psmodel_obj$missingness_treated_dataset,balanced_data$weights)
    colnames(psmodel_obj$missingness_treated_dataset)[colnames(psmodel_obj$missingness_treated_dataset) == "balanced_data$weights"] <- "weights"
    return(list(psmodel_obj$missingness_treated_dataset, process = "cc_iptw"))
    
    
    ## to do: change below design obj - ids = subclass, strata as is first entered?
    
  } else if(missing_method=="weighting" & class(balanced_data)=="weightit"){
    survey_data = psmodel_obj$survey_design_object$variables
    survey_data = cbind(survey_data,balanced_data$weights)
    colnames(survey_data)[colnames(survey_data) == "balanced_data$weights"] <- "weights"
    extracted_balanced_data = svydesign(ids=~1, weights = (survey_data[[weighting_variable]]*survey_data$weights), 
                                          data = survey_data)
    extracted_balanced_data = extracted_balanced_data
    return(list(extracted_balanced_data, process = "weighting_iptw"))
    
  } 
  
}



















