extract_outcome_results <- function(fitted_model, missing_method,...){
  if("svyglm" %in% class(fitted_model)){
    extracted_outcome_results = summary(fitted_model)
    return(list(extracted_outcome_results, process = "weighting"))
    
  }else if("comparisons" %in% class(fitted_model) & missing_method == "complete"){
    extracted_outcome_results = summary(fitted_model, conf.int = TRUE)
    return(list(extracted_outcome_results, process = "cc"))
    
  }
  else if("mipo" %in% class(fitted_model) & missing_method == "mi"){
    extracted_outcome_results = summary(fitted_model, conf.int = TRUE)
    return(list(extracted_outcome_results, process = "mi"))
    
  }
}
