
fit_outcome_model <- function(extracted_balanced_data, 
                              outcome_variable, treatment_variable, matching_variable,
                              covariates = NULL, doubly = TRUE,
                              ...){
  if(doubly){
    if(!is.null(covariates)){
      model_formula = as.formula(paste0(outcome_variable,"~",treatment_variable,"*(",paste0(matching_variable, covariates, collapse="+"), ")"))
    } else{
      model_formula = as.formula(paste0(outcome_variable,"~",treatment_variable,"*(",paste0(matching_variable, collapse="+"), ")"))
    }
  } else {
    if(!is.null(covars)){
      model_formula = as.formula(paste0(outcome_variable,"~",treatment_variable,"*(",paste0(covariates, collapse="+"), ")"))
    } else{
      model_formula = as.formula(paste0(outcome_variable,"~",treatment_variable))
    }
  }
  
  if(extracted_balanced_data$process == "mi_psm"){
    model_fit = lapply(extracted_balanced_data[[1]], 
                        function(d){
                          lm(model_formula, data = d)
                        })
    return(model_fit)
  } else if(extracted_balanced_data$process == "cc_psm"){ 
    model_fit = lm(model_formula, data = extracted_balanced_data[[1]])
    return(model_fit)
    
  } 
  else if (extracted_balanced_data$process == "mi_iptw"){
  model_fit = lapply(extracted_balanced_data[[1]], 
                     function(d){
                       lm(model_formula, data = d, weights = weights)
                     })
  return(model_fit)
  
  } 
  else if (extracted_balanced_data$process == "cc_iptw"){
    model_fit = lm(model_formula, data = extracted_balanced_data[[1]], weights = weights)
  return(model_fit)
  
  } else if (extracted_balanced_data$process == "weighting_iptw"){
    model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]])
    return(model_fit)

  } else if (extracted_balanced_data$process == "weighting_psm"){
  model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]])
  return(model_fit)
  
}
}


































# fit_outcome_model <- function(extracted_balanced_data, counterfactual_method = NULL,
#                               outcome_variable, treatment_variable, matching_variable,
#                               ...){
#   
#   if(counterfactual_method == "iptw"){
#     fit_outcome_iptw()
#   }
#   if()){
#     fit_outcome_psm
#   }
#   if(counterfactual_method == "NBP"){
#     fit_outcome_nbp
#   }
# }












