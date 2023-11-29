
fit_outcome_model <- function(balanced_data,extracted_balanced_data,
                              outcome_variable, treatment_variable, matching_variable,
                              covariates = NULL, outcome_formula, missing_method,
                              ...){
  
  switch(outcome_formula,
         
         unadjusted = {
           model_fit = outcome_unadjusted(balanced_data,
                                          extracted_balanced_data,
                                          outcome_variable,
                                          treatment_variable,
                                          matching_variable, covariates,
                                          missing_method,...)
         },
         with_matching_variables = {
           model_fit = outcome_matching_variables(balanced_data,
                                                       extracted_balanced_data,
                                                       outcome_variable,
                                                       treatment_variable,
                                                       matching_variable, covariates, 
                                                       missing_method,...)
         },
         marginal_effects = {
           model_fit = outcome_marginal_effects(balanced_data,
                                                extracted_balanced_data,
                                                outcome_variable,
                                                treatment_variable,
                                                matching_variable, covariates, 
                                                missing_method,...)
         },
         stop("Need a valid outcome formula (unadjusted, with matching variables, marginal effects)")
  )
  return(model_fit)
}

outcome_unadjusted <- function(balanced_data,
                               extracted_balanced_data,
                               outcome_variable,
                               treatment_variable,
                               matching_variable, covariates,
                               missing_method,...){
  
  
  if(!is.null(covariates)){
    model_formula = paste0(outcome_variable,"~",paste0(c(treatment_variable,covariates),collapse="+"))
  } else{
    model_formula = as.formula(paste0(outcome_variable,"~",treatment_variable))
  }
  
  if(extracted_balanced_data$process == "mi_psm"){
    lm_model_fit <- lapply(complete(balanced_data, "all"), function(d) {
      lm(model_formula, data = d,
         weights = weights)
    })
    model_fit <- mice::pool(lm_model_fit)
    
  } else if(extracted_balanced_data$process == "cc_psm"){ 
    model_fit = lm(model_formula, data = extracted_balanced_data[[1]], weights = weights)
    
  } else if (extracted_balanced_data$process == "mi_iptw"){
    lm_model_fit <- lapply(complete(balanced_data, "all"), function(d) {
      lm(model_formula, data = d,
         weights = weights)
    })
    model_fit <- mice::pool(lm_model_fit)
    
  } 
  else if (extracted_balanced_data$process == "cc_iptw"){
    model_fit = lm(model_formula, data = extracted_balanced_data[[1]], weights = weights)
    
  } else if (extracted_balanced_data$process == "weighting_iptw"){
    model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]])
    
  } else if (extracted_balanced_data$process == "weighting_psm"){
    model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]])
  }
  else if (extracted_balanced_data$process == "cc_nbp"){
    model_fit = lm(model_formula, data = extracted_balanced_data[[1]])
    
  }
  return(model_fit)
}


outcome_matching_variables <- function(balanced_data,
                                    extracted_balanced_data,
                                    outcome_variable,
                                    treatment_variable,
                                    matching_variable, covariates, 
                                    missing_method,...){
  
  
  if(!is.null(covariates)){
    model_formula = paste0(outcome_variable,"~",paste0(c(treatment_variable,matching_variable,covariates),collapse="+"))
  } else{
    model_formula = paste0(outcome_variable,"~",paste0(c(treatment_variable,matching_variable),collapse="+"))
  }
  
  if(extracted_balanced_data$process == "mi_psm"){
    lm_model_fit <- lapply(complete(balanced_data, "all"), function(d) {
      lm(model_formula, data = d,
         weights = weights)
    })
    model_fit <- mice::pool(lm_model_fit)
    
  } else if(extracted_balanced_data$process == "cc_psm"){ 
    model_fit = lm(model_formula, data = extracted_balanced_data[[1]], weights = weights)
    
  } else if (extracted_balanced_data$process == "mi_iptw"){
    lm_model_fit <- lapply(complete(balanced_data, "all"), function(d) {
      lm(model_formula, data = d,
         weights = weights)
    })
    model_fit <- mice::pool(lm_model_fit)
    
  } 
  else if (extracted_balanced_data$process == "cc_iptw"){
    model_fit = lm(model_formula, data = extracted_balanced_data[[1]], weights = weights)
    
  } else if (extracted_balanced_data$process == "weighting_iptw"){
    model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]])
    
  } else if (extracted_balanced_data$process == "weighting_psm"){
    model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]])
  }
  else if (extracted_balanced_data$process == "cc_nbp"){
    model_fit = lm(model_formula, data = extracted_balanced_data[[1]])
    
  }
  return(model_fit)
}


outcome_marginal_effects <- function(balanced_data,
                             extracted_balanced_data,
                             outcome_variable,
                             treatment_variable,
                             matching_variable, covariates, 
                             missing_method,...){
  if(!is.null(covariates)){
    model_formula <- as.formula(paste0(outcome_variable, " ~ ", 
                                       treatment_variable, " * (", 
                                       paste(c(matching_variable, covariates), collapse = " + "), ")"))
    
  } else{
    model_formula = as.formula(paste0(outcome_variable,
                                      "~",treatment_variable,
                                      "*(",paste0(matching_variable, collapse="+"), ")"))
  }
  if(extracted_balanced_data$process == "mi_psm"){
    lm_model_fit <- lapply(complete(balanced_data, "all"), function(d) {
      lm(model_formula, data = d,
         weights = weights)
    })
    model_fit <- lapply(lm_model_fit, function(fit) {
     marginaleffects::avg_comparisons(fit, newdata = subset(fit$data, treatment_variable == 1),
                                      variables = treatment_variable, wts = "weights", vcov = ~subclass)
    })
    model_fit <- mice::pool(model_fit)
    
  } else if(extracted_balanced_data$process == "cc_psm"){ 
    lm_model_fit = lm(model_formula, data = extracted_balanced_data[[1]], weights = weights)
    
    model_fit = marginaleffects::avg_comparisons(lm_model_fit, variables = treatment_variable,
                                                 vcov = ~subclass, 
                                                 newdata = subset(extracted_balanced_data[[1]], 
                                                                  extracted_balanced_data[[1]][[treatment_variable]] == 1),
                                                 wts = "weights")
    
  } else if (extracted_balanced_data$process == "mi_iptw"){
    lm_model_fit <- lapply(complete(balanced_data, "all"), function(d) {
      lm(model_formula, data = d,
         weights = weights)
    })
    model_fit <- lapply(lm_model_fit, function(fit) {
      marginaleffects::avg_comparisons(fit, newdata = subset(fit$data, treatment_variable == 1),
                                       variables = treatment_variable, wts = "weights", vcov = "HC3")
    })
    model_fit <- mice::pool(model_fit)
    
  } 
  else if (extracted_balanced_data$process == "cc_iptw"){
    model_fit = lm(model_formula, data = extracted_balanced_data[[1]], weights = weights)
    model_fit = marginaleffects::avg_comparisons(model_fit, variables = treatment_variable,
                                                 vcov = "HC3",
                                                 newdata = subset(extracted_balanced_data[[1]], 
                                                                  extracted_balanced_data[[1]][[treatment_variable]] == 1),
                                                 wts = "weights")
    
  } else if (extracted_balanced_data$process == "weighting_iptw"){
    model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]])
    model_fit = marginaleffects::avg_comparisons(model_fit, variables = treatment_variable)
    
  } else if (extracted_balanced_data$process == "weighting_psm"){
    model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]])
    model_fit = marginaleffects::avg_comparisons(model_fit, variables = treatment_variable)
    
  }
  return(model_fit)
}












