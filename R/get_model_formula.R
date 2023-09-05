get_model_formula <- function(doubly = TRUE, outcome_variable, treatment_variable, matching_variable,
                        covariates = NULL, ...){
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
  return(model_formula)
}