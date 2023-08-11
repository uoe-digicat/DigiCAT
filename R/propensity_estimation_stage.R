source("R/create_design.R")
source("R/handle_missingness.R")
source("R/estimate_model.R")
source("R/get_propensity.R")

estimation_stage <- function(.data, missing_method, model_type, 
                             treatment_variable, matching_variable,
                             weighting_variable = NULL, cluster_variable = NULL,
                             strata_variable = NULL,
                             ...){
  design_object <- create_design(.data, weighting_variable, cluster_variable, strata_variable,...)
  handled_missingness <- handle_missingness(.data, missing_method,design_object,...)
  propensity_model <- estimate_model(handled_missingness, model_type, treatment_variable, matching_variable,
                                     missing_method,...)
  prop_scores <- get_propensity(propensity_model, model_type, 
                                treatment_variable, matching_variable,
                                handled_missingness, missing_method,...)
  return(list(missingness_treated_dataset = handled_missingness, 
              propensity_scores = prop_scores, 
              estimated_propensity_model = propensity_model,
              propensity_model_class = class(propensity_model), # nb: ensure this works now changing models
              survey_design_object = propensity_model$survey.design)) # note if weighting, this is the object containing data, not missingness_treated_dataset
}
