source("R/create_design.R")
source("R/handle_missingness.R")
source("R/estimate_model.R")
source("R/get_propensity.R")

estimation_stage <- function(.data, missing_method, model_type, 
                             treatment_variable, matching_variable,
                             nonresponse_weights = NULL, sampling_weights = NULL,
                             ...){
  design_object <- create_design(.data, nonresponse_weights, sampling_weights,...)
  handled_missingness <- handle_missingness(.data, missing_method, nonresponse_weights,design_object,...)
  propensity_model <- estimate_model(handled_missingness, model_type, treatment_variable, matching_variable,
                                     missing_method,...)
  prop_scores <- get_propensity(propensity_model, model_type, 
                                treatment_variable, matching_variable,
                                handled_missingness, missing_method,...)
  return(list(missingness_treated_dataset = handled_missingness, 
              propensity_scores = prop_scores, 
              estimated_propensity_model = propensity_model,
              propensity_model_class = "glm"))
}
