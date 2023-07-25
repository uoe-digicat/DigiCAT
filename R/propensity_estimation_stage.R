source("R/handle_missingness.R")
source("R/estimate_model.R")
source("R/get_propensity.R")

estimation_stage <- function(.data, missing_method, model_type, 
                             treatment_variable, matching_variable,
                             ...){
  complete_dataset <- handle_missingness(.data, missing_method)
  propensity_model <- estimate_model(complete_dataset, model_type, treatment_variable, matching_variable)
  prop_scores <- get_propensity(estimated_propensity_model = propensity_model, model_type, 
                                treatment_variable, matching_variable,
                                complete_dataset)
  return(list(missingness_treated_dataset = complete_dataset, 
              propensity_scores = prop_scores, 
              estimated_propensity_model = propensity_model,
              propensity_model_class = "glm"))
}
