
#' Propensity score estimation
#' 
#' This function calculates the propensity score (or likelihood of belonging to the treatment group) 
#' for each individual in the sample data, based on the matching variables provided.
#' 
#' @param .data dataframe
#' @param missing_method Character string indicating your chosen method of dealing with missingness `"complete"`, `"mi"`, or `"weighting"`
#' @param model_type Character string indicating your chosen model for calculating propensity scores `"glm"`
#' @param treatment_variable Character string matching column name of your treatment variable
#' @param matching_variable Character vector/string matching column name(s) of your matching variable(s)
#' @param weighting_variable Character string matching column name of your weighting variable
#' @param cluster_variable Character string matching column name of your clustering variable
#' @param strata_variable Character string matching column name of your stratification variable
#' @param ... 
#'
#' @return Estimation model object
#' @import gbm
#' @import randomForest
#'
#' @examples 
#' estimation_stage(
#' .data = DigiCAT::zp_eg,
#' missing_method = "complete",
#' model_type = "glm",
#' treatment_variable = "Reading_age15",
#' matching_variable = names(DigiCAT::zp_eg)[-c(2:4)],
#' weighting_variable = NULL,
#' cluster_variable = NULL,
#' strata_variable = NULL
#' )
#' 
estimation_stage <- function(.data, missing_method, model_type, 
                             treatment_variable, matching_variable,
                             weighting_variable = NULL, cluster_variable = NULL,
                             strata_variable = NULL,
                             ...){
  design_object <- create_design(.data, weighting_variable, cluster_variable, strata_variable,...)
  handled_missingness <- handle_missingness(.data, missing_method,design_object,...)
  propensity_model <- estimate_model(handled_missingness, model_type, treatment_variable, matching_variable,
                                     missing_method,...)
  prop_scores <- get_propensity(estimated_propensity_model = propensity_model, model_type, 
                                treatment_variable, matching_variable,
                                handled_missingness, missing_method, .data,...)
  return(list(missingness_treated_dataset = handled_missingness, 
              propensity_scores = prop_scores, 
              estimated_propensity_model = propensity_model,
              propensity_model_class = model_type, # nb: want to alter to class(propensity_model) or use indicator
              survey_design_object = propensity_model$survey.design)) # note if weighting, this is the object containing data, not missingness_treated_dataset
}
