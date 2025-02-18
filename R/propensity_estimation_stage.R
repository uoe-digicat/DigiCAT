
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
#' @import survey
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
  #browser()
  handled_missingness_objects <- handle_missingness(.data, missing_method,
                                            counterfactual_method,
                                            treatment_variable,
                                            cluster_variable, weighting_variable,
                                            strata_variable,
                                            ...)
  handled_missingness <- handled_missingness_objects[[1]]
  design_object <- handled_missingness_objects[[2]]
  estimated_propensity_model <- estimate_model(handled_missingness, model_type, treatment_variable, matching_variable,
                                     missing_method,...)
  propensity_score <- get_propensity(estimated_propensity_model = estimated_propensity_model, model_type, 
                                treatment_variable, matching_variable,
                                handled_missingness, missing_method, .data,...)

  return(list(missingness_treated_dataset = handled_missingness,
              propensity_scores = propensity_score,
              estimated_propensity_model = estimated_propensity_model,
              propensity_model_class = model_type, # nb: want to alter to class(estimated_propensity_model) or use indicator
              survey_design_object = design_object)) # note if weighting, this is the object containing data, not missingness_treated_dataset
}

