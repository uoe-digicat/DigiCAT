
#' Counterfactual analysis outcome model
#' 
#' This function runs the outcome model for counterfactual analysis in DigiCAT. 
#'
#' @param balanced_data 
#' @param counterfactual_method 
#' @param outcome_variable 
#' @param treatment_variable 
#' @param matching_variable 
#' @param psmodel_obj 
#' @param cluster_variable 
#' @param nonresponse_weights 
#' @param sampling_weights 
#' @param missing_method 
#' @param ... 
#'
#' @return Estimated effect of treatment on outcome
#' @export
#'
#' @examples
#' estimates <- estimation_stage(
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
#' balanced_data <- balance_data(
#' counterfactual_method = "psm",
#' treatment_variable = "Reading_age15",
#' matching_variable = names(DigiCAT::zp_eg)[-c(2:4)],
#' PS_estimation_object = estimates,
#' missing_method = "complete",
#' )
#' 
#' outcome_analysis_stage(
#' balanced_data = balanced_data, 
#' counterfactual_method = "psm",
#' missing_method = "complete",
#' outcome_variable = "Anxiety_age17",
#' treatment_variable = "Reading_age15",
#' matching_variable = names(DigiCAT::zp_eg)[-c(2:4)],
#' psmodel_obj = estimates
#' )

outcome_analysis_stage <- function(balanced_data, counterfactual_method, outcome_variable,
                                   treatment_variable, matching_variable, psmodel_obj,
                                   cluster_variable, nonresponse_weights, sampling_weights,
                                   missing_method,...){
  extracted_balanced_data <- extract_balanced_data(balanced_data, psmodel_obj, 
                                                   missing_method, weighting_variable,
                                                   counterfactual_method,...)
  fitted_model <- fit_outcome_model(balanced_data, extracted_balanced_data,outcome_variable, 
                                    treatment_variable, 
                                    matching_variable,...) 
  extracted_outcome_results <- extract_outcome_results(fitted_model, missing_method,...) # 
  standardised_format <- standardise_outcome_format(extracted_outcome_results, counterfactual_method) # 
  return(standardised_format)
}


