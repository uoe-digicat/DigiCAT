
#' Counterfactual analysis outcome model
#' 
#' This function runs the outcome model for counterfactual analysis in DigiCAT. 
#'
#' @param balanced_data Balanced data object
#' @param counterfactual_method Character string indicating your chosen approach `"psm"`, `"iptw"`, or `"nbp"`
#' @param outcome_variable Character string matching column name of your outcome variable
#' @param treatment_variable Character string matching column name of your treatment variable
#' @param matching_variable Character vector matching column name(s) of your matching variable(s)
#' @param psmodel_obj Propensity score estimation object
#' @param cluster_variable Character string matching column name of your clustering variable
#' @param nonresponse_weights Character string matching column name of your non-response weight variable
#' @param sampling_weights Character string matching column name of your sampling weight variable
#' @param missing_method Character string indicating your chosen method of dealing with missingness `"complete"`, `"mi"`, or `"weighting"`
#' @param ... 
#'
#' @return Estimated effect of treatment on outcome
#' @import marginaleffects
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
                                   treatment_variable, matching_variable, covariates = NULL, psmodel_obj,
                                   cluster_variable = NULL, weighting_variable = NULL, strata_variable = NULL,
                                   missing_method, outcome_formula,...){
  extracted_balanced_data <- extract_balanced_data(balanced_data, psmodel_obj, 
                                                   missing_method, weighting_variable,
                                                   counterfactual_method, cluster_variable,
                                                   strata_variable,...)
  fitted_model <- fit_outcome_model(balanced_data,extracted_balanced_data,
                                    outcome_variable, treatment_variable, matching_variable,
                                    covariates, outcome_formula, missing_method,
                                    psmodel_obj, cluster_variable, weighting_variable,
                                    strata_variable,
                                    ...) 
  extracted_outcome_results <- extract_outcome_results(fitted_model, missing_method,...) # 
  standardised_format <- standardise_outcome_format(extracted_outcome_results, counterfactual_method,
                                                    outcome_formula,fitted_model,...) # 
  return(list(standardised_format = standardised_format, extracted_balanced_data = extracted_balanced_data, fitted_model = fitted_model, extracted_outcome_results = extracted_outcome_results))
}


