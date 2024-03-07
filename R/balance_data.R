#' Function to balance datasets
#'
#' @param counterfactual_method 
#' @param treatment_variable 
#' @param matching_variable 
#' @param PS_estimation_object 
#' @param missing_method 
#' @param eff 
#' @param ... 
#'
#' @import nbpMatching
#' @import WeightIt
#' @import MatchIt
#' @import MatchThem
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

balance_data <- function(counterfactual_method, treatment_variable, matching_variable, PS_estimation_object,
                         missing_method,
                         eff, ...){
  switch(counterfactual_method,
         
         psm = {
           balanced_data = balancing_psm(treatment_variable, matching_variable, PS_estimation_object, missing_method,...)
         },
         iptw = {
           balanced_data = balancing_iptw(treatment_variable, matching_variable, PS_estimation_object, missing_method,...)
         },
         cem = {
           balanced_data = balancing_cem(treatment_variable, matching_variable, PS_estimation_object, missing_method,...)
         },
         nbp = {
           balanced_data = balancing_nbp(treatment_variable, PS_estimation_object, missing_method,...)
         },
         cbps = {
           balanced_data = balancing_cbps(treatment_variable, matching_variable, PS_estimation_object, 
                                          missing_method,...)
         },
         stop("Need a valid method to balance (psm, iptw, cem, nbp)")
  )
  return(balanced_data)
}

balancing_iptw <- function(treatment_variable, matching_variable, PS_estimation_object, missing_method,...){
  
  f = paste0(treatment_variable,"~",paste0(matching_variable,collapse="+"))
  
  if(missing_method=="mi" ) {
    balanced_data = weightthem(as.formula(f), datasets = PS_estimation_object$missingness_treated_dataset,
                               approach = "within", method = "ps",...)
    
  } else if(missing_method=="complete"){
    balanced_data = weightit(as.formula(f), data = PS_estimation_object$missingness_treated_dataset, 
                             ps = PS_estimation_object$propensity_scores, 
                             estimand = "ATE", 
                             method = PS_estimation_object$propensity_model_class,...)
    
  } else if(missing_method=="weighting"){
    balanced_data = weightit(as.formula(f), data = PS_estimation_object$estimated_propensity_model$survey.design$variables, 
                             method = "ps", ...)
  }
  
  return(balanced_data)
}


balancing_psm <- function(treatment_variable, matching_variable, PS_estimation_object, missing_method,...){
  
  f = paste0(treatment_variable,"~",paste0(matching_variable,collapse="+"))
  
  if(missing_method=="mi" ) {
    balanced_data = matchthem(as.formula(f), datasets = PS_estimation_object$missingness_treated_dataset, approach = "within", distance=PS_estimation_object$propensity_model_class, ...)
    
    
  } else if(missing_method=="complete"){
    balanced_data = matchit(as.formula(f), data = PS_estimation_object$missingness_treated_dataset, distance = PS_estimation_object$propensity_scores, ...)
    
  } else if(missing_method=="weighting"){
    balanced_data = matchit(as.formula(f), data = PS_estimation_object$estimated_propensity_model$survey.design$variables, 
                            ps = PS_estimation_object$propensity_score, ...)
  }
  
  return(balanced_data)
  
}

balancing_cem <- function(treatment_variable, matching_variable, PS_estimation_object, ...){ # ignore for now
  f = paste0(treatment_variable,"~",paste0(matching_variable,collapse="+"))
  
  balanced_data = matchit(as.formula(f), data = PS_estimation_object$missingness_treated_dataset, method = "cem")
  
  return(balanced_data)
}


balancing_nbp <- function(treatment_variable, PS_estimation_object, missing_method,...){ 
  if(missing_method == "complete"){
    
    propensity_scores <- PS_estimation_object[[2]]
    propensity_data <- prepare_dataset_nbp(propensity_scores,treatment_variable, missing_method,...) 
    created_distance_matrix <- make_matrix_nbp(propensity_data, estimated_propensity_model = PS_estimation_object$estimated_propensity_model, 
                                               treatment_variable, missing_method,...) 
    formatted_matrix <- distancematrix(created_distance_matrix,...) 
    performed_matching <- nonbimatch(formatted_matrix) # threshold = 999999, precision = 7? 
    #performed_matching$halves <- performed_matching$halves[-(n + 1), ]
    matched_data<-performed_matching$halves[performed_matching$halves$Distance!=999999, ] 
    balanced_data <- restructure_rejoin_nbp(matched_data, propensity_data, treatment_variable, missing_method,...)
    
      } 
  
  else if(missing_method == "mi"){
    propensity_scores <- PS_estimation_object[[2]]
    propensity_data <- prepare_dataset_nbp(propensity_scores,treatment_variable, missing_method,...) 
    created_distance_matrix <- make_matrix_nbp(propensity_data, 
                                    estimated_propensity_model = PS_estimation_object$estimated_propensity_model, 
                                    PS_estimation_object = PS_estimation_object,
                                    treatment_variable, missing_method,...)
    formatted_matrix <- lapply(created_distance_matrix, function(x) distancematrix(x))
    performed_matching <- lapply(formatted_matrix, function(x) nonbimatch(x))
    matched_data <- lapply(performed_matching, function(x) x$halves[x$halves$Distance != 999999,])
    balanced_data <- restructure_rejoin_nbp(matched_data, propensity_data, treatment_variable, missing_method,...)
  }
  
  else if(missing_method == "weighting"){
    propensity_scores <- PS_estimation_object[[2]]
    propensity_data <- prepare_dataset_nbp(propensity_scores,treatment_variable, missing_method,...) 
    created_distance_matrix <- make_matrix_nbp(propensity_data, estimated_propensity_model = PS_estimation_object$estimated_propensity_model, 
                                               treatment_variable, missing_method,...) 
    formatted_matrix <- distancematrix(created_distance_matrix,...) 
    performed_matching <- nonbimatch(formatted_matrix) # threshold = 999999, precision = 7? 
    matched_data<-performed_matching$halves[performed_matching$halves$Distance!=999999, ] 
    balanced_data <- restructure_rejoin_nbp(matched_data, propensity_data, treatment_variable, missing_method,...)
  }
  
  return(balanced_data)
  
}

balancing_cbps <- function(treatment_variable, matching_variable, PS_estimation_object, 
                           missing_method,...){
  f = paste0(treatment_variable,"~",paste0(matching_variable,collapse="+"))
  if(missing_method == "complete"){
    data_to_use = cbind(PS_estimation_object$missingness_treated_dataset, PS_estimation_object$propensity_scores)
    balanced_data = weightit(as.formula(f), data = data_to_use, method = "cbps",
                             cbps.args = list(model = PS_estimation_object$estimated_propensity_model))
  } else if(missing_method == "mi"){
    balanced_data = weightthem(as.formula(f), datasets = PS_estimation_object$missingness_treated_dataset,
                               approach = "within", method = "cbps")
  } else if(missing_method == "weighting"){
    balanced_data = weightit(as.formula(f), data = PS_estimation_object$estimated_propensity_model$survey.design$variables, 
                             method = "cbps", ...)
  }
  return(balanced_data)
}


























