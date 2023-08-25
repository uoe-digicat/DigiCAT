require(WeightIt)
require(MatchIt)
require(MatchThem)
require(nbpMatching)

source("R/prepare_dataset_nbp.R")
source("R/make_matrix_nbp.R")
source("R/assign_id_nbp.R")
source("R/Restructure_nbp.R")



balance_data <- function(counterfactual_method, treatment_variable, matching_variable, PS_estimation_object,
                         missing_method,
                         eff, ...){
  
  # ## If no input has been selected, stop and give informative error
  # if (is.null(ratio)){
  #   stop("I need a matching ratio! Please specify with the slider above")
  # }
  # if (is.null(method)){
  #   stop("I need a matching method! Please select from the matching methods above")
  # }
  
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
    balanced_data = weightit(as.formula(f), data = PS_estimation_object$missingness_treated_dataset, ps = PS_estimation_object$propensity_scores, estimand = "ATE", ...)
    
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
  
  propensity_scores <- PS_estimation_object[[2]]
  propensity_data <- prepare_dataset_nbp(propensity_scores,treatment_variable, missing_method,...) 
  created_distance_matrix <- make_matrix_nbp(propensity_data, estimated_propensity_model = PS_estimation_object$estimated_propensity_model, 
                                             treatment_variable, missing_method,...) 
  formatted_matrix <- distancematrix(created_distance_matrix,...) 
  performed_matching <- nonbimatch(formatted_matrix, ...) # threshold = 999999, precision = 7? 
  matched_data<-performed_matching$halves[performed_matching$halves$Distance!=999999, ] 
  balanced_data <- restructure_rejoin_nbp(matched_data, propensity_data, treatment_variable,...)
  
  return(balanced_data)
  
}