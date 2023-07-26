require(WeightIt)
require(MatchIt)
require(MatchThem)
require(nbpMatching)

source("R/prepare_dataset_nbp.R")
source("R/make_matrix_nbp.R")
source("R/assign_id_nbp.R")


balance_data <- function(counterfactual_method, treatment_variable, matching_variable, psmodel_obj,
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
           balanced_data = balancing_psm(treatment_variable, matching_variable, psmodel_obj, missing_method,...)
         },
         iptw = {
           balanced_data = balancing_iptw(treatment_variable, matching_variable, psmodel_obj, missing_method,...)
         },
         cem = {
           balanced_data = balancing_cem(treatment_variable, matching_variable, psmodel_obj, missing_method,...)
         },
         nbp = {
           balanced_data = balancing_nbp(treatment_variable, matching_variable, psmodel_obj, missing_method,...)
         },
         stop("Need a valid method to balance (psm, iptw, cem, nbp)")
  )
  return(balanced_data)
}

balancing_iptw <- function(treatment_variable, matching_variable, psmodel_obj, missing_method,...){
  
  f = paste0(treatment_variable,"~",paste0(matching_variable,collapse="+"))
  
  if(missing_method=="mi" ) {
     balanced_data = weightthem(as.formula(f), datasets = psmodel_obj$missingness_treated_dataset,
                     approach = "within", method = "ps",...)
    
  } else {
    balanced_data = weightit(as.formula(f), data = psmodel_obj$missingness_treated_dataset, ps = psmodel_obj$propensity_scores, estimand = "ATE", ...)
    
   }
  
  return(balanced_data)
}


balancing_psm <- function(treatment_variable, matching_variable, psmodel_obj, missing_method,...){
  
  f = paste0(treatment_variable,"~",paste0(matching_variable,collapse="+"))
  
  if(missing_method=="mi" ) {
    balanced_data = matchthem(as.formula(f), datasets = psmodel_obj$missingness_treated_dataset, approach = "within", distance=psmodel_obj$propensity_model_class, ...)
    
    
   } else if(missing_method=="complete"){
    balanced_data = matchit(as.formula(f), data = psmodel_obj$missingness_treated_dataset, ps = psmodel_obj$propensity_score, ...)
    
   } else if(missing_method=="weighting"){
       balanced_data = matchit(as.formula(f), data = psmodel_obj$missingness_treated_dataset$variables, ps = psmodel_obj$score, ...)
   }
  
  return(balanced_data)
  
}

balancing_cem <- function(treatment_variable, matching_variable, psmodel_obj, ...){
  f = paste0(treatment_variable,"~",paste0(matching_variable,collapse="+"))

  balanced_data = matchit(as.formula(f), data = psmodel_obj$missingness_treated_dataset, method = "cem")

  return(balanced_data)
}

balancing_nbp <- function(treatment_variable, matching_variable, psmodel_obj, ...){ # to finish
  
  prepared_for_nbp <- prepare_dataset_nbp(propensity_score,...)
  created_distance_matrix <- make_matrix_nbp(propensity_score, estimated_propensity_model, treatment_variable,...)
  assigned_matrix <- assign_id_nbp(distance_matrix, id_variable, propensity_score, ...)
  formatted_matrix <- distancematrix(assigned_matrix,...)
  performed_matching <- nonbimatch(formatted_matrix, threshold, precision, ...)

  return(balanced_data)
  
}