require(WeightIt)
require(MatchIt)
require(MatchThem)

source("source/func/prepare_dataset_nbp.R")
source("source/func/make_matrix_nbp.R")
source("source/func/assign_id_nbp.R")


balance_data <- function(counterfactual_method, treatment_variable, matching_variable, psmodel_obj,...){
  
  switch(counterfactual_method,
         
         psm = {
           balanced_data = balancing_psm(treatment_variable, matching_variable, psmodel_obj,...)
         },
         iptw = {
           balanced_data = balancing_iptw(treatment_variable, matching_variable, psmodel_obj, ...)
         },
         cem = {
           balanced_data = balancing_cem(treatment_variable, matching_variable, psmodel_obj, ...)
         },
         nbp = {
           balanced_data = balancing_nbp(treatment_variable, matching_variable, psmodel_obj, ...)
         },
         stop("Need a valid method to balance (psm, iptw, cem, nbp)")
  )
  return(balanced_data)
}

balancing_iptw <- function(treatment_variable, matching_variable, psmodel_obj, ...){
  
  f = paste0(treatment_variable,"~",paste0(matching_variable,collapse="+"))
  
  if(class(psmodel_obj$missingness_treated_dataset)=="mids" ) {
    balanced_data = with(psmodel_obj, weightthem(as.formula(f), datasets = missingness_treated_dataset,
                                                 approach = "within", method = "ps"))
    # balanced_data = weightthem(as.formula(f), datasets = psmodel_obj$data,
    #                  approach = "within", method = "ps", estimand = "ATE", ...)
    # balanced_data_indicator <- list(indicator = "wimids_output")
    # balanced_data <- append(balanced_data, balanced_data_indicator)
    
  } else {
    balanced_data = weightit(as.formula(f), data = psmodel_obj$missingness_treated_dataset, ps = psmodel_obj$propensity_scores, estimand = "ATE", ...)
    # balanced_data_indicator <- list(indicator = "weightit_output")
    # balanced_data <- append(balanced_data, balanced_data_indicator)
    
   }
  
  return(balanced_data)
}


balancing_psm <- function(treatment_variable, matching_variable, psmodel_obj, ...){
  
  f = paste0(treatment_variable,"~",paste0(matching_variable,collapse="+"))
  
  if( class(psmodel_obj$missingness_treated_dataset)=="mids" ) {
    balanced_data = matchthem(as.formula(f), datasets = psmodel_obj$missingness_treated_dataset, approach = "within", distance=psmodel_obj$propensity_model_class, ...)
    # balanced_data_indicator <- list(indicator = "mimids_output")
    # balanced_data <- append(balanced_data, balanced_data_indicator)
    
    
   } else {
    balanced_data = matchit(as.formula(f), data = psmodel_obj$missingness_treated_dataset, ps = psmodel_obj$score, ...)
    # balanced_data_indicator <- list(indicator = "matchit_output")
    # balanced_data <- append(balanced_data, balanced_data_indicator)
    
   }
  
  return(balanced_data)
  
}

balancing_cem <- function(treatment_variable, matching_variable, psmodel_obj, ...){
  f = paste0(treatment_variable,"~",paste0(matching_variable,collapse="+"))
  
  balanced_data = matchit(as.formula(f), data = psmodel_obj$missingness_treated_dataset, method = "cem")
  
  return(balanced_data)
}

balancing_nbp <- function(treatment_variable, matching_variable, psmodel_obj, ...){
  
  prepared_for_nbp <- prepare_dataset_nbp(propensity_score,...)
  created_distance_matrix <- make_matrix_nbp(propensity_score, estimated_propensity_model, treatment_variable,...)
  assigned_matrix <- assign_id_nbp(distance_matrix, id_variable, propensity_score, ...)
  formatted_matrix <- distancematrix(assigned_matrix,...)
  performed_matching <- nonbimatch(formatted_matrix, threshold, precision, ...)

  return(balanced_data)
  
}