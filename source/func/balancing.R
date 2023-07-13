require(WeightIt)
require(MatchIt)
require(MatchThem)


#' @param cf_method counterfactual approach employed, one of `"matching"`, `"weighting"`, `"cem"`, TODO add here
#' @param t_var name of treatment variable in dataset (character string)
#' @param m_vars vector of matching variable names in dataset
#' @param psmodel_obj an object obtained from get_score(). this is required for matching and iptw methods, and is a list containing data, model, propensity score, and string indicating class of model. note the first 3 of these may be lists if multiple imputation has been used.  
#' @param ... additional arguments to be passed to matchit/weightit/matchthem/weightthem functions
balancing <- function(cf_method, t_var, m_vars, psmodel_obj, eff, ...){
  
  # 1. matchdata on dists (1:1)
  # algorithm choice
  # 2. matchdata on dists (K:1)
  # algorithm choice
  # 3a. ATT weights = treat + (1-treat)*p.score/(1-p.score)
  # 3b. ATE weights = ?
  # 4. CEM
  # return(balance) # either match.data or weights
  
  
  # ## If no input has been selected, stop and give informative error
  # if (is.null(ratio)){
  #   stop("I need a matching ratio! Please specify with the slider above")
  # }
  # if (is.null(method)){
  #   stop("I need a matching method! Please select from the matching methods above")
  # }
  
  
  
  switch(cf_method,
         
         matching = {
           res = balancing_match(t_var, m_vars, psmodel_obj, ...)
         },
         weighting = {
           res = balancing_weight(t_var, m_vars, psmodel_obj, ...)
         },
         cem = {
           res = balancing_cem(t_var, m_vars, psmodel_obj, ...)
         },
         stop("Need a valid method to balance (matching, iptw, cem, etc)")
  )
  return(res)
}

balancing_weight <- function(t_var, m_vars, psmodel_obj, ...){
  
  f = paste0(t_var,"~",paste0(m_vars,collapse="+"))
  
  if( class(psmodel_obj$data)=="mids" ) {
    res = weightthem(as.formula(f), datasets = psmodel_obj$data, approach = "within", method=psmodel_obj$ps_modclass, ...)
  } else {
    res = weightit(as.formula(f), data = psmodel_obj$data, ps = psmodel_obj$score, ...)
  }
  
  return(res)
}
  


balancing_match <- function(t_var, m_vars, psmodel_obj, ...){
  
  f = paste0(t_var,"~",paste0(m_vars,collapse="+"))
  
  if( class(psmodel_obj$data)=="mids" ) {
    res = matchthem(as.formula(f), datasets = psmodel_obj$data, approach = "within", distance=psmodel_obj$ps_modclass, ...)
  } else {
    res = matchit(as.formula(f), data = psmodel_obj$data, ps = psmodel_obj$score, ...)
  }
  
  return(res)
  
}

balancing_cem <- function(t_var, m_vars, psmodel_obj, ...){
  f = paste0(t_var,"~",paste0(m_vars,collapse="+"))
  
  res = matchit(as.formula(f), data = psmodel_obj$data, method = "cem")
  
  return(res)
}



