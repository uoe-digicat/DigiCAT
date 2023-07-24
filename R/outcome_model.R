#' DigiCAT outcome function
#' @import MatchThem
#' @import MatchIt
#' @import survey
#' @import mice
#' @import marginaleffects
#' @param outcome_method outcome analysis, one of `"unweighted linear regression"`, `"design-weighted linear regression"`, `"g-computation"`
#' @param outcome_variable name of outcome variable in dataset (character string)
#' @param treatment_variable name of treatment/exposure variable in dataset (character string)
#' @param matching_variable vector of matching variables/covariates names in dataset - does not need to be matching variables
#' @param balanced_data an object obtained from balance()
#' @param ids name of variable to use in svydesign() for weighted analysis
#' @param weights name of variable to use in svydesign() for weighted analysis
#' @param strata name of variable to use in svydesign() for weighted analysis
#' @param fpc name of variable to use in svydesign() for weighted analysis
#' @param counterfactual_method whether matching or weighting was used as determines outcome procedures later on
#' @param ... additional arguments 
<<<<<<< HEAD:R/outcome_model.R
outcome_analysis <- function(outcome_method, y_var, t_var, m_vars, balanced_data,
                             ids = NULL, weights = NULL, strata = NULL, fpc = NULL, cf_method,...){
=======

outcome_analysis <- function(outcome_method, outcome_variable, treatment_variable, matching_variable,
                             balanced_data,
                             ids = NULL, weights = NULL, strata = NULL, fpc = NULL, counterfactual_method,...){
>>>>>>> 537a203 (wip):source/func/outcome_model.R
  
  # (i) linear regression (unweighted)
  # (ii) design-weighted regression
  # (iii) g computation
  # return output (coefficients)
  switch(outcome_method,
         
         unweighted = {
           output = outcome_reg(outcome_variable, treatment_variable, matching_variable, balanced_data, counterfactual_method,...)
         },
         weighted = {
           output = outcome_reg_wt(outcome_variable, treatment_variable, matching_variable, balanced_data, counterfactual_method,
                                   ids, weights, strata, fpc, ...)
         },
         g_comp = {
           output = outcome_g(outcome_variable, treatment_variable, matching_variable, balanced_data, counterfactual_method,...)
         },
         stop("Need a valid outcome method (unweighted regression, design-weighted regression, g-computation)")
  )
  return(output)
}

outcome_reg <- function(outcome_variable, treatment_variable, matching_variable, 
                        balanced_data, counterfactual_method,...){
  
  f = as.formula(paste0(outcome_variable,"~",treatment_variable,"*(",paste0(matching_variable, collapse="+"), ")"))
  
  if( class(balanced_data)=="mimids" & counterfactual_method == "psm") { 
    fits = lapply(MatchThem::complete(balanced_data, "all", all = FALSE), function(d) {
      lm(as.formula(f), data = d)})
    # fits = with(balanced_data,
    #                        lm(as.formula(f)))
    output = mice::pool(fits) |> summary()
  } else if( class(balanced_data)=="wimids" & counterfactual_method == "iptw"){
    fits = lapply(MatchThem::complete(balanced_data, "all", all = FALSE), function(d) {
      lm(as.formula(f), data = d)})
   # fits = with(balanced_data,
   #                          lm(as.formula(f)))
    output_temp = mice::pool(fits) |> summary()
    
  } else {
    matched_data = match.data(balanced_data)
    fits = lm(as.formula(f), data = matched_data)
    output = summary(fits)
  }
  
  return(list(summary_output = output, outcome_model_object = fits))
}

outcome_reg_wt <- function(y_var, t_var, m_vars, balanced_data, cf_method,
                           ids, weights, strata, fpc,...){
  
  f = as.formula(paste0(outcome_variable,"~",treatment_variable,"*(",paste0(matching_variable, collapse="+"), ")"))
  
  if( class(balanced_data)=="mimids" & cf_method == "psm") {
    fits = lapply(MatchThem::complete(balanced_data, "all", all = FALSE), function(d) {
      design_data = svydesign(ids = ids, weights = weights, fpc = fpc, strata = strata,
                              data = d)
      svyglm(as.formula(f), design = design_data)})
    output = mice::pool(fits) |> summary()
  } else {
    matched_data = match.data(balanced_data)
    design_data = svydesign(ids = ids, weights = weights, fpc = fpc, strata = strata,
                            data = matched_data)
    fits = svyglm(as.formula(f), design = design_data)
    output = summary(fits)
  }
  
  return(list(summary_output = output, outcome_model_object = fit))
}

outcome_g <- function(y_var, t_var, m_vars, balanced_data, cf_method,...){
  
  f = as.formula(paste0(outcome_variable,"~",treatment_variable,"*(",paste0(matching_variable, collapse="+"), ")"))
  
  if( class(balanced_data)=="mimids" & cf_method == "psm") { 
    fits = lapply(MatchThem::complete(balanced_data, "all", all = FALSE), function(d) {
      lm(as.formula(f), data = d)})
    output <- summary(mice::pool(lapply(fits, function(fit) {
      avg_comparisons(fit, newdata = subset(fit$data, t_var == 1),
                      variables = t_var, vcov = "HC3",...)})), conf.int = TRUE)
      
  } else {
    matched_data = match.data(balanced_data)
    fits = lm(as.formula(f), data = matched_data)
    output <- avg_comparisons(fit,
                    variables = t_var,
                    vcov = ~subclass,
                    newdata = subset(matched_data, t_var == 1)) # debug when get a sec
  }
  
  return(list(summary_output = output, outcome_model_object = fit))
}

