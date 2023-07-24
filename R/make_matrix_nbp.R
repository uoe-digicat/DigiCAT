make_matrix_nbp <- function(propensity_score, estimated_propensity_model, treatment_variable, ...){
  
  eps = 1*10^-100 
  result = matrix(ncol = nrow(propensity_score), nrow = nrow(propensity_score))
  
  # Matrix (in Lu et al. 2011)
  
  matj = matrix(data = treatment_variable, nrow = nrow(propensity_score), ncol = nrow(propensity_score), byrow = F)
  matk = matrix(data = treatment_variable, nrow = nrow(propensity_score), ncol = nrow(propensity_score), byrow = T)
  
  res = matj - matk
  res_squared = res^2
  
  lpj = matrix(data = estimated_propensity_model$lp, nrow = nrow(propensity_score), ncol = nrow(propensity_score), byrow = F)
  lpk = matrix(data = estimated_propensity_model$lp, nrow = nrow(propensity_score), ncol = nrow(propensity_score), byrow = T)
  
  lp_res = lpj - lpk
  lp_res_abs = abs(lp_res)
  lp_calc = 0.15 * sqrt(var(estimated_propensity_model$lp))
  lp_logical = lp_res_abs <= lp_calc
  
  lp_res_squared_plus_eps = (lp_res^2) + eps
  
  res_squared[!lp_logical] = 10^11
  res_squared[lp_logical] = 10^11 * (lp_res_squared_plus_eps[lp_logical]) / res_squared[lp_logical]
  
  distance_matrix_nbp = res_squared
  
}