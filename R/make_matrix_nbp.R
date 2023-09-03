make_matrix_nbp <- function(propensity_data, estimated_propensity_model, treatment_variable, missing_method,...){

  if(missing_method == "complete"){
  eps = 1*10^-100 
  result = matrix(ncol = nrow(propensity_data), nrow = nrow(propensity_data))
  
  matj = matrix(data = propensity_data[[treatment_variable]], nrow = nrow(propensity_data), ncol = nrow(propensity_data), byrow = F)
  matk = matrix(data = propensity_data[[treatment_variable]], nrow = nrow(propensity_data), ncol = nrow(propensity_data), byrow = T)
  
  res = matj - matk 
  res_squared = res^2
  
  lpj = matrix(data = estimated_propensity_model$lp, nrow = nrow(propensity_data), ncol = nrow(propensity_data), byrow = F)
  lpk = matrix(data = estimated_propensity_model$lp, nrow = nrow(propensity_data), ncol = nrow(propensity_data), byrow = T)
  
  lp_res = lpj - lpk
  lp_res_abs = abs(lp_res)
  lp_calc = 0.15 * sqrt(var(estimated_propensity_model$lp))
  lp_logical = lp_res_abs <= lp_calc
  
  lp_res_squared_plus_eps = (lp_res^2) + eps
  
  res_squared[!lp_logical] = 10^11
  res_squared[lp_logical] = 10^11 * (lp_res_squared_plus_eps[lp_logical]) / res_squared[lp_logical]
  
  distance_matrix_nbp = res_squared
  row.names(distance_matrix_nbp) <- propensity_data$ID
  
  }
  
  else if(missing_method == "mi"){

  eps<-1*10^-100 #specify epsilon to be very small positive number
  
  #result <- data.frame() # either collect in list or in dataframe by specifying dimensions in advance
  
  #result <- matrix(ncol = 9538, nrow = (9537*5))
  distance_matrix_nbp = matrix(ncol = nrow(propensity_data)+1, nrow = nrow(propensity_data)*5)
  
  comp = complete(handled_missingness, "all", include = FALSE)
  
  for (i in 1:length(comp)) {
    
    data_for_this_iteration <- subset(estimated_propensity_model, impset == i) # select all rows where impset is for this imputation
    
    # Matrix (in Lu et al. 2011)
    
    matj <- matrix(data = data_for_this_iteration[[treatment_variable]], nrow = nrow(propensity_data), ncol = nrow(propensity_data), byrow = F) # dims set by prop data not data for this iter?
    matk <- matrix(data = data_for_this_iteration[[treatment_variable]], nrow = nrow(propensity_data), ncol = nrow(propensity_data), byrow = T)
    
    res <- matj - matk
    res_squared <- res^2
    
    lpj <- matrix(data = data_for_this_iteration$'polly$lp', nrow = nrow(propensity_data), ncol = nrow(propensity_data), byrow = F)
    lpk <- matrix(data = data_for_this_iteration$'polly$lp', nrow = nrow(propensity_data), ncol = nrow(propensity_data), byrow = T)
    
    lp_res <- lpj - lpk
    lp_res_abs <- abs(lp_res)
    lp_calc <- 0.15 * sqrt(var(data_for_this_iteration$'polly$lp'))
    lp_logical <- lp_res_abs <= lp_calc
    
    lp_res_squared_plus_eps <- (lp_res^2) + eps
    
    res_squared[!lp_logical] <- 10^11
    res_squared[lp_logical] <- 10^11 * (lp_res_squared_plus_eps[lp_logical]) / res_squared[lp_logical]
    
    # impset <- i
    res_squared <- cbind(res_squared, i)
    # result <- rbind(result, res_squared)
    start <- (i-1)*9537+1
    end <- start + 9537 - 1
    distance_matrix_nbp[start:end,] <- res_squared
  }
  
  #   
  # }
  return(distance_matrix_nbp)
}

  
  
  
  
  
  distance_matrix_nbp = matrix(ncol = nrow(propensity_scores)+1, nrow = nrow(propensity_scores)*5) # 5 will have
  # to be subbed by imp number (m)
  comp = complete(abc$missingness_treated_dataset, "all", include = FALSE)
  
  
  for (i in 1:length(comp)) {
    
    data_for_this_iteration <- subset(propensity_scores, impset == i) # select all rows where impset is for this imputation
    
    # Matrix (in Lu et al. 2011)
    
    matj <- matrix(data = data_for_this_iteration$gear, nrow = nrow(propensity_scores), ncol = nrow(propensity_scores), byrow = F) # dims set by prop data not data for this iter?
    matk <- matrix(data = data_for_this_iteration$gear, nrow = nrow(propensity_scores), ncol = nrow(propensity_scores), byrow = T)
    
    res <- matj - matk
    res_squared <- res^2
    
    lpj <- matrix(data = data_for_this_iteration$'polly$lp', nrow = nrow(propensity_scores), ncol = nrow(propensity_scores), byrow = F)
    lpk <- matrix(data = data_for_this_iteration$'polly$lp', nrow = nrow(propensity_scores), ncol = nrow(propensity_scores), byrow = T)
    
    lp_res <- lpj - lpk
    lp_res_abs <- abs(lp_res)
    lp_calc <- 0.15 * sqrt(var(data_for_this_iteration$'polly$lp'))
    lp_logical <- lp_res_abs <= lp_calc
    
    lp_res_squared_plus_eps <- (lp_res^2) + eps
    
    res_squared[!lp_logical] <- 10^11
    res_squared[lp_logical] <- 10^11 * (lp_res_squared_plus_eps[lp_logical]) / res_squared[lp_logical]
    
   # impset <- i
   # print(i)
    res_squared <- cbind(res_squared, i)
    #result <- rbind(result, res_squared)
    start <- (i-1)*nrow(propensity_scores)+1
    end <- start + nrow(propensity_scores) - 1
    distance_matrix_nbp[start:end,] <- res_squared
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  