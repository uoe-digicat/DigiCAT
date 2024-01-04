make_matrix_nbp <- function(propensity_data, estimated_propensity_model, treatment_variable, missing_method,
                            PS_estimation_object,...){
  
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
  
  else if(missing_method == "weighting"){
    eps = 1*10^-100 
    result = matrix(ncol = nrow(propensity_data), nrow = nrow(propensity_data))
    
    matj = matrix(data = propensity_data[[treatment_variable]], nrow = nrow(propensity_data), ncol = nrow(propensity_data), byrow = F)
    matk = matrix(data = propensity_data[[treatment_variable]], nrow = nrow(propensity_data), ncol = nrow(propensity_data), byrow = T)
    
    res = matj - matk 
    res_squared = res^2
    
    lpj = matrix(data = propensity_data$propensity_score, nrow = nrow(propensity_data), ncol = nrow(propensity_data), byrow = F)
    lpk = matrix(data = propensity_data$propensity_score, nrow = nrow(propensity_data), ncol = nrow(propensity_data), byrow = T)
    
    lp_res = lpj - lpk
    lp_res_abs = abs(lp_res)
    lp_calc = 0.15 * sqrt(var(propensity_data$propensity_score))
    lp_logical = lp_res_abs <= lp_calc
    
    lp_res_squared_plus_eps = (lp_res^2) + eps
    
    res_squared[!lp_logical] = 10^11
    res_squared[lp_logical] = 10^11 * (lp_res_squared_plus_eps[lp_logical]) / res_squared[lp_logical]
    
    distance_matrix_nbp = res_squared
    row.names(distance_matrix_nbp) <- propensity_data$ID
    
  }
  
  else if(missing_method == "mi"){
    
    eps<-1*10^-100 #specify epsilon to be very small positive number
    
    comp = complete(PS_estimation_object$missingness_treated_dataset, action = "all", include = FALSE)
    multiply_by <- length(comp)
    
    distance_matrix_nbp = matrix(ncol = (nrow(PS_estimation_object[[1]][[1]])+1), nrow = (nrow(PS_estimation_object[[1]][[1]]))*multiply_by) # OG prop data not stacked
    
    distance_matrix_nbp_list <- list()
    
    for (i in 1:length(comp)) {
      
      data_for_this_iteration <- subset(propensity_data, impset == i) # select all rows where impset is for this imputation
      
      # Matrix (in Lu et al. 2011)
      
      matj <- matrix(data = data_for_this_iteration[[treatment_variable]], nrow = nrow(PS_estimation_object[[1]][[1]]), ncol = nrow(PS_estimation_object[[1]][[1]]), byrow = F) # dims set by prop data not data for this iter?
      matk <- matrix(data = data_for_this_iteration[[treatment_variable]], nrow = nrow(PS_estimation_object[[1]][[1]]), ncol = nrow(PS_estimation_object[[1]][[1]]), byrow = T)
      
      res <- matj - matk
      res_squared <- res^2
      
      lpj <- matrix(data = data_for_this_iteration$'polly$lp', nrow = nrow(PS_estimation_object[[1]][[1]]), ncol = nrow(PS_estimation_object[[1]][[1]]), byrow = F)
      lpk <- matrix(data = data_for_this_iteration$'polly$lp', nrow = nrow(PS_estimation_object[[1]][[1]]), ncol = nrow(PS_estimation_object[[1]][[1]]), byrow = T)
      
      lp_res <- lpj - lpk
      lp_res_abs <- abs(lp_res)
      lp_calc <- 0.15 * sqrt(var(data_for_this_iteration$'polly$lp'))
      lp_logical <- lp_res_abs <= lp_calc
      
      lp_res_squared_plus_eps <- (lp_res^2) + eps
      
      res_squared[!lp_logical] <- 10^11
      res_squared[lp_logical] <- 10^11 * (lp_res_squared_plus_eps[lp_logical]) / res_squared[lp_logical]
      
      
      distance_matrix_nbp_list[[i]] <- res_squared
      
    }
    
    if (missing_method %in% c("complete", "weighting")) {
      # If complete case or weighting is selected, return distance_matrix_nbp
      return(distance_matrix_nbp)
    } else if (missing_method == "mi") {
      # If mi is selected, return matches_list
      return(distance_matrix_nbp_list)
    } else {
      stop("Invalid missing_method. Supported values: 'complete', 'weighting', 'mi'")
    }
  }
}
    
#   }
#   return(matches_list)
# }


  
  
  
  
  
  
  
  
  
  
  










  
  
  
  