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
  
  else if(missing_method == "mi"){

  eps<-1*10^-100 #specify epsilon to be very small positive number
  
  comp = complete(PS_estimation_object$missingness_treated_dataset, "all", include = FALSE)
  multiply_by <- length(comp)
  
  distance_matrix_nbp = matrix(ncol = (nrow(PS_estimation_object[[1]][[1]])+1), nrow = (nrow(PS_estimation_object[[1]][[1]]))*multiply_by) # OG prop data not stacked
  
  for (i in 1:length(comp)) {
    
    data_for_this_iteration <- subset(estimated_propensity_model, impset == i) # select all rows where impset is for this imputation
    
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
    
    res_squared <- cbind(res_squared, i)
    start <- (i-1)*nrow(PS_estimation_object[[1]][[1]])+1
    end <- start + nrow(PS_estimation_object[[1]][[1]]) - 1
    distance_matrix_nbp[start:end,] <- res_squared
  }
  
  #   
  # }
  return(distance_matrix_nbp)
}

  ####
  
  
  data_for_this_iteration <- subset(propensity_scores, impset == i) # select all rows where impset is for this imputation
  
  distance_matrix_nbp = matrix(ncol = 33, nrow = (32*5)) # 5 will have to be subbed by imp number (m)
  comp = complete(abc$missingness_treated_dataset, "all", include = FALSE)
  
  
  for (i in 1:5) { # 1:length(comp)
    
    data_for_this_iteration <- subset(propensity_scores, impset == i) # select all rows where impset is for this imputation
    
    # Matrix (in Lu et al. 2011)
    
    matj <- matrix(data = data_for_this_iteration$gear, nrow = 32, ncol = 32, byrow = F) # dims set by prop data not data for this iter?
    matk <- matrix(data = data_for_this_iteration$gear, nrow = 32, ncol = 32, byrow = T)
    
    res <- matj - matk
    res_squared <- res^2
    
    lpj <- matrix(data = data_for_this_iteration$'polly$lp', nrow = 32, ncol = 32, byrow = F)
    lpk <- matrix(data = data_for_this_iteration$'polly$lp', nrow = 32, ncol = 32, byrow = T)
    
    lp_res <- lpj - lpk
    lp_res_abs <- abs(lp_res)
    lp_calc <- 0.15 * sqrt(var(data_for_this_iteration$'polly$lp'))
    lp_logical <- lp_res_abs <= lp_calc
    
    lp_res_squared_plus_eps <- (lp_res^2) + eps
    
    res_squared[!lp_logical] <- 10^11
    res_squared[lp_logical] <- 10^11 * (lp_res_squared_plus_eps[lp_logical]) / res_squared[lp_logical]
    
    res_squared <- cbind(res_squared, i)
    start <- (i-1)*32+1
    end <- start + 32 - 1
    distance_matrix_nbp[start:end,] <- res_squared
  }
  
  
  distance_matrix_nbp <- distance_matrix_nbp[,-33]
  
  mat1<- distance_matrix_nbp[1:32,]
  mat2<- distance_matrix_nbp[33:64,]
  mat3<- distance_matrix_nbp[65:96,]
  mat4<- distance_matrix_nbp[97:128,]
  mat5<- distance_matrix_nbp[129:160,]
  
  mat_split <- function(M, r, c){
    nr <- ceiling(nrow(M)/r)
    nc <- ceiling(ncol(M)/c)
    newM <- matrix(NA, nr*r, nc*c)
    newM[1:nrow(M), 1:ncol(M)] <- M
    
    div_k <- kronecker(matrix(seq_len(nr*nc), nr, byrow = TRUE), matrix(1, r, c))
    matlist <- split(newM, div_k)
    N <- length(matlist)
    mats <- unlist(matlist)
    dim(mats)<-c(r, c, N)
    return(mats)
  }
  
  c <- mat_split(distance_matrix_nbp, r = nrow(abc$missingness_treated_dataset[[1]]), 
                                               c = nrow(abc$missingness_treated_dataset[[1]]))
  
  list_matrices <- list(mat1,mat2,mat3,mat4,mat5) # end here and pass to next func
  
  nbp_matrices <- lapply(list_matrices, function(x) nbpMatching::distancematrix(x)) # end here and pass to next func
  
  # add id?
  
  nbp_matches <- lapply(nbp_matrices, function(x) nbpMatching::nonbimatch(x, threshold = 999999, # end here and pass to next func
                                                                          precision = 7))
  

  matched_data <- lapply(nbp_matches, function(x) x$halves[x$halves$Distance!=999999,]) # end here and pass to next fun
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  