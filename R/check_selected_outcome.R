
check_selected_outcome <- function(data, outcome, categorical_variables){
  

  ## If no categorical variables, outcome is continuous
  if(is.null(categorical_variables)){
    var_type = 'continuous'
  } else{
    non_na_values <- length(setdiff(data[[outcome]], c(NA)))
    if (non_na_values == 2){
      var_type = 'binary'
    } else if (outcome %in% categorical_variables){
      var_type = 'categorical'
    } else if (!outcome %in% categorical_variables){
      var_type = 'continuous'
    }
  }
  return(var_type)
}