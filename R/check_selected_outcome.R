
check_selected_outcome <- function(data, outcome, categorical_variables){
  
  ## If no categorical variables, outcome is continuous
  if(is.null(categorical_variables)){
    var_type = 'Continuous'
  } else{
    non_na_values <- length(setdiff(data[[outcome]], c(NA)))
    if (non_na_values == 2){
      var_type = 'Binary'
    } else if (outcome %in% categorical_variables){
      var_type = 'Categorical'
    } else if (!outcome %in% categorical_variables){
      var_type = 'Continuous'
    }
  }
  return(var_type)
}