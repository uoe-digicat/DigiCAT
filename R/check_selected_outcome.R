
check_selected_outcome <- function(data, outcome){
  
  non_na_values <- length(setdiff(data[[outcome]], c(NA)))
  
  if (non_na_values == 2){
    var_type = 'Binary'
  } else if (non_na_values > 2 & non_na_values <= 5){
    var_type = 'Categorical'
  } else if (non_na_values > 5){
    var_type = 'Continuous'
  } else{
    var_type = 'Constant'
  }
  return(var_type)
}