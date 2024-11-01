
check_selected_outcome <- function(data, outcome){
  
  non_na_values <- length(setdiff(data[[outcome]], c(NA)))
  
  if (non_na_values == 2){
    var_type = 'binary'
  } else if (non_na_values > 2 & non_na_values <= 5){
    var_type = 'categorical'
  } else if (non_na_values > 5){
    var_type = 'continuous'
  } else{
    var_type = 'constant'
  }
  return(var_type)
}