
check_selected_outcome <- function(data, outcome){
  
  if (length(unique(data[[outcome]])) == 2){
    var_type = 'Binary'
  } else if (length(unique(data[[outcome]])) > 2 & length(unique(data[[outcome]])) < 5){
    var_type = 'Categarical'
  } else if (length(unique(data[[outcome]])) > 5){
    var_type = 'Continuous'
  } else{
    var_type = 'Constant'
  }
  return(var_type)
}