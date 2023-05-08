## function to automatically get list of categorical column names

get_categorical_variables <- function(x){
  
  categorical_variables <- NULL
  
  ## If column contains equal to or less than 5 unique values, class as categorical
  categorical_variables <- names(x[sapply(x, function(x) length(unique(x))<=5)])
  
  return(categorical_variables)
  
}
