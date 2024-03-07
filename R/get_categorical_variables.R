#' Get categorical column names function
get_categorical_variables <- function(df){
  
  categorical_variables <- NULL
  
  ## If column contains equal to or less than 5 unique values, get column name
  categorical_variables <- names(df[sapply(df, function(df) length(unique(df))<=5)])
  
  return(categorical_variables)
  
}


