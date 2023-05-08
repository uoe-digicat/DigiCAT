## Script to check data upon initial upload

initial_data_check <- function(x){
  
  ## Make list to store function output
  initial_data_check_ls <- list(some_nonnumeric = NULL,
                                impossible_value = NULL,
                                too_small = NULL)
  
  ## Check if dataframe only contains numeric values
  if (all(sapply(x, is.numeric))){
    initial_data_check_ls$some_nonnumeric <- FALSE
  }else{initial_data_check_ls$some_nonnumeric <- TRUE}
  
  ## Check if dataframe contains impossible values
  if (any(x == -999, na.rm = T)){
    initial_data_check_ls$impossible_value <- TRUE
  }else{ initial_data_check_ls$impossible_value <- FALSE}
  
  ## Check dimensions of dataframe
  if (nrow(x) < 10){
    initial_data_check_ls$too_small <- TRUE
  }else{initial_data_check_ls$too_small <- FALSE}
  return(initial_data_check_ls)
}

