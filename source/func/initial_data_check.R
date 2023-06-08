## Script to check data upon initial upload

initial_data_check <- function(x){
  
  ## Make list to store function output
  initial_data_check_ls <- list(some_nonnumeric = NULL,
                                impossible_value = NULL,
                                small_rows = NULL,
                                small_cols = NULL)
  
  ## Check if dataframe only contains numeric values
  if (all(sapply(x, is.numeric))){
    initial_data_check_ls$some_nonnumeric <- FALSE
  }else{initial_data_check_ls$some_nonnumeric <- TRUE}
  
  ## Check dimensions of dataframe
  if (nrow(x) < 10){
    initial_data_check_ls$small_rows <- TRUE
  }else{initial_data_check_ls$small_rows <- FALSE}
  if (ncol(x) < 2){
    initial_data_check_ls$small_cols <- TRUE
  }else{initial_data_check_ls$small_cols <- FALSE}
  return(initial_data_check_ls)
}

