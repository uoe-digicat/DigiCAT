## Initial Data Check in DigiCAT
initial_data_check <- function(df){
  
  ## Make list to store function output
  initial_data_check_ls <- list(some_nonnumeric = NULL,
                                impossible_value = NULL,
                                small_rows = NULL,
                                small_cols = NULL)
  
  ## Check if dataframe only contains numeric values
  if (all(sapply(df, is.numeric))){
    initial_data_check_ls$some_nonnumeric <- FALSE
  }else{initial_data_check_ls$some_nonnumeric <- TRUE}
  
  ## Check dimensions of dataframe
  if (nrow(df) < 10){
    initial_data_check_ls$small_rows <- TRUE
  }else{initial_data_check_ls$small_rows <- FALSE}
  if (ncol(df) < 2){
    initial_data_check_ls$small_cols <- TRUE
  }else{initial_data_check_ls$small_cols <- FALSE}
  return(initial_data_check_ls)
}

