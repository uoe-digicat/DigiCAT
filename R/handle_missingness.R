#' Function to handle missingness in datasets
#'
#' @param .data 
#' @param missing_method 
#' @param design_object 
#' @param ... 
#'
#' @import mice
#' @import parallel

handle_missingness <- function(.data, missing_method = NULL,
                               design_object = NULL,
                               counterfactual_method = NULL,
                               ...){
  switch(missing_method, 
         
         complete = {
           
           handled_missingness = na.omit(.data)
         },
         
         mi = {
           
           # if(counterfactual_method == "psm"){
           
           handled_missingness = mice(.data, m = 5, maxit = 20,
                                      method = "rf") # default options
           # allow user to alter m & maxit according to FMI & convergence
           # will not be congenial unless include interactions of substantive outcome model
           # cannot reliably obtain congeniality -> default is random forest imputation
           # add condition - if weighting, using MI-bootstrap approach?
           # switch to ML/RF method to remove need to consider functional form of imp model
         }, 
         # else if(counterfactual_method == "iptw"){
         #   handled_missingness = mice(.data, m = 5, maxit = 20,
         #                              method = "norm.boot") # default options
         #  }
         # },
         
         weighting = {
           
           handled_missingness = design_object # currently treats non-response and sampling the same
         },
         
         parallel_mi = {
           
           # Using all cores can slow down the computer
           # significantly, I therefore try to leave one
           # core alone in order to be able to do something 
           # else during the time the code runs
           cores_2_use <- detectCores() - 1
           handled_missingness = futuremice(.data, m = 5, maxit = 20, # maybe add parallel seed etc
                                            method = "rf", n.core = cores_2_use) 
           # if n cores exceeds m, cores used will be set to equal m
         },
         stop("How should i deal with missingness? Should be one of 'mi', 'complete', 'weighting', 'parallel_mi'")
  )
  return(handled_missingness) 
  
}

















