#' Function to handle missingness in datasets
#'
#' @param .data 
#' @param missing_method 
#' @param design_object 
#' @param ... 
#'
#' @import mice

handle_missingness <- function(.data, missing_method = NULL,
                               design_object = NULL,
                                ...){
  switch(missing_method, 
         
         complete = {
           
           handled_missingness = na.omit(.data)
           },
         
         mi = {
           
           handled_missingness = mice(.data, m = 5, maxit = 20,
                                      method = "rf") # default options
           # allow user to alter m & maxit according to FMI & convergence
           # will not be congenial unless include interactions of substantive outcome model
           # cannot reliably obtain congeniality -> default is random forest imputation
           # add condition - if weighting, using MI-bootstrap approach?
           # switch to ML/RF method to remove need to consider functional form of imp model
         },
         
         weighting = {
           
           handled_missingness = design_object # currently treats non-response and sampling the same
         },
         stop("How should i deal with missingness? Should be one of 'mi', 'complete', 'weighting'")
  )
  return(handled_missingness) 

}


















