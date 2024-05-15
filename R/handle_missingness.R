#' Function to handle missingness in datasets
#'
#' @param .data 
#' @param missing_method 
#' @param design_object 
#' @param ... 
#'
#' @import mice
#' @import parallel
#' @import mitools

handle_missingness <- function(.data,missing_method = NULL,
                               counterfactual_method = NULL,
                               treatment_variable = NULL,
                               cluster_variable = NULL, weighting_variable = NULL,
                               strata_variable = NULL,
                               ...){
  switch(missing_method, 
         
         complete = {
           
           handled_missingness = na.omit(.data)
           
           if (!is.null(cluster_variable)) {
             cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
           } else {
             # Set cluster_formula to ~1 if cluster_variable is not provided
             cluster_formula <- as.formula("~1")
           }
           
           # Check if weighting_variable is provided
           if (!is.null(weighting_variable)) {
             # Convert weighting_variable to a formula
             weighting_formula <- as.formula(paste("~as.numeric(as.character(", weighting_variable, "))"))
           } else {
             # Use another variable as the default if weighting_variable is not provided
             weighting_formula <- NULL
           }
           
           # Check if strata_variable is provided
           if (!is.null(strata_variable)) {
             strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
           } else {
             # Set strata_formula to NULL if strata_variable is not provided
             strata_formula <- NULL
           }
           
           design_object <- svydesign(ids = cluster_formula,
                                      weights = weighting_formula,
                                      strata = strata_formula,
                                      data = handled_missingness)
 
          # design_object <- svydesign(ids = if (!is.null(cluster_variable)) handled_missingness[[cluster_variable]] else ~1,
          #                                weights = if (!is.null(weighting_variable)) handled_missingness[[weighting_variable]] else NULL,
          #                                strata = if (!is.null(strata_variable)) handled_missingness[[strata_variable]] else NULL,
          #                                data = handled_missingness)
           
         },
         
         mi = {
           
           # if(counterfactual_method == "psm"){
           
           ## Remove any rows without treatment data - HC
           .data <- .data[!is.na(.data[,treatment_variable]),]
           
           ## Perform MI without treatment as we do not want this to be used as a predictor - HC
           handled_missingness = mice(.data, m = 5, maxit = 20,
                                      method = "rf",
                                      quickpred(.data, exclude = treatment_variable)) # default options
           # allow user to alter m & maxit according to FMI & convergence
           # will not be congenial unless include interactions of substantive outcome model
           # cannot reliably obtain congeniality -> default is random forest imputation
           # add condition - if weighting, using MI-bootstrap approach?
           # switch to ML/RF method to remove need to consider functional form of imp model
           
           complete_imps <- complete(handled_missingness, "all")

           # Check if cluster_variable is provided
           if (!is.null(cluster_variable)) {
             cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
           } else {
             # Set cluster_formula to ~1 if cluster_variable is not provided
             cluster_formula <- as.formula("~1")
           }
           
           # Check if weighting_variable is provided
           if (!is.null(weighting_variable)) {
             # Convert weighting_variable to a formula
             weighting_formula <- as.formula(paste("~as.numeric(as.character(", weighting_variable, "))"))
           } else {
             # Use another variable as the default if weighting_variable is not provided
            weighting_formula <- NULL
           }
           
           # Check if strata_variable is provided
           if (!is.null(strata_variable)) {
             strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
           } else {
             # Set strata_formula to NULL if strata_variable is not provided
             strata_formula <- NULL
           }
           
           design_object <- svydesign(ids = cluster_formula,
                                          weights = weighting_formula,
                                          strata = strata_formula,
                                          data = imputationList(complete_imps))
         }, 
        
         
         weighting = {
           if (!is.null(cluster_variable)) {
             cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
           } else {
             # Set cluster_formula to ~1 if cluster_variable is not provided
             cluster_formula <- as.formula("~1")
           }
           
           # Check if weighting_variable is provided
           if (!is.null(weighting_variable)) {
             # Convert weighting_variable to a formula
             weighting_formula <- as.formula(paste("~as.numeric(as.character(", weighting_variable, "))"))
           } else {
             # Use another variable as the default if weighting_variable is not provided
             weighting_formula <- NULL
           }
           
           # Check if strata_variable is provided
           if (!is.null(strata_variable)) {
             strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
           } else {
             # Set strata_formula to NULL if strata_variable is not provided
             strata_formula <- NULL
           }
           
           .data = subset(.data, (!is.na(.data[[weighting_variable]])))
           
           design_object <- svydesign(ids = cluster_formula,
                                       weights = weighting_formula,
                                       strata = NULL,
                                       data = .data)
           
           handled_missingness = design_object 
           design_object = design_object
         },
         
         parallel_mi = {
           
           # Using all cores can slow down the computer
           # significantly, I therefore try to leave one
           # core alone in order to be able to do something 
           # else during the time the code runs
           cores_2_use <- detectCores() - 1
           handled_missingness = futuremice(.data, m = 5, maxit = 20, # maybe add parallel seed etc
                                            method = "rf", n.core = cores_2_use) 
           
           complete_imps <- complete(handled_missingness, "all")
           design_object <- svydesign(ids = if (!is.null(cluster_variable)) handled_missingness[[cluster_variable]] else ~1,
                                      weights = if (!is.null(weighting_variable)) handled_missingness[[weighting_variable]] else NULL,
                                      strata = if (!is.null(strata_variable)) handled_missingness[[strata_variable]] else NULL,
                                      data = imputationList(complete_imps))
           # if n cores exceeds m, cores used will be set to equal m
         },
         stop("How should i deal with missingness? Should be one of 'mi', 'complete', 'weighting', 'parallel_mi'")
  )
  return(list(handled_missingness, design_object))
  
}

















