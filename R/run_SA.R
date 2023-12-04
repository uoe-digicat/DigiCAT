#### Sensitivity Analysis ####
run_SA <- function(PS_object, balanced_data, missing_method, outcome_variable, SA_type,...){ # sensitivity analysis type
  switch(SA_type,
         rosenbaum_SA = {
           SA_results = perform_rosenbaum_SA(PS_object, balanced_data, missing_method, outcome_variable,...)
         },
         PS_calibration = {
           
         },
         VW_A_formula = {
           
         },
         E_value = {
           
         },
         stop("Need a valid method to run the sensitivity analysis")
  )
  return(SA_results)
  
}


perform_rosenbaum_SA <- function(PS_object, balanced_data, missing_method, outcome_variable,...){
  if(missing_method == "complete"){
  mpairs <- cbind(PS_object$missingness_treated_dataset[row.names(balanced_data$match.matrix), outcome_variable],
                  PS_object$missingness_treated_dataset[balanced_data$match.matrix, outcome_variable])
  SA_results <- rbounds::psens(x=mpairs[,1],y=mpairs[,2])
  }
 return(SA_results)
}

