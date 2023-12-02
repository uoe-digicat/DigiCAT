#### Sensitivity Analysis ####
run_SA <- function(matched_object, .data, SA_type){ # sensitivity analysis type
  switch(SA_type,
         rosenbaum_SA = {
           SA_results = perform_rosenbaum_SA(model_type, fitted_model, missing_method,...)
         },
         PS_calibration = {
           
         },
         VW_A_formula = {
           
         },
         E_value = {
           
         })
}


### scrap
mpairs <- cbind(abc$missingness_treated_dataset[row.names(ghi$match.matrix), 'y'],
                abc$missingness_treated_dataset[ghi$match.matrix, 'y'])
library(rbounds)

psens(x=mpairs[,1],y=mpairs[,2], Gamma = 6, GammaInc = 1) # x is Tr group outcomes, y is ctrl group outcomes
