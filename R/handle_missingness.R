# perhaps want to saved any logged events somewhere for diagnosis

require(mice)

handle_missingness <- function(.data, missing_method = NULL, ...){
  switch(missing_method, 
         
         complete = {
           
           complete_data = na.omit(.data)
           },
         
         mi = {
           
           complete_data = mice(.data, m = 5, maxit = 20)
         },
         
         non_response_weights = {
           data_design = svydesign(ids = id_variable, strata = strata_variable, weights = nonresponse_weights)
         },
         stop("How should i deal with missingness? Should be one of 'mi', 'complete', 'non_response_weights'")
  )
  return(complete_data) # in case of weighting, add in return of data_design

}
