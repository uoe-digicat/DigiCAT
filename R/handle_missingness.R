# perhaps want to saved any logged events somewhere for diagnosis

require(mice)

handle_missingness <- function(.data, missing_method = NULL, nonresponse_weights = weighting_variable,
                                ...){
  switch(missing_method, 
         
         complete = {
           
           handled_missingness = na.omit(.data)
           },
         
         mi = {
           
           handled_missingness = mice(.data, m = 5, maxit = 20)
         },
         
         weighting = {
           handled_missingness = svydesign(ids = ~1, weights = nonresponse_weights, data = .data, ...) 
         },
         stop("How should i deal with missingness? Should be one of 'mi', 'complete', 'weighting'")
  )
  return(handled_missingness) # in case of weighting, add in return of data_design

}
