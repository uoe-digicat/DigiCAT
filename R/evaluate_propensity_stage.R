# source("R/evaluate_imputations.R") # only runs for MI
# source("R/check_common_support.R") # only runs for CC currently - add in MI ? NOT POLR

evaluate_propensity_stage <- function(estimation_model_object, evaluation_method, graph_display,
                                      missing_method,
                                      ...){
  switch(evaluation_method, 
         mi = {
           performance =  evaluate_imputations(estimation_model_object, # only for MI objects
                                               evaluation_method, graph_display,...)
         },
         support = {
           performance = check_support(estimation_model_object, missing_method,...) # only for CC objects
         },
         gbm_oob = {
           performance = gbm.perf(estimation_model_object$estimated_propensity_model, method = "OOB")
         },
         gbm_heldout = { # need ylim
           performance = gbm.perf(estimation_model_object$estimated_propensity_model, method = "test")
         },
         gbm_cv = { # need cv folds > 1
           performance = gbm.perf(estimation_model_object$estimated_propensity_model, method = "cv")
         },
  )
  return(performance)
}