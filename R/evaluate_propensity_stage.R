source("R/evaluate_imputations.R") # only runs for MI
source("R/check_common_support.R") # only runs for CC currently - add in MI ? NOT POLR


evaluate_propensity_stage <- function(estimation_model_object, evaluation_method, graph_display,
                                      ...){
  # switch(evaluation_method, 
  #        mi_performance = {
  # performance =  evaluate_imputations(estimation_model_object, # only for MI objects
  #                                           evaluation_method, graph_display,...)
  # },
  # support = {
  performance = check_support(estimation_model_object) # only for CC objects
  # })
  return(performance)
}