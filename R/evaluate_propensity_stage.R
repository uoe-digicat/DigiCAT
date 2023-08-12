source("source/func/evaluate_imputations.R") # only runs for MI
source("source/func/check_common_support.R") # only runs for CC currently - add in MI ?


evaluate_propensity_stage <- function(estimation_model_object, evaluation_method, graph_display,
                                      ...){
  check_imputations <- evaluate_imputations(estimation_model_object, # only for MI objects
                                            evaluation_method, graph_display,...)
  common_support <- check_common_support(estimation_model_object) # only for CC objects
}