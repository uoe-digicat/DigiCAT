source("source/func/evaluate_imputations.R") # only runs for MI
source("source/func/check_common_support.R") # only runs for CC currently


evaluate_propensity_stage <- function(estimation_model_object, evaluation_method, graph_display,
                                      ...){
  check_imputations <- evaluate_imputations(estimation_model_object, # only for MI objects
                                            evaluation_method, graph_display,...)
  #auc_value <- check_auc(estimated_propensity_model)
  common_support <- check_common_support(estimation_model_object) # only for CC objects
  #propensity_model_residuals <- get_propensity_model_residuals(estimated_propensity_model)
}