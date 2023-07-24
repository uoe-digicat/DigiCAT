prepare_dataset_nbp <- function(propensity_score,...){
  
  propensity_score$treatment_variable<-as.numeric(propensity_score$treatment_variable)
  
}