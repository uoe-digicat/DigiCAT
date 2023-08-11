prepare_dataset_nbp <- function(propensity_scores, treatment_variable, missing_method,...){
  
  if(missing_method == "complete"){
    propensity_score[[treatment_variable]] = as.numeric(propensity_score[[treatment_variable]])
  }
  return(propensity_score)
}
