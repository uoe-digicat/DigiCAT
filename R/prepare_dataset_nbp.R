prepare_dataset_nbp <- function(propensity_scores, treatment_variable, missing_method,...){
  
  if(missing_method == "complete"){
    propensity_scores[[treatment_variable]] = as.numeric(as.character(propensity_scores[[treatment_variable]]))
    propensity_scores$ID <- seq_along(propensity_scores[,1])
  } 
  #else if(missing_method == "mi"){
   # add 
  #}
  return(propensity_scores)
}
