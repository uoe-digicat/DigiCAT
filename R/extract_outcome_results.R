extract_outcome_results <- function(fitted_model){
  if(class(fitted_model) == "list"){
    extracted_outcome_results = mice::pool(fitted_model) |> summary()
    return(list(extracted_outcome_results, process = "mi"))
    
  } else if(class(fitted_model) == "lm"){
    extracted_outcome_results = summary(fitted_model)
    return(list(extracted_outcome_results, process = "cc"))
    
  }
}
