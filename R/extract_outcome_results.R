extract_outcome_results <- function(fitted_model, missing_method,...){
  if("comparisons" %in% class(fitted_model) & missing_method == "weighting"){ # weighting ME
    extracted_outcome_results = summary(fitted_model, conf.int = TRUE)
    return(list(extracted_outcome_results, process = "weighting"))
    
  }else if("comparisons" %in% class(fitted_model) & missing_method == "complete"){ # complete ME
    extracted_outcome_results = summary(fitted_model, conf.int = TRUE)
    return(list(extracted_outcome_results, process = "cc"))
    
  }
  else if("mipo" %in% class(fitted_model) & missing_method == "mi"){ # MI ME
    extracted_outcome_results = summary(fitted_model, conf.int = TRUE)
    return(list(extracted_outcome_results, process = "mi"))
    
  }else if("lm" %in% class(fitted_model) & missing_method == "complete"){ # LM no ME, complete
    extracted_outcome_results =as.data.frame(dplyr::bind_cols(coef(summary(fitted_model)), confint(fitted_model)))
    return(list(extracted_outcome_results, process = "cc"))
    
  }else if("svyglm" %in% class(fitted_model) & missing_method == "weighting"){
    extracted_outcome_results = summary(fitted_model)
    return(list(extracted_outcome_results, process = "weighting"))
    
  }
} 

