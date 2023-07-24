#source("source/func/get_model_formula.R")
source("source/func/extract_balanced_data.R")
source("source/func/fit_outcome_model.R")
source("source/func/extract_outcome_results.R")
source("source/func/standardise_outcome_format.R")


outcome_analysis_stage <- function(balanced_data, counterfactual_method, outcome_variable,
                                   treatment_variable, matching_variable, psmodel_obj,...){
  extracted_balanced_data <- extract_balanced_data(balanced_data, psmodel_obj) # 
  #model_formula <- get_model_formula(outcome_variable,
   #                                  treatment_variable,
    #                                 matching_variable,
     #                                doubly = TRUE)  
  fitted_model <- fit_outcome_model(extracted_balanced_data, outcome_variable = outcome_variable, 
                                    treatment_variable = treatment_variable, 
                                    matching_variable = matching_variable) # 
  extracted_outcome_results <- extract_outcome_results(fitted_model) # 
  standardised_format <- standardise_outcome_format(extracted_outcome_results) # 
  return(standardised_format)
}


