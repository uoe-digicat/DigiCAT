## Function to check variable selection for counterfactual analysis

check_selected_variables <- function(outcome, treatment, matchvars, covars){
  
  input <- reactive({
    # Make sure requirements are met
    req(input$input)
    get(input$input, "package:datasets", inherits = FALSE)
  })
  
  
  
  
  
  
  # ## Create list with input
  # input_ls <- list(outcome = NULL,
  #                  treatment = NULL,
  #                  matchvars = NULL,
  #                  covars = NULL)
  # 
  # ## If variables exist, amend list
  # if (exists("outcome")){input_ls$outcome <- outcome}
  # if (exists("treatment")){input_ls$treatment <- treatment}
  # if (exists("matchvars")){input_ls$matchvars <- matchvars}
  # if (exists("covars")){input_ls$covars <- covars}
  # 
  # 
  # 
  # ## Create list to store information about inputed variables
  # check_variable_input_df <- list(outcome_exists = NULL,
  #                                 treatment_exists = NULL,
  #                                 matching_exists = NULL,
  #                                 outcome_treatment_different = NULL,
  #                                 outcome_matching_different = NULL,
  #                                 outcome_covar_different = NULL,
  #                                 treatment_matching_different = NULL,
  #                                 treatment_covar_different = NULL)
  # 
  # ## Check all input exists
  # if (!is.null(input_ls$outcome)){
  #   check_variable_input_df$outcome_exists <- TRUE
  # } else{check_variable_input_df$outcome_exists <- FALSE}
  # 
  # if (!is.null(input_ls$treatment)){
  #   check_variable_input_df$treatment_exists <- TRUE
  # } else{check_variable_input_df$treatment_exists <- FALSE}
  # 
  # if (!is.null(input_ls$matchvars)){
  #   check_variable_input_df$matching_exists <- TRUE
  # } else{check_variable_input_df$matching_exists <- FALSE}  
  # 
  # 
  # ## If any input missing, build missing variable statement
  # if (any(!as.logical(check_variable_input_df[grepl("_exists", names(check_variable_input_df))]))){
  #   
  #   missing_variable_statment <- paste0("Oops! Looks like you havn't selected all of the nessessary input, please select the following before proceeding:")
  #   
  #   if (check_variable_input_df$outcome_exists == FALSE){
  #     missing_variable_statment <- paste0(missing_variable_statment, "\n", " Outcome")
  #   }
  #   
  #   if (check_variable_input_df$treatment_exists == FALSE){
  #     missing_variable_statment <- paste0(missing_variable_statment, "\n", "Treatment")
  #   }
  # 
  #   if (check_variable_input_df$matching_exists == FALSE){
  #     missing_variable_statment <- paste0(missing_variable_statment, br(), "Matching Variables")
  #   }
  #   
  #   ## return statement and exit function if more input needed
  #   return(missing_variable_statment)
  #   stop()
  # }
  # 
  # ## If all required input is present carry on with checks
  # if (input_ls$outcome == input_ls$treatment){
  #   check_variable_input_df$outcome_treatment_different <- FALSE
  # } else{check_variable_input_df$outcome_treatment_different <- TRUE}
  # 
  # if (any(input_ls$outcome %in% input_ls$matchvars)){
  #   check_variable_input_df$outcome_matching_different <- FALSE
  # } else{check_variable_input_df$outcome_matching_different <- TRUE}
  # 
  # if (any(input_ls$outcome %in% input_ls$covars)){
  #   check_variable_input_df$outcome_covar_different <- FALSE
  # } else{check_variable_input_df$outcome_covar_different <- TRUE}
  # 
  # if (any(input_ls$treatment %in% input_ls$matchvars)){
  #   check_variable_input_df$treatment_matching_different <- FALSE
  # } else{check_variable_input_df$treatment_matching_different <- TRUE}
  # 
  # if (any(input_ls$treatment %in% input_ls$covars)){
  #   check_variable_input_df$treatment_covar_different <- FALSE
  # } else{check_variable_input_df$treatment_covar_different <- TRUE}
  # 
  # # if (any(input_ls$matchvars %in% input_ls$covars)){
  # #   check_variable_input_df$matching_covar_different <- FALSE
  # # } else{check_variable_input_df$matching_covar_different <- TRUE}
  # 
  # ## If there is a variable selection conflict, build statement to alert user
  # if (any(!as.logical(check_variable_input_df[grepl("_different", names(check_variable_input_df))]))){
  #   
  #   variable_conflict_statment <- paste0("Oops! Looks like you've selected the same variables for multiple inputs, please ammend the following before proceeding:")
  # 
  #   if (check_variable_input_df$outcome_treatment_different == FALSE){
  #     variable_conflict_statment <- paste0(variable_conflict_statment,
  #                                                              br(),
  #                                                              "You have selected ", input_ls$outcome, 
  #                                                              " as both an outcome and treatment, please reselect outcome and/or treatment")
  #     
  #   }
  #   if (check_variable_input_df$outcome_matching_different == FALSE){
  #     variable_conflict_statment <- paste0(variable_conflict_statment,
  #                                          br(),
  #                                          "You have selected ", input_ls$outcome, 
  #                                          " as both an outcome and a matching variable, please reselect outcome and/or matching variables")
  #     
  #   }
  #   if (check_variable_input_df$outcome_covar_different == FALSE){
  #     variable_conflict_statment <- paste0(variable_conflict_statment,
  #                                          br(),
  #                                          "You have selected ", input_ls$outcome, 
  #                                          " as both an outcome and as a covariate, please reselect outcome and/or covariates")
  #   }
  #   if (check_variable_input_df$treatment_matching_different == FALSE){
  #     variable_conflict_statment <- paste0(variable_conflict_statment,
  #                                          br(),
  #                                          "You have selected ", input_ls$treatment, 
  #                                          " as both a treatment and a matching variable, please reselect outcome and/or  matching variables")
  #   }
  #   if (check_variable_input_df$treatment_covar_different == FALSE){
  #     variable_conflict_statment <- paste0(variable_conflict_statment,
  #                                          br(),
  #                                          "You have selected ", input_ls$treatment, 
  #                                          " as both a treatment and a covariate, please reselect outcome and/or covariates")
  #   }
  #   # if (check_variable_input_df$matching_covar_different == FALSE){
  #   #   variable_conflict_statment <- paste0(variable_conflict_statment,
  #   #                                        br(),
  #   #                                        "You have selected ", input_ls$matchvars[input_ls$matchvars %in% input_ls$covars], 
  #   #                                        " as both (a) matching variable(s) and as (s) covariate(s), please reselect matching variables and/or covariates")
  #   # }
  #   
  #   return(variable_conflict_statment)
  # }
}


