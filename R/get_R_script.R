#' @param data Dataframe
#' @param data_source Source of data: "own", or "sample"
#' @param file_path File path to data
#' @param categorical_variables Vector of categorical variable names in dataset
#' @param outcome_variable Name of outcome variable in dataset (character string)
#' @param treatment_variable Name of treatment variable in dataset (character string)
#' @param matching_variables Vector of matching variable names in dataset
#' @param covariates Vector of covariate names in dataset
#' @param CF_approach Name of counterfactual appraoch being taken (character string)
#' @param balancing_model Name of balancing model being used (character string)
#' @param missingness Name of method of dealing with missingness being used (character string): "mi", or "complete"
#' @param matching_method Name of balancing method being used (character string): "nearest" or "optimal"
#' @param matching_ratio Name of balancing ratio being used (numeric)
#' @param outcome_model Name of outcome model being used (character string)

get_R_script <- function(
  ## Data upload
  data_source, 
  file_path = NULL,
  categorical_variables = NULL,
  outcome_variable, 
  treatment_variable, 
  matching_variables, 
  covariates = NULL,
  weighting_variable = NULL,
  cluster_variable = NULL,
  stratification_variable = NULL,
  ## Counterfactual approach
  CF_approach, 
  missingness,
  balancing_model, 
  ## Balancing
  matching_method = NULL,
  matching_ratio = NULL, 
  ## Outcome model
  outcome_model){
  
  ## Libraries required ----
  library_code <- paste0(
    "install.packages('remotes')\n",
    "library(remotes)\n",
    "remotes::install_github('josiahpjking/DigiCAT@develop')\n",
    "library(DigiCAT)\n"
  )

  
  ## Data upload ----
  if (data_source == "own"){
    data_source_code <- paste0("\n","\n", "## Load in data (own)\n", "df <- read.csv(",file_path,")")
    }
  if (data_source == "sample"){
    data_source_code <- paste0("\n","\n", "## Load in data (sample)\n", "df <- DigiCAT::zp_eg")
    }
  
  ## Variable input
  variable_input_code <- paste0("\n \n ## Define input variables \n",
                                "categorical_vars <- c(", paste0("'", categorical_variables, "'", collapse = ","),")\n",
                                "y_var <- ", "'",  outcome_variable, "'","\n",
                                "t_var <- ", "'", treatment_variable, "'\n",
                                "m_vars <- c(", paste0("'", matching_variables, "'", collapse = ","),")\n"
                                )
  
  if (is.null(covariates)){
    variable_input_code <- paste0(variable_input_code, "covars <- NULL \n")
  } else{
    variable_input_code <- paste0(variable_input_code, "covars <- c(", paste0("'",covariates, "'", collapse = ","),")\n")
  }
  
  if (is.null(weighting_variable)){
    variable_input_code <- paste0(variable_input_code, "weighting_var <- NULL \n")
  } else{
    variable_input_code <- paste0(variable_input_code, "weighting_var <- ", "'", weighting_variable, "'\n")
  }
  
  if (is.null(cluster_variable)){
    variable_input_code <- paste0(variable_input_code, "cluster_var <- NULL \n")
  } else{
    variable_input_code <- paste0(variable_input_code, "cluster_var <- ", "'", cluster_variable, "'\n")
  }
  
  if (is.null(stratification_variable)){
    variable_input_code <- paste0(variable_input_code, "stratification_var <- NULL \n")
  } else{
    variable_input_code <- paste0(variable_input_code, "stratification_var <- ", "'", stratification_variable, "'\n")
  }
  
  ## Propensity score estimation ----
  
  PS_score_estimation_code <- paste0(
    "\n",
    "\n", 
    "## Estimate propensity scores\n",
    "PS_estimation_results <- DigiCAT:::estimation_stage(\n",
    "  .data = df,\n",
    "  treatment_variable = t_var,\n",
    "  matching_variable = m_vars,\n",
    "  weighting_variable = weighting_var,\n",
    "  cluster_variable = cluster_var,\n",
    "  strata_variable = stratification_var,\n",
    "  missing_method = '", missingness,"',\n",
    "  model_type = '", balancing_model,"'\n",
    ")"
    )
  
  
  ## Balancing ----
    
  if (CF_approach == "psm"){
    
    balancing_code <- paste0("\n",
                             "\n", 
                             "## Balance datasets using propensity score matching\n",
                             "balancing_results <- DigiCAT:::balance_data(\n",
                             "  treatment_variable = t_var,\n",
                             "  matching_variable = m_vars,\n",
                             "  counterfactual_method = '", CF_approach, "',\n",
                             "  missing_method = '", missingness,"',\n",
                             "  PS_estimation_object = PS_estimation_results,\n",
                             "  ratio = ", matching_ratio, ",\n",
                             "  method = '", matching_method,"'\n",
                             ")"
    )
    }
  
    if (CF_approach == "iptw"){
    
    balancing_code <- paste0("\n",
                             "\n", 
                             "## Balance datasets using IPTW\n\n",
                             "balancing_results <- DigiCAT:::balance_data(\n",
                             "  treatment_variable = t_var,\n",
                             "  matching_variable = m_vars,\n",
                             "  counterfactual_method = '", CF_approach, "',\n",
                             "  missing_method = '", missingness,"',\n",
                             "  PS_estimation_object = PS_estimation_results\n",
                             ")"
                             )
    }
  

  
  ## Outcome model ----
  
  outcome_model_code <- paste0(
    "\n",
    "\n", 
    "## Run outcome model \n",
    "outcome_model_results <- DigiCAT:::outcome_analysis_stage(\n",
    "  balanced_data = balancing_results,\n",
    "  outcome_variable = y_var,\n",
    "  treatment_variable = t_var,\n",
    "  matching_variable = m_vars,\n",
    "  sampling_weights = weighting_var,\n",
    "  nonresponse_weights = weighting_var,\n",
    "  weighting_variable = weighting_var,\n",
    "  cluster_variable = cluster_var,\n",
    "  strata_variable = stratification_var,\n",
    "  counterfactual_method = '", CF_approach, "',\n",
    "  missing_method = '", missingness,"',\n",
    "  psmodel_obj = PS_estimation_results\n",
    ")"
  )
  
  ## Create R script ----
  r_script <- c(library_code, data_source_code, variable_input_code, PS_score_estimation_code, 
                balancing_code, outcome_model_code)
  noquote(capture.output(cat(r_script)))
  
}

# get_R_script(data_source = "sample",
#              file_path = NULL,
#              categorical_variables = c("Gender", "Reading_age15"),
#              outcome_variable = "Anxiety_age17",
#              treatment_variable = "Reading_age15",
#              matching_variables = names(DigiCAT::zp_eg)[-c(1:3)],
#              covariates = NULL,
#              CF_approach = "psm",
#              balancing_model = "glm",
#              missingness = "mi",
#              matching_method = "NN",
#              matching_ratio = 1
#              )

