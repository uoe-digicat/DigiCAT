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

get_R_script <- function(data = NULL, data_source, 
  file_path = NULL,
  categorical_variables = NULL,
  outcome_variable = NULL, 
  treatment_variable = NULL, 
  matching_variables = NULL, 
  covariates = NULL,
  ## Counterfactual approach
  CF_approach = NULL, 
  ## Balancing model
  balancing_model = NULL, 
  missingness = NULL,
  ## Balancing
  matching_method = NULL,
  matching_ratio = NULL, 
  ## Outcome model
  outcome_model = NULL){
  
  ## Libraries required ----
  library_code <- paste0(
    "library(WeightIt)", "\n",
    "library(MatchIt)", "\n",
    "library(MatchThem)", "\n"
  )
  
  ## Data upload ----
  if (data_source == "own"){
    data_source_code <- paste0("\n","\n", "## Load in data (own)", "\n", "df <- read.csv(",file_path,")")
    }
  if (data_source == "sample"){
    data_source_code <- paste0("\n","\n", "## Load in data (sample)", "\n", "df <- DigiCAT::zp_eg")
    }
  
  ## Variable input
  variable_input_code <- paste0("\n","\n", "## Define input variables","\n", 
                                paste0("categorical_vars <- c(", paste0("'", categorical_variables, "'", collapse = ","),")"),"\n",
                                "y_var <- ","'",  outcome_variable, "'","\n",
                                "t_var <- ", "'", treatment_variable, "'", "\n",
                                paste0("m_vars <- c(", "'", paste0(matching_variables, collapse = ","), "'",")"),"\n",
                                paste0("covars <- c(", "'", paste0(covariates, collapse = ","), "'",")"), "\n")
  ## Balancing model ----
  
  ## glm formula
  formula_code <- paste0(
    paste0("\n","\n", "## Define balancing model formula"),"\n",
    paste0("balancing_model_mod <- paste0(t_var,'~',paste0(m_vars,collapse='+'))"))
  
  ## Missingness
  
  if(missingness == "complete"){
    balancing_model_code <- paste0("\n","\n", "## Remove missing values from data using listwise deletion","\n",
                                   paste0("ps_data <- na.omit(df[,unique(c(t_var, y_var, m_vars, covars))])"),"\n",
                                   paste0("\n","\n", "## Run balancing model using probit regression"),"\n",
                                   paste0("ps_mod <- sem(balancing_model_mod, ps_data, link='probit', ordered = c(t_var))"),"\n",
                                   paste0("ps_score <- pnorm(ps_mod@Data@eXo[[1]] %*% coef(ps_mod)[1:ncol(ps_mod@Data@eXo[[1]])]-coef(ps_mod)[ncol(ps_mod@Data@eXo[[1]])+1])"),
                                   "\n")
  }
  
  if(missingness == "mi"){
    balancing_model_code <- paste0("\n","\n", "## Remove missing values from data using multiple imputation","\n",
                                   paste0("ps_data <- mice(df, m = 5)"),"\n",
                                   paste0("ps_mod = lapply(complete(ps_data, 'all'),", "\n", 
                                          "function(x) glm(as.formula(balancing_model_mod), data = x, family=binomial(link='probit')))"),"\n",
                                          paste0("ps_score = lapply(complete(ps_data, 'all'), 
                                                 function(x) predict(glm(as.formula(balancing_model_mod), data = x, family=binomial(link='probit')), type = 'response'))"),
                                   "\n")
  }
  
  balancing_model_code <- c(balancing_model_code,
                            paste0("psmodel_obj <- (list(data = ps_data, mod = ps_mod, score = ps_score, ps_modclass = 'glm'))"))
  
  ## Balancing ----
  
  balancing_code <- paste0("\n","\n", "## Balance datasets using IPTW","\n")
  
  if (CF_approach == "weighting"){
    
    if (missingness == "mi"){
      balancing_code <- c(balancing_code, paste0("balancing_res <- weightthem(as.formula(mod), datasets = psmodel_obj$data, approach = 'within', method=psmodel_obj$ps_modclass)"))
    } else{
      balancing_code <- c(balancing_code, paste0("balancing_res <- weightit(as.formula(mod), data = psmodel_obj$data, ps = psmodel_obj$score)"))
    }
  }
  
  if (CF_approach == "matching"){
    
    if (missingness == "mi"){
      balancing_code <- c(balancing_code, paste0("balancing_res <- matchthem(as.formula(mod), datasets = psmodel_obj$data, approach = 'within', distance=psmodel_obj$ps_modclass, method = ", 
             matching_method, ", ratio = ", matching_ratio, ")"))
    }else{
      balancing_code <- c(balancing_code, paste0("balancing_res <- matchit(as.formula(mod), data = psmodel_obj$data, ps = psmodel_obj$score, method = ", 
             matching_method, ", ratio = ", matching_ratio, ")"))
    }
  }
  
  ## Outcome model ----
  
  
  ## Create R script ----
  r_script <- c(library_code, data_source_code, variable_input_code, formula_code, balancing_model_code, balancing_code)
  
  cat(r_script)
  
}

# get_R_script(data = DigiCAT::zp_eg, 
#              data_source = "sample",
#              file_path = NULL,
#              categorical_variables = ,
#              outcome_variable = "Anxiety_age17",
#              treatment_variable = "Reading_age15",
#              matching_variables = names(DigiCAT::zp_eg)[-c(1:3)],
#              covariates = NULL,
#              CF_approach = "matching",
#              balancing_model = "glm",
#              missingness = "mi",
#              matching_method = "NN",
#              matching_ratio = 1)

