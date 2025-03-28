#' Function to produce downloadable R script

#' @param data Dataframe
#' @param data_source Source of data: `"own"`, or `"sample"`
#' @param file_path File path to data
#' @param df Uploaded dataset/DigiCAT example data
#' @param categorical_variables Vector of categorical variable names in dataset
#' @param outcome_variable Name of outcome variable in dataset (character string)
#' @param treatment_variable Name of treatment variable in dataset (character string)
#' @param matching_variables Vector of matching variable names in dataset
#' @param covariates Vector of covariate names in dataset
#' @param counterfactual_method Name of counterfactual appraoch being taken (character string)
#' @param balancing_model Name of balancing model being used (character string)
#' @param missing_method Name of method of dealing with missingness being used (character string): `"mi"`, or `"complete"`
#' @param matching_method Name of balancing method being used (character string): `"nearest"` or `"optimal"`
#' @param matching_ratio Name of balancing ratio being used (numeric)
#' @param outcome_formula Name of outcome model being used (character string)
#' @param outcome_type Data tyupe of outcome variable: `"continuous"`, `"binary"` or `"categorical"`
#' @param DigiCAT_balanced_data Balanced data object from tool
#' @param DigiCAT_extracted_balanced_data Balanced data object from tool extracted for outcome model run
#' @param DigiCAT_fitted_model Fitted outcome model from tool
#' @param DigiCAT_extracted_hedges_g Hedges g from tool
#' @param DigiCAT_extracted_outcome_results Extracted outcome model results from tool
#' @param include_sensitivity Include sensitivity analysis in script: `TRUE` or `FALSE`

get_R_script <- function(
  ## Data upload
  data_source, 
  file_path = NULL,
  df = NULL,
  categorical_variables = NULL,
  outcome_variable, 
  treatment_variable, 
  matching_variables, 
  covariates = NULL,
  weighting_variable = NULL,
  cluster_variable = NULL,
  strata_variable = NULL,
  ## Counterfactual approach
  counterfactual_method, 
  missing_method,
  balancing_model, 
  ## Balancing
  matching_method = NULL,
  matching_ratio = NULL, 
  ## Outcome model
  outcome_formula,
  outcome_type,
  ## DigiCAT output
  DigiCAT_balanced_data,
  DigiCAT_extracted_balanced_data,
  DigiCAT_fitted_model,
  DigiCAT_extracted_outcome_results,
  DigiCAT_extracted_hedges_g,
  include_sensitivity){
  
  ## Define function to extract lines from existing code
  extract_lines_between_patterns <- function(function_name, start_pattern, end_pattern, add_lines = 0, skip_lines = 0, result_variable = NULL, sub_string = c("", "")) {

    ## Read file into a character vector (each line is an element)
    lines <- capture.output(function_name)
    
    ## Find the indices of the lines that match the start and end patterns
    start_index <- which(grepl(start_pattern, lines, fixed = TRUE))
    end_indices <- which(grepl(end_pattern, lines, fixed = TRUE))
    ## Find the first end index that occurs after the start index
    end_index <- end_indices[end_indices >= start_index][1]
    
    ## Ensure valid indices
    if (length(start_index) == 0 | length(end_index) == 0) {
      stop("Start or end pattern not found.")
    }
    
    ## Add number of lines specified (default = 0)
    end_index <- end_index + add_lines
    
    ## Skip number of lines specified (default = 0)
    start_index <- start_index + skip_lines
    
    ## Extract the lines between the first occurrence of start and end pattern
    extracted_lines <- lines[start_index[1]:end_index[1]]
    
    ## Replace string if specified
    extracted_lines <- gsub(sub_string[1], sub_string[2], extracted_lines)
    
    ## Remove "return(NULL)"
    extracted_lines <- gsub("return\\(NULL\\)", "", extracted_lines)
    
    ## Modify any return statement - save to a variable instead
    extracted_lines <- gsub("return\\(", paste0(result_variable," <- ("), extracted_lines)
    
    ## Remove ", ...)" or ",...)" and replace with ")"
    extracted_lines <- gsub(",\\s*\\.\\.\\.\\)", ")", extracted_lines)

    ## Remove comments
    extracted_lines <- gsub("#.*", "", extracted_lines)
    
    ## Remove empty lines that may have resulted from removing comments
    extracted_lines <- extracted_lines[nchar(trimws(extracted_lines)) > 0]
    
    ## Find the minimum leading whitespace across all lines (ignoring empty lines)
    if (length(extracted_lines) > 0) {
      min_indent <- min(nchar(gsub("^(\\s*).*", "\\1", extracted_lines[extracted_lines != ""])))
      
      ## Remove common leading whitespace while preserving relative indentation
      extracted_lines <- sub(paste0("^ {0,", min_indent, "}"), "", extracted_lines)
    }

    ## Add a blank line at the end
    extracted_lines <- c(extracted_lines, "")
    
    return(extracted_lines)
  }
  
  ## Libraries required ----
  library_code <- paste0(
    "library(MatchIt)
library(MatchThem)
library(WeightIt)
library(cobalt)
library(ggplot2)
library(knitr)
library(marginaleffects)
library(mice)
library(moments)
library(nbpMatching)
library(rmarkdown)
library(survey)
library(gbm)
library(randomForest)
library(EValue)"
)
  
  ## Data upload ----
  if (data_source == "own"){
    data_source_code <- paste0("\n## Load in Data----\n", "## Load in own data - for security reasons filepaths cannot be stored, please fill in local filepath below\n", ".data <- read.csv('')")
  }
  if (data_source == "sample"){
    data_source_code <- paste0("\n## Load in Data----\n", "## Load in data (sample dataset (DigiCAT package is required))\n", ".data <- DigiCAT::zp_eg")
  }
  
  ## Variable input
  variable_input_code <- paste0("\n## Define input variables----- \n",
                                "categorical_vars <- c(", paste0("'", categorical_variables, "'", collapse = ","),")\n",
                                "outcome_variable <- ", "'",  outcome_variable, "'","\n",
                                "treatment_variable <- ", "'", treatment_variable, "'\n",
                                "matching_variable <- c(", paste0("'", matching_variables, "'", collapse = ","),")\n"
  )
  
  if (is.null(covariates)){
    variable_input_code <- paste0(variable_input_code, "covariates <- NULL \n")
  } else{
    variable_input_code <- paste0(variable_input_code, "covariates <- c(", paste0("'",covariates, "'", collapse = ","),")\n")
  }
  
  if (is.null(weighting_variable)){
    variable_input_code <- paste0(variable_input_code, "weighting_variable <- NULL \n")
  } else{
    variable_input_code <- paste0(variable_input_code, "weighting_variable <- ", "'", weighting_variable, "'\n")
  }
  
  if (is.null(cluster_variable)){
    variable_input_code <- paste0(variable_input_code, "cluster_variable <- NULL \n")
  } else{
    variable_input_code <- paste0(variable_input_code, "cluster_variable <- ", "'", cluster_variable, "'\n")
  }
  
  if (is.null(strata_variable)){
    variable_input_code <- paste0(variable_input_code, "strata_variable <- NULL \n")
  } else{
    variable_input_code <- paste0(variable_input_code, "strata_variable <- ", "'", strata_variable)
  }
  
  ## Specify outcome variable type
  variable_input_code <- c(variable_input_code,
                           paste0("outcome_type <- ", "'", outcome_type, "'"))
  
  ## Define counterfactual analysis inputs
  define_CA_input_code <- paste0("\n## Define counterfactual analysis options ----- \n",
                                 "model_type <- '", balancing_model, "'","\n",
                                 "missing_method <- '", missing_method, "'")
  
  ## Factorise categorical variables
  factorise_categorical_code <- paste0("\n## Factorise categorical variables \n", ".data[categorical_vars] <- lapply(.data[categorical_vars], factor)")
  
  ## Reduce dataframe to inputted variables ----
  reduce_data_code <- paste0("\n## Reduce df to selected columns \n", ".data <- .data[,unique(c(treatment_variable, outcome_variable, matching_variable, covariates, weighting_variable, cluster_variable, strata_variable))]")
  
  ## Propensity score estimation----
  ### handle_missingness() ----
  handled_missingness_code <- "\n## Handle Missingness----"
  
  if (missing_method == "complete"){
    handled_missingness_code <- c(
        handled_missingness_code,
        extract_lines_between_patterns(
          function_name = handle_missingness,
          start_pattern = "complete = {",
          end_pattern = "data = handled_missingness)",
          skip_lines = 1)
    )
  }
  if (missing_method == "mi"){
    handled_missingness_code <- c(
      handled_missingness_code,
      extract_lines_between_patterns(
        function_name = handle_missingness,
        start_pattern = "## Remove any rows without treatment data - HC",
        end_pattern = "data = imputationList(complete_imps))",
        skip_lines = 1)
    )
  }
  if (missing_method == "weighting"){
    handled_missingness_code <- c(
      handled_missingness_code,
      extract_lines_between_patterns(
        function_name = handle_missingness,
        start_pattern = "weighting = {",
        end_pattern = "design_object = design_object",
        skip_lines = 1)
    )
  }  
  
  handled_missingness_code <- c(handled_missingness_code, "\n",
                                "handled_missingness_objects <- list(handled_missingness, design_object)")
  
  ### estimate_model() and get_propensity() ----
  propensity_score_model_code <- "\n## Get Propensity Scores----"
  
  if (!is.null(balancing_model)){ ## If model selected
    propensity_score_model_code <- c(
      propensity_score_model_code,
      extract_lines_between_patterns(
        function_name = estimate_model,
        start_pattern = 'if (model_type == "gbm" | model_type == "glm" | model_type == "lm"){',
        end_pattern = 'f = as.formula(paste0("as.factor(", treatment_variable,") ~",paste0(matching_variable, collapse="+")))',
        add_lines = 1)
    )
  
    if (balancing_model == "glm"){
    propensity_score_model_code <- c(
      propensity_score_model_code,
      extract_lines_between_patterns(
        function_name = estimate_model,
        start_pattern = "glm = {",
        end_pattern = "estimated_propensity_model = svyglm(f, design = handled_missingness)",
        add_lines = 1,
        skip_lines = 1)
    )
    propensity_score_model_code <- c(
      propensity_score_model_code,
      extract_lines_between_patterns(
        function_name = get_propensity,
        start_pattern = "glm = {",
        end_pattern = "propensity_score = estimated_propensity_model$fitted.values",
        add_lines = 1,
        skip_lines = 1)
    )
  }
  if (balancing_model == "gbm"){
    propensity_score_model_code <- c(
      propensity_score_model_code,
      extract_lines_between_patterns(
        function_name = estimate_model,
        start_pattern = "gbm = {",
        end_pattern = "estimated_propensity_model = gbm(as.formula(f), data = handled_missingness,...)",
        add_lines = 1,
        skip_lines = 1)
    )
    propensity_score_model_code <- c(
      propensity_score_model_code,
      extract_lines_between_patterns(
        function_name = get_propensity,
        start_pattern = "gbm = {",
        end_pattern = 'propensity_score = predict(estimated_propensity_model, type = "response")',
        add_lines = 1,
        skip_lines = 1)
    )
  }
    if (balancing_model == "randomforest"){
      propensity_score_model_code <- c(
        propensity_score_model_code,
        extract_lines_between_patterns(
          function_name = estimate_model,
          start_pattern = "randomforest = {",
          end_pattern = "estimated_propensity_model <- randomForest::randomForest(as.formula(f), data = handled_missingness, ...)",
          add_lines = 1,
          skip_lines = 1)
      )
      propensity_score_model_code <- c(
        propensity_score_model_code,
        extract_lines_between_patterns(
          function_name = get_propensity,
          start_pattern = "randomforest = {",
          end_pattern = 'propensity_score = predict(estimated_propensity_model, type = "prob")[,2]',
          add_lines = 1,
          skip_lines = 1)
      )
    }
    if (balancing_model == "poly"){
      propensity_score_model_code <- c(
        propensity_score_model_code,
        extract_lines_between_patterns(
          function_name = estimate_model,
          start_pattern = "poly = {",
          end_pattern = "estimated_propensity_model = svyolr(f, design=handled_missingness)",
          add_lines = 1,
          skip_lines = 1)
      )
      propensity_score_model_code <- c(
        propensity_score_model_code,
        extract_lines_between_patterns(
          function_name = get_propensity,
          start_pattern = "poly = {",
          end_pattern = 'propensity_score <- cbind(handled_missingness$variables, propensity_score)',
          add_lines = 1,
          skip_lines = 1)
      )
    }
    propensity_score_model_code <- c(
      propensity_score_model_code,
      extract_lines_between_patterns(
        function_name = estimation_stage,
        start_pattern = "return(list(missingness_treated_dataset = handled_missingness,",
        end_pattern = 'survey_design_object = design_object)) # note if weighting, this is the object containing data, not missingness_treated_dataset',
        result_variable = "PS_estimation_object")
    )
    
  } else{
    propensity_score_model_code <- ""
  }
  
  ## Balancing ----
  balancing_code <- "## Balance data----"
  
  ### balance_data() ----
  
  if (counterfactual_method == "iptw"){
    balancing_code <- c(
      balancing_code,
      extract_lines_between_patterns(
        function_name = balancing_iptw,
        start_pattern = "## Balance Data: IPTW",
        end_pattern = 'method = "ps", ...)',
        add_lines = 1)
    )
  }
  if (counterfactual_method == "psm"){
    balancing_code <- c(
      balancing_code,
      extract_lines_between_patterns(
        function_name = balancing_psm,
        start_pattern = "## Balance Data: PSM",
        end_pattern = ' ps = PS_estimation_object$propensity_score, ...)',
        add_lines = 1)
    )
  }
  if (counterfactual_method == "nbp"){
    balancing_code <- c(
      balancing_code,
      extract_lines_between_patterns(
        function_name = balancing_psm,
        start_pattern = "## Balance Data: NBP",
        end_pattern = 'balanced_data <- restructure_rejoin_nbp(matched_data, propensity_data, treatment_variable, missing_method,...)',
        add_lines = 1)
    )
  }
  if (counterfactual_method == "cbps"){
    balancing_code <- c(
      balancing_code,
      extract_lines_between_patterns(
        function_name = balancing_cbps,
        start_pattern = "## Balance Data: CBPS",
        end_pattern = 'approach = "within", method = "cbps")',
        add_lines = 1)
    )
  }
  
  ## Outcome model ----
  outcome_model_code <- "## Outcome Model----"
  
  ### extract_balanced_data() ----
  outcome_model_code <- c(outcome_model_code, "### Extract balanced data----")
  
  if( "mimids" %in% class(DigiCAT_balanced_data)) {
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = extract_balanced_data,
        start_pattern = 'if( "mimids" %in% class(balanced_data)) {',
        end_pattern = 'return(list(extracted_balanced_data, process = "mi_psm"))',
        skip_lines = 1,
        add_lines = 0,
        result_variable = "extracted_balanced_data")
      )
  }
  if("wimids" %in% class(DigiCAT_balanced_data) & counterfactual_method == "iptw") {
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = extract_balanced_data,
        start_pattern = '} else if ( "wimids" %in% class(balanced_data) & counterfactual_method == "iptw"){',
        end_pattern = 'return(list(extracted_balanced_data, process = "mi_iptw"))',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "extracted_balanced_data")
    )
  }
  if("wimids" %in% class(DigiCAT_balanced_data) & counterfactual_method == "cbps") {
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = extract_balanced_data,
        start_pattern = '} else if ( "wimids" %in% class(balanced_data) & counterfactual_method == "cbps"){',
        end_pattern = 'return(list(extracted_balanced_data, process = "mi_cbps"))',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "extracted_balanced_data")
    )
  }
  if("matchit" %in% class(DigiCAT_balanced_data) & missing_method == "complete") {
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = extract_balanced_data,
        start_pattern = '}else if ( "matchit" %in% class(balanced_data) & missing_method == "complete"){',
        end_pattern = 'return(list(extracted_balanced_data, process = "cc_psm"))',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "extracted_balanced_data")
    )
  }
  if(missing_method =="weighting" & "matchit" %in% class(DigiCAT_balanced_data)) {
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = extract_balanced_data,
        start_pattern = '} else if(missing_method =="weighting" & "matchit" %in% class(balanced_data)){',
        end_pattern = 'return(list(extracted_balanced_data, process = "weighting_psm"))',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "extracted_balanced_data")
    )
  }
  if("weightit" %in% class(DigiCAT_balanced_data) & missing_method == "complete" & counterfactual_method == "iptw") {
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = extract_balanced_data,
        start_pattern = '} else if ( "weightit" %in% class(balanced_data) & missing_method == "complete" & counterfactual_method == "iptw"){',
        end_pattern = 'return(list(PS_estimation_object$missingness_treated_dataset, process = "cc_iptw"))',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "extracted_balanced_data")
    )
  }
  if(missing_method=="weighting" & "weightit" %in% class(DigiCAT_balanced_data)) {
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = extract_balanced_data,
        start_pattern = '} else if(missing_method=="weighting" & "weightit" %in% class(balanced_data)){',
        end_pattern = 'return(list(extracted_balanced_data, process = "weighting_iptw"))',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "extracted_balanced_data")
    )
  }
  if(counterfactual_method == "nbp" & missing_method == "complete") {
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = extract_balanced_data,
        start_pattern = '} else if(counterfactual_method == "nbp" & missing_method == "complete"){',
        end_pattern = 'return(list(extracted_balanced_data, process = "cc_nbp"))',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "extracted_balanced_data")
    )
  }
  if(counterfactual_method == "nbp" & missing_method == "weighting") {
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = extract_balanced_data,
        start_pattern = 'else if(counterfactual_method == "nbp" & missing_method == "weighting"){',
        end_pattern = 'return(list(extracted_balanced_data, process = "weighting_nbp"))',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "extracted_balanced_data")
    )
  }
  if(counterfactual_method == "nbp" & missing_method == "mi") {
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = extract_balanced_data,
        start_pattern = 'else if(counterfactual_method == "nbp" & missing_method == "mi"){',
        end_pattern = 'return(list(extracted_balanced_data, process = "mi_nbp"))',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "extracted_balanced_data")
    )
  }
  if(counterfactual_method == "cbps" & missing_method == "complete") {
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = extract_balanced_data,
        start_pattern = 'else if(counterfactual_method == "cbps" & missing_method == "complete"){',
        end_pattern = 'return(list(extracted_balanced_data, process = "cc_cbps"))',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "extracted_balanced_data")
    )
  }
  
  ### fit_outcome_model() ----
  outcome_model_code <- c(outcome_model_code, "### Fit outcome model----")
  
  if(outcome_formula == "unadjusted"){
    
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = outcome_unadjusted,
        start_pattern = 'model_formula = paste0(outcome_variable,"~",paste0(c(treatment_variable,covariates),collapse="+"))',
        end_pattern = 'model_formula = as.formula(paste0(outcome_variable,"~",treatment_variable))',
        add_lines = 1,
        skip_lines = -1
        )
    )
    if(DigiCAT_extracted_balanced_data$process == "mi_psm"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_unadjusted,
          start_pattern = 'if(extracted_balanced_data$process == "mi_psm"){',
          end_pattern = 'model_fit = with(mi_matched_design, svyVGAM::svy_vglm(model_formula, family = multinomial))',
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "mi_nbp"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_unadjusted,
          start_pattern = 'else if(extracted_balanced_data$process == "mi_nbp"){',
          end_pattern = "model_fit = with(mi_matched_design, svyglm(model_formula, family = 'binomial')) # leave unpooled until next step",
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "cc_psm"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_unadjusted,
          start_pattern = '} else if(extracted_balanced_data$process == "cc_psm"){ ',
          end_pattern = "model_fit = svyVGAM::svy_vglm(model_formula, design = updated_design,  family = multinomial)",
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "mi_iptw"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_unadjusted,
          start_pattern = '} else if (extracted_balanced_data$process == "mi_iptw"){',
          end_pattern = "model_fit = with(mi_matched_design, svyVGAM::svy_vglm(model_formula, family = multinomial))",
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "cc_iptw"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_unadjusted,
          start_pattern = 'else if (extracted_balanced_data$process == "cc_iptw"){',
          end_pattern = "model_fit = svyVGAM::svy_vglm(model_formula, design = updated_design,  family = multinomial)",
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "weighting_iptw"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_unadjusted,
          start_pattern = '} else if (extracted_balanced_data$process == "weighting_iptw"){',
          end_pattern = "model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]], family = 'binomial')",
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "weighting_psm"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_unadjusted,
          start_pattern = '} else if (extracted_balanced_data$process == "weighting_psm"){',
          end_pattern = "model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]], family = 'binomial')",
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "cc_nbp" | DigiCAT_extracted_balanced_data$process == "weighting_nbp"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_unadjusted,
          start_pattern = 'else if (extracted_balanced_data$process == "cc_nbp" | extracted_balanced_data$process == "weighting_nbp"){',
          end_pattern = "model_fit = svyglm(model_formula, design = updated_design, family = 'binomial')",
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "cc_cbps"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_unadjusted,
          start_pattern = 'else if(extracted_balanced_data$process == "cc_cbps"){',
          end_pattern = "model_fit = svyVGAM::svy_vglm(model_formula, design = updated_design,  family = multinomial)",
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "mi_cbps"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_unadjusted,
          start_pattern = 'else if(extracted_balanced_data$process == "mi_cbps"){',
          end_pattern = "model_fit = with(mi_matched_design, svyVGAM::svy_vglm(model_formula, family = multinomial))",
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
  }
  
  if(outcome_formula == "with_matching_variables"){
    
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = outcome_matching_variables,
        start_pattern = 'model_formula = paste0(outcome_variable,"~",paste0(c(treatment_variable,matching_variable,covariates),collapse="+"))',
        end_pattern = 'model_formula = paste0(outcome_variable,"~",paste0(c(treatment_variable,matching_variable),collapse="+"))',
        add_lines = 1,
        skip_lines = -1
      )
    )
    if(DigiCAT_extracted_balanced_data$process == "mi_psm"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_matching_variables,
          start_pattern = 'if(extracted_balanced_data$process == "mi_psm"){',
          end_pattern = 'model_fit = with(mi_matched_design, svyVGAM::svy_vglm(formula(model_formula), family = multinomial))',
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "mi_psm"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_matching_variables,
          start_pattern = '} else if(extracted_balanced_data$process == "mi_nbp"){',
          end_pattern = 'model_fit = with(mi_matched_design, svyVGAM::svy_vglm(formula(model_formula), family = multinomial))',
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "mi_nbp"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_matching_variables,
          start_pattern = '} else if(extracted_balanced_data$process == "mi_nbp"){',
          end_pattern = 'model_fit = with(mi_matched_design, svyVGAM::svy_vglm(formula(model_formula), family = multinomial))',
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "cc_psm"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_matching_variables,
          start_pattern = '} else if(extracted_balanced_data$process == "cc_psm"){ ',
          end_pattern = 'model_fit = svyVGAM::svy_vglm(formula(model_formula), design = updated_design,  family = multinomial)',
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "mi_iptw"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_matching_variables,
          start_pattern = '} else if (extracted_balanced_data$process == "mi_iptw"){',
          end_pattern = 'model_fit = with(mi_matched_design, svyVGAM::svy_vglm(formula(model_formula), family = multinomial))',
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "cc_iptw"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_matching_variables,
          start_pattern = 'else if (extracted_balanced_data$process == "cc_iptw"){',
          end_pattern = 'model_fit = svyVGAM::svy_vglm(formula(model_formula), design = updated_design,  family = multinomial)',
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "weighting_iptw"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_matching_variables,
          start_pattern = '} else if (extracted_balanced_data$process == "weighting_iptw"){',
          end_pattern = "model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]], family = 'binomial')",
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "weighting_psm"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_matching_variables,
          start_pattern = '} else if (extracted_balanced_data$process == "weighting_psm"){',
          end_pattern = "model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]], family = 'binomial')",
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "cc_nbp" | DigiCAT_extracted_balanced_data$process == "weighting_nbp"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_matching_variables,
          start_pattern = 'else if (extracted_balanced_data$process == "cc_nbp" | extracted_balanced_data$process == "weighting_nbp"){',
          end_pattern = "model_fit = svyVGAM::svy_vglm(model_formula, design = updated_design,  family = multinomial)",
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "cc_cbps"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_matching_variables,
          start_pattern = 'else if(extracted_balanced_data$process == "cc_cbps"){',
          end_pattern = "model_fit = svyVGAM::svy_vglm(formula(model_formula), design = updated_design,  family = multinomial)",
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "mi_cbps"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_matching_variables,
          start_pattern = 'else if(extracted_balanced_data$process == "mi_cbps"){',
          end_pattern = "model_fit = with(mi_matched_design, svyVGAM::svy_vglm(formula(model_formula), family = multinomial))",
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "mi_cbps"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_matching_variables,
          start_pattern = 'else if(extracted_balanced_data$process == "mi_cbps"){',
          end_pattern = "model_fit = with(mi_matched_design, svyVGAM::svy_vglm(formula(model_formula), family = multinomial))",
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
  }
  
  if(outcome_formula == "marginal_effects"){
    
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = outcome_marginal_effects,
        start_pattern = 'model_formula <- as.formula(paste0(outcome_variable, " ~ ", ',
        end_pattern = '"*(",paste0(matching_variable, collapse="+"), ")"))',
        add_lines = 1,
        skip_lines = -1
      )
    )
    if(DigiCAT_extracted_balanced_data$process == "mi_psm"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_marginal_effects,
          start_pattern = 'if(extracted_balanced_data$process == "mi_psm"){',
          end_pattern = 'model_fit = mice::pool(model_fit)',
          add_lines = 1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "cc_psm"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_marginal_effects,
          start_pattern = '} else if(extracted_balanced_data$process == "cc_psm"){ ',
          end_pattern = '} else if (extracted_balanced_data$process == "mi_iptw"){',
          add_lines = -1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "mi_iptw"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_marginal_effects,
          start_pattern = '} else if (extracted_balanced_data$process == "mi_iptw"){',
          end_pattern = 'model_fit <- mice::pool(model_fit)',
          add_lines = 0,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "cc_iptw"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_marginal_effects,
          start_pattern = 'else if (extracted_balanced_data$process == "cc_iptw"){',
          end_pattern = '} else if (extracted_balanced_data$process == "weighting_iptw"){',
          add_lines = -1,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "weighting_iptw"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_marginal_effects,
          start_pattern = '} else if (extracted_balanced_data$process == "weighting_iptw"){',
          end_pattern = ' model_fit = marginaleffects::avg_comparisons(model_fit, variables = treatment_variable)',
          add_lines = 0,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "weighting_psm"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_marginal_effects,
          start_pattern = '} else if (extracted_balanced_data$process == "weighting_psm"){',
          end_pattern = ' model_fit = marginaleffects::avg_comparisons(model_fit, variables = treatment_variable)',
          add_lines = 0,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "cc_cbps"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_marginal_effects,
          start_pattern = '} else if(extracted_balanced_data$process == "cc_cbps"){',
          end_pattern = 'model_fit = marginaleffects::avg_comparisons(model_fit, variables = treatment_variable, wts = model_fit$weights)',
          add_lines = 0,
          skip_lines = 1
        )
      )
    }
    if(DigiCAT_extracted_balanced_data$process == "mi_cbps"){
      outcome_model_code <- c(
        outcome_model_code,
        extract_lines_between_patterns(
          function_name = outcome_marginal_effects,
          start_pattern = 'else if(extracted_balanced_data$process == "mi_cbps"){',
          end_pattern = 'model_fit <- mice::pool(model_fit)',
          add_lines = 0,
          skip_lines = 1
        )
      )
    }
  }

  outcome_model_code <- c(outcome_model_code,
                               "fitted_model <- model_fit\n")
  
  ### extract_outcome_results() ----
  outcome_model_code <- c(outcome_model_code, "### Extract results of outcome model----")
  
  if("comparisons" %in% class(DigiCAT_fitted_model) & missing_method == "weighting"){
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = extract_outcome_results,
        start_pattern = 'if("comparisons" %in% class(fitted_model) & missing_method == "weighting"){ # weighting ME',
        end_pattern = 'return(list(extracted_outcome_results = fitted_model, process = "weighting"))',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "extracted_outcome_results")
    )
  }
  if("comparisons" %in% class(DigiCAT_fitted_model) & missing_method == "complete"){
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = extract_outcome_results,
        start_pattern = '}else if("comparisons" %in% class(fitted_model) & missing_method == "complete"){ # complete ME',
        end_pattern = 'return(list(extracted_outcome_results = fitted_model, process = "cc"))',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "extracted_outcome_results")
    )
  }
  if("mipo" %in% class(DigiCAT_fitted_model) & missing_method == "mi"){
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = extract_outcome_results,
        start_pattern = 'else if("mipo" %in% class(fitted_model) & missing_method == "mi"){ # MI ME',
        end_pattern = 'return(list(extracted_outcome_results, process = "mi"))',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "extracted_outcome_results")
    )
  }
  if("list" %in% class(DigiCAT_fitted_model) & missing_method == "mi"){
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = extract_outcome_results,
        start_pattern = '} else if("list" %in% class(fitted_model) & missing_method == "mi"){',
        end_pattern = 'return(list(extracted_outcome_results, process = "mi"))',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "extracted_outcome_results")
    )
  }
  if(("lm" %in% class(DigiCAT_fitted_model)  |"svy_vglm" %in% class(DigiCAT_fitted_model)) & missing_method == "complete"){
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = extract_outcome_results,
        start_pattern = 'else if(("lm" %in% class(fitted_model) | "svy_vglm" %in% class(fitted_model)) & missing_method == "complete"){ # LM no ME, complete',
        end_pattern = 'return(list(extracted_outcome_results, process = "cc"))',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "extracted_outcome_results")
    )
  }
  if("svyglm" %in% class(DigiCAT_fitted_model) & missing_method == "weighting"){
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = extract_outcome_results,
        start_pattern = '}else if("svyglm" %in% class(fitted_model) & missing_method == "weighting"){',
        end_pattern = 'return(list(extracted_outcome_results, process = "weighting"))',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "extracted_outcome_results")
    )
  }
  if("svyglm" %in% class(DigiCAT_fitted_model) & missing_method == "weighting"){
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = extract_outcome_results,
        start_pattern = '} else if("comparisons" %in% class(fitted_model[[1]]) & missing_method == "mi"){',
        end_pattern = 'return(summary(fitted_model, conf.int = T))',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "extracted_outcome_results")
    )
  }
  ### standardise_outcome_format.R ----
  outcome_model_code <- c(outcome_model_code, "### Standardise results of outcome model----")
  
  if(DigiCAT_extracted_balanced_data$process == "mi" & outcome_formula == "marginal_effects"){ # ME MI with/without covs
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = standardise_outcome_format,
        start_pattern = 'if(extracted_outcome_results[[2]] == "mi" & outcome_formula == "marginal_effects"){ # ME MI with/without covs',
        end_pattern = 'return(results_dataframe)',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "standardised_format")
    )
  }
  if(DigiCAT_extracted_balanced_data$process == "mi" & outcome_formula == "unadjusted"){ # ME MI with/without covs
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = standardise_outcome_format,
        start_pattern = '} else if(extracted_outcome_results[[2]] == "mi" & outcome_formula == "unadjusted"){ # unadjusted MI with/without covs',
        end_pattern = 'return(results_dataframe)',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "standardised_format")
    )
  }
  if(DigiCAT_extracted_balanced_data$process == "mi" & outcome_formula == "with_matching_variables"){ # ME MI with/without covs
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = standardise_outcome_format,
        start_pattern = '} else if(extracted_outcome_results[[2]] == "mi" & outcome_formula == "with_matching_variables"){ # adjusted for matching variables with MI with/without covs',
        end_pattern = 'return(results_dataframe)',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "standardised_format")
    )
  }
  if(DigiCAT_extracted_outcome_results[[2]] == "cc" & counterfactual_method != "nbp" & outcome_formula == "marginal_effects"){ # ME MI with/without covs
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = standardise_outcome_format,
        start_pattern = '} else if(extracted_outcome_results[[2]] == "cc" & counterfactual_method != "nbp" & outcome_formula == "marginal_effects"){ # ME CCA with/without covs',
        end_pattern = ' return(results_dataframe)',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "standardised_format")
    )
  }
  if(DigiCAT_extracted_outcome_results[[2]] == "cc" & counterfactual_method != "nbp" & outcome_formula == "unadjusted"){ # ME MI with/without covs
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = standardise_outcome_format,
        start_pattern = '} else if(extracted_outcome_results[[2]] == "cc" & counterfactual_method != "nbp" & outcome_formula == "unadjusted"){ # unadjusted CCA with/without covs',
        end_pattern = ' return(results_dataframe)',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "standardised_format")
    )
  }
  if(DigiCAT_extracted_outcome_results[[2]] == "cc" & counterfactual_method != "nbp" & outcome_formula == "with_matching_variables"){ # ME MI with/without covs
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = standardise_outcome_format,
        start_pattern = '} else if(extracted_outcome_results[[2]] == "cc" & counterfactual_method != "nbp" & outcome_formula == "with_matching_variables"){# adjusted for matching variables with CCA with/without covs',
        end_pattern = ' return(results_dataframe)',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "standardised_format")
    )
  }
  if(DigiCAT_extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "psm" & outcome_formula == "marginal_effects"){ # ME MI with/without covs
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = standardise_outcome_format,
        start_pattern = '} else if(extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "psm" & outcome_formula == "marginal_effects"){',
        end_pattern = ' return(results_dataframe)',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "standardised_format")
    )
  }
  if(DigiCAT_extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "iptw" & outcome_formula == "marginal_effects"){ # ME MI with/without covs
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = standardise_outcome_format,
        start_pattern = 'else if(extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "iptw" & outcome_formula == "marginal_effects"){',
        end_pattern = ' return(results_dataframe)',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "standardised_format")
    )
  }
  if(DigiCAT_extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "psm" & outcome_formula == "unadjusted"){ # ME MI with/without covs
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = standardise_outcome_format,
        start_pattern = 'else if(extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "psm" & outcome_formula == "unadjusted"){',
        end_pattern = ' return(results_dataframe)',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "standardised_format")
    )
  }
  if(DigiCAT_extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "iptw" & outcome_formula == "unadjusted"){ # ME MI with/without covs
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = standardise_outcome_format,
        start_pattern = 'else if(extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "iptw" & outcome_formula == "unadjusted"){',
        end_pattern = ' return(results_dataframe)',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "standardised_format")
    )
  }
  if(DigiCAT_extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "iptw" & outcome_formula == "with_matching_variables"){ # ME MI with/without covs
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = standardise_outcome_format,
        start_pattern = 'else if(extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "iptw" & outcome_formula == "with_matching_variables"){',
        end_pattern = ' return(results_dataframe)',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "standardised_format")
    )
  }
  if(DigiCAT_extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "psm" & outcome_formula == "with_matching_variables"){ # ME MI with/without covs
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = standardise_outcome_format,
        start_pattern = 'else if(extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "psm" & outcome_formula == "with_matching_variables"){',
        end_pattern = ' return(results_dataframe)',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "standardised_format")
    )
  }
  if(DigiCAT_extracted_outcome_results[[2]] == "cc" & counterfactual_method == "nbp" & outcome_formula == "unadjusted"){ # ME MI with/without covs
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = standardise_outcome_format,
        start_pattern = 'else if(extracted_outcome_results[[2]] == "cc" & counterfactual_method == "nbp" & outcome_formula == "unadjusted"){',
        end_pattern = ' return(results_dataframe)',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "standardised_format")
    )
  }
  if(DigiCAT_extracted_outcome_results[[2]] == "cc" & counterfactual_method == "nbp" & outcome_formula == "with_matching_variables"){ # ME MI with/without covs
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = standardise_outcome_format,
        start_pattern = ' } else if(extracted_outcome_results[[2]] == "cc" & counterfactual_method == "nbp" & outcome_formula == "with_matching_variables"){',
        end_pattern = ' return(results_dataframe)',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "standardised_format")
    )
  }
  if(DigiCAT_extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "nbp" & outcome_formula == "unadjusted"){ # ME MI with/without covs
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = standardise_outcome_format,
        start_pattern = '} else if(extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "nbp" & outcome_formula == "unadjusted"){',
        end_pattern = ' return(results_dataframe)',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "standardised_format")
    )
  }
  if(DigiCAT_extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "nbp" & outcome_formula == "with_matching_variables"){ # ME MI with/without covs
    outcome_model_code <- c(
      outcome_model_code,
      extract_lines_between_patterns(
        function_name = standardise_outcome_format,
        start_pattern = '} else if(extracted_outcome_results[[2]] == "weighting" & counterfactual_method == "nbp" & outcome_formula == "with_matching_variables"){',
        end_pattern = ' return(results_dataframe)',
        add_lines = 0,
        skip_lines = 1,
        result_variable = "standardised_format")
    )
  }
  
  ### Compile outcome model outputs ----
  outcome_model_code <- c(
    outcome_model_code,
    extract_lines_between_patterns(
      function_name = outcome_analysis_stage,
      start_pattern = 'return(list(standardised_format = standardised_format, extracted_balanced_data = extracted_balanced_data, fitted_model = fitted_model, extracted_outcome_results = extracted_outcome_results))',
      end_pattern = 'return(list(standardised_format = standardised_format, extracted_balanced_data = extracted_balanced_data, fitted_model = fitted_model, extracted_outcome_results = extracted_outcome_results))',
      add_lines = 0,
      skip_lines = 0,
      result_variable = "outcome_results")
  )
  
  ### hedges_g()----
  
  if(!is.null(DigiCAT_extracted_hedges_g)){
    
    if(missing_method == "complete"){
      outcome_model_code <- c(
        outcome_model_code,
        paste("### Hedge's g----\n"),
        extract_lines_between_patterns(
          function_name = hedges_g,
          start_pattern = 'if(missing_method == "complete"){',
          end_pattern = 'g <- mean_diff / sqrt(pooled_variance)',
          add_lines = 0,
          skip_lines = 1)
      )
    }
    if(missing_method == "mi"){
      outcome_model_code <- c(
        outcome_model_code,
        paste("### Hedge's g----\n"),
        extract_lines_between_patterns(
          function_name = hedges_g,
          start_pattern = 'if (missing_method == "mi") {',
          end_pattern = 'g <- mean_diff / pooled_SD',
          add_lines = 0,
          skip_lines = 1)
      )
    }
    if(missing_method == "weighting"){
      outcome_model_code <- c(
        outcome_model_code,
        paste("### Hedge's g----\n"),
        extract_lines_between_patterns(
          function_name = hedges_g,
          start_pattern = 'if(missing_method == "weighting"){',
          end_pattern = 'g <- mean_diff / sqrt(pooled_variance)',
          add_lines = 0,
          skip_lines = 1)
      )
    }
  }
  
  ## Sensitivity analysis ----
  ### run_sensitivity() ----
  
  sensitivity_analysis_code <- "##Sensitivity Analysis ----"
  
  if(include_sensitivity){
    
    if(counterfactual_method == "psm"){
    
      sensitivity_analysis_code <- c(sensitivity_analysis_code, "### Rosenbaum’s Bounds ----")
      if(missing_method == "complete"){
        sensitivity_analysis_code <- c(
          sensitivity_analysis_code,
          extract_lines_between_patterns(
            function_name = perform_rosenbaum_sensitivity,
            start_pattern = 'if (missing_method == "complete") {',
            end_pattern = 'sensitivity_result <- rbounds::psens(x = mpairs[, 1], y = mpairs[, 2]) # default Gamma and GammaInc = 6 & 1',
            skip_lines = 1,
            add_lines = 0,
            sub_string = c("sensitivity_result", "sensitivity_result_RB"))
        )
      }
      if(missing_method == "mi"){
        sensitivity_analysis_code <- c(
          sensitivity_analysis_code,
          extract_lines_between_patterns(
            function_name = perform_rosenbaum_sensitivity,
            start_pattern = ' } else if (missing_method == "mi") {',
            end_pattern = 'sensitivity_result$bounds$`Upper bound` <- avg_upper_bounds',
            skip_lines = 1,
            add_lines = 0,
            sub_string = c("sensitivity_result", "sensitivity_result_RB"))
        )
      }
      if(missing_method == "weighting"){
        sensitivity_analysis_code <- c(
          sensitivity_analysis_code,
          extract_lines_between_patterns(
            function_name = perform_rosenbaum_sensitivity,
            start_pattern = '} else if (missing_method == "weighting") {',
            end_pattern = 'return(NULL)',
            skip_lines = 1,
            add_lines = 1,
            sub_string = c("sensitivity_result", "sensitivity_result_RB"))
          )
      }
    }
    
    if(missing_method == "complete"){
      sensitivity_analysis_code <- c(sensitivity_analysis_code, "### E-value ----")
      sensitivity_analysis_code <- c(
        sensitivity_analysis_code,
        extract_lines_between_patterns(
          function_name = perform_VW_Evalue,
          start_pattern = '# Calculate E-value using the point estimate',
          end_pattern = 'names(sensitivity_result) <- "point"',
          skip_lines = 1,
          result_variable = "sensitivity_result_EV",
          sub_string = c("sensitivity_result", "sensitivity_result_EV"))
      )
      }
  } else{
    sensitivity_analysis_code <- ""
  }
  
  ## Create R script ----
  r_script <- c(library_code, data_source_code, variable_input_code, define_CA_input_code, factorise_categorical_code, reduce_data_code, handled_missingness_code, propensity_score_model_code, balancing_code, outcome_model_code, sensitivity_analysis_code)
  noquote(capture.output(cat(r_script, sep = "\n")))
}

# get_R_script(data_source = "sample",
#              file_path = NULL,
#              df = DigiCAT::zp_eg,
#              categorical_variables = c("Gender", "Reading_age15"),
#              outcome_variable = "Anxiety_age17",
#              treatment_variable = "Reading_age15",
#              matching_variables = names(DigiCAT::zp_eg)[-c(1:3)],
#              covariates = NULL,
#              weighting_variable = NULL,
#              cluster_variable = NULL,
#              strata_variable = NULL,
#              counterfactual_method = "iptw",
#              balancing_model = "glm",
#              missing_method = "complete",
#              # matching_method = "NN",
#              # matching_ratio = 1,
#              outcome_formula = "unadjusted",
#              outcome_type = "continuous",
#              DigiCAT_balanced_data = ghi,
#              DigiCAT_extracted_balanced_data = mno$extracted_balanced_data,
#              DigiCAT_fitted_model = mno$fitted_model,
#              DigiCAT_extracted_outcome_results  = mno$extracted_outcome_results,
#              DigiCAT_extracted_hedges_g = rst,
#              include_sensitivity = T
#              )
# 
