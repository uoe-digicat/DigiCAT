#' Function to produce downloadable R script

#' @param data Dataframe
#' @param data_source Source of data: `"own"`, or `"sample"`
#' @param file_path File path to data
#' @param categorical_variables Vector of categorical variable names in dataset
#' @param outcome_variable Name of outcome variable in dataset (character string)
#' @param treatment_variable Name of treatment variable in dataset (character string)
#' @param matching_variables Vector of matching variable names in dataset
#' @param covariates Vector of covariate names in dataset
#' @param CF_approach Name of counterfactual appraoch being taken (character string)
#' @param balancing_model Name of balancing model being used (character string)
#' @param missing_method Name of method of dealing with missingness being used (character string): `"mi"`, or `"complete"`
#' @param matching_method Name of balancing method being used (character string): `"nearest"` or `"optimal"`
#' @param matching_ratio Name of balancing ratio being used (numeric)
#' @param outcome_model Name of outcome model being used (character string)
#' @param outcome_model Name of outcome model being used (character string)
#' @param DigiCAT_balanced_data Balanced data object from tool
#' @param DigiCAT_extracted_balanced_data Balanced data object from tool extracted for outcome model run
#' @param DigiCAT_fitted_model Fitted outcome model from tool
#' @param DigiCAT_extracted_outcome_results Extracted outcome model results from tool

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
  strata_variable = NULL,
  ## Counterfactual approach
  CF_approach, 
  missing_method,
  balancing_model, 
  ## Balancing
  matching_method = NULL,
  matching_ratio = NULL, 
  ## Outcome model
  outcome_model,
  ## DigiCAT output
  DigiCAT_balanced_data,
  DigiCAT_extracted_balanced_data,
  DigiCAT_fitted_model,
  DigiCAT_extracted_outcome_results){
  
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
library(shiny)
library(shinyFeedback)
library(shinycssloaders)
library(survey)
library(gbm)
library(randomForest)"
)
  
  
  ## Data upload ----
  if (data_source == "own"){
    data_source_code <- paste0("\n","\n", "## Load in own data - for security reasons filepaths cannot be stored, please fill in local filepath below\n", "df <- read.csv('')")
  }
  if (data_source == "sample"){
    data_source_code <- paste0("\n","\n", "## Load in data (sample dataset (DigiCAT package is required))\n", "df <- DigiCAT::zp_eg")
  }
  
  ## Variable input
  variable_input_code <- paste0("\n \n## Define input variables \n",
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
    variable_input_code <- paste0(variable_input_code, "weighting_var <- NULL \n")
  } else{
    variable_input_code <- paste0(variable_input_code, "weighting_var <- ", "'", weighting_variable, "'\n")
  }
  
  if (is.null(cluster_variable)){
    variable_input_code <- paste0(variable_input_code, "cluster_var <- NULL \n")
  } else{
    variable_input_code <- paste0(variable_input_code, "cluster_var <- ", "'", cluster_variable, "'\n")
  }
  
  if (is.null(strata_variable)){
    variable_input_code <- paste0(variable_input_code, "stratification_var <- NULL \n")
  } else{
    variable_input_code <- paste0(variable_input_code, "stratification_var <- ", "'", strata_variable, "'\n")
  }
  
  ## Reduce dataframe to inputted variables ----
  
  reduce_data_code <- paste0("\n","\n", "## Reduce df to selected columns \n", "df <- df[,unique(c(treatment_variable, outcome_variable, matching_variable, covariates, weighting_var, cluster_var, stratification_var))]")
  
  ## Create design matrix----
  ## Code taken from 'create_design.R'
  
  if (!is.null(weighting_variable) & is.null(cluster_variable) & is.null(strata_variable)) { # currently as sampling & NR have same workflow, is not controlled by NR = TRUE
    design_matrix_code <- paste0("\n","\n", "## Create design matrix \n",
    "data_complete = subset(df, !is.na(df[[weighting_variable]])) # only weighting
    design_object = svydesign(ids = ~1, 
                              weights = data_complete[[weighting_variable]], 
                              data = data_complete)"
    )
  }
  
  if(!is.null(cluster_variable) & !is.null(weighting_variable) & is.null(strata_variable)){

    design_matrix_code <- paste0("\n","\n", "## Create design matrix \n",
                                 "data_complete = subset(df, (!is.na(df[[weighting_variable]])) & (!is.na(df[[cluster_variable]]))) # weighting and ids
    design_object = svydesign(ids = data_complete[[cluster_variable]],  
                              weights = data_complete[[weighting_variable]], 
                              data = data_complete) "
    )
  }
  
  if(!is.null(strata_variable) & is.null(cluster_variable) & !is.null(weighting_variable)){
    
    design_matrix_code <- paste0("\n","\n", "## Create design matrix \n",
                                 "data_complete = subset(df, (!is.na(df[[weighting_variable]])) & (!is.na(df[[strata_variable]]))) # weighting and strata
    design_object = svydesign(ids = ~1, 
                              weights = data_complete[[weighting_variable]], 
                              strata = data_complete[[strata_variable]], 
                              data = data_complete)"
    )
    }
  
  if(!is.null(strata_variable) & !is.null(cluster_variable) & !is.null(weighting_variable)){
    
    design_matrix_code <- paste0("\n","\n", "## Create design matrix \n",
                                 "    data_complete = subset(df, (!is.na(df[[weighting_variable]])) & (!is.na(df[[strata_variable]])) & (!is.na(df[[cluster_variable]]))) # all 3
    tryCatch(
      expr = {
        design_object = svydesign(ids = data_complete[[cluster_variable]], 
                                  weights = data_complete[[weighting_variable]], 
                                  strata = data_complete[[strata_variable]], 
                                  data = data_complete) 
      },
      error = function(e) {
        design_object = svydesign(ids = data_complete[[cluster_variable]], 
                                  weights = data_complete[[weighting_variable]], 
                                  strata = data_complete[[strata_variable]], 
                                  data = data_complete, nest = TRUE) 
      }
    )"
    )
    }
  if(!is.null(cluster_variable) & is.null(weighting_variable) & is.null(strata_variable)){
    
    design_matrix_code <- paste0("\n","\n", "## Create design matrix \n",
                                 "data_complete = subset(df, !is.na(df[[cluster_variable]])) # ids only
    design_object = svydesign(ids = data_complete[[cluster_variable]], 
                              data = data_complete) "
    )
  }
  if(!is.null(strata_variable) & is.null(weighting_variable) & is.null(cluster_variable)){
    
    design_matrix_code <- paste0("\n","\n", "## Create design matrix \n",
                                 "    data_complete = subset(df, !is.na(df[[strata_variable]])) # strata only
    design_object = svydesign(ids = ~1, 
                              strata = data_complete[[strata_variable]],
                              data = data_complete)"
    )
  }
  if(!is.null(strata_variable) & is.null(weighting_variable) & !is.null(cluster_variable)){
    
    design_matrix_code <- paste0("\n","\n", "## Create design matrix \n",
                                 "data_complete = subset(df, (!is.na(df[[strata_variable]])) & (!is.na(df[[cluster_variable]]))) # strata only
    tryCatch(
      expr = {
        design_object = svydesign(ids = data_complete[[cluster_variable]], 
                                  strata = data_complete[[strata_variable]], 
                                  data = data_complete) 
      },
      error = function(e) {
        design_object = svydesign(ids = data_complete[[cluster_variable]], 
                                  strata = data_complete[[strata_variable]], 
                                  data = data_complete, nest = TRUE) 
      }
    )"
    )
  }
  if(is.null(strata_variable) & is.null(weighting_variable) & is.null(cluster_variable)){
    
    design_matrix_code <-paste0("\n","\n", "## No design matrix \n")
  }
  
  
  ## Deal with data missingness----
  
  if (missing_method == "complete"){
    handled_missingness_code <- paste0("\n","\n", 
                                       "## Handle missingness \n", 
                                       "handled_missingness <- na.omit(df)")
  }
  if (missing_method == "mi"){
    handled_missingness_code <- paste0("\n","\n", 
                                       "## Handle missingness \n", 
                                       "handled_missingness = mice(df, m = 5, maxit = 20,
                                      method = 'rf')")
  }
  if (missing_method == "weighting"){
    handled_missingness_code <- paste0("\n","\n", 
                                       "## Handle missingness \n", 
                                       "handled_missingness = design_object")
  }
  
  ## Propensity score estimation----
  ## From estimate_model.R
  propensity_score_model_code <- paste0("\n","\n", 
                                        "## Get propensity scores \n",
                                        "f = paste0(treatment_variable,'~',paste0(matching_variable, collapse='+'))")
  
  if (balancing_model == "glm"){
    if (missing_method == "complete"){
      propensity_score_model_code <- paste0(propensity_score_model_code,"\n\n",
                                            "estimated_propensity_model = glm(f, data = handled_missingness,
                                            family = binomial(link='probit'))")
    }
    if (missing_method == "mi"){
      propensity_score_model_code <- paste0(propensity_score_model_code,"\n\n",
                                            "estimated_propensity_model = lapply(complete(handled_missingness, 'all'), 
                                            function(x) glm(f, data = x, family=binomial(link='probit')))")
    }
    if (missing_method == "weighting"){
      propensity_score_model_code <- paste0(propensity_score_model_code,"\n\n",
                                            "estimated_propensity_model = svyglm(f, design = handled_missingness)")
    }
  }
  
  if (balancing_model == "gbm"){
    if (missing_method == "complete"){
      propensity_score_model_code <- paste0(propensity_score_model_code,"\n\n",
                                            "estimated_propensity_model = gbm(as.formula(f), data = handled_missingness)")
    }
    if (missing_method == "mi"){
      propensity_score_model_code <- paste0(propensity_score_model_code,"\n\n",
                                            "estimated_propensity_model = lapply(complete(handled_missingness, 'all'),
                                          function(x) gbm(as.formula(f), data = x))")
    }
  }
  
  if (balancing_model == "rf"){
    if (missing_method == "complete"){
      propensity_score_model_code <- paste0(propensity_score_model_code,"\n\n",
                                            "estimated_propensity_model = randomForest::randomForest(as.formula(f), data = handled_missingness)")
    }
    if (missing_method == "mi"){
      propensity_score_model_code <- paste0(propensity_score_model_code,"\n\n",
                                            "estimated_propensity_model = lapply(complete(handled_missingness, 'all'), # switch to ranger for comp speed
                                          function(x) randomForest::randomForest(as.formula(f), data = x))")
    }
  }

  if (balancing_model == "poly"){
    
    if (missing_method == "complete"){
      propensity_score_model_code <- paste0(propensity_score_model_code,"\n\n",
      "handled_missingness[[treatment_variable]] <- as.factor(handled_missingness[[treatment_variable]])
      estimated_propensity_model = MASS::polr(f, data = handled_missingness, Hess = T)")
    }
  }
    
  ## From get_propensity.R
  if (balancing_model == "glm"){
    if(missing_method == "mi"){
      propensity_score_model_code <- paste0(propensity_score_model_code,"\n\n",
                                            "propensity_score = lapply(complete(handled_missingness, 'all'), 
                                function(x) predict(glm(f, data = x, family=binomial(link='probit')),
                                                    type = 'response'))")
    } else { # for CC and weighting approaches alike
      propensity_score_model_code <- paste0(propensity_score_model_code,"\n\n",
                                            "propensity_score = estimated_propensity_model$fitted.values")
    } 
  }
  
  if (balancing_model == "gbm"){
    if(missing_method == "mi"){
      propensity_score_model_code <- paste0(propensity_score_model_code,"\n\n",
                                            "propensity_score = lapply(complete(handled_missingness, 'all'), 
                                function(x) predict(gbm(as.formula(f), data = x),
                                                    type = 'response'))")
    } else { # for CC and weighting approaches alike
      propensity_score_model_code <- paste0(propensity_score_model_code,"\n\n",
                                            "propensity_score = predict(estimated_propensity_model)")
    } 
  }
  
  if (balancing_model == "rf"){
    if(missing_method == "mi"){
      propensity_score_model_code <- paste0(propensity_score_model_code,"\n\n",
                                            "propensity_score = lapply(complete(handled_missingness, 'all'), 
                                function(x) predict(randomForest(as.formula(f), data = x),
                                                    type = 'response'))")
    } else { # for CC and weighting approaches alike
      propensity_score_model_code <- paste0(propensity_score_model_code,"\n\n",
                                            "propensity_score = predict(estimated_propensity_model)")
    } 
  }
  
  if (balancing_model == "poly"){
    if(missing_method == "mi"){
      propensity_score_model_code <- paste0(propensity_score_model_code,"\n\n",
                                            "propensity_score = estimated_propensity_model")
    }
    
    if(missing_method == "complete"){
      propensity_score_model_code <- paste0(propensity_score_model_code,"\n\n",
                                            "propensity_score = as.data.frame(cbind(handled_missingness, estimated_propensity_model$lp))
                                            names(propensity_score)[names(propensity_score) == 'polly$lp'] <- 'lp'")
    }
    
    if(missing_method == "weighting"){
      propensity_score_model_code <- paste0(propensity_score_model_code,"\n\n",
                                            "propensity_score = estimated_propensity_model$fitted.values")
      }
  }
  
  ## Return 'PS_estimation_results'
  propensity_score_model_code <- paste0(propensity_score_model_code,"\n\n",
                                        "PS_estimation_results <- list(missingness_treated_dataset = handled_missingness, 
                                        propensity_scores = propensity_score, 
                                        estimated_propensity_model = estimated_propensity_model,
                                        propensity_model_class = 'glm', # nb: want to alter to class(propensity_model) or use indicator
                                        survey_design_object = estimated_propensity_model$survey.design)")
  
  ## Balancing ----
  
  if (CF_approach == "iptw"){
    
    balancing_code <- paste0("\n\n## Balance data\n",
                             "f = paste0(treatment_variable,'~',paste0(matching_variable,collapse='+'))")
    
    if (missing_method == "complete"){
      balancing_code <- paste0(balancing_code,"\n",
                               "balanced_data = weightit(as.formula(f), data = PS_estimation_results$missingness_treated_dataset, ps = PS_estimation_results$propensity_scores, estimand = 'ATE')")
    }
    if (missing_method == "mi"){
      balancing_code <- paste0(balancing_code,"\n",
                               "balanced_data = weightthem(as.formula(f), datasets = PS_estimation_results$missingness_treated_dataset,
                               approach = 'within', method = 'ps')")
    }
    if (missing_method == "weighting"){
      balancing_code <- paste0(balancing_code,"\n",
                               "balanced_data = weightit(as.formula(f), data = PS_estimation_results$estimated_propensity_model$survey.design$variables, 
                             method = 'ps')")
    }
  }
  
  if (CF_approach == "psm"){
    
    balancing_code <- paste0("\n\n ##Balance data\n",
                             "f = paste0(treatment_variable,'~',paste0(matching_variable,collapse='+'))")
    
    if (missing_method == "complete"){
      balancing_code <- paste0(balancing_code,"\n",
                               "balanced_data = matchit(as.formula(f), data = PS_estimation_results$missingness_treated_dataset, distance = PS_estimation_results$propensity_scores, ratio = ",
                               matching_ratio, ",", " method = '", matching_method,"')")
    }
    if (missing_method == "mi"){
      balancing_code <- paste0(balancing_code,"\n",
                               "balanced_data = matchthem(as.formula(f), datasets = PS_estimation_results$missingness_treated_dataset, approach = 'within', distance=PS_estimation_results$propensity_model_class)")
    }
    if (missing_method == "weighting"){
      balancing_code <- paste0(balancing_code,"\n",
                               "balanced_data = matchit(as.formula(f), data = PS_estimation_results$estimated_propensity_model$survey.design$variables, 
                            ps = PS_estimation_results$propensity_score)")
    }
  }
  
  if (CF_approach == "nbp"){
    
    if (missing_method == "complete"){
      balancing_code <- paste0("\n\n ##Balance data\n",
      "propensity_scores <- PS_estimation_results[[2]]")
      
      ## From 'prepare_dataset_nbp.R'
      balancing_code <- paste0(balancing_code,"\n\n",
      "propensity_scores[[treatment_variable]] = as.numeric(as.character(propensity_scores[[treatment_variable]]))
      propensity_scores$ID <- seq_along(propensity_scores[,1])
      propensity_data <- propensity_scores")
      
      ## From 'make_matrix_nbp.R'
      balancing_code <- paste0(balancing_code,"\n\n",
      "eps = 1*10^-100 
result = matrix(ncol = nrow(propensity_data), nrow = nrow(propensity_data))

matj = matrix(data = propensity_data[[treatment_variable]], nrow = nrow(propensity_data), ncol = nrow(propensity_data), byrow = F)
matk = matrix(data = propensity_data[[treatment_variable]], nrow = nrow(propensity_data), ncol = nrow(propensity_data), byrow = T)

res = matj - matk 
res_squared = res^2

lpj = matrix(data = estimated_propensity_model$lp, nrow = nrow(propensity_data), ncol = nrow(propensity_data), byrow = F)
lpk = matrix(data = estimated_propensity_model$lp, nrow = nrow(propensity_data), ncol = nrow(propensity_data), byrow = T)

lp_res = lpj - lpk
lp_res_abs = abs(lp_res)
lp_calc = 0.15 * sqrt(var(estimated_propensity_model$lp))
lp_logical = lp_res_abs <= lp_calc

lp_res_squared_plus_eps = (lp_res^2) + eps

res_squared[!lp_logical] = 10^11
res_squared[lp_logical] = 10^11 * (lp_res_squared_plus_eps[lp_logical]) / res_squared[lp_logical]

distance_matrix_nbp = res_squared
row.names(distance_matrix_nbp) <- propensity_data$ID
created_distance_matrix <- distance_matrix_nbp")
      
      balancing_code <- paste0(balancing_code,"\n\n",
      "formatted_matrix <- distancematrix(created_distance_matrix) 
performed_matching <- nonbimatch(formatted_matrix) # threshold = 999999, precision = 7? 
matched_data<-performed_matching$halves[performed_matching$halves$Distance!=999999, ]")
                               
      ## From 'Restrucutre_nbp.R'
      balancing_code <- paste0(balancing_code,"\n\n",
      "matched_data$pairID<-paste('p', 1:length(matched_data$Group1.ID), sep='') #add in 'pair ID' var
matched_data<-tibble(matched_data) #tibble so that tidyverse can be leveraged
matched_data_long<- tidyr::pivot_longer(matched_data,                   # long format
                                          cols = c(Group1.ID, Group2.ID), #### will need to change accordingly
                                          names_to = 'group', 
                                          values_to = 'ID')
  
propensity_data$ID <- as.integer(propensity_data$ID)
matched_data_long$ID <- as.integer(matched_data_long$ID)

# merge with data incl. Tr and outcomes:
data_paired <- left_join(propensity_data,matched_data_long, by = 'ID')

#create variable indicating high vs low members of pairs
dose_paired_data <- data_paired %>%
  mutate(treatment_variable = as.numeric(data_paired[[treatment_variable]])) %>%   
  group_by(pairID) %>%  #pairID is the ID for each pair
  mutate(first = max(treatment_variable) , #create dose variable
         treatment_exposure = factor(ifelse(treatment_variable == first, 'high', 'low'))) %>%
  select(-c(group, treatment_variable)) %>%
  arrange(., pairID) %>%
  ungroup()

dose_paired_data[['treatment']] = dose_paired_data[[treatment_variable]]
dose_paired_data[[treatment_variable]] = dose_paired_data$treatment_exposure

dose_paired_data <- dose_paired_data %>%
  select(-treatment_exposure)
balanced_data = dose_paired_data")
    }
  }
  
  ## Outcome model ----
  outcome_model_code <- paste0("\n\n ##Outcome model")
  
  ## From extract_balanced_data.R
  if( "mimids" %in% class(DigiCAT_balanced_data)) {
    outcome_model_code <- paste0(outcome_model_code, "\n",
                                 "extracted_balanced_data = MatchThem::complete(balanced_data, 'all', all = FALSE)
extracted_balanced_data <- list(extracted_balanced_data, process = 'mi_psm')")
  } else if ( "wimids" %in% class(DigiCAT_balanced_data)) {
    outcome_model_code <- paste0(outcome_model_code, "\n",
                                 "extracted_balanced_data = MatchThem::complete(balanced_data, 'all', all = FALSE)
extracted_balanced_data <- list(extracted_balanced_data, process = 'mi_iptw')")
  } else if ( "matchit" %in% class(DigiCAT_balanced_data) & missing_method == "complete"){
    outcome_model_code <- paste0(outcome_model_code,"\n",
                                 "extracted_balanced_data = match.data(balanced_data)
extracted_balanced_data <- list(extracted_balanced_data, process = 'cc_psm')")
    } else if(missing_method =="weighting" & "matchit" %in% class(DigiCAT_balanced_data)){
    outcome_model_code <- paste0(outcome_model_code,"\n",
                                 "extracted_balanced_data = match.data(balanced_data)
extracted_balanced_design = svydesign(ids=~subclass, weights = (extracted_balanced_data[[weighting_variable]]*extracted_balanced_data$weights), 
data = extracted_balanced_data)
extracted_balanced_data = extracted_balanced_design
extracted_balanced_data <- list(extracted_balanced_data, process = 'weighting_psm')")
    } else if ( "weightit" %in% class(DigiCAT_balanced_data) & missing_method == "complete"){
    outcome_model_code <- paste0(outcome_model_code,"\n",
                                 "PS_estimation_results$missingness_treated_dataset = cbind(PS_estimation_results$missingness_treated_dataset,balanced_data$weights)
colnames(PS_estimation_results$missingness_treated_dataset)[colnames(PS_estimation_results$missingness_treated_dataset) == 'balanced_data$weights'] <- 'weights'
extracted_balanced_data <- list(PS_estimation_results$missingness_treated_dataset, process = 'cc_iptw')")
    } else if(missing_method=="weighting" & "weightit" %in% class(DigiCAT_balanced_data)){
    outcome_model_code <- paste0(outcome_model_code,"\n",
                                 "survey_data = PS_estimation_results$survey_design_object$variables
survey_data = cbind(survey_data,balanced_data$weights)
colnames(survey_data)[colnames(survey_data) == 'balanced_data$weights'] <- 'weights'
extracted_balanced_data = svydesign(ids=~1, weights = (survey_data[[weighting_variable]]*survey_data$weights), 
                                    data = survey_data)
extracted_balanced_data <- list(extracted_balanced_data, process = 'weighting_iptw')")
    } else if(CF_approach == "nbp" & missing_method == "complete"){
      outcome_model_code <- paste0(outcome_model_code,"\n",
                                   "extracted_balanced_data = balanced_data
extracted_balanced_data <- list(extracted_balanced_data, process = 'cc_nbp')")
    }
  
  ## From fit_outcome_model
  if(!is.null(covariates)){
    outcome_model_code <- paste0(outcome_model_code, "\n\n",
                                 "model_formula = as.formula(paste0(outcome_variable,'~',treatment_variable,'*(',paste0(matching_variable, covariates, collapse='+'), ')'))")
  }  else{
    outcome_model_code <- paste0(outcome_model_code, "\n\n",
                                 "model_formula = as.formula(paste0(outcome_variable,'~',treatment_variable,'*(',paste0(matching_variable, collapse='+'), ')'))")
  }
  
  if(DigiCAT_extracted_balanced_data$process == "mi_psm"){
    outcome_model_code <- paste0(outcome_model_code, "\n\n",
                                 "lm_model_fit <- lapply(complete(balanced_data, 'all'), function(d) {
lm(model_formula, data = d,
     weights = weights)
})
model_fit <- lapply(lm_model_fit, function(fit) {
  marginaleffects::avg_comparisons(fit, newdata = subset(fit$data, treatment_variable == 1),
                                   variables = treatment_variable, wts = 'weights', vcov = ~subclass)
})
model_fit <- mice::pool(model_fit)")
    
    } else if(DigiCAT_extracted_balanced_data$process == "cc_psm"){ 
      outcome_model_code <- paste0(outcome_model_code, "\n\n",
                                   "lm_model_fit = lm(model_formula, data = extracted_balanced_data[[1]], weights = weights)
                                   
model_fit = marginaleffects::avg_comparisons(lm_model_fit, variables = treatment_variable,
  vcov = ~subclass, 
  newdata = subset(extracted_balanced_data[[1]], 
  extracted_balanced_data[[1]][[treatment_variable]] == 1),
  wts = 'weights')")
      
    } else if (DigiCAT_extracted_balanced_data$process == 'mi_iptw'){
      outcome_model_code <- paste0(outcome_model_code, "\n\n",
                                   "lm_model_fit <- lapply(complete(balanced_data, 'all'), function(d) {
lm(model_formula, data = d,
   weights = weights)
})
model_fit <- lapply(lm_model_fit, function(fit) {
marginaleffects::avg_comparisons(fit, newdata = subset(fit$data, treatment_variable == 1),
                                 variables = treatment_variable, wts = 'weights', vcov = 'HC3')
})
model_fit <- mice::pool(model_fit)")
      
    } else if (DigiCAT_extracted_balanced_data$process == "cc_iptw"){
      outcome_model_code <- paste0(outcome_model_code, "\n\n",
                                   "model_fit = lm(model_formula, data = extracted_balanced_data[[1]], weights = weights)
model_fit = marginaleffects::avg_comparisons(model_fit, variables = treatment_variable,
                                             vcov = 'HC3',
                                             newdata = subset(extracted_balanced_data[[1]], 
                                             extracted_balanced_data[[1]][[treatment_variable]] == 1),
                                             wts = 'weights')")
      
    } else if (DigiCAT_extracted_balanced_data$process == "weighting_iptw"){
      outcome_model_code <- paste0(outcome_model_code, "\n\n",
                                   "model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]])
      model_fit = marginaleffects::avg_comparisons(model_fit, variables = treatment_variable)")
      
    } else if (DigiCAT_extracted_balanced_data$process == "weighting_psm"){
      outcome_model_code <- paste0(outcome_model_code, "\n\n",
      "model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]])
      model_fit = marginaleffects::avg_comparisons(model_fit, variables = treatment_variable)")
      
    } else if (DigiCAT_extracted_balanced_data$process == "cc_nbp"){
      outcome_model_code <- paste0(outcome_model_code, "\n\n",
      "model_fit = lm(model_formula, data = extracted_balanced_data[[1]])")
      
    }
  outcome_model_code <- paste0(outcome_model_code, "\n\n",
                               "fitted_model <- model_fit")
  
  ## From extract_outcome_results.R
  if("comparisons" %in% class(DigiCAT_fitted_model) & missing_method == "weighting"){
  
  outcome_model_code <- paste0(outcome_model_code, "\n\n",
  "extracted_outcome_results = summary(fitted_model, conf.int = TRUE)
extracted_outcome_results <-  list(extracted_outcome_results, process = 'weighting')")
      
    } else if("comparisons" %in% class(DigiCAT_fitted_model) & missing_method == "complete"){
      
      outcome_model_code <- paste0(outcome_model_code, "\n\n",
                                   "extracted_outcome_results = summary(fitted_model, conf.int = TRUE)
extracted_outcome_results <- list(extracted_outcome_results, process = 'cc')")
      } else if("mipo" %in% class(DigiCAT_fitted_model) & missing_method == "mi"){
        
        outcome_model_code <- paste0(outcome_model_code, "\n\n",
                                     "extracted_outcome_results = summary(fitted_model, conf.int = TRUE)
extracted_outcome_results <- list(extracted_outcome_results, process = 'mi')")
      
    }else if("lm" %in% class(DigiCAT_fitted_model) & missing_method == "complete"){
      
      outcome_model_code <- paste0(outcome_model_code, "\n\n",
                                   "extracted_outcome_results = summary(fitted_model)
extracted_outcome_results <- list(extracted_outcome_results, process = 'cc')")
    }
  
  ## From standardise_outcome_format.R
  
    if(DigiCAT_extracted_outcome_results$process == "mi"){
      outcome_model_code <- paste0(outcome_model_code, "\n\n",
      "results_dataframe = as.data.frame(extracted_outcome_results)
results_dataframe <- results_dataframe[,-c(2,5,6,10)]
colnames(results_dataframe) <- c('Term','Coefficient Estimate', 'Standard Error', 'P-value', 'Lower CI (2.5%)', 'Upper CI (97.5%)')
rownames(results_dataframe) <- results_dataframe[,1]  
results_dataframe <- results_dataframe[,-1]")
      
    } else if(DigiCAT_extracted_outcome_results$process == 'cc' & CF_approach != 'nbp'){
      outcome_model_code <- paste0(outcome_model_code, "\n\n",
                                   "results_dataframe = as.data.frame(extracted_outcome_results)
results_dataframe <- results_dataframe[,-c(2,5,7,10)]
colnames(results_dataframe) <- c('Term','Coefficient Estimate', 'Standard Error', 'P-value', 'Lower CI (2.5%)', 'Upper CI (97.5%)')
rownames(results_dataframe) <- results_dataframe[,1]  
results_dataframe <- results_dataframe[,-1]")
      
    } else if(DigiCAT_extracted_outcome_results$process == 'weighting' & CF_approach == 'psm'){
      outcome_model_code <- paste0(outcome_model_code, "\n\n",
                                   "results_dataframe = as.data.frame(extracted_outcome_results)
results_dataframe <- results_dataframe[,-c(2,5,7,10)]
colnames(results_dataframe) <- c('Term','Coefficient Estimate', 'Standard Error', 'P-value', 'Lower CI (2.5%)', 'Upper CI (97.5%)')
rownames(results_dataframe) <- results_dataframe[,1]  
results_dataframe <- results_dataframe[,-1]")
    } else if(DigiCAT_extracted_outcome_results$process == 'weighting' & CF_approach == 'iptw'){
      outcome_model_code <- paste0(outcome_model_code, "\n\n",
                                   "results_dataframe = as.data.frame(extracted_outcome_results)
results_dataframe <- results_dataframe[,-c(2,5,7,10)]
colnames(results_dataframe) <- c('Term','Coefficient Estimate', 'Standard Error', 'P-value', 'Lower CI (2.5%)', 'Upper CI (97.5%)')
rownames(results_dataframe) <- results_dataframe[,1]  
results_dataframe <- results_dataframe[,-1]")
    } else if(DigiCAT_extracted_outcome_results$process == 'cc' & CF_approach == 'nbp'){
      outcome_model_code <- paste0(outcome_model_code, "\n\n",
                                   "results_dataframe = as.data.frame(extracted_outcome_results[[1]][[4]])
colnames(results_dataframe) <- c('Coefficient Estimate', 'Standard Error', 'T statistic', 'P-value')
results_dataframe <- results_dataframe[c(1,2),]")
    }
  
  
  ## Create R script ----
  r_script <- c(library_code, data_source_code, variable_input_code, reduce_data_code, design_matrix_code, handled_missingness_code, propensity_score_model_code, balancing_code, outcome_model_code)
  noquote(capture.output(cat(r_script)))
  
}

# get_R_script(data_source = "sample", 
#              file_path = NULL,
#              categorical_variables = c("Gender", "Reading_age15"),
#              outcome_variable = "Anxiety_age17",
#              treatment_variable = "Reading_age15",
#              matching_variables = names(DigiCAT::zp_eg)[-c(1:3)],
#              covariates = NULL,
#              weighting_variable = NULL,
#              cluster_variable = NULL,
#              strata_variable = NULL,
#              CF_approach = "psm",
#              balancing_model = "glm",
#              missing_method = "mi",
#              matching_method = "NN",
#              matching_ratio = 1,
#              DigiCAT_balanced_data = ghi,
#              DigiCAT_extracted_balanced_data = mno$extracted_balanced_data,
#              DigiCAT_fitted_model = mno$fitted_model,
#              DigiCAT_extracted_outcome_results  = mno$extracted_outcome_results
#              )

