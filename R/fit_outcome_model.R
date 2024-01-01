
fit_outcome_model <- function(balanced_data,extracted_balanced_data,
                              outcome_variable, treatment_variable, matching_variable,
                              covariates = NULL, outcome_formula, missing_method,
                              psmodel_obj, cluster_variable = NULL, weighting_variable = NULL,
                              strata_variable = NULL,
                              ...){
  
   switch(outcome_formula,
         
         unadjusted = {
           model_fit = outcome_unadjusted(balanced_data,
                                          extracted_balanced_data,
                                          outcome_variable,
                                          treatment_variable,
                                          matching_variable, covariates,
                                          missing_method,
                                          psmodel_obj, cluster_variable,
                                          weighting_variable,
                                          strata_variable,...)
         },
         with_matching_variables = {
           model_fit = outcome_matching_variables(balanced_data,
                                                  extracted_balanced_data,
                                                  outcome_variable,
                                                  treatment_variable,
                                                  matching_variable, covariates,
                                                  missing_method,
                                                  psmodel_obj, cluster_variable,
                                                  weighting_variable,
                                                  strata_variable,...)
         },
         marginal_effects = {
           model_fit = outcome_marginal_effects(balanced_data,
                                                extracted_balanced_data,
                                                outcome_variable,
                                                treatment_variable,
                                                matching_variable, covariates,
                                                missing_method,
                                                psmodel_obj, cluster_variable,
                                                weighting_variable,
                                                strata_variable,...)
         },
         stop("Need a valid outcome formula (unadjusted, with matching variables, marginal effects)")
  )
  return(model_fit)
}

outcome_unadjusted <- function(balanced_data,
                               extracted_balanced_data,
                               outcome_variable,
                               treatment_variable,
                               matching_variable, covariates,
                               missing_method,
                               psmodel_obj, cluster_variable = NULL,
                               weighting_variable = NULL,
                               strata_variable = NULL,...){
 # browser()
  
  if(!is.null(covariates)){
    model_formula = paste0(outcome_variable,"~",paste0(c(treatment_variable,covariates),collapse="+"))
  } else{
    model_formula = as.formula(paste0(outcome_variable,"~",treatment_variable))
  }
  
  if(extracted_balanced_data$process == "mi_psm"){
      data_to_use <- extracted_balanced_data[[1]]
      
      # Check if cluster_variable is provided
      if (!is.null(cluster_variable)) {
        cluster_formula <- as.formula(paste("~", cluster_variable))
      } else {
        # Set cluster_formula to ~1 if cluster_variable is not provided
        cluster_formula <- as.formula("~1")
      }
      
      # Check if weighting_variable is provided
      if (!is.null(weighting_variable)) {
        # Convert weighting_variable to a formula
        weighting_formula <- as.formula(paste("~", weighting_variable))
      } else {
        # Use another variable as the default if weighting_variable is not provided
        weighting_formula <- as.formula("~ weights")  # Replace "weights" with the appropriate variable
      }
      
      # Check if strata_variable is provided
      if (!is.null(strata_variable)) {
        strata_formula <- as.formula(paste("~", strata_variable))
      } else {
        # Set strata_formula to NULL if strata_variable is not provided
        strata_formula <- NULL
      }
      
      mi_matched_design <- svydesign(ids = cluster_formula,
                                     weights = weighting_formula,
                                     strata = strata_formula,
                                     data = imputationList(data_to_use))
      
      model_fit = with(mi_matched_design, svyglm(model_formula)) # leave unpooled until next step

    } else if(extracted_balanced_data$process == "cc_psm"){ 
      data_to_use <- extracted_balanced_data[[1]]
      
      # Check if cluster_variable is provided
      if (!is.null(cluster_variable)) {
        cluster_formula <- as.formula(paste("~", cluster_variable))
      } else {
        # Set cluster_formula to ~1 if cluster_variable is not provided
        cluster_formula <- as.formula("~1")
      }
      
      # Check if weighting_variable is provided
      if (!is.null(weighting_variable)) {
        # Convert weighting_variable to a formula
        weighting_formula <- as.formula(paste("~", weighting_variable))
      } else {
        # Use another variable as the default if weighting_variable is not provided
        weighting_formula <- as.formula("~ weights")  # Replace "weights" with the appropriate variable
      }
      
      # Check if strata_variable is provided
      if (!is.null(strata_variable)) {
        strata_formula <- as.formula(paste("~", strata_variable))
      } else {
        # Set strata_formula to NULL if strata_variable is not provided
        strata_formula <- NULL
      }
      
      updated_design <- svydesign(ids = cluster_formula,
                                  weights = weighting_formula,
                                  strata = strata_formula,
                                  data = data_to_use)
    
      model_fit = svyglm(model_formula, design = updated_design)

  } else if (extracted_balanced_data$process == "mi_iptw"){
  
    data_to_use <- extracted_balanced_data[[1]]
    
    # Check if cluster_variable is provided
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~", cluster_variable))
    } else {
      # Set cluster_formula to ~1 if cluster_variable is not provided
      cluster_formula <- as.formula("~1")
    }

    # Check if weighting_variable is provided
    if (!is.null(weighting_variable)) {
      weighting_formula <- as.formula(paste("~", weighting_variable, "* weights"))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- as.formula("~ weights")  
    }
    
    # Check if strata_variable is provided
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~", strata_variable))
    } else {
      # Set strata_formula to NULL if strata_variable is not provided
      strata_formula <- NULL
    }
    
    mi_matched_design <- svydesign(ids = cluster_formula,
                                   weights = weighting_formula,
                                   strata = strata_formula,
                                   data = imputationList(data_to_use))
    model_fit = with(mi_matched_design, svyglm(model_formula)) # leave unpooled until next step
    
  } 
  else if (extracted_balanced_data$process == "cc_iptw"){

    data_to_use <- extracted_balanced_data[[1]]
    
    # Check if cluster_variable is provided
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~", cluster_variable))
    } else {
      # Set cluster_formula to ~1 if cluster_variable is not provided
      cluster_formula <- as.formula("~1")
    }
    
    # Check if weighting_variable is provided
    if (!is.null(weighting_variable)) {
      weighting_formula <- as.formula(paste("~", weighting_variable, "* weights"))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- as.formula("~ weights")  
    }
    
    # Check if strata_variable is provided
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~", strata_variable))
    } else {
      # Set strata_formula to NULL if strata_variable is not provided
      strata_formula <- NULL
    }
    
    updated_design <- svydesign(ids = cluster_formula,
                                weights = weighting_formula,
                                strata = strata_formula,
                                data = data_to_use)
  
    
    model_fit = svyglm(model_formula, design = updated_design)
    
  } else if (extracted_balanced_data$process == "weighting_iptw"){
    model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]])
    
  } else if (extracted_balanced_data$process == "weighting_psm"){
    model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]])
  }
  else if (extracted_balanced_data$process == "cc_nbp"){
    model_fit = lm(model_formula, data = extracted_balanced_data[[1]])
    
  }
  return(model_fit)
}


outcome_matching_variables <- function(balanced_data,
                                       extracted_balanced_data,
                                       outcome_variable,
                                       treatment_variable,
                                       matching_variable, covariates,
                                       missing_method,
                                       psmodel_obj, cluster_variable = NULL,
                                       weighting_variable = NULL,
                                       strata_variable = NULL,...){
  
  
  if(!is.null(covariates)){
    model_formula = paste0(outcome_variable,"~",paste0(c(treatment_variable,matching_variable,covariates),collapse="+"))
  } else{
    model_formula = paste0(outcome_variable,"~",paste0(c(treatment_variable,matching_variable),collapse="+"))
  }
  
  if(extracted_balanced_data$process == "mi_psm"){
    data_to_use <- extracted_balanced_data[[1]]
    
    # Check if cluster_variable is provided
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~", cluster_variable))
    } else {
      # Set cluster_formula to ~1 if cluster_variable is not provided
      cluster_formula <- as.formula("~1")
    }
    
    # Check if weighting_variable is provided
    if (!is.null(weighting_variable)) {
      # Convert weighting_variable to a formula
      weighting_formula <- as.formula(paste("~", weighting_variable))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- as.formula("~ weights")  # Replace "weights" with the appropriate variable
    }
    
    # Check if strata_variable is provided
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~", strata_variable))
    } else {
      # Set strata_formula to NULL if strata_variable is not provided
      strata_formula <- NULL
    }
    
    mi_matched_design <- svydesign(ids = cluster_formula,
                                   weights = weighting_formula,
                                   strata = strata_formula,
                                   data = imputationList(data_to_use))
    
    model_fit = with(mi_matched_design, svyglm(model_formula)) # leave unpooled until next step
    
  } else if(extracted_balanced_data$process == "cc_psm"){ 
      data_to_use <- extracted_balanced_data[[1]]
      
      # Check if cluster_variable is provided
      if (!is.null(cluster_variable)) {
        cluster_formula <- as.formula(paste("~", cluster_variable))
      } else {
        # Set cluster_formula to ~1 if cluster_variable is not provided
        cluster_formula <- as.formula("~1")
      }
      
      # Check if weighting_variable is provided
      if (!is.null(weighting_variable)) {
        # Convert weighting_variable to a formula
        weighting_formula <- as.formula(paste("~", weighting_variable))
      } else {
        # Use another variable as the default if weighting_variable is not provided
        weighting_formula <- as.formula("~ weights")  # Replace "weights" with the appropriate variable
      }
      
      # Check if strata_variable is provided
      if (!is.null(strata_variable)) {
        strata_formula <- as.formula(paste("~", strata_variable))
      } else {
        # Set strata_formula to NULL if strata_variable is not provided
        strata_formula <- NULL
      }
      
      updated_design <- svydesign(ids = cluster_formula,
                                  weights = weighting_formula,
                                  strata = strata_formula,
                                  data = data_to_use)
      
      model_fit = svyglm(model_formula, design = updated_design)
    
  } else if (extracted_balanced_data$process == "mi_iptw"){
    
    data_to_use <- extracted_balanced_data[[1]]
    
    # Check if cluster_variable is provided
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~", cluster_variable))
    } else {
      # Set cluster_formula to ~1 if cluster_variable is not provided
      cluster_formula <- as.formula("~1")
    }
    
    # Check if weighting_variable is provided
    if (!is.null(weighting_variable)) {
      weighting_formula <- as.formula(paste("~", weighting_variable, "* weights"))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- as.formula("~ weights")  
    }
    
    # Check if strata_variable is provided
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~", strata_variable))
    } else {
      # Set strata_formula to NULL if strata_variable is not provided
      strata_formula <- NULL
    }
    
    mi_matched_design <- svydesign(ids = cluster_formula,
                                   weights = weighting_formula,
                                   strata = strata_formula,
                                   data = imputationList(data_to_use))
    model_fit = with(mi_matched_design, svyglm(model_formula)) # leave unpooled until next step
    
  } 
  
  else if (extracted_balanced_data$process == "cc_iptw"){

    data_to_use <- extracted_balanced_data[[1]]
    
    # Check if cluster_variable is provided
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~", cluster_variable))
    } else {
      # Set cluster_formula to ~1 if cluster_variable is not provided
      cluster_formula <- as.formula("~1")
    }
    
    # Check if weighting_variable is provided
    if (!is.null(weighting_variable)) {
      weighting_formula <- as.formula(paste("~", weighting_variable, "* weights"))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- as.formula("~ weights")  
    }
    
    # Check if strata_variable is provided
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~", strata_variable))
    } else {
      # Set strata_formula to NULL if strata_variable is not provided
      strata_formula <- NULL
    }
    
    updated_design <- svydesign(ids = cluster_formula,
                                weights = weighting_formula,
                                strata = strat_formula,
                                data = data_to_use)
    
    
    model_fit = svyglm(model_formula, design = updated_design)
    
  } else if (extracted_balanced_data$process == "weighting_iptw"){
    model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]])
    
  } else if (extracted_balanced_data$process == "weighting_psm"){
    model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]])
  }
  else if (extracted_balanced_data$process == "cc_nbp"){
    model_fit = lm(model_formula, data = extracted_balanced_data[[1]])
    
  }
  return(model_fit)
}


outcome_marginal_effects <- function(balanced_data,
                                     extracted_balanced_data,
                                     outcome_variable,
                                     treatment_variable,
                                     matching_variable, covariates,
                                     missing_method,
                                     psmodel_obj, cluster_variable = NULL,
                                     weighting_variable = NULL,
                                     strata_variable = NULL,...){
  if(!is.null(covariates)){
    model_formula <- as.formula(paste0(outcome_variable, " ~ ", 
                                       treatment_variable, " * (", 
                                       paste(c(matching_variable, covariates), collapse = " + "), ")"))
    
  } else{
    model_formula = as.formula(paste0(outcome_variable,
                                      "~",treatment_variable,
                                      "*(",paste0(matching_variable, collapse="+"), ")"))
  }
  if(extracted_balanced_data$process == "mi_psm"){

    data_to_use <- extracted_balanced_data[[1]]
    
    # Check if cluster_variable is provided
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~", cluster_variable))
    } else {
      # Set cluster_formula to ~1 if cluster_variable is not provided
      cluster_formula <- as.formula("~1")
    }
    
    # Check if weighting_variable is provided
    if (!is.null(weighting_variable)) {
      weighting_formula <- as.formula(paste("~", weighting_variable, "* weights"))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- as.formula("~ weights")  
    }
    
    # Check if strata_variable is provided
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~", strata_variable))
    } else {
      # Set strata_formula to NULL if strata_variable is not provided
      strata_formula <- NULL
    }
    
    mi_matched_design <- svydesign(ids = cluster_formula,
                                   weights = weighting_formula,
                                   strata = strata_formula,
                                   data = imputationList(data_to_use))

    model_fit = with(mi_matched_design, svyglm(model_formula))
    
    model_fit = lapply(model_fit, function(fit){
       marginaleffects::avg_comparisons(fit, newdata = subset(fit$data, get(treatment_variable) == 1),
                                        variables = treatment_variable, wts = "weights", vcov = ~subclass)
     })


    
  } else if(extracted_balanced_data$process == "cc_psm"){ 
    if(!is.null(psmodel_obj$survey_design_object)){
      data_to_use <- extracted_balanced_data[[1]]
      
      # Check if cluster_variable is provided
      if (!is.null(cluster_variable)) {
        cluster_formula <- as.formula(paste("~", cluster_variable))
      } else {
        # Set cluster_formula to ~1 if cluster_variable is not provided
        cluster_formula <- as.formula("~1")
      }
      
      # Check if weighting_variable is provided
      if (!is.null(weighting_variable)) {
        # Convert weighting_variable to a formula
        weighting_formula <- as.formula(paste("~", weighting_variable))
      } else {
        # Use another variable as the default if weighting_variable is not provided
        weighting_formula <- as.formula("~ weights")  # Replace "weights" with the appropriate variable
      }
      
      # Check if strata_variable is provided
      if (!is.null(strata_variable)) {
        strata_formula <- as.formula(paste("~", strata_variable))
      } else {
        # Set strata_formula to NULL if strata_variable is not provided
        strata_formula <- NULL
      }
      
      updated_design <- svydesign(ids = cluster_formula,
                                  weights = weighting_formula,
                                  strata = strata_formula,
                                  data = data_to_use)
      
      model_fit = svyglm(model_formula, design = updated_design)
      model_fit = marginaleffects::avg_comparisons(model_fit, variables = treatment_variable)
      
    }else{
      model_fit = lm(model_formula, data = extracted_balanced_data[[1]], weights = weights)

      model_fit = marginaleffects::avg_comparisons(model_fit, variables = treatment_variable,
                                                 vcov = ~subclass, 
                                                 newdata = subset(extracted_balanced_data[[1]], 
                                                                  extracted_balanced_data[[1]][[treatment_variable]] == 1),
                                                 wts = "weights")
    
  } 
    } else if (extracted_balanced_data$process == "mi_iptw"){

      data_to_use <- extracted_balanced_data[[1]]
      
      # Check if cluster_variable is provided
      if (!is.null(cluster_variable)) {
        cluster_formula <- as.formula(paste("~", cluster_variable))
      } else {
        # Set cluster_formula to ~1 if cluster_variable is not provided
        cluster_formula <- as.formula("~1")
      }
      
      # Check if weighting_variable is provided
      if (!is.null(weighting_variable)) {
        weighting_formula <- as.formula(paste("~", weighting_variable, "* weights"))
      } else {
        # Use another variable as the default if weighting_variable is not provided
        weighting_formula <- as.formula("~ weights")  
      }
      
      # Check if strata_variable is provided
      if (!is.null(strata_variable)) {
        strata_formula <- as.formula(paste("~", strata_variable))
      } else {
        # Set strata_formula to NULL if strata_variable is not provided
        strata_formula <- NULL
      }
      
      mi_matched_design <- svydesign(ids = cluster_formula,
                                     weights = weighting_formula,
                                     strata = strata_formula,
                                     data = imputationList(data_to_use))
      
      model_fit = with(mi_matched_design, svyglm(model_formula)) # leave unpooled until next step
    
     #  model_fit = lapply(model_fit, function(fit){
     #  marginaleffects::avg_comparisons(fit, newdata = subset(fit$data, treatment_variable == 1),
     #                                   variables = treatment_variable, wts = "weights", vcov = "HC3")
     # })
     # 
    # model_fit <- mice::pool(model_fit)
  } 
  else if (extracted_balanced_data$process == "cc_iptw"){
    
    data_to_use <- extracted_balanced_data[[1]]
    
    # Check if cluster_variable is provided
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~", cluster_variable))
    } else {
      # Set cluster_formula to ~1 if cluster_variable is not provided
      cluster_formula <- as.formula("~1")
    }
    
    # Check if weighting_variable is provided
    if (!is.null(weighting_variable)) {
      weighting_formula <- as.formula(paste("~", weighting_variable, "* weights"))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- as.formula("~ weights")  
    }
    
    # Check if strata_variable is provided
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~", strata_variable))
    } else {
      # Set strata_formula to NULL if strata_variable is not provided
      strata_formula <- NULL
    }
    
    updated_design <- svydesign(ids = cluster_formula,
                                weights = weighting_formula,
                                strata = strat_formula,
                                data = data_to_use)
    
    
    model_fit = svyglm(model_formula, design = updated_design)
    
    model_fit = marginaleffects::avg_comparisons(model_fit, variables = treatment_variable,
                                                 vcov = "HC3",
                                                 newdata = subset(extracted_balanced_data[[1]], 
                                                                  extracted_balanced_data[[1]][[treatment_variable]] == 1),
                                                 wts = "weights")
    
  } else if (extracted_balanced_data$process == "weighting_iptw"){
    model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]])
    model_fit = marginaleffects::avg_comparisons(model_fit, variables = treatment_variable)
    
  } else if (extracted_balanced_data$process == "weighting_psm"){
    model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]])
    model_fit = marginaleffects::avg_comparisons(model_fit, variables = treatment_variable)
    
  }
  return(model_fit)
}












