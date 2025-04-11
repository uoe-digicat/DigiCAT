
fit_outcome_model <- function(balanced_data,extracted_balanced_data,
                              outcome_variable, outcome_type, treatment_variable, matching_variable,
                              covariates = NULL, outcome_formula, missing_method,
                              PS_estimation_object, cluster_variable = NULL, weighting_variable = NULL,
                              strata_variable = NULL,
                              ...){
   switch(outcome_formula,
         
         unadjusted = {
           model_fit = outcome_unadjusted(balanced_data,
                                          extracted_balanced_data,
                                          outcome_variable,
                                          outcome_type,
                                          treatment_variable,
                                          matching_variable, covariates,
                                          missing_method,
                                          PS_estimation_object, cluster_variable,
                                          weighting_variable,
                                          strata_variable,...)
         },
         with_matching_variables = {
           model_fit = outcome_matching_variables(balanced_data,
                                                  extracted_balanced_data,
                                                  outcome_variable,
                                                  outcome_type,
                                                  treatment_variable,
                                                  matching_variable, covariates,
                                                  missing_method,
                                                  PS_estimation_object, cluster_variable,
                                                  weighting_variable,
                                                  strata_variable,...)
         },
         marginal_effects = {
           model_fit = outcome_marginal_effects(balanced_data,
                                                extracted_balanced_data,
                                                outcome_variable,
                                                outcome_type,
                                                treatment_variable,
                                                matching_variable, covariates,
                                                missing_method,
                                                PS_estimation_object, cluster_variable,
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
                               outcome_type,
                               treatment_variable,
                               matching_variable, covariates = NULL,
                               missing_method,
                               PS_estimation_object, cluster_variable = NULL,
                               weighting_variable = NULL,
                               strata_variable = NULL,...){

  if(!is.null(covariates)){
    model_formula = paste0(outcome_variable,"~",paste0(c(treatment_variable,covariates),collapse="+"))
  } else{
    model_formula = as.formula(paste0(outcome_variable,"~",treatment_variable))
  }
  
  if(extracted_balanced_data$process == "mi_psm"){
      data_to_use <- extracted_balanced_data[[1]]
      
      # Check if cluster_variable is provided
      if (!is.null(cluster_variable)) {
        cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
      } else {
        # Set cluster_formula to ~1 if cluster_variable is not provided
        cluster_formula <- as.formula("~1")
      }
      
      # Check if weighting_variable is provided
      if (!is.null(weighting_variable)) {
        # Convert weighting_variable to a formula
        weighting_formula <- as.formula(paste("~as.numeric(as.character(", weighting_variable, "))"))
      } else {
        # Use another variable as the default if weighting_variable is not provided
        weighting_formula <- as.formula("~ weights")  # Replace "weights" with the appropriate variable
      }
      
      # Check if strata_variable is provided
      if (!is.null(strata_variable)) {
        strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
      } else {
        # Set strata_formula to NULL if strata_variable is not provided
        strata_formula <- NULL
      }
      
      mi_matched_design <- svydesign(ids = cluster_formula,
                                     weights = weighting_formula,
                                     strata = strata_formula,
                                     data = imputationList(data_to_use))
      
      if (outcome_type == 'continuous'){
        model_fit = with(mi_matched_design, svyglm(model_formula)) # leave unpooled until next step
      } else if (outcome_type == 'binary'){
        model_fit = with(mi_matched_design, svyglm(model_formula, family = 'binomial')) # leave unpooled until next step
      } else if (outcome_type == 'categorical'){
        model_fit = with(mi_matched_design, svyVGAM::svy_vglm(model_formula, family = multinomial))
      }
      

  } else if(extracted_balanced_data$process == "mi_nbp"){
    
    data_to_use <- extracted_balanced_data[[1]]
    
    ## Set <treatment variable>:low as reference group
    data_to_use <- lapply(data_to_use, function(df) {
      df[[treatment_variable]] <- relevel(df[[treatment_variable]], ref = "low")
      return(df)
    })
    
    # Check if cluster_variable is provided
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
    } else {
      # Set cluster_formula to ~1 if cluster_variable is not provided
      cluster_formula <- as.formula("~1")
    }
    
    # Check if weighting_variable is provided
    if (!is.null(weighting_variable)) {
      # Convert weighting_variable to a formula
      weighting_formula <- as.formula(paste("~as.numeric(as.character(", weighting_variable, "))"))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- NULL  # Replace "weights" with the appropriate variable
    }
    
    # Check if strata_variable is provided
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
    } else {
      # Set strata_formula to NULL if strata_variable is not provided
      strata_formula <- NULL
    }
    
    mi_matched_design <- svydesign(ids = cluster_formula,
                                   weights = weighting_formula,
                                   strata = strata_formula,
                                   data = imputationList(data_to_use))
    
    if (outcome_type == 'continuous'){
      model_fit = with(mi_matched_design, svyglm(model_formula)) # leave unpooled until next step
    } else if (outcome_type == 'binary'){
      model_fit = with(mi_matched_design, svyglm(model_formula, family = 'binomial')) # leave unpooled until next step
    } 
  
    } else if(extracted_balanced_data$process == "cc_psm"){ 
      data_to_use <- extracted_balanced_data[[1]]
      
      # Check if cluster_variable is provided
      if (!is.null(cluster_variable)) {
        cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
      } else {
        # Set cluster_formula to ~1 if cluster_variable is not provided
        cluster_formula <- as.formula("~1")
      }
      
      # Check if weighting_variable is provided
      if (!is.null(weighting_variable)) {
        # Convert weighting_variable to a formula
        weighting_formula <- as.formula(paste("~as.numeric(as.character(", weighting_variable, "))"))
      } else {
        # Use another variable as the default if weighting_variable is not provided
        weighting_formula <- as.formula("~ weights")  # Replace "weights" with the appropriate variable
      }
      
      # Check if strata_variable is provided
      if (!is.null(strata_variable)) {
        strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
      } else {
        # Set strata_formula to NULL if strata_variable is not provided
        strata_formula <- NULL
      }
      
      updated_design <- svydesign(ids = cluster_formula,
                                  weights = weighting_formula,
                                  strata = strata_formula,
                                  data = data_to_use)
      
      if (outcome_type == 'continuous'){
        model_fit = svyglm(model_formula, design = updated_design)
      } else if (outcome_type == 'binary'){
        model_fit = svyglm(model_formula, design = updated_design, family = 'binomial')
      } else if (outcome_type == 'categorical'){
        model_fit = svyVGAM::svy_vglm(model_formula, design = updated_design,  family = multinomial)
      }
      

  } else if (extracted_balanced_data$process == "mi_iptw"){
  
    data_to_use <- extracted_balanced_data[[1]]
    
    # Check if cluster_variable is provided
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
    } else {
      # Set cluster_formula to ~1 if cluster_variable is not provided
      cluster_formula <- as.formula("~1")
    }

    # Check if weighting_variable is provided
    if (!is.null(weighting_variable)) {
      weighting_formula <- as.formula(paste("~as.numeric(as.character(", weighting_variable, "))* weights"))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- as.formula("~ weights")  
    }
    
    # Check if strata_variable is provided
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
    } else {
      # Set strata_formula to NULL if strata_variable is not provided
      strata_formula <- NULL
    }
    
    mi_matched_design <- svydesign(ids = cluster_formula,
                                   weights = weighting_formula,
                                   strata = strata_formula,
                                   data = imputationList(data_to_use))
    
    if (outcome_type == 'continuous'){
      model_fit = with(mi_matched_design, svyglm(model_formula)) # leave unpooled until next step
    } else if (outcome_type == 'binary'){
      model_fit = with(mi_matched_design, svyglm(model_formula, family = 'binomial')) # leave unpooled until next step
    } else if (outcome_type == 'categorical'){
      model_fit = with(mi_matched_design, svyVGAM::svy_vglm(model_formula, family = multinomial))
    }
     
    
  } 
  else if (extracted_balanced_data$process == "cc_iptw"){

    data_to_use <- extracted_balanced_data[[1]]
    
    # Check if cluster_variable is provided
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
    } else {
      # Set cluster_formula to ~1 if cluster_variable is not provided
      cluster_formula <- as.formula("~1")
    }
    
    # Check if weighting_variable is provided
    if (!is.null(weighting_variable)) {
      weighting_formula <- as.formula(paste("~as.numeric(as.character(", weighting_variable, ")) * weights"))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- as.formula("~ weights")  
    }
    
    # Check if strata_variable is provided
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
    } else {
      # Set strata_formula to NULL if strata_variable is not provided
      strata_formula <- NULL
    }
    
    updated_design <- svydesign(ids = cluster_formula,
                                weights = weighting_formula,
                                strata = strata_formula,
                                data = data_to_use)
  
    if (outcome_type == 'continuous'){
      model_fit = svyglm(model_formula, design = updated_design)
    } else if (outcome_type == 'binary'){
      model_fit = svyglm(model_formula, design = updated_design, family = 'binomial')
    } else if (outcome_type == 'categorical'){
      model_fit = svyVGAM::svy_vglm(model_formula, design = updated_design,  family = multinomial)
    }
    
    
  } else if (extracted_balanced_data$process == "weighting_iptw"){
    
    if (outcome_type == 'continuous'){
      model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]])
    } else if (outcome_type == 'binary'){
      model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]], family = 'binomial')
    }
    
    
  } else if (extracted_balanced_data$process == "weighting_psm"){
    if (outcome_type == 'continuous'){
      model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]])
    } else if (outcome_type == 'binary'){
      model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]], family = 'binomial')
    }
    
  }
  else if (extracted_balanced_data$process == "cc_nbp" | extracted_balanced_data$process == "weighting_nbp"){
    
    data_to_use <- extracted_balanced_data[[1]]
   
    ## Set <treatment variable>:low as reference group
    data_to_use[[treatment_variable]] <- relevel(data_to_use[[treatment_variable]], ref= "low")
    
    # Check if cluster_variable is provided
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
    } else {
      # Set cluster_formula to ~1 if cluster_variable is not provided
      cluster_formula <- as.formula("~1")
    }
    
    # Check if weighting_variable is provided
    if (!is.null(weighting_variable)) {
      weighting_formula <- as.formula(paste("~as.numeric(as.character(", weighting_variable, "))"))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- NULL  
    }
    
    # Check if strata_variable is provided
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
    } else {
      # Set strata_formula to NULL if strata_variable is not provided
      strata_formula <- NULL
    }
    
    updated_design <- svydesign(ids = cluster_formula,
                                weights = weighting_formula,
                                strata = strata_formula,
                                data = data_to_use)
    
    if (outcome_type == 'continuous'){
      model_fit = svyglm(model_formula, design = updated_design)
    } else if (outcome_type == 'binary'){
      model_fit = svyglm(model_formula, design = updated_design, family = 'binomial')
    }
  }
  else if(extracted_balanced_data$process == "cc_cbps"){
    data_to_use <- extracted_balanced_data[[1]]
    
    # Check if cluster_variable is provided
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
    } else {
      # Set cluster_formula to ~1 if cluster_variable is not provided
      cluster_formula <- as.formula("~1")
    }
    
    # Check if weighting_variable is provided
    if (!is.null(weighting_variable)) {
      weighting_formula <- as.formula(paste("~ as.numeric(as.character(", weighting_variable, ")) * weights"))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- as.formula("~ weights")   
    }
    
    # Check if strata_variable is provided
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
    } else {
      # Set strata_formula to NULL if strata_variable is not provided
      strata_formula <- NULL
    }
    
    updated_design <- svydesign(ids = cluster_formula,
                                weights = weighting_formula,
                                strata = strata_formula,
                                data = data_to_use)
    
    if (outcome_type == 'continuous'){
      model_fit = svyglm(model_formula, design = updated_design)
    } else if (outcome_type == 'binary'){
      model_fit = svyglm(model_formula, design = updated_design, family = 'binomial')
    } else if (outcome_type == 'categorical'){
      model_fit = svyVGAM::svy_vglm(model_formula, design = updated_design,  family = multinomial)
    }
    
  } 
  else if(extracted_balanced_data$process == "mi_cbps"){
    data_to_use <- extracted_balanced_data[[1]]
    
    # Check if cluster_variable is provided
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
    } else {
      # Set cluster_formula to ~1 if cluster_variable is not provided
      cluster_formula <- as.formula("~1")
    }
    
    # Check if weighting_variable is provided
    if (!is.null(weighting_variable)) {
      weighting_formula <- as.formula(paste("~ as.numeric(as.character(", weighting_variable, ")) * weights"))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- as.formula("~ weights")  
    }
    
    # Check if strata_variable is provided
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
    } else {
      # Set strata_formula to NULL if strata_variable is not provided
      strata_formula <- NULL
    }
    
    mi_matched_design <- svydesign(ids = cluster_formula,
                                   weights = weighting_formula,
                                   strata = strata_formula,
                                   data = imputationList(data_to_use))
    
    if (outcome_type == 'continuous'){
      model_fit = with(mi_matched_design, svyglm(model_formula)) # leave unpooled until next step
    } else if (outcome_type == 'binary'){
      model_fit = with(mi_matched_design, svyglm(model_formula, family = 'binomial')) # leave unpooled until next step
    } else if (outcome_type == 'categorical'){
      model_fit = with(mi_matched_design, svyVGAM::svy_vglm(model_formula, family = multinomial))
    }
    
  }
  return(model_fit)
}


outcome_matching_variables <- function(balanced_data,
                                       extracted_balanced_data,
                                       outcome_variable,
                                       outcome_type,
                                       treatment_variable,
                                       matching_variable, covariates = NULL,
                                       missing_method,
                                       PS_estimation_object, cluster_variable = NULL,
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
      cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
    } else {
      # Set cluster_formula to ~1 if cluster_variable is not provided
      cluster_formula <- as.formula("~1")
    }
    
    # Check if weighting_variable is provided
    if (!is.null(weighting_variable)) {
      # Convert weighting_variable to a formula
      weighting_formula <- as.formula(paste("~as.numeric(as.character(", weighting_variable, "))"))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- as.formula("~ weights")  # Replace "weights" with the appropriate variable
    }
    
    # Check if strata_variable is provided
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
    } else {
      # Set strata_formula to NULL if strata_variable is not provided
      strata_formula <- NULL
    }
    
    mi_matched_design <- svydesign(ids = cluster_formula,
                                   weights = weighting_formula,
                                   strata = strata_formula,
                                   data = imputationList(data_to_use))
    
    if (outcome_type == 'continuous'){
      model_fit = with(mi_matched_design, svyglm(model_formula))
    } else if (outcome_type == 'binary'){
      model_fit = with(mi_matched_design, svyglm(model_formula, family = 'binomial'))
    } else if (outcome_type == 'categorical'){
      model_fit = with(mi_matched_design, svyVGAM::svy_vglm(formula(model_formula), family = multinomial))
    }
     # leave unpooled until next step
    
  } else if(extracted_balanced_data$process == "mi_nbp"){

    data_to_use <- extracted_balanced_data[[1]]
    
    ## Set <treatment variable>:low as reference group
    data_to_use <- lapply(data_to_use, function(df) {
      df[[treatment_variable]] <- relevel(df[[treatment_variable]], ref = "low")
      return(df)
    })
    
    # Check if cluster_variable is provided
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
    } else {
      # Set cluster_formula to ~1 if cluster_variable is not provided
      cluster_formula <- as.formula("~1")
    }
    
    # Check if weighting_variable is provided
    if (!is.null(weighting_variable)) {
      # Convert weighting_variable to a formula
      weighting_formula <- as.formula(paste("~as.numeric(as.character(", weighting_variable, "))"))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- NULL  # Replace "weights" with the appropriate variable
    }
    
    # Check if strata_variable is provided
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
    } else {
      # Set strata_formula to NULL if strata_variable is not provided
      strata_formula <- NULL
    }
    
    mi_matched_design <- svydesign(ids = cluster_formula,
                                   weights = weighting_formula,
                                   strata = strata_formula,
                                   data = imputationList(data_to_use))
    
    if (outcome_type == 'continuous'){
      model_fit = with(mi_matched_design, svyglm(model_formula)) # leave unpooled until next step
    } else if (outcome_type == 'binary'){
      model_fit = with(mi_matched_design, svyglm(model_formula, family = 'binomial')) # leave unpooled until next step
    } else if (outcome_type == 'categorical'){
      model_fit = with(mi_matched_design, svyVGAM::svy_vglm(formula(model_formula), family = multinomial))
    }
    
    
    } else if(extracted_balanced_data$process == "cc_psm"){ 
      data_to_use <- extracted_balanced_data[[1]]
      
      # Check if cluster_variable is provided
      if (!is.null(cluster_variable)) {
        cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
      } else {
        # Set cluster_formula to ~1 if cluster_variable is not provided
        cluster_formula <- as.formula("~1")
      }
      
      # Check if weighting_variable is provided
      if (!is.null(weighting_variable)) {
        # Convert weighting_variable to a formula
        weighting_formula <- as.formula(paste("~as.numeric(as.character(", weighting_variable, "))"))
      } else {
        # Use another variable as the default if weighting_variable is not provided
        weighting_formula <- as.formula("~ weights")  # Replace "weights" with the appropriate variable
      }
      
      # Check if strata_variable is provided
      if (!is.null(strata_variable)) {
        strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
      } else {
        # Set strata_formula to NULL if strata_variable is not provided
        strata_formula <- NULL
      }
      
      updated_design <- svydesign(ids = cluster_formula,
                                  weights = weighting_formula,
                                  strata = strata_formula,
                                  data = data_to_use)
      
      if (outcome_type == 'continuous'){
        model_fit = svyglm(model_formula, design = updated_design)
      } else if (outcome_type == 'binary'){
        model_fit = svyglm(model_formula, design = updated_design, family = 'binomial')
      } else if (outcome_type == 'categorical'){
        model_fit = svyVGAM::svy_vglm(formula(model_formula), design = updated_design,  family = multinomial)
      }
      
    
  } else if (extracted_balanced_data$process == "mi_iptw"){
    
    data_to_use <- extracted_balanced_data[[1]]
    
    # Check if cluster_variable is provided
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
    } else {
      # Set cluster_formula to ~1 if cluster_variable is not provided
      cluster_formula <- as.formula("~1")
    }
    
    # Check if weighting_variable is provided
    if (!is.null(weighting_variable)) {
      weighting_formula <- as.formula(paste("~ as.numeric(as.character(", weighting_variable, ")) * weights"))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- as.formula("~ weights")  
    }
    
    # Check if strata_variable is provided
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
    } else {
      # Set strata_formula to NULL if strata_variable is not provided
      strata_formula <- NULL
    }
    
    mi_matched_design <- svydesign(ids = cluster_formula,
                                   weights = weighting_formula,
                                   strata = strata_formula,
                                   data = imputationList(data_to_use))
    
    if (outcome_type == 'continuous'){
      model_fit = with(mi_matched_design, svyglm(model_formula)) # leave unpooled until next step
    } else if (outcome_type == 'binary'){
      model_fit = with(mi_matched_design, svyglm(model_formula, family = 'binomial')) # leave unpooled until next step
    } else if (outcome_type == 'categorical'){
      model_fit = with(mi_matched_design, svyVGAM::svy_vglm(formula(model_formula), family = multinomial))
    }
    
    
  } 
  
  else if (extracted_balanced_data$process == "cc_iptw"){

    data_to_use <- extracted_balanced_data[[1]]
    
    # Check if cluster_variable is provided
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
    } else {
      # Set cluster_formula to ~1 if cluster_variable is not provided
      cluster_formula <- as.formula("~1")
    }
    
    # Check if weighting_variable is provided
    if (!is.null(weighting_variable)) {
      weighting_formula <- as.formula(paste("~ as.numeric(as.character(", weighting_variable, "))* weights"))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- as.formula("~ weights")  
    }
    
    # Check if strata_variable is provided
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
    } else {
      # Set strata_formula to NULL if strata_variable is not provided
      strata_formula <- NULL
    }
    
    updated_design <- svydesign(ids = cluster_formula,
                                weights = weighting_formula,
                                strata = strata_formula,
                                data = data_to_use)
    
    if (outcome_type == 'continuous'){
      model_fit = svyglm(model_formula, design = updated_design)  
    } else if (outcome_type == 'binary'){
      model_fit = svyglm(model_formula, design = updated_design, family = 'binomial')
    } else if (outcome_type == 'categorical'){
      model_fit = svyVGAM::svy_vglm(formula(model_formula), design = updated_design,  family = multinomial)
    }
    
    
  } else if (extracted_balanced_data$process == "weighting_iptw"){
    if (outcome_type == 'continuous'){
      model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]])
    } else if (outcome_type == 'binary'){
      model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]], family = 'binomial')
    }
    
    
  } else if (extracted_balanced_data$process == "weighting_psm"){
    if (outcome_type == 'continuous'){
      model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]])
    } else if (outcome_type == 'binary'){
      model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]], family = 'binomial')
    }
    
  }
  else if (extracted_balanced_data$process == "cc_nbp" | extracted_balanced_data$process == "weighting_nbp"){
    
    data_to_use <- extracted_balanced_data[[1]]
    
    ## Set <treatment variable>:low as reference group
    data_to_use[[treatment_variable]] <- relevel(data_to_use[[treatment_variable]], ref= "low")

    # Check if cluster_variable is provided
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
    } else {
      # Set cluster_formula to ~1 if cluster_variable is not provided
      cluster_formula <- as.formula("~1")
    }
    
    # Check if weighting_variable is provided
    if (!is.null(weighting_variable)) {
      weighting_formula <- as.formula(paste("~as.numeric(as.character(", weighting_variable, "))"))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- NULL 
    }
    
    # Check if strata_variable is provided
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
    } else {
      # Set strata_formula to NULL if strata_variable is not provided
      strata_formula <- NULL
    }
    
    updated_design <- svydesign(ids = cluster_formula,
                                weights = weighting_formula,
                                strata = strata_formula,
                                data = data_to_use)
    

    if (outcome_type == 'continuous'){
      model_fit = svyglm(model_formula, design = updated_design)
    } else if (outcome_type == 'binary'){
      model_fit = svyglm(model_formula, design = updated_design, family = 'binomial')
    } else if (outcome_type == 'categorical'){
      model_fit = svyVGAM::svy_vglm(model_formula, design = updated_design,  family = multinomial)
    }
  }
  else if(extracted_balanced_data$process == "cc_cbps"){
    data_to_use <- extracted_balanced_data[[1]]
    
    # Check if cluster_variable is provided
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
    } else {
      # Set cluster_formula to ~1 if cluster_variable is not provided
      cluster_formula <- as.formula("~1")
    }
    
    # Check if weighting_variable is provided
    if (!is.null(weighting_variable)) {
      weighting_formula <- as.formula(paste("~ as.numeric(as.character(", weighting_variable, ")) * weights"))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- as.formula("~ weights")   
    }
    
    # Check if strata_variable is provided
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
    } else {
      # Set strata_formula to NULL if strata_variable is not provided
      strata_formula <- NULL
    }
    
    updated_design <- svydesign(ids = cluster_formula,
                                weights = weighting_formula,
                                strata = strata_formula,
                                data = data_to_use)
    
    if (outcome_type == 'continuous'){
      model_fit = svyglm(model_formula, design = updated_design)
    } else if (outcome_type == 'binary'){
      model_fit = svyglm(model_formula, design = updated_design, family = 'binomial')
    } else if (outcome_type == 'categorical'){
      model_fit = svyVGAM::svy_vglm(formula(model_formula), design = updated_design,  family = multinomial)
    }
    
  }
  else if(extracted_balanced_data$process == "mi_cbps"){
    data_to_use <- extracted_balanced_data[[1]]
    
    # Check if cluster_variable is provided
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
    } else {
      # Set cluster_formula to ~1 if cluster_variable is not provided
      cluster_formula <- as.formula("~1")
    }
    
    # Check if weighting_variable is provided
    if (!is.null(weighting_variable)) {
      weighting_formula <- as.formula(paste("~ as.numeric(as.character(", weighting_variable, ")) * weights"))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- as.formula("~ weights")  
    }
    
    # Check if strata_variable is provided
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
    } else {
      # Set strata_formula to NULL if strata_variable is not provided
      strata_formula <- NULL
    }
    
    mi_matched_design <- svydesign(ids = cluster_formula,
                                   weights = weighting_formula,
                                   strata = strata_formula,
                                   data = imputationList(data_to_use))
    
    if (outcome_type == 'continuous'){
      model_fit = with(mi_matched_design, svyglm(model_formula)) # leave unpooled until next step
    } else if (outcome_type == 'binary'){
      model_fit = with(mi_matched_design, svyglm(model_formula, family = 'binomial')) # leave unpooled until next step
    } else if (outcome_type == 'categorical'){
      model_fit = with(mi_matched_design, svyVGAM::svy_vglm(formula(model_formula), family = multinomial))
    }
     
  }
  return(model_fit)
}


outcome_marginal_effects <- function(balanced_data,
                                     extracted_balanced_data,
                                     outcome_variable,
                                     outcome_type,
                                     treatment_variable,
                                     matching_variable, covariates,
                                     missing_method,
                                     PS_estimation_object, cluster_variable = NULL,
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
      cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
    } else {
      # Set cluster_formula to ~1 if cluster_variable is not provided
      cluster_formula <- as.formula("~1")
    }
    
    # Check if weighting_variable is provided
    if (!is.null(weighting_variable)) {
      weighting_formula <- as.formula(paste("~ as.numeric(as.character(", weighting_variable, ")) * weights"))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- as.formula("~ weights")  
    }
    
    # Check if strata_variable is provided
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
    } else {
      # Set strata_formula to NULL if strata_variable is not provided
      strata_formula <- NULL
    }
    
    mi_matched_design <- svydesign(ids = cluster_formula,
                                   weights = weighting_formula,
                                   strata = strata_formula,
                                   data = imputationList(data_to_use))
    if (outcome_type == 'continuous'){
      model_fit = with(mi_matched_design, svyglm(model_formula))
    } else if (outcome_type == 'binary'){
      model_fit = with(mi_matched_design, svyglm(model_formula, family = 'binomial'))
    } 
    
    model_fit = lapply(model_fit, function(fit){
       marginaleffects::avg_comparisons(fit, newdata = subset(fit$data,get(treatment_variable) == 1),
                                        variables = treatment_variable, wts = "weights", vcov = ~subclass)
     })

    model_fit = mice::pool(model_fit)
    
  } else if(extracted_balanced_data$process == "cc_psm"){ 
    if(!is.null(PS_estimation_object$survey_design_object)){
      data_to_use <- extracted_balanced_data[[1]]
      
      # Check if cluster_variable is provided
      if (!is.null(cluster_variable)) {
        cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
      } else {
        # Set cluster_formula to ~1 if cluster_variable is not provided
        cluster_formula <- as.formula("~1")
      }
      
      # Check if weighting_variable is provided
      if (!is.null(weighting_variable)) {
        # Convert weighting_variable to a formula
        weighting_formula <- as.formula(paste("~as.numeric(as.character(", weighting_variable, "))"))
      } else {
        # Use another variable as the default if weighting_variable is not provided
        weighting_formula <- as.formula("~ weights")  # Replace "weights" with the appropriate variable
      }
      
      # Check if strata_variable is provided
      if (!is.null(strata_variable)) {
        strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
      } else {
        # Set strata_formula to NULL if strata_variable is not provided
        strata_formula <- NULL
      }
      
      updated_design <- svydesign(ids = cluster_formula,
                                  weights = weighting_formula,
                                  strata = strata_formula,
                                  data = data_to_use)
      
      if (outcome_type == 'continuous'){
        model_fit = svyglm(model_formula, design = updated_design)
      } else if (outcome_type == 'binary'){
        model_fit = svyglm(model_formula, design = updated_design, family = 'binomial')
      }
      
      model_fit = marginaleffects::avg_comparisons(model_fit, 
                                                   variables = treatment_variable,
                                                   vcov = ~subclass, 
                                                   wts = "weights",
                                                   newdata = subset(extracted_balanced_data[[1]], 
                                                                    extracted_balanced_data[[1]][[treatment_variable]] == 1)
                                                   )
      
    }else{
      if (outcome_type == 'continuous'){
        model_fit = lm(model_formula, data = extracted_balanced_data[[1]], weights = weights)
      } else if (outcome_type == 'binary'){
        model_fit = glm(model_formula, data = extracted_balanced_data[[1]], weights = weights, family = 'binomial')
      }
      

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
        cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
      } else {
        # Set cluster_formula to ~1 if cluster_variable is not provided
        cluster_formula <- as.formula("~1")
      }
      
      # Check if weighting_variable is provided
      if (!is.null(weighting_variable)) {
        weighting_formula <- as.formula(paste("~ as.numeric(as.character(", weighting_variable, ")) * weights"))
      } else {
        # Use another variable as the default if weighting_variable is not provided
        weighting_formula <- as.formula("~ weights")  
      }
      
      # Check if strata_variable is provided
      if (!is.null(strata_variable)) {
        strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
      } else {
        # Set strata_formula to NULL if strata_variable is not provided
        strata_formula <- NULL
      }
      
      mi_matched_design <- svydesign(ids = cluster_formula,
                                     weights = weighting_formula,
                                     strata = strata_formula,
                                     data = imputationList(data_to_use))
      
      
      if (outcome_type == 'continuous'){
        model_fit = with(mi_matched_design, svyglm(model_formula)) 
      } else if (outcome_type == 'binary'){
        model_fit = with(mi_matched_design, svyglm(model_formula, family = 'binomial')) 
      }
      
    
     model_fit = lapply(model_fit, function(fit){
     marginaleffects::avg_comparisons(fit, newdata = fit$data,
                                      variables = treatment_variable, wts = "weights", vcov = "HC3")
    })

    model_fit <- mice::pool(model_fit)
  } 
  else if (extracted_balanced_data$process == "cc_iptw"){
    
    data_to_use <- extracted_balanced_data[[1]]
    
    # Check if cluster_variable is provided
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
    } else {
      # Set cluster_formula to ~1 if cluster_variable is not provided
      cluster_formula <- as.formula("~1")
    }
    
    # Check if weighting_variable is provided
    if (!is.null(weighting_variable)) {
      weighting_formula <- as.formula(paste("~ as.numeric(as.character(", weighting_variable, ")) * weights"))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- as.formula("~ weights")  
    }
    
    # Check if strata_variable is provided
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
    } else {
      # Set strata_formula to NULL if strata_variable is not provided
      strata_formula <- NULL
    }
    
    updated_design <- svydesign(ids = cluster_formula,
                                weights = weighting_formula,
                                strata = strata_formula,
                                data = data_to_use)
    
    if (outcome_type == 'continuous'){
      model_fit = svyglm(model_formula, design = updated_design)
    } else if (outcome_type == 'binary'){
      model_fit = svyglm(model_formula, design = updated_design, family = 'binomial')
    }

    
    
    model_fit = marginaleffects::avg_comparisons(model_fit, variables = treatment_variable,
                                                 vcov = "HC3",
                                                 newdata = extracted_balanced_data[[1]], 
                                                 wts = "weights")
    
  } else if (extracted_balanced_data$process == "weighting_iptw"){
    
    if (outcome_type == 'continuous'){
      model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]])
    } else if (outcome_type == 'binary'){
      model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]], family = 'binomial')
    }
    
    model_fit = marginaleffects::avg_comparisons(model_fit, variables = treatment_variable)
    
  } else if (extracted_balanced_data$process == "weighting_psm"){
    if (outcome_type == 'continuous'){
      model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]])
    } else if (outcome_type == 'binary'){
      model_fit = svyglm(model_formula, design = extracted_balanced_data[[1]], family = 'binomial')
    }
    
    model_fit = marginaleffects::avg_comparisons(model_fit, variables = treatment_variable)
    
  } else if(extracted_balanced_data$process == "cc_cbps"){
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
    
    if (outcome_type == 'continuous'){
      model_fit = svyglm(model_formula, design = updated_design)
    } else if (outcome_type == 'binary'){
      model_fit = svyglm(model_formula, design = updated_design, family = 'binomial')
    }
    
    model_fit = marginaleffects::avg_comparisons(model_fit, variables = treatment_variable, wts = model_fit$weights)
    
  }
  else if(extracted_balanced_data$process == "mi_cbps"){
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
    
    if (outcome_type == 'continuous'){
      model_fit = with(mi_matched_design, svyglm(model_formula)) # leave unpooled until next step
    } else if (outcome_type == 'binary'){
      model_fit = with(mi_matched_design, svyglm(model_formula, family = 'binomial')) # leave unpooled until next step
    }
    
    # START HERE ON WEDNESDAY
    # NEED to aggregate to get the correct estiamte.
    model_fit = lapply(model_fit, function(fit){
      marginaleffects::avg_comparisons(fit, newdata = fit$data,
                                       variables = treatment_variable, wts = "weights", vcov = "HC3")
    })
    
    model_fit <- mice::pool(model_fit)
    
  }
  
  return(model_fit)
}












