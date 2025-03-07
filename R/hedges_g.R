#' Function to calculate Hedge's G
#'
#' @param treatment_variable Name of treatment variable
#' @param outcome_variable Name of outcome variable
#' @param missing_method 
#' @param balanced_data Balanced data object
#' @param outcome_model Outcome model object 
#' @param weighting_variable Character string matching column name of your weighting variable
#' @param cluster_variable Character string matching column name of your clustering variable
#' @param strata_variable Character string matching column name of your stratification variable
#' 
#' @import WeightIt
#' @import MatchIt
#' @import MatchThem
#' @import survey


hedges_g <- function(treatment_variable, 
                     missing_method, 
                     outcome_variable, 
                     balanced_data, 
                     outcome_model,  
                     weighting_variable = NULL, 
                     cluster_variable = NULL, 
                     strata_variable = NULL){
  
  if(missing_method == "complete"){
    
    # Define survey design
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
    } else {
      cluster_formula <- as.formula("~1")
    }
    
    if (!is.null(weighting_variable)) {
      weighting_formula <- as.formula(paste("~as.numeric(as.character(", weighting_variable, "))"))
    } else {
      weighting_formula <- as.formula("~ weights")
    }
    
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
    } else {
      strata_formula <- NULL
    }
    
    svy_design <- svydesign(
      ids = cluster_formula,
      strata = strata_formula,
      weights = weighting_formula,
      data = outcome_model$extracted_balanced_data[[1]]
    )
    
    # ## Get mean difference
    # f <- paste0(outcome_variable, " ~ ", treatment_variable)
    # svy_mean_diff <- svyglm(
    #   formula = f, 
    #   design = svy_design
    # )
    # mean_diff <- coef(svy_mean_diff)[2]
    
    ## Will use coefficient already calculated? Same as above if "unadjusted" outcome model used
    mean_diff <- outcome_model$standardised_format$`Coefficient Estimate`
    
    # Get SD of outcome in treatment group
    treatment_data <- subset(svy_design, get(treatment_variable) == 1)
    f <- paste0(" ~ as.numeric(as.character(", outcome_variable, "))")
    SD_t <- sqrt(svyvar(as.formula(f), design = treatment_data)[1])
    
    # Get SD of outcome in control group
    control_data <- subset(svy_design, get(treatment_variable) == 0)
    f <- paste0(" ~ as.numeric(as.character(", outcome_variable, "))")
    SD_c <- sqrt(svyvar(as.formula(f), design = control_data)[1])
    
    # Get N treat and N control (effective sample sizes)
    f <- paste0(" ~ ", treatment_variable)
    N_t <- sum(svytotal(as.formula(f), design = treatment_data))
    N_c <- sum(svytotal(as.formula(f), design = control_data))
    
    # Hedge's g calculation
    pooled_variance <- (((SD_t^2) * (N_t - 1)) + ((SD_c^2) * (N_c - 1))) / (N_t + N_c - 2)
    g <- mean_diff / sqrt(pooled_variance)
  }

  if(missing_method == "weighting"){
    
    # Survey design already defined
    svy_design <- outcome_model$extracted_balanced_data[[1]]
    
    ## Coefficient already defined
    mean_diff <- outcome_model$standardised_format$`Coefficient Estimate`
    
    # Get SD of outcome in treatment group
    treatment_data <- subset(svy_design, get(treatment_variable) == 1)
    f <- paste0(" ~ ", outcome_variable)
    SD_t <- sqrt(svyvar(as.formula(f), design = treatment_data, na.rm=TRUE)[1])
    
    # Get SD of outcome in control group
    control_data <- subset(svy_design, get(treatment_variable) == 0)
    f <- paste0(" ~ ", outcome_variable)
    SD_c <- sqrt(svyvar(as.formula(f), design = control_data,  na.rm=TRUE)[1])
    
    # Get N treat and N control (effective sample sizes)
    f <- paste0(" ~ ", treatment_variable)
    N_t <- sum(svytotal(as.formula(f), design = treatment_data))
    N_c <- sum(svytotal(as.formula(f), design = control_data))
    
    # Hedge's g calculation
    pooled_variance <- (((SD_t^2) * (N_t - 1)) + ((SD_c^2) * (N_c - 1))) / (N_t + N_c - 2)
    g <- mean_diff / sqrt(pooled_variance)
  }
  
  if (missing_method == "mi") {
    
    ## Get imputed dataset
    imputed_data <- as.list(outcome_model$extracted_balanced_data[[1]])
    
    # Define survey design
    if (!is.null(cluster_variable)) {
      cluster_formula <- as.formula(paste("~as.numeric(as.character(", cluster_variable, "))"))
    } else {
      cluster_formula <- as.formula("~1")
    }
    
    if (!is.null(weighting_variable)) {
      weighting_formula <- as.formula(paste("~ as.numeric(as.character(", weighting_variable, ")) * weights"))
    } else {
      # Use another variable as the default if weighting_variable is not provided
      weighting_formula <- as.formula("~ weights")  
    }
    
    if (!is.null(strata_variable)) {
      strata_formula <- as.formula(paste("~as.numeric(as.character(", strata_variable, "))"))
    } else {
      strata_formula <- NULL
    }
    
    # Define survey design
    svy_design_list <- lapply(imputed_data, function(data) {
      svydesign(
        ids = cluster_formula,
        strata = strata_formula,
        weights = weighting_formula,
        data = data
      )
    })
    
    ## Get mean difference
    mean_diff <- outcome_model$standardised_format$`Coefficient Estimate`
    
    ## Get SD of outcome in treatment group
    results <- lapply(svy_design_list, function(svy_design) {
      # Subset for the treatment group
      treatment_data <- subset(svy_design, get(treatment_variable) == 1)
      control_data <- subset(svy_design, get(treatment_variable) == 0)
      
      # Define formulas
      f_SD <- as.formula(paste0(" ~ ", outcome_variable))
      f_N <- as.formula(paste0(" ~ ", treatment_variable))
      
      # Calculate statistics
      list(
        SD_t = sqrt(svyvar(f_SD, design = treatment_data)[1]),  # Standard deviation for treatment group
        SD_c = sqrt(svyvar(f_SD, design = control_data)[1]),    # Standard deviation for control group
        N_t = svytotal(f_N, design = treatment_data),     # Total for treatment group
        N_c = svytotal(f_N, design = control_data)        # Total for control group
      )
    })
    
    ## Aggregate results across imputations
    mean_SD_t <- mean(sapply(results, `[[`, "SD_t"))
    mean_SD_c <- mean(sapply(results, `[[`, "SD_c"))
    mean_N_t <- mean(sapply(results, `[[`, "N_t"))*2
    mean_N_c <- mean(sapply(results, `[[`, "N_c"))*2

    ## Pooled standard deviation
    pooled_SD <- sqrt(((mean_SD_t^2) * (mean_N_t - 1) + (mean_SD_c^2) * (mean_N_c - 1)) / (mean_N_t + mean_N_c - 2))
    
    ## Hedge's g calculation
    g <- mean_diff / pooled_SD
  }
  
  ## Return Hedge's g
  return(g)
}
