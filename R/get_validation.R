#' Data validation in DigiCAT
#' @import moments
#' @param data Name of upladed dataset. Default is 'inputData$rawdata'.
#' @param treatment Name of treatment variable.
#' @param outcome Name of outcome variable.
#' @param matchvars Name of matching variables.
#' @param NRW_var Name of non-response weight variable.
get_validation <- function(.data, treatment, outcome, matchvars, covars, survey_weight_var, non_response_weight, clustering_var, stratification_var){
  
  ## Keep log of data validation
  validation_log <- list(
    treatment_variable_error = FALSE,
    survey_weight_available = FALSE,
    clustering_available = FALSE,
    stratification_available = FALSE,
    survey_weight_no_missingness = FALSE,
    non_response_weight_no_missingness = FALSE,
    clustering_no_missingness = FALSE,
    stratification_no_missingness = FALSE,
    no_design_matrix_error = FALSE,
    some_missingness_no_non_response = FALSE,
    some_missingness_but_non_response = FALSE,
    no_missingness_no_non_response = FALSE,
    no_missingness_but_non_response = FALSE
  )
  
  ## Save variable to log error
  error_check <- NA
  design_matrix_error <- NULL
  
  ## Create new variables to input varible names in desing matrix function - NULL will be input if varaible conatins missingness
  survey_weight_var_for_matrix <- NULL
  clustering_var_for_matrix <- NULL
  stratification_var_for_matrix <- NULL

  ## Remove rows with NAs
  data_Nas <- .data
  .data <- na.omit(.data)
  
  ## Calculate correlation matrix of matchvars and covars and get pair of highly correlated variables
  cor_dat <- cor(.data[c(matchvars, covars)])
  w <- which(abs(cor_dat)>0.80 & row(cor_dat)<col(cor_dat), arr.ind=TRUE)
  high_cor <- matrix(colnames(cor_dat)[w],ncol=2)
  
  print_validation <- p(
    h4("Data Dimensions:"),
    h5("Data contains ", dim(.data)[2], " columns and ", dim(.data)[1], " rows."),
    
    ## Print info on number of columns
    if((dim(.data)[2] > 2) & (dim(.data)[2] < 100)){
      h5("Your data has an appropriate number of columns.", style = "color:green")
    }else{h5("It is recommended that data has between 2 and 100 columns, please reconsider that data you are using.", style = "color:red")},
    
    ## Print info on number of rows
    if((dim(.data)[1] > 10) & (dim(.data)[1] < 10000)){
      h5("Your data has an appropriate number of rows.", style = "color:green")
    }else{h5("It is recommended that data has between 10 and 10,000 rows, please reconsider the data you are using as performing counterfactual analysis on this data may require a large amount of run time.", style = "color:red")},  br(),
    
    ## Check outcome variable is continuous
    h4("Outcome Variable Type:"),
    if(length(unique(.data[[outcome]])) < 5){
      h5("There are ", length(unique(.data[[outcome]])), " unique observations in your chosen outcome variable:", outcome,". This seems a bit low for a continuous variable. We recommend you double check this before continuing." , style = "color:red")
    } else{h5("There are ", length(unique(.data[[outcome]])), " unique observations in your chosen outcome variable.", style = "color:green")}, br(),
    
    ## Check outcome variable is normally distributed
    h4("Outcome Variable Skewness:"),
    
    if (abs(skewness(.data[[outcome]])) <= 0.8){
      h5("Your outcome variable does not appear highly skewed, skewness: ", round(skewness(.data[[outcome]]), 2), style = "color:green")
    } else{h5("Your outcome variable appears to be highly skewed, skewness: ", round(skewness(.data[[outcome]]), 2), style = "color:red")},
    
    renderPlot(hist(.data[[outcome]], 
                    main = paste("Histogram of ", outcome),
                    xlab = "",
                    col = "#76b9f5")), br(),
    
    ## Check treatment variable is binary/ordinal
    h4("Treatment Variable Type:"),
    if(length(unique(.data[[treatment]])) == 2 & all(.data[[treatment]]%in% 0:1)){
      h5("You have selected ",treatment, " as your treatment variable. This has been detected as a binary variable and can be used in the 
      current counterfactual approaches offered by DigiCAT." , style = "color:green")
    }, 
    if(length(unique(.data[[treatment]])) == 2 & !all(.data[[treatment]]%in% 0:1)){
      validation_log$treatment_variable_error <- TRUE
      h5("You have selected ",treatment, " as your treatment variable. This has been detected as a binary variable but contains values other than 1 and 0,
      meaning it cannot be used in the counterfactual analysis currently supported by DigiCAT. To proceed, please recode your treatment variable.", style = "color:red")
    },

    if((length(unique(.data[[treatment]])) > 2) & (length(unique(.data[[treatment]])) < 6)){
      h5("You have selected ",treatment, " as your treatment variable. This has been detected as an ordinal variable and can be used in the 
      current counterfactual approaches offered by DigiCAT." , style = "color:green")
    },
    if(length(unique(.data[[treatment]])) > 5){
      validation_log$treatment_variable_error <- TRUE
      h5("You have selected ",treatment, " as your treatment variable. This has been detected as a continuous variable and cannot be used in the 
      current counterfactual approaches offered by DigiCAT. Please reselect or categorize your current treatment variable." , style = "color:red")
    },
    br(),
  
    ## Check multicollinearity between variables
    h4("Multicollinearity Between Variables:"),
    
    if (nrow(high_cor) == 0){
      p(h5("None of the selected matching variables or covariates appear to be strongly correlated (Pearson correaltion > -0.9 or < 0.9).", style = "color:green"),br())
    } else{
      
      p(h5("It looks like some of the  matching variables and/or covariates you have selected are highly correlated (Pearson correaltion > -0.9 or < 0.9). 
       Please consider removing one of each highly correlated variable pair:", paste0(high_cor[,1], " and ", high_cor[,2], collapse = ", "), style = 'color:red'), br())
    },
    
    ## Check selected survey weight variable for missingness
    if (!is.null(survey_weight_var)){
      
      validation_log$survey_weight_available <- TRUE
      
      ## If missingness detected, give warning
      if (any(is.na(data_Nas[[survey_weight_var]]))){

        p(h4("Survey Weight Variable Missingness:"),
          h5(paste0("You have selected ", survey_weight_var, " as your survey weight.
                  As this includes missing data it will not be used in counterfactual analysis."), 
             style = 'color:red'), br())
        
      }
      else{
        
        survey_weight_var_for_matrix <- survey_weight_var
        
        validation_log$survey_weight_no_missingness <- TRUE
        
        p(h4("Survey Weight Variable Missingness:"),
          h5(paste0("You have selected ", survey_weight_var, " as your survey weight. No missingness has been detected in this variable."), 
             style = 'color:green'), br())
      }
    },

    ## Check selected non-response weight variable has no missingness
    if (!is.null(survey_weight_var) & isTruthy(non_response_weight)){
      if (any(is.na(data_Nas[[survey_weight_var]]))){
        
        p(h4("Non-response Variable Missingness:"),
        h5(paste0("You have selected ", survey_weight_var, " as your survey weight and have indicated that this compensates for non-response.
                  As this includes missing data it won't be used as a weight when accounting for missingness"), 
           style = 'color:red'), br())

      }
      else{
        
        validation_log$non_response_weight_no_missingness <- TRUE
        
        p(h4("Non-response Variable Missingness:"),
        h5(paste0("You have selected ", survey_weight_var, " as your survey weight and have indicated that this compensates for non-response. No missingness has been detected in this variable."), 
           style = 'color:green'), br())

      }
    },
    
    
    ## Check selected clustering variable for missingness
    if (!is.null(clustering_var)){
      
      validation_log$clustering_available <- TRUE
      
      ## If missingness detected, give warning
      if (any(is.na(data_Nas[[clustering_var]]))){
        
        p(h4("Clustering Variable Missingness:"),
          h5(paste0("You have selected ", clustering_var, " as your clustering variable.
                  As this includes missing data it will not be used in counterfactual analysis."), 
             style = 'color:red'), br())
        
      }
      else{
        
        clustering_var_for_matrix <- clustering_var
        
        validation_log$clustering_no_missingness <- TRUE
        
        p(h4("Clustering Variable Missingness:"),
          h5(paste0("You have selected ", clustering_var, " as your clustering variable. No missingness has been detected in this variable."), 
             style = 'color:green'), br())

        
      }
    },
    
    
    ## Check selected stratification variable for missingness
    if (!is.null(stratification_var)){
      
      validation_log$stratification_available <- TRUE
      
      ## If missingness detected, give warning
      if (any(is.na(data_Nas[[stratification_var]]))){

        p(h4("Stratification Variable Missingness:"),
          h5(paste0("You have selected ", stratification_var, " as your stratification variable.
                  As this includes missing data it will not be used in counterfactual analysis."), 
             style = 'color:red'), br())
        
      }
      else{
        
        stratification_var_for_matrix <- stratification_var
        
        validation_log$clustering_no_missingness <- TRUE
        
        p(h4("Stratification Variable Missingness:"),
          h5(paste0("You have selected ", stratification_var, " as your stratification variable. No missingness has been detected in this variable."), 
             style = 'color:green'), br())
        
      }
    },
    
    
    # Check if design matrix can be created with inputted survey design weights, clustering and stratification variable
    # Ensure at least one variable is present w/o missingness beforehand
    
    if (validation_log$survey_weight_no_missingness | validation_log$clustering_no_missingness | validation_log$stratification_no_missingness){
      error_check <- tryCatch(
      create_design(.data = data_Nas,
                    weighting_variable = survey_weight_var_for_matrix,
                    clustering_variable = clustering_var_for_matrix,
                    strata_variable = stratification_var_for_matrix),
      ## If error in creating design matrix, return error message
      error = function(cond) {
        ## Output error message
        design_matrix_error <- p(h4("Design Matrix Creation:"),
          p(paste0("Design matrix cannot be created with the provided input and will not be used in counterfactual analysis.
                   Error: ", conditionMessage(cond)) , style = "color:red"), br())

      })
      
      p(design_matrix_error)
      
      },
    
    ## If error in design matrix creation, log
    if (all(grepl("Error:", error_check))){
      
      validation_log$no_design_matrix_error <- FALSE
      
      p("")
      
    }
    else{
      
      validation_log$no_design_matrix_error <- TRUE
      
      p("")
    },
    
    ## Check for presence of missingness in uploaded data
    ## If there is missingness
    if (any(is.na(data_Nas[,c(treatment, outcome, matchvars, covars, survey_weight_var_for_matrix, clustering_var_for_matrix, stratification_var_for_matrix)]))){
      
      p(h4("Missing Data:"),
        h5(paste0("Missing data had been detected in your inputted variables. Choices of dealing with missingness and further information are available in the next analysis step."), 
           style = 'color:green'), br())
      
      ## Log if there is missingness with or without non-reponse weight
      if (validation_log$non_response_weight_no_missingness){
        
        validation_log$some_missingness_but_non_response <- TRUE
        p("")
        
      }else{
        
        validation_log$some_missingness_no_non_response <- TRUE
        p("")
        
      }
      
    },
    ## If there is no missingness but there is a non-response variable
    if (!any(is.na(data_Nas[,c(treatment, outcome, matchvars, covars, survey_weight_var_for_matrix, clustering_var_for_matrix, stratification_var_for_matrix)])) & validation_log$non_response_weight_no_missingness){
      
      validation_log$no_missingness_but_non_response <- TRUE
      
      p(h4("Missing Data:"),
        h5(paste0("No missing data had been detected in your inputted variables, however, as the survey weight in your data compensates for non-response, 'weighting' as a method of dealing with missingness in the next analysis step (currently only available for binary treatment variables)."),
           style = 'color:green'), br())
    },
    ## If there is no missingness and no non-response variable
    if (!any(is.na(data_Nas[,c(treatment, outcome, matchvars, covars, survey_weight_var_for_matrix, clustering_var_for_matrix, stratification_var_for_matrix)])) & !validation_log$non_response_weight_no_missingness){
      
      validation_log$no_missingness_no_non_response <- TRUE
      
      p(h4("Missing Data:"),
        h5(paste0("No missing data had been detected in your inputted variables, complete case as a method of dealing with data missingness will be automitically applied to your data."),
           style = 'color:green'), br())
    },
    
    
    
    ## If error in design matrix creation, overwrite missingness so weighting as a method of dealing with missingness cannot be used
    if (all(grepl("Error:", error_check))){
      
      validation_log$no_missingness_but_non_response <- FALSE
      validation_log$some_missingness_but_non_response <- FALSE
      
      p("")
      
    }
  )
  
  ## Return validation output and log
  return(list(
    print = print_validation,
    log = validation_log
  ))
}










 
