#' Data validation in DigiCAT
#' @import moments
#' @param data Name of uploaded dataset.
#' @param treatment Name of treatment variable.
#' @param outcome Name of outcome variable.
#' @param matchvars Name of matching variables.
#' @param covars Name of covariates.
#' @param survey_weight_var Name of survey weight variable.
#' @param non_response_weight Is there a non-response weight variable?
#' @param clustering_var Name of clustering variable.
#' @param stratification_var Name of stratification variable.
get_validation <- function(.data, treatment, outcome, matchvars, covars, survey_weight_var, non_response_weight, clustering_var, stratification_var, i18n){
  
  ## Keep log of data validation
  validation_log <- list(
    treatment_variable_error = FALSE,
    no_GBM = FALSE,
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
  
  ## Get treatment counts
  df <- data.frame(x = names(table(.data[[treatment]])),
                   y  = as.character(table(.data[[treatment]])))
  
  names(df) <- c(i18n$t("Upload Validation treatment counts group"), i18n$t("Upload Validation treatment counts count"))
  
  outcome_type <- check_selected_outcome(.data, outcome)
  
  print_validation <- p(
    h4(i18n$t("Upload Validation dimensions")),
    h5(i18n$t("Upload Validation dimensions columns"), dim(.data)[2]),
    h5(i18n$t("Upload Validation dimensions rows"), dim(.data)[1]),
    
    ## Print info on number of columns
    if((dim(.data)[2] > 2) & (dim(.data)[2] < 100)){
      h5(i18n$t("Upload Validation dimensions correct columns"), style = "color:green")
    }else{h5(i18n$t("Upload Validation dimensions incorrect columns"), style = "color:red")},
    
    ## Print info on number of rows
    if((dim(.data)[1] > 10) & (dim(.data)[1] < 10000)){
      h5(i18n$t("Upload Validation dimensions correct rows"), style = "color:green")
    }else{h5(i18n$t("Upload Validation dimensions incorrect rows"), style = "color:red")},
    
    ## Print message about GBM not being available if there are less than 50 rows
    if(dim(.data)[1] < 50){
      validation_log$no_GBM <- TRUE
      h5(i18n$t("Upload Validation no GBM"), style = "color:grey")
    },br(),
    
    ## Check outcome variable is continuous
    h4(i18n$t("Upload Validation outcome type")),
    
    if (outcome_type == 'Continuous'){
      
      p(h5("Continuous outcome variable selected" , style = "color:green"),
      h4(i18n$t("Upload Validation skewness")),
      if (abs(skewness(.data[[outcome]])) <= 0.8){
        h5(i18n$t("Upload Validation skewness correct"), round(skewness(.data[[outcome]]), 2), style = "color:green")
      } else{h5(i18n$t("Upload Validation skewness incorrect"), round(skewness(.data[[outcome]]), 2), style = "color:red")})
    
      renderPlot(hist(.data[[outcome]], 
                      main = paste(i18n$t("Upload Validation histogram"), outcome),
                      xlab = "",
                      col = "#76b9f5"))
    },br(),
    
    
    ## Check treatment variable is binary/ordinal
    h4(i18n$t("Upload Validation treatment")),
    if(length(unique(.data[[treatment]])) == 1){
      validation_log$treatment_variable_error <- TRUE
      h5(treatment, i18n$t("Upload Validation treatment unary incorrect") , style = "color:red")
      }, 
    if(length(unique(.data[[treatment]])) == 2 & all(.data[[treatment]]%in% 0:1)){
      h5(treatment, i18n$t("Upload Validation treatment binary correct") , style = "color:green")
      }, 
    if(length(unique(.data[[treatment]])) == 2 & !all(.data[[treatment]]%in% 0:1)){
      validation_log$treatment_variable_error <- TRUE
      h5(treatment, i18n$t("Upload Validation treatment binary incorrect"), style = "color:red")
    },
    
    if((length(unique(.data[[treatment]])) > 2) & (length(unique(.data[[treatment]])) < 6)){
      h5(treatment, i18n$t("Upload Validation treatment ordinal correct") , style = "color:green")
    },

    if(length(unique(.data[[treatment]])) > 5){
      h5(treatment, i18n$t("Upload Validation treatment continuous correct") , style = "color:green")
    },
    br(),
    
    ## Only show treatment counts for binary and ordinal treatments
    if(length(unique(.data[[treatment]])) < 6){
      
      p(
        h4(i18n$t("Upload Validation treatment counts")),
        ## Print n for each treatment group
        renderTable(df),
        h5(i18n$t("Upload Validation treatment counts description"))
      )
      
      },
    
    br(),
    
    ## Check multicollinearity between variables
    h4(i18n$t("Upload Validation multicollinearity")),
    
    if (nrow(high_cor) == 0){
      p(h5(i18n$t("Upload Validation multicollinearity correct"), style = "color:green"),br())
    } else{
      
      p(h5(i18n$t("Upload Validation multicollinearity incorrect"), paste0(high_cor[,1], " and ", high_cor[,2], collapse = ", "), style = 'color:red'), br())
    },
    
    ## Check selected survey weight variable for missingness
    if (!is.null(survey_weight_var)){
      
      validation_log$survey_weight_available <- TRUE
      
      ## If missingness detected, give warning
      if (any(is.na(data_Nas[[survey_weight_var]]))){
        
        p(h4(i18n$t("Upload Validation survey weight")),
          h5(paste0(survey_weight_var, i18n$t("Upload Validation survey weight missingness")), 
             style = 'color:red'), br())
        
      }
      else{
        
        survey_weight_var_for_matrix <- survey_weight_var
        
        validation_log$survey_weight_no_missingness <- TRUE
        
        p(h4(i18n$t("Upload Validation survey weight")),
          h5(paste0(survey_weight_var, i18n$t("Upload Validation survey weight no missingness")), 
             style = 'color:green'), br())
      }
    },
    
    ## Check selected non-response weight variable has no missingness
    if (!is.null(survey_weight_var) & isTruthy(non_response_weight)){
      if (any(is.na(data_Nas[[survey_weight_var]]))){
        
        p(h4(i18n$t("Upload Validation nonresponse")),
          h5(paste0(survey_weight_var, i18n$t("Upload Validation nonresponse missingness")), 
             style = 'color:red'), br())
        
      }
      else{
        
        validation_log$non_response_weight_no_missingness <- TRUE
        
        p(h4(i18n$t("Upload Validation nonresponse")),
          h5(paste0(survey_weight_var, i18n$t("Upload Validation nonresponse no missingness")), 
             style = 'color:green'), br())
        
      }
    },
    
    
    ## Check selected clustering variable for missingness
    if (!is.null(clustering_var)){
      
      validation_log$clustering_available <- TRUE
      
      ## If missingness detected, give warning
      if (any(is.na(data_Nas[[clustering_var]]))){
        
        p(h4(i18n$t("Upload Validation clustering")),
          h5(paste0(clustering_var, i18n$t("Upload Validation clustering missingness"), 
             style = 'color:red'), br()))
        
      }
      else{
        
        clustering_var_for_matrix <- clustering_var
        
        validation_log$clustering_no_missingness <- TRUE
        
        p(h4(i18n$t("Upload Validation clustering")),
          h5(paste0(clustering_var, i18n$t("Upload Validation clustering no missingness"), 
             style = 'color:green'), br()))
        
        
      }
    },
    
    
    ## Check selected stratification variable for missingness
    if (!is.null(stratification_var)){
      
      validation_log$stratification_available <- TRUE
      
      ## If missingness detected, give warning
      if (any(is.na(data_Nas[[stratification_var]]))){
        
        p(h4(i18n$t("Upload Validation stratification")),
          h5(paste0(stratification_var, i18n$t("Upload Validation stratification missingness")), 
             style = 'color:red'), br())
        
      }
      else{
        
        stratification_var_for_matrix <- stratification_var
        
        validation_log$clustering_no_missingness <- TRUE
        
        p(h4(i18n$t("Upload Validation stratification")),
          h5(paste0(stratification_var, i18n$t("Upload Validation stratification no missingness")), 
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
          design_matrix_error <- p(h4(i18n$t("Upload Validation design matrix")),
                                   p(paste0(i18n$t("Upload Validation design matrix description"),
                                            conditionMessage(cond)) , style = "color:red"), br())
          
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
      
      p(h4(i18n$t("Upload Validation missingness")),
        h5(paste0(i18n$t("Upload Validation missingness yes")), 
           style = 'color:green'), br())
    }, ## If statements cannot be combined for some reason - prevents message from printing
    
    if (any(is.na(data_Nas[,c(treatment, outcome, matchvars, covars, survey_weight_var_for_matrix, clustering_var_for_matrix, stratification_var_for_matrix)]))){
      
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
      
      p(h4(i18n$t("Upload Validation missingness")),
        h5(paste0(i18n$t("Upload Validation missingness no non-response yes")),
           style = 'color:green'), br())
    },
    ## If there is no missingness and no non-response variable
    if (!any(is.na(data_Nas[,c(treatment, outcome, matchvars, covars, survey_weight_var_for_matrix, clustering_var_for_matrix, stratification_var_for_matrix)])) & !validation_log$non_response_weight_no_missingness){
      
      validation_log$no_missingness_no_non_response <- TRUE
      
      p(h4(i18n$t("Upload Validation missingness")),
        h5(paste0(i18n$t("Upload Validation missingness no non-response no")),
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











