## Functions that validate uploaded data and variables
#' @param data Name of upladed dataset. Default is 'inputData$rawdata'.
#' @param treatment Name of treatment variable.
#' @param outcome Name of outcome variable.
#' @param matchvars Name of matching variables.



get_validation <- function(.data, treatment, outcome, matchvars, covars){

  require(moments)
  
  ## Remove rows with NAs
  .data <- na.omit(.data)
  
 ## Calculate correlation matrix of matchvars and covars and get pair of highly correlated variables
  cor_dat <- cor(.data[c(matchvars, covars)])
  w <- which(abs(cor_dat)>0.80 & row(cor_dat)<col(cor_dat), arr.ind=TRUE)
  high_cor <- matrix(colnames(cor_dat)[w],ncol=2)
  
  p(
    h4("Data Dimensions:"),
    h5("Data contains ", dim(.data)[2], " columns and ", dim(.data)[1], " rows."),
    
    ## Print info on number of columns
    if((dim(.data)[2] > 2) & (dim(.data)[2] < 100)){
      h5("Your data has an appropriate number of columns.", style = "color:green")
    }else{h5("It is reccomended that data has between 2 and 100 columns, please reconsider that data you are using.", style = "color:red")},
    
    ## Print info on number of rows
    if((dim(.data)[1] > 10) & (dim(.data)[1] < 10000)){
      h5("Your data has an appropriate number of rows.", style = "color:green")
    }else{h5("It is reccomended that data has between 10 and 10,000 rows, please reconsider that data you are using.", style = "color:red")},  br(),
    
    ## Print warning if any variables contain letters
    h4("Non-Numberic Data:"),
    
    ## Check all data is numeric
    if(all(sapply(.data, is.numeric))){
      h5("All of your uploaded data is numeric.", style = "color:green")
    } else{h5("Looks like some of your data is non-numeric, this could mean it contains identifiable information and could violate ", actionLink("TCs_link", "DigiCAT Customer Agreement."))},  br(),
    
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
    
    ## Check multicollinearity between variables
    h4("Multicollinearity Between Variables:"),
    
    if (nrow(high_cor) == 0){
      h5("None of the selected matching variables or covariates appear to be strongly correlated (Pearson correaltion > -0.9 or < 0.9).", style = "color:green")
    } else{
    
    h5("It looks like some of the  matching variables and/or covariates you have selected are highly correlated (Pearson correaltion > -0.9 or < 0.9). 
       Please consider removing one of each highly correlated variable pair:", paste0(high_cor[,1], " and ", high_cor[,2], collapse = ", "), style = 'color:red')
    },
    br(),
    br()
  )
}




 
