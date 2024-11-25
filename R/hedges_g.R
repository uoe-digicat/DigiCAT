#' Function to calculate Hedge's G
#'
#' @param treatment_variable Name of treatment variable
#' @param outcome_variable Name of outcome variable
#' @param missing_method 
#' @param balanced_data Balanced data object
#' @param outcome_model Outcome model object 
#' 
#' @import WeightIt
#' @import MatchIt
#' @import MatchThem


hedges_g <- function(treatment_variable, missing_method, outcome_variable, balanced_data, outcome_model){
  
  if(missing_method == "complete"){
    
    ## Get mean difference
    mean_diff <- outcome_model$standardised_format$`Coefficient Estimate`
    
    ## Get SD of outcome in treatment group
    MD<-match.data(balanced_data)
    
    SD_t <- sd(MD[outcome_variable][MD[treatment_variable]==1])
    
    ## Get SD of outcome in control group
    SD_c <- sd(MD[outcome_variable][MD[treatment_variable]==0])
    
    ## N treat
    N_t <- length(MD[outcome_variable][MD[treatment_variable]==1]) #NB I don't think we want to use length to calculate N more generally ?
    
    ## N control
    N_c < -length(MD[outcome_variable][MD[treatment_variable]==0])
    
    ## Hedge's g calculation
    g <- mean_diff/(((SD_t^2)*(N_t-1)+(SD_c^2)*(N_c-1))/((N_t+N_c-2)))^0.5
  }
  
  if (missing_method == "mi") {
    
    ## Get imputed dataset
    imputed_data <- complete(balanced_data, "long", all = F) ## Those excluded during matching?
    
    ## Get SD of outcome in treatment group
    SD_t <- mean(with(subset(imputed_data, imputed_data[treatment_variable] == 1), tapply(as.numeric(get(outcome_variable)), .imp, sd)))
    
    ## Get SD of outcome in control group
    SD_c <- mean(with(subset(imputed_data, imputed_data[treatment_variable] == 1), tapply(as.numeric(get(outcome_variable)), .imp, sd)))
    
    ## N treat
    N_t <- nrow(subset(imputed_data, imputed_data[treatment_variable] == 1)) / balanced_data$object$m
    
    ## N control
    N_c <- nrow(subset(imputed_data, imputed_data[treatment_variable] == 0)) / balanced_data$object$m
    
    ## Pooled standard deviation
    pooled_SD <- sqrt(((SD_t^2) * (N_t - 1) + (SD_c^2) * (N_c - 1)) / (N_t + N_c - 2))
    
    ## Hedge's g calculation
    g <- mean_diff / pooled_SD

  }
  
  ## Return Hedge's g
  return(g)
  
}

## Additional code from Aja
# #douhle check hedge's g calculation
# num<-mean_diff
# denom1<-(SD_t^2)*(N_t-1)+(SD_c^2)*(N_c-1)
# denom2<-(N_t+N_c-2)
# denom<-(denom1/denom2)^0.5
# num/denom

