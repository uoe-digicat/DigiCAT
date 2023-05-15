## Function to flag errors in variable input

#' @param outcome Specify input. 
#' @param treatment Specify treatment 
#' @param matchvars Specify matchvars 
#' @param covars Specify covars 


check_selected_variables <- function(outcome, treatment, matchvars, covars){
  
  
  ## Create variables to track issues with input
  required_input_missing <- FALSE
  required_input_missmatched <- FALSE
  
  ## Check if all required input has been specified, if not give error
  if (is.null(outcome) | is.null(treatment) | is.null(matchvars)){
    
    required_input_missing <- TRUE
    if (is.null(outcome)){feedbackDanger("outcome", show = TRUE, "Please select outcome before proceeding")}
    if (is.null(treatment)){feedbackDanger("treatment", show = TRUE, "Please select treatment before proceeding")}
    if (is.null(matchvars)){feedbackDanger("matchvars", show = TRUE, "Please select matching variables before proceeding")}
  }
  
  ## If all required input selected, check if there are input conflicts
  if (!required_input_missing){
    if (outcome == treatment | outcome %in% matchvars | treatment %in% matchvars | treatment %in% covars | outcome %in% covars){
      
      required_input_missmatched <- TRUE
      if (outcome == treatment){
        feedbackDanger("outcome", show = TRUE, "Outcome and treatment cannot be the same")
        feedbackDanger("treatment", show = TRUE, "Outcome and treatment cannot be the same")}
      if (outcome %in% matchvars){
        feedbackDanger("outcome", show = TRUE, "Outcome and matching variables cannot be the same")
        feedbackDanger("matchvars", show = TRUE, "Outcome and matching variables cannot be the same")}
      if (treatment %in% matchvars){
        feedbackDanger("treatment", show = TRUE, "Treatment and matching variables cannot be the same")
        feedbackDanger("matchvars", show = TRUE, "Treatment and matching variables cannot be the same")}
      if (outcome %in% covars){
        feedbackDanger("outcome", show = TRUE, "Outcome and covariates cannot be the same")
        feedbackDanger("covars", show = TRUE, "Outcome and covariates cannot be the same")}
      if (treatment %in% covars){
        feedbackDanger("treatment", show = TRUE, "Treatment and covariates cannot be the same")
        feedbackDanger("covars", show = TRUE, "Treatment and covariates cannot be the same")}
    }
  }
  
  ## Return information on missing and mismatched variables to decide whether or not to continue onto next page 
  return(list("required_input_missmatched" = required_input_missmatched, "required_input_missing" = required_input_missing))
}

