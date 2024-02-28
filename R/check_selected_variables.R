#' Flag variable input errors function
#' @import shinyFeedback
#' 
check_selected_variables <- function(outcome, treatment, matchvars, covars, i18n){
  
  
  ## Create variables to track issues with input
  required_input_missing <- FALSE
  required_input_missmatched <- FALSE
  
  ## Check if all required input has been specified, if not give error
  if (is.null(outcome) | is.null(treatment) | is.null(matchvars)){
    
    required_input_missing <- TRUE
    if (is.null(outcome)){feedbackDanger("outcome", show = TRUE, i18n$t("Upload Warning no outcome"))}
    if (is.null(treatment)){feedbackDanger("treatment", show = TRUE, i18n$t("Upload Warning no treatment"))}
    if (is.null(matchvars)){feedbackDanger("matchvars", show = TRUE, i18n$t("Upload Warning no matching"))}
  }
  
  ## If all required input selected, check if there are input conflicts
  if (!required_input_missing){
    if (outcome == treatment | outcome %in% matchvars | treatment %in% matchvars | treatment %in% covars | outcome %in% covars){
      
      required_input_missmatched <- TRUE
      if (outcome == treatment){
        feedbackDanger("outcome", show = TRUE, i18n$t("Upload Warning same outcome treatment"))
        feedbackDanger("treatment", show = TRUE, i18n$t("Upload Warning same outcome treatment"))}
      if (outcome %in% matchvars){
        feedbackDanger("outcome", show = TRUE, i18n$t("Upload Warning same outcome matching"))
        feedbackDanger("matchvars", show = TRUE, i18n$t("Upload Warning same outcome matching"))}
      if (treatment %in% matchvars){
        feedbackDanger("treatment", show = TRUE, i18n$t("Upload Warning same treatment matching"))
        feedbackDanger("matchvars", show = TRUE, i18n$t("Upload Warning same treatment matching"))}
      if (outcome %in% covars){
        feedbackDanger("outcome", show = TRUE, i18n$t("Upload Warning same outcome covariate"))
        feedbackDanger("covars", show = TRUE, i18n$t("Upload Warning same outcome covariate"))}
      if (treatment %in% covars){
        feedbackDanger("treatment", show = TRUE, i18n$t("Upload Warning same treatment covariate"))
        feedbackDanger("covars", show = TRUE, "Upload Warning same treatment covariate")}
    }
  }
  
  ## Return information on missing and mismatched variables to decide whether or not to continue onto next page 
  return(list("required_input_missmatched" = required_input_missmatched, "required_input_missing" = required_input_missing))
}

