## Functions that resets data upload page
#' @param reset_errors Do you want to reset error messages? Deafult is FALSE
#' @param hide_descriptives Do you want to hide the descriptives tab? Deafult is TRUE


reset_upload_page <- function(reset_errors = FALSE, hide_descriptives = FALSE){
  
  if (reset_errors){
    ## Remove error message if any present from previous upload
    feedbackDanger("file1", show = FALSE)
    feedbackDanger("outcome", show = FALSE)
    feedbackDanger("treatment", show = FALSE)
    feedbackDanger("matchvars", show = FALSE)
    feedbackDanger("covars", show = FALSE)
      }
  
  if (hide_descriptives){
    ## Hide descriptives tab
    hideTab(inputId = "Tab_data", target = "descriptives")
  }


} 
