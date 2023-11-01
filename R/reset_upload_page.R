#' Functions that resets data upload page
#' @param reset_errors Do you want to reset error messages? Default is FALSE
#' @param hide_data Do you want to hide the data tab? Default is FALSE
#' @param hide_validation Do you want to hide the data tab? Default is FALSE

reset_upload_page <- function(reset_errors = FALSE, hide_data = FALSE, hide_validation = FALSE, parent){
  
  if (reset_errors){
    ## Remove variable input error message if any present from previous upload
    feedbackDanger("file1", show = FALSE)
    feedbackDanger("Btn_sampledata", show = FALSE)
    feedbackDanger("outcome", show = FALSE)
    feedbackDanger("treatment", show = FALSE)
    feedbackDanger("matchvars", show = FALSE)
    feedbackDanger("covars", show = FALSE)
  }
  
  if (hide_data){
    ## Hide descriptive tab
    hideTab(session = parent, inputId = "data_upload-data_panel", target = "data_upload-raw_data")
    
  }
  
  if (hide_validation){
    ## Hide validation tab
    hideTab(session = parent, inputId = "data_upload-data_panel", target = "data_upload-data_validation")
  }
} 
