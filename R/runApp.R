
#' Run DigiCAT locally
#' 
#' @param enableLocal Enable the upload of local data `TRUE` or `FALSE`. Default is `TRUE`.
#' @export
#' @examples
#' run_DigiCAT(enableLocal = TRUE)

run_DigiCAT <- function(enableLocal = TRUE) {
  ## Check if DigiCAT has been installed
  appDir <- system.file("DigiCAT", package = "DigiCAT")
  if (appDir == "") {
    stop("Could not find DigiCAT. Try re-installing DigiCAT.", call. = FALSE)
  }
  ## Save local data upload setting to global.R
  if(enableLocal){
    cat("enable_local_data <- TRUE", file=paste0(appDir,"/global.R"))
  } else {
    cat("enable_local_data <- FALSE", file=paste0(appDir,"/global.R"))
  }
  ## Launch app app
  shiny::runApp(appDir, display.mode = "normal")
}
