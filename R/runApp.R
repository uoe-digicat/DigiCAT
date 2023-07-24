#' @export
run_DigiCAT <- function(enableLocal = FALSE) {
  appDir <- system.file("DigiCAT", package = "DigiCAT")
  if (appDir == "") {
    stop("Could not find DigiCAT. Try re-installing DigiCAT.", call. = FALSE)
  }
  
  if(enableLocal){
    cat("enable_local_data <- TRUE", file=paste0(appDir,"/global.R"))
  } else {
    cat("enable_local_data <- FALSE", file=paste0(appDir,"/global.R"))
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
