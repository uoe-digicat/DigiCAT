#' @export
run_DigiCAT <- function() {
  appDir <- system.file("DigiCAT", package = "DigiCAT")
  if (appDir == "") {
    stop("Could not find DigiCAT. Try re-installing DigiCAT.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
