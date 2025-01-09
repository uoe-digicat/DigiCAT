
#' Run DigiCAT locally
#' 
#' @param enableLocal Enable the upload of local data `TRUE` or `FALSE`. Default is `TRUE`.
#' @param docker_version Dockerised version of tool `TRUE` or `FALSE`. Default is `FALSE`.
#' @export
#' @examples
#' run_DigiCAT(enableLocal = TRUE)

run_DigiCAT <- function(enableLocal = TRUE, docker_version = FALSE, filePath = fs::path_home()) {
  ## Check if DigiCAT has been installed
  appDir <- system.file("DigiCAT", package = "DigiCAT")
  if (appDir == "") {
    stop("Could not find DigiCAT. Try re-installing DigiCAT.", call. = FALSE)
  }
  ## Write global.R based on inputed settings
  cat(paste0("enable_local_data <-", enableLocal, "\ndocker_version_global <-", docker_version), file=paste0(appDir,"/global.R"))
  
  ## Save file path to global environment
  file_path_global <<- filePath
    
  ## Launch app app
  shiny::runApp(appDir, display.mode = "normal")
}
