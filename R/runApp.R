#' @title Run paramGUI
#'
#' @description Runs the shiny paramGUI app.
#'
#' @keywords GUI, shiny
#' @export
#' @examples
#' \dontrun{
#' runGUI()
#' }
#'
#'
runGUI <- function() {
  appDir <- system.file("shinyApps", "paramGUI", package = "paramGUI")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `paramGUI`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}

#' Start paramGUI
#'
#' @description The same as runGUI(), starts the shiny paramGUI app.
#'
#' @export
startGUI <- function() {
  runGUI()
}