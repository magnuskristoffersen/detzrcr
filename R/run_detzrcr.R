#' Launch shiny interface
#'
#'@param ... Pass arguments on to shiny::runApp
#' @export
#'
run_detzrcr <- function(...) {
  app_dir <- system.file('shiny-apps', 'detzrcr_app', package = 'detzrcr')
  shiny::runApp(app_dir, display.mode = "normal", ...)
}
