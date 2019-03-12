#' launches the shinyAppDemo app
#'
#' @export scorecardbuilder
#'
#' @return shiny application object
#'
#' @example \dontrun {launchApp()}
#'
#' @import shiny
#'


# wrapper for shiny::shinyApp()
scorecardbuilder <- function() {
  shinyApp(ui = ui, server = server)
}