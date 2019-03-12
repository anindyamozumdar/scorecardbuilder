#' launches the shinyAppDemo app
#'
#' @export scorecardbuilder
#'
#' @return shiny application object
#'
#' @example \dontrun {launchApp()}
#'
#' @import shiny shinythemes rhandsontable readr knitr summarytools ggplot2
#'     dplyr smbinning tidyr car


# wrapper for shiny::shinyApp()
scorecardbuilder <- function() {
  shinyApp(ui = ui, server = server)
}