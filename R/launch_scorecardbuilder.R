#' Launches the scorecardbuilder application
#'
#' @export scorecardbuilder
#'
#' @return Shiny application object
#'
#' @import shiny shinythemes rhandsontable data.table ggplot2
#' @importFrom stats binomial glm predict quantile reformulate sd var
#'     weighted.mean
#' @importFrom scales percent number_format
#' @importFrom car vif
#' @importFrom shinyhelper helper observe_helpers
#' @importFrom Hmisc wtd.quantile
scorecardbuilder <- function() {
  options(shiny.maxRequestSize = Inf)
  options(shiny.minified = TRUE)
  shinyApp(ui = ui, server = server)
}