#' Launches the scorecardbuilder application
#'
#' @export scorecardbuilder
#'
#' @return Shiny application object
#'
#' @import shiny shinythemes rhandsontable data.table ggplot2 smbinning
#' @importFrom stats binomial glm predict quantile reformulate sd var
#'     weighted.mean
#' @importFrom scales percent number_format
#' @importFrom car vif
scorecardbuilder <- function() {
  options(shiny.maxRequestSize = Inf)
  options(shiny.minified = TRUE)
  shinyApp(ui = ui, server = server)
}