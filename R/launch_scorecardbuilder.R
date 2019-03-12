#' Launches the scorecardbuilder application
#'
#' @export scorecardbuilder
#'
#' @return Shiny application object
#'
#' @import shiny shinythemes rhandsontable readr knitr summarytools ggplot2
#'     dplyr smbinning tidyr
#' @importFrom stats binomial glm predict quantile reformulate sd var
#'     weighted.mean
#' @importFrom scales percent number_format
#' @importFrom car vif
scorecardbuilder <- function() {
  shinyApp(ui = ui, server = server)
}