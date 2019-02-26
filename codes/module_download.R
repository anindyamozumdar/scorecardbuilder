# Download Module - Helpers -----------------------------------------------

# Download Module - UI ----------------------------------------------------

#' Interface to download relevant datasets
#'
#' @param id character used to specify namespace,
#'     see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
download_ui <- function(id) {
  
  ns <- NS(id)
  tagList(
    uiOutput(ns("downloadbinning_ui")),
    br(),
    br(),
    uiOutput(ns("downloaddata_ui")),
    br(),
    br(),
    uiOutput(ns("downloadscorecard_ui")),
    br(),
    br(),
    uiOutput(ns("downloadvalidation_ui"))
  )
  
}

# Download Module - Server ------------------------------------------------

#' Download module server-side processing
#'
#' @param input, output, session standard \code{shiny} boilerplate
#' @param binning binning object created in binning module
#' @param scorecard scorecard object created in model module
#' @param validation validation object created in validation module
download_server <- function(input, output, session, binning, scorecard,
                            validation) {
  
  # Render the download module window.
  output$downloadbinning_ui <- renderUI({
    req(binning())
    validate(need(names(binning()), message = FALSE))
    downloadButton(session$ns("downloadbinning"), "Download Binning")
  })
  output$downloaddata_ui <- renderUI({
    req(scorecard())
    downloadButton(session$ns("downloaddata"), "Download Scored Data")
  })
  output$downloadscorecard_ui <- renderUI({
    req(scorecard())
    downloadButton(session$ns("downloadscorecard"), "Download Scorecard")
  })
  output$downloadvalidation_ui <- renderUI({
    req(validation())
    downloadButton(session$ns("downloadvalidation"), "Download Validation")
  })
  
  # Binning.
  output$downloadbinning <- downloadHandler(
    filename = paste0("binning-", Sys.Date(), ".csv"),
    content = function(con) {
      binvars <- names(binning())
      bindata <- NULL
      for (binvar in binvars) {
        iv <- binning()[[binvar]]$bin_train$ivtable
        v <- rep(binvar, nrow(iv))
        bindata <- bind_rows(bindata, bind_cols(data.frame(variable = v), iv))
      }
      write_csv(bindata, con)
    }
  )
  
  # Scored data.
  output$downloaddata <- downloadHandler(
    filename = paste0("scored_data-", Sys.Date(), ".csv"),
    content = function(con) {
      scr <- scorecard()
      scrdata <- bind_rows(scr$traindata, scr$testdata)
      write_csv(scrdata, con)
    }
  )
  
  # Scorecard.
  output$downloadscorecard <- downloadHandler(
    filename = paste0("scoredcard-", Sys.Date(), ".csv"),
    content = function(con) {
      scr <- scorecard()
      scrdata <- bind_rows(scr$scorecard_table)
      write_csv(scrdata, con)
    }
  )
  
  # Validation.
  output$downloadvalidation <- downloadHandler(
    filename = paste0("validation-", Sys.Date(), ".csv"),
    content = function(con) {
      vl <- validation()
      train <- vl$train_summ
      train[["ds"]] <- "train"
      test <- vl$test_summ
      test[["ds"]] <- "test"
      write_csv(bind_rows(train, test), con)
    }
  )
  
}