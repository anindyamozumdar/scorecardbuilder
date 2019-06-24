# Data Module - Helpers ---------------------------------------------------

#' Return the data specification table given a data frame
#'
#' @param df a data frame
#' 
#' @return a data frame comprising the variable names in the input,
#'     variable type (as detected by the function 'class'),
#'     logical to identify a particular variable as the good/bad flag,
#'     logical to identify a particular variable to be used for stratified
#'     sampling, and a
#'     logical to identify a particular variable to be considered for binning
#' @noRd
#' @keywords internal
data_generate_specs <- function(df) {
  
  vnames <- gsub("\\.", "_", make.names(colnames(df), unique = TRUE))
  vclasses <- vapply(df, class, character(1))
  vtypes <- vapply(df, class, character(1))
  vtypes <- factor(vtypes, levels = c("numeric", "integer", "character",
                                      "Date"))
  vgoodbad <- rep(FALSE, times = length(vnames))
  vstratsample <- rep(FALSE, times = length(vnames))
  vbin <- rep(TRUE, times = length(vnames))
  data.frame(vnames = vnames, vtypes = vtypes, vgoodbad = vgoodbad,
             vstratsample = vstratsample, vbin = vbin,
             row.names = NULL, stringsAsFactors = FALSE)
  
}

# Data Module - UI --------------------------------------------------------

#' Interface to upload model data and modify data specifications
#'
#' @param id character used to specify namespace,
#'     see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @noRd
#' @keywords internal
data_ui <- function(id) {
  
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("modeldata"), "Upload Data", accept = "text/csv",
                  placeholder = "CSV file"),
        downloadLink(ns("downloadmodeldata"), "Download simulated data"),
        br(),
        br(),
        br(),
        selectInput(ns("gbflag"), "Choose good/bad flag (bad = 1)",
                    choices = NULL),
        selectInput(ns("ttflag"),
                    "Choose training/testing flag (training = 1)",
                    choices = NULL),
        radioButtons(ns("sampwt"), "Sample weights present?",
                     choices = c("Y", "N"), inline = TRUE),
        selectInput(ns("sampwtvar"), "Choose sample weight variable",
                    choices = NULL)
      ),
      mainPanel(
        rHandsontableOutput(ns("modeldataspecs"))
      )
    )
  )
  
}

# Data Module - Server ----------------------------------------------------

#' Data upload and specification module server-side processing
#'
#' @param input, output, session standard \code{shiny} boilerplate
#'
#' @return list with following components
#' \describe{
#'   \item{modeldata}{reactive data frame of data uploaded by user}
#'   \item{modeldataspecs}{reactive data frame of data specifications}
#' }
#' @noRd
#' @keywords internal
data_server <- function(input, output, session) {
  
  # Download simulated data.
  output$downloadmodeldata <- downloadHandler(
    filename = "smbsimdf1.csv",
    content = function(con) {
      write_csv(smbinning::smbsimdf1, con)
    }
  )
  
  # Ensure user upload.
  modeldatafile <- reactive({
    validate(need(input$modeldata, message = FALSE))
    input$modeldata
  })
  
  # Read the user upload into a reactive data frame.
  modeldata <- reactive({
    withProgress({
      md <- fread(modeldatafile()$datapath)
      setProgress(message = "Completed", value = 1)
      Sys.sleep(1)
    }, message = "Reading data...", value = 0.5)
    md
  })
  
  # If the user uploads new data, then regenerate the specifications.
  # Otherwise store any edits made via the user interface.
  modeldataspecs <- reactive({
    imds <- input$modeldataspecs
    specs <- data_generate_specs(modeldata())
    if (length(imds) > 0) {
      ospecs <- hot_to_r(imds)
      ovnames <- ospecs[["vnames"]]
      vnames <- specs[["vnames"]]
      if (!all(vapply(ovnames, function(x) {
        x %in% vnames
      }, logical(1)))) {
        mds <- specs
      } else {
        mds <- ospecs
      }
    } else {
      mds <- specs
    }
    mds
  })
  
  # Render the data specifications table. Ensure that any edits made by the
  # user to the logical fields are highlighted with a different colour.
  output$modeldataspecs <- renderRHandsontable({
    
    bool_renderer <- "
    function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
    var col_value = instance.getData()[row][col]
    if (col_value) {
    td.style.background = 'lightgreen';
    } else {
    td.style.background = 'initial';
    }
    }
    "
    
    reverse_bool_renderer <- "
    function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
    var col_value = instance.getData()[row][col]
    if (!col_value) {
    td.style.background = 'orange';
    } else {
    td.style.background = 'initial';
    }
    }
    "
    
    rhandsontable(modeldataspecs(),
                  height = 1000,
                  contextMenu = FALSE,
                  stretchH = "all",
                  rowHeaders = NULL,
                  colHeaders = c("Variable", "Type", "Good/Bad Flag",
                                 "Stratification", "Bin")) %>%
      hot_cols(columnSorting = FALSE) %>%
      hot_col("Variable", readOnly = TRUE) %>%
      hot_col("Good/Bad Flag", renderer = bool_renderer) %>%
      hot_col("Stratification", renderer = bool_renderer) %>%
      hot_col("Bin", renderer = reverse_bool_renderer)
  })
  
  # Return the reactive data and data specifications.
  list(
    modeldata = modeldata,
    modeldataspecs = modeldataspecs
  )
  
}