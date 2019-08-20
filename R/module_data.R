# Data Module - Helpers ---------------------------------------------------

#' Return the data specification table given a data frame
#'
#' @param df a data frame
#' 
#' @return a data frame comprising the variable names in the input,
#'     variable type (as detected by the function 'class'), and a
#'     logical to identify whether a particular variable is to be
#'     considered for binning
#'     
#' @details
#' The variable names will be converted to valid names using the
#' \code{make.names} function and all instances of . converted to _
#' @noRd
#' @keywords internal
data_generate_specs <- function(df) {
  
  vnames <- gsub("\\.", "_", make.names(colnames(df), unique = TRUE))
  vtypes <- vapply(df, class, character(1))
  vtypes <- factor(vtypes, levels = c("numeric", "integer", "character",
                                      "Date"))
  vbin <- rep(TRUE, times = length(vnames))
  data.frame(vnames = vnames, vtypes = vtypes, vbin = vbin,
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
        helper(
          selectInput(ns("gbflag"), "Choose good/bad flag (bad = 1)",
                      choices = NULL),
          type = "inline",
          title = "Help",
          content = "help"
        ),
        helper(
          selectInput(ns("ttflag"),
                      "Choose train/test flag (train = 1)",
                      choices = NULL),
          type = "inline",
          title = "Help",
          content = "help"
        ),
        helper(
          radioButtons(ns("sampwt"), "Sample weights?",
                       choices = c("N", "Y"), inline = TRUE),
          type = "inline",
          title = "Help",
          content = "help"
        ),
        selectInput(ns("sampwtvar"), "Choose sample weight variable",
                    choices = NULL)
      ),
      mainPanel(
        div(rHandsontableOutput(ns("modeldataspecs")), style = "width:80%;")
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
#'       modeldata = modeldata,
#'   \item{gbflag}{name of the good/bad flag variable}
#'   \item{ttflag}{name of the train/test flag variable}
#'   \item{sampwt}{Y/N to indicate if there is a sample weight}
#'   \item{sampwtvar}{name of the variable holding the sample weight}
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
    req(modeldatafile())
    withProgress({
      md <- fread(modeldatafile()$datapath)
      setProgress(message = "Completed", value = 1)
      Sys.sleep(1)
    }, message = "Reading data...", value = 0.5)
    md
  })
  
  # Generate the specs whenever modeldata changes.
  generatespecs <- reactive({
    req(modeldata())
    data_generate_specs(modeldata())
  })
  
  # If the user uploads new data, then regenerate the specifications.
  # Otherwise store any edits made via the user interface.
  modeldataspecs <- reactive({
    imds <- input$modeldataspecs
    specs <- generatespecs()
    if (length(imds) > 0) {
      ospecs <- hot_to_r(imds)
      ovnames <- ospecs[["vnames"]]
      vnames <- specs[["vnames"]]
      if (!all(vapply(ovnames, function(x) x %in% vnames, logical(1)))) {
        mds <- specs
      } else {
        mds <- ospecs
      }
    } else {
      mds <- specs
    }
    mds
  })
  
  # Observe any specs change to update the select inputs.
  observeEvent(generatespecs(), {
    specs <- generatespecs()
    updateSelectInput(session, "gbflag", choices = specs[["vnames"]])
    updateSelectInput(session, "ttflag", choices = specs[["vnames"]])
    updateSelectInput(session, "sampwtvar", choices = specs[["vnames"]])
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
                  colHeaders = c("Variable", "Type", "Bin")) %>%
      hot_cols(columnSorting = FALSE) %>%
      hot_col("Variable", readOnly = TRUE) %>%
      hot_col("Bin", renderer = reverse_bool_renderer)
  })
  
  # Good/bad flag name.
  gbflag <- reactive({
    req(input$gbflag)
    input$gbflag
  })
  
  # Train/test flag name.
  ttflag <- reactive({
    req(input$ttflag)
    input$ttflag
  })
  
  # Sample weight yes/no.
  sampwt <- reactive({
    req(input$sampwt)
    input$sampwt
  })
  
  # Sample weight variables if yes, NULL if no.
  sampwtvar <- reactive({
    req(input$sampwtvar)
    sampwt <- sampwt()
    if (sampwt == "Y") {
      return(input$sampwtvar)
    } else {
      return(NULL)
    }
  })
  
  # Observe the good/bad flag and train/test flag to ensure it is 1/0.
  observeEvent(c(gbflag(), ttflag()), {
    gb <- gbflag()
    tt <- ttflag()
    md <- modeldata()
    agb <- any(!(md[[gb]] %in% c(0, 1)))
    att <- any(!(md[[tt]] %in% c(0, 1)))
    if (agb || att) {
      showNotification("Good/Bad and Train/Test flag must contain 0/1 values",
                       duration = NULL, type = "error")
    }
  })
  
  # Observe the sample weight variable to ensure it is numeric.
  observeEvent(sampwtvar(), {
    sw <- sampwt()
    swv <- sampwtvar()
    ds <- generatespecs()
    vtype <- ds[ds[["vnames"]] == swv, "vtypes"]
    if (sw == "Y") {
      if (!(vtype %in% c('numeric', 'integer'))) {
        showNotification("Sample weight must be a numeric variable",
                         duration = NULL, type = "error")
      }
    }
  })
  
  # Return the reactive data and data specifications.
  list(
    modeldata = modeldata,
    modeldataspecs = modeldataspecs,
    gbflag = gbflag,
    ttflag = ttflag,
    sampwt = sampwt,
    sampwtvar = sampwtvar
  )
  
}