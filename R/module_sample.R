# Sample Module - Helpers -------------------------------------------------

#' Sample a dataset 
#'
#' @param ds A dataset to be divided into train and test samples
#' @param stratifiers A character vector indicating which variables should be
#'     to stratify the samples
#' @param train_perc Percentage of records which should be in the training
#'     dataset
#'
#' @return A list of two components, the first component is the training
#'     sample and the other the test sample
sample_data <- function(ds, stratifiers = NULL, train_perc = 0.5) {
  
  ds$TEMP_ROW_NUMBER <- seq.int(nrow(ds))
  ds_train <- ds %>%
    group_by_at(stratifiers) %>%
    sample_frac(size = train_perc) %>%
    as.data.frame()
  train_rows <- ds_train$TEMP_ROW_NUMBER
  ds_test <- ds[-train_rows, ]
  ds_train$TEMP_ROW_NUMBER <- NULL
  ds_test$TEMP_ROW_NUMBER <- NULL
  list(train = ds_train, test = ds_test)
  
}

# Sample Module - UI ------------------------------------------------------

#' Interface to sample the data based on data specifications
#'
#' @param id character used to specify namespace,
#'     see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
sample_ui <- function(id) {
  
  ns <- NS(id)
  tagList(
    column(
      9,
      sliderInput(ns("sampledata"),
                  label = "Choose % of records in training sample",
                  min = 20, max = 80, value = 50, step = 5),
      actionButton(ns("sampledo"), "Sample", icon = icon("random")),
      textOutput(ns("samplestratvars")),
      br(),
      uiOutput(ns("sampleoutput"))
    ),
    column(3, includeMarkdown("qh_sample.md"))
  )
  
}

# Sample Module - Server --------------------------------------------------

#' Sample data module server-side processing
#'
#' @param input, output, session standard \code{shiny} boilerplate
#' @param modeldata data frame containing the credit scoring data
#' @param modeldataspecs data specification table for credit scoring data
#'
#' @return list with following components
#' \describe{
#'   \item{goodbad_var}{good/bad variable}
#'   \item{stratification_vars}{stratification variables}
#'   \item{modeltraintest}{list of training and test data}
#' }
sample_server <- function(input, output, session, modeldata, modeldataspecs) {
  
  # Return the variable which has been chosen as the good/bad flag.
  goodbad_var <- reactive({
    mds <- modeldataspecs()
    gbvar <- mds$vnames[mds$vgoodbad]
    req(gbvar)
    gbvar[[1]]
  })
  
  # Return the list of variables for stratification as a character vector.
  stratification_vars <- reactive({
    gbvar <- req(goodbad_var())
    mds <- modeldataspecs()
    stratvars <- mds$vnames[mds$vstratsample]
    stratvars <- unique(c(gbvar, stratvars))
    stratvars
  })
  
  # Render the list of stratification variables.
  output$samplestratvars <- renderText({
    stratvars <- req(stratification_vars())
    stratvars <- paste(stratvars, collapse = ", ")
    stratvars <- paste0("(", "Stratification variables: ", stratvars, ")")
    stratvars
  })
  
  # Return the train/test data.
  modeltraintest <- eventReactive(c(input$sampledo, modeldata()), {
    req(stratification_vars())
    withProgress({
      mtt <- sample_data(modeldata(), stratification_vars(),
                         input$sampledata / 100)
      setProgress(value = 1, message = "Sampling complete.")
      Sys.sleep(1)
    }, value = 0.5, message = "Sampling data...")

    mtt
  })
  
  # Render the sampling output.
  output$sampleoutput <- renderUI({
    mtt <- modeltraintest()
    tagList(
      helpText(paste0("Sampling done. # rows train = ", nrow(mtt$train),
                      ". # rows test = ", nrow(mtt$test), ".")),
      selectInput(session$ns("sampledistvar"),
                  label = "Choose variable to see distribution",
                  choices = stratification_vars(),
                  selected = goodbad_var()),
      column(10, div(helpText("Training Data"), style = "font-weight:bold;")),
      br(),
      column(10, plotOutput(session$ns("sampledisttrain"))),
      br(),
      column(10, div(helpText("Test Data"), style = "font-weight:bold;")),
      br(),
      column(10, plotOutput(session$ns("sampledisttest")))
    )
  })
  output$sampledisttrain <- renderPlot({
    explore_distribution_categorical(
      modeltraintest()$train[[input$sampledistvar]],
      vname = input$sampledistvar
    )$distplot
  })
  output$sampledisttest <- renderPlot({
    explore_distribution_categorical(
      modeltraintest()$test[[input$sampledistvar]],
      vname = input$sampledistvar
    )$distplot
  })
  
  # Return the name of the good/bad flag, stratification variables and the
  # train/test datasets.
  list(
    goodbad_var = goodbad_var,
    stratification_vars = stratification_vars,
    modeltraintest = modeltraintest
  )
  
}