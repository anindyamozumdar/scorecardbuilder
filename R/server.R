# Application - Server ----------------------------------------------------

server <- shinyServer(function(input, output, session) {

  # Session Globals ---------------------------------------------------------
  current_tab <- "data"
  
  # Data --------------------------------------------------------------------
  md <- callModule(data_server, "data_module")
  
  # Explore ----------------------------------------------------------------
  ex <- callModule(explore_server, "explore_module", md$modeldata,
                   md$modeldataspecs)

  # Sample ------------------------------------------------------------------
  smpl <- callModule(sample_server, "sample_module", md$modeldata,
                     md$modeldataspecs)

  # Binning -----------------------------------------------------------------
  bin <- callModule(binning_server, "binning_module", smpl$modeltraintest,
                    md$modeldataspecs, smpl$goodbad_var)

  # Model -------------------------------------------------------------------
  model <- callModule(model_server, "model_module", smpl$modeltraintest,
                      md$modeldataspecs, bin$binning, smpl$goodbad_var)

  # Validation --------------------------------------------------------------
  validation <- callModule(validation_server, "validation_module",
                           model$scorecard, smpl$goodbad_var)

  # Download ----------------------------------------------------------------
  dw <- callModule(download_server, "download_module", bin$binning,
                   model$scorecard, validation$validation)

  # Others ------------------------------------------------------------------
  
  # Observe whenever tabs are changed
  observeEvent(input$nav_top, {
    nav_top <- input$nav_top
    
    # Stop the application if the Stop icon is clicked
    if (nav_top == "stop") {
      stopApp()
      return(NULL)
    }
    
    # Display help page if the Help icon is clicked and return to existing tab
    if (nav_top == "help") {
      updateTabsetPanel(session, "nav_top", selected = current_tab)
      showModal(
        modalDialog(
          includeMarkdown(paste0("h_", current_tab, ".md")),
          title = "Help",
          size = "l",
          easyClose = FALSE,
          fade = TRUE
        )
      )
    } else {
      current_tab <<- nav_top
    }
  })
  
})