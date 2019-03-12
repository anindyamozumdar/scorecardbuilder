# Binning Module - Helpers ------------------------------------------------

#' Bin numeric variable
#'
#' @param x Character string indicating which variable is being binned 
#' @param y Binary response variable 
#' @param dftrain Data frame containing the training data
#' @param dftest Data frame containing the test data
#' @param cuts Vector of pre-determined cut points for binning
#' @param type Whether to bin using conditional inference trees or use quantiles
#'
#' @return A list of two components, the first component is the binned variable
#'     in the training data and the second component is the binning applied on
#'     the test data
binning_numeric <- function(x, y, dftrain, dftest, cuts = NULL,
                            type = "CIT") {
  if (is.null(cuts) && type == "CIT") {
    bin_train <- smbinning(dftrain, y, x)
  } else if (is.null(cuts) && type != "CIT") {
    cuts <- unique(
      c(min(dftrain[[x]], na.rm = TRUE),
        quantiles = quantile(
          dftrain[[x]], probs = seq(0.05, 1, by = 0.05),
          na.rm = TRUE
        )
      )
    )
    bin_train <- smbinning.custom(dftrain, y, x, cuts)
  } else {
    bin_train <- smbinning.custom(dftrain, y, x, cuts)
  }
  if (class(bin_train) != "character") {
    bin_test <- smbinning.custom(dftest, y, x, bin_train$cuts)
  } else {
    bin_test <- bin_train
  }
  list(bin_train = bin_train, bin_test = bin_test)
}

#' Bin categorical variable
#'
#' @param x Character string indicating which variable is being binned 
#' @param y Binary response variable 
#' @param dftrain Data frame containing the training data
#' @param dftest Data frame containing the test data
#' @param groups Pre-determined vector for binning
#'
#' @return A list of two components, the first component is the binned variable
#'     in the training data and the second component is the binning applied on
#'     the test data
binning_categorical <- function(x, y, dftrain, dftest, groups = NULL) {
  dftrain[[x]] <- factor(dftrain[[x]])
  dftest[[x]] <- factor(dftest[[x]])
  if (is.null(groups)) {
    bin_train <- smbinning.factor.custom(dftrain, y, x,
                                         groups = quotify(levels(dftrain[[x]])))
  } else {
    bin_train <- smbinning.factor.custom(dftrain, y, x, groups = groups)
  }
  if (class(bin_train) != "character") {
    if (is.null(groups)) {
      bin_test <- smbinning.factor.custom(
        dftest, y, x, groups = quotify(levels(dftrain[[x]]))
      )
    } else {
      bin_test <- smbinning.factor.custom(dftest, y, x, groups = groups)
    }
  } else {
    bin_test <- bin_train
  }
  list(bin_train = bin_train, bin_test = bin_test)
}

# Binning Module - UI -----------------------------------------------------

#' Interface to bin the variables for scorecard development
#'
#' @param id character used to specify namespace,
#'     see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
binning_ui <- function(id) {
  
  ns <- NS(id)
  tagList(
    column(
      9,
      uiOutput(ns("binningwindow"))
    )
    # column(3, includeMarkdown("qh_binning.md"))
  )
  
}

# Binning Module - Server -------------------------------------------------

#' Binning module server-side processing
#'
#' @param input, output, session standard \code{shiny} boilerplate
#' @param modeltraintest data frame containing the credit scoring data split
#'     into training and testing data
#' @param modeldataspecs data specification table for credit scoring data
#' @param goodbad_var good/bad variable
#'
#' @return list with following components
#' \describe{
#'   \item{binning}{list containing binning of variables created by user}
#' }
binning_server <- function(input, output, session, modeltraintest,
                           modeldataspecs, goodbad_var) {
  
  # Reactive values to hold binnings.
  binning_g <- reactiveValues()
  
  # Clear binning on sample change.
  observeEvent(modeltraintest(), {
    for (name in names(binning_g)) {
      binning_g[[name]] <- NULL
    }
  }, priority = 10)
  
  # Return the list of variables for binning as a character vector.
  binning_vars <- reactive({
    gbvar <- req(goodbad_var())
    mds <- modeldataspecs()
    binvars <- mds$vnames[mds$vbin]
    binvars_nc <- mds$vnames[mds$vtypes == "numeric" | mds$vtypes == "integer"
                             | mds$vtypes == "character"]
    binvars <- unique(intersect(binvars, binvars_nc))
    binvars <- unique(setdiff(binvars, gbvar))
    binvars
  })
  
  # Render the binning variables.
  output$binningwindow <- renderUI({
    req(modeltraintest())
    sidebarLayout(
      sidebarPanel(
        selectInput(session$ns("binvar"), "Choose variable",
                    choices = binning_vars()),
        radioButtons(session$ns("bintype"),
                     label = "Choose method (numeric only)",
                     choices = c("CIT", "Quantile (20 Groups)"),
                     inline = TRUE),
        actionButton(session$ns("dobin"), "Bin"),
        br(),
        br(),
        div(helpText("Choose bins to combine"), style = "font-style:italic;"),
        selectInput(session$ns("combine1"), "Bin", choices = NULL),
        selectInput(session$ns("combine2"), "Bin", choices = NULL),
        actionButton(session$ns("docombine"), "Combine"),
        br(),
        br(),
        div(helpText("Split bin at"), style = "font-style:italic;"),
        selectInput(session$ns("split1"), "Bin", choices = NULL),
        numericInput(session$ns("splitval"), "Value", value = 0),
        actionButton(session$ns("dosplit"), "Split")
      ),
      mainPanel(
        plotOutput(session$ns("woetraintest")),
        br(),
        div(
          helpText("Weight of Evidence and Information Value - Training"),
          style = "font-weight:bold;"
        ),
        rHandsontableOutput(session$ns("ivtrain")),
        br(),
        div(
          helpText("Weight of Evidence and Information Value - Test"),
          style = "font-weight:bold;"
        ),        
        rHandsontableOutput(session$ns("ivtest"))
      )
    )
  })
  
  # Binning object.
  binning <- reactive({
    reactiveValuesToList(binning_g)
  })
  
  # Handle action button dobin.
  observeEvent(input$dobin, {
    binvar <- req(input$binvar)
    mtt <- modeltraintest()
    mds <- modeldataspecs()
    binvar_type <- mds$vtypes[mds$vnames == binvar]
    
    updateSelectInput(session, "combine1", choices = character(0))
    updateSelectInput(session, "combine2", choices = character(0))
    updateSelectInput(session, "split1", choices = character(0))
    updateNumericInput(session, "splitval", value = 0)
    
    if (binvar_type == "numeric" || binvar_type == "integer") {
      withProgress({
        if (input$bintype == "CIT") {
          b <- binning_numeric(binvar, goodbad_var(), mtt$train, mtt$test)
        } else {
          b <- binning_numeric(binvar, goodbad_var(), mtt$train, mtt$test,
                               type = input$bintype)
        }
        binning_g[[binvar]] <- b
        setProgress(value = 1, message = "Auto-binning complete.")
        Sys.sleep(1)
      }, value = 0.5, message = paste0("Auto-binning ", binvar,
                                       ". Please wait."))
    } else if (binvar_type == "character") {
      withProgress({
        b <- binning_categorical(binvar, goodbad_var(), mtt$train, mtt$test)
        binning_g[[binvar]] <- b
        setProgress(value = 1, message = "Auto-binning complete.")
        Sys.sleep(1)
      }, value = 0.5, message = paste0("Auto-binning ", binvar,
                                       ". Please wait."))
    } else {
      showNotification(paste0("Error: Attempt to bin variable which is not ",
                              "numeric or character."), duration = NULL,
                       type = "error")
    }
    
    if (!notify_bin_errors(b)) {
      notify_bin_warnings(b)
      updateSelectInput(session, "combine1",
                        choices = as.character(
                          1:(nrow(b$bin_train$ivtable) - 2))
                        )
      updateSelectInput(session, "combine2",
                        choices = as.character(
                          1:(nrow(b$bin_train$ivtable) - 2))
      )
      updateSelectInput(session, "split1",
                        choices = as.character(
                          1:(nrow(b$bin_train$ivtable) - 2))
      )
    }
  })
  
  # Handle variable selection change.
  observeEvent(input$binvar, {
    binvar <- req(input$binvar)
    bin <- req(binning()[[binvar]])
    validate(need(class(bin$bin_train) != "character",
                  message = FALSE))
    
    updateSelectInput(session, "combine1",
                      choices = as.character(
                        1:(nrow(bin$bin_train$ivtable) - 2))
    )
    updateSelectInput(session, "combine2",
                      choices = as.character(
                        1:(nrow(bin$bin_train$ivtable) - 2))
    )
    updateSelectInput(session, "split1",
                      choices = as.character(
                        1:(nrow(bin$bin_train$ivtable) - 2))
    )
  })
  
  # Handle action button docombine.
  observeEvent(input$docombine, {
    binvar <- req(input$binvar)
    bin <- req(binning()[[binvar]])
    validate(need(class(bin$bin_train) != "character",
                  message = "Variable not binned"))
    
    mtt <- modeltraintest()
    mds <- modeldataspecs()
    binvar_type <- mds$vtypes[mds$vnames == binvar]
    
    cmb1 <- as.numeric(input$combine1)
    cmb2 <- as.numeric(input$combine2)
    if (cmb1 == cmb2) {
      showNotification("Cannot combine the same bins.", type = "message")
      return(NULL)
    }
    
    if (binvar_type == "numeric" || binvar_type == "integer") {
      if (abs(cmb1 - cmb2) > 1) {
        showNotification(paste0("For numeric variables, only adjacent bins ",
                                "can be combined."), type = "message")
        return(NULL)
      }
      
      cts <- bin$bin_train$cuts
      if (max(cmb1, cmb2) > length(cts)) {
        cts <- cts[1:(length(cts) - 1)]
      } else {
        cts[min(cmb1, cmb2)] <- cts[max(cmb1, cmb2)]
      }
      cts <- unique(cts)
      
      withProgress({
        b <- binning_numeric(binvar, goodbad_var(), mtt$train, mtt$test,
                             cuts = cts)
        binning_g[[binvar]] <- b
        setProgress(value = 1, message = "Binning complete.")
        Sys.sleep(1)
      }, value = 0.5, message = paste0("Binning ", binvar,
                                       ". Please wait."))
    } else if (binvar_type == "character") {
      grps <- bin$bin_train$groups
      grps[cmb1] <- paste(grps[cmb1], ",", grps[cmb2])
      grps <- grps[grps != grps[cmb2]]
      if (length(grps) == 1) {
        showNotification(paste0("At least two distinct non-missing ",
                                "groups required."), type = "message")
        return(NULL)
      }
      withProgress({
        b <- binning_categorical(binvar, goodbad_var(), mtt$train, mtt$test,
                                 grps)
        binning_g[[binvar]] <- b
        setProgress(value = 1, message = "Binning complete.")
        Sys.sleep(1)
      }, value = 0.5, message = paste0("Binning ", binvar,
                                       ". Please wait."))
    } else {
      showNotification(paste0("Error: Attempt to bin variable which is not ",
                              "numeric or character."), duration = NULL,
                       type = "error")
    }
    
    if (!notify_bin_errors(b)) {
      notify_bin_warnings(b)
      updateSelectInput(session, "combine1",
                        choices = as.character(
                          1:(nrow(b$bin_train$ivtable) - 2))
      )
      updateSelectInput(session, "combine2",
                        choices = as.character(
                          1:(nrow(b$bin_train$ivtable) - 2))
      )
      updateSelectInput(session, "split1",
                        choices = as.character(
                          1:(nrow(b$bin_train$ivtable) - 2))
      )
    }
  })
  
  # Handle action button dosplit.
  observeEvent(input$dosplit, {
    binvar <- req(input$binvar)
    bin <- req(binning()[[binvar]])
    validate(need(class(bin$bin_train) != "character",
                  message = "Variable not binned"))
    
    mtt <- modeltraintest()
    mds <- modeldataspecs()
    binvar_type <- mds$vtypes[mds$vnames == binvar]
    
    spl1 <- as.numeric(input$split1)
    splv <- as.numeric(input$splitval)
    
    if (binvar_type == "numeric" || binvar_type == "integer") {
      cts <- bin$bin_train$cuts
      if (spl1 == length(cts) + 1) {
        if (splv <= cts[length(cts)]) {
          showNotification(paste0("Enter a value higher than the ",
                                  "maximum value."), type = "message")
          return(NULL)
        } else {
          cts <- c(cts, splv)
        }
      } else if (spl1 == 1) {
        if (splv >= cts[1]) {
          showNotification(paste0("Enter a value lower than the ",
                                  "minimum value."), type = "message")
          return(NULL)
        } else {
          cts <- c(splv, cts)
        }
      } else {
        if (!(splv < cts[spl1] & splv > cts[spl1 - 1])) {
          showNotification(paste0("Enter a value between the ",
                                  "bounds."), type = "message")
          return(NULL)
        } else {
          cts <- c(cts[1:(spl1 - 1)], splv, cts[spl1:length(cts)])
        }
      }
      cts <- unique(cts)

      withProgress({
        b <- binning_numeric(binvar, goodbad_var(), mtt$train, mtt$test,
                             cuts = cts)
        binning_g[[binvar]] <- b
        setProgress(value = 1, message = "Binning complete.")
        Sys.sleep(1)
      }, value = 0.5, message = paste0("Binning ", binvar,
                                       ". Please wait."))
    } else if (binvar_type == "character") {
      withProgress({
        grps <- bin$bin_train$groups
        splts <- trimws(strsplit(grps[spl1], ",", fixed = TRUE)[[1]])
        grps <- if (spl1 == 1) {
          c(splts, grps[(spl1+1):length(grps)])
        } else if (spl1 < length(grps)) {
          c(grps[1:(spl1-1)], splts, grps[(spl1+1):length(grps)])
        } else {
          c(grps[1:(spl1-1)], splts)
        }
        b <- binning_categorical(binvar, goodbad_var(), mtt$train, mtt$test,
                                 grps)
        binning_g[[binvar]] <- b
        setProgress(value = 1, message = "Binning complete.")
        Sys.sleep(1)
      }, value = 0.5, message = paste0("Binning ", binvar,
                                       ". Please wait."))
    } else {
      showNotification(paste0("Error: Attempt to bin variable which is not ",
                              "numeric or character."), duration = NULL,
                       type = "error")
    }
    
    if (!notify_bin_errors(b)) {
      notify_bin_warnings(b)
      updateSelectInput(session, "combine1",
                        choices = as.character(
                          1:(nrow(b$bin_train$ivtable) - 2))
      )
      updateSelectInput(session, "combine2",
                        choices = as.character(
                          1:(nrow(b$bin_train$ivtable) - 2))
      )
      updateSelectInput(session, "split1",
                        choices = as.character(
                          1:(nrow(b$bin_train$ivtable) - 2))
      )
    }
  })
  
  # WoE plot.
  output$woetraintest <- renderPlot({
    bin <- req(binning()[[input$binvar]])
    validate(need(class(bin$bin_train) != "character",
                  message = "Variable not binned"))
    gtrain <- select(bin$bin_train$ivtable, Cutpoint, WoE) %>%
      mutate(ds = "Training")
    gtest <- select(bin$bin_test$ivtable, Cutpoint, WoE) %>%
      mutate(ds = "Test")
    gds <- bind_rows(gtrain, gtest) %>%
      filter(Cutpoint != "Total")
    gds_breaks <- unique(c("Missing", pull(gds, 1)))
    ggplot(gds, aes(factor(Cutpoint), WoE, colour = ds, group = ds)) +
      theme_bw() +
      geom_point(size = 5) +
      geom_line(size = 1) +
      scale_x_discrete(limits = gds_breaks) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
      ggtitle(paste0("Weight of Evidence (WoE) by bin - ", input$binvar)) +
      xlab(NULL) +
      ylab(NULL) +
      theme(
        plot.title = element_text(size = 20, hjust = 0.5, vjust = 0.5),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 15)
      )
  })
  
  # Train/test iv tables.
  prepare_HOT <- function(ivtable) {
    ivtable %>%
      select(-CntCumRec, -CntCumGood, -CntCumBad, - GoodRate) %>%
      rhandsontable(
        contextMenu = FALSE,
        colHeaders = c(
          "Bin", "# Records", "# Good", "# Bad", "% Records",
          "Bad Rate", "Odds", "LN(Odds)", "WoE", "IV"
        ),
        stretchH = "all",
        readOnly = TRUE
      ) %>%
      hot_cols(columnSorting = FALSE) %>%
      hot_col("# Records", format = "0,0") %>%
      hot_col("# Good", format = "0,0") %>%
      hot_col("# Bad", format = "0,0") %>%
      hot_col("% Records", format = "0.00%") %>%
      hot_col("Bad Rate", format = "0.00%") %>%
      hot_col("Odds", format = "0.00") %>%
      hot_col("LN(Odds)", format = "0.00") %>%
      hot_col("WoE", format = "0.0000") %>%
      hot_col("IV", format = "0.00%")
  }
  output$ivtrain <- renderRHandsontable({
    bin <- req(binning()[[input$binvar]])
    validate(need(class(bin$bin_train) != "character",
                  message = FALSE))
    prepare_HOT(bin$bin_train$ivtable)
  })
  output$ivtest <- renderRHandsontable({
    bin <- req(binning()[[input$binvar]])
    validate(need(class(bin$bin_train) != "character",
                  message = FALSE))
    prepare_HOT(bin$bin_test$ivtable)
  })
  
  # Return the binning object.
  list(binning = binning)
  
}