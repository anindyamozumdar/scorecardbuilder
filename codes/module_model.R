# Model Module - Helpers ------------------------------------------------

#' Create the WoEs in the data for a numeric variable
#'
#' @param df Data set where the WoE for the variable is to be created
#' @param bin The binning object created for this variable
#' @param var Name of the variable
#' @param newvar Name of the variable where the WoE is to be stored
#' 
#' @return Dataset with the WoE created in a new variable
model_woe_numeric <- function(df, bin, var, newvar) {
  ivtable <- bin$ivtable
  nr <- nrow(ivtable)
  df[[newvar]] <- NA_real_
  sql_str <- ""
  sql_str <- paste0(sql_str, "UPDATE df SET ", newvar, " =")
  sql_str <- paste0(sql_str, "(CASE ")
  for (i in 1:(nr - 2)) {
    sql_str <- paste0(sql_str, " WHEN ", var, " ", ivtable[["Cutpoint"]][i],
                      " THEN ", as.character(ivtable[["WoE"]][i]))
  }
  misswoe <- ivtable[["WoE"]][nr - 1]
  if (is.nan(misswoe) || is.infinite(misswoe) || is.na(misswoe)) {
    misswoe <- 0
  }
  sql_str <- paste0(sql_str, " WHEN ", var, " IS NULL",
                    " THEN ", as.character(misswoe))
  sql_str <- paste0(sql_str, " ELSE 0 END)")
  sql_str <- c(sql_str, paste0("SELECT * from df"))
  sqldf(sql_str)
}

#' Create the WoEs in the data for a categorical variable
#'
#' @param df Data set where the WoE for the variable is to be created
#' @param bin The binning object created for this variable
#' @param var Name of the variable
#' @param newvar Name of the variable where the WoE is to be stored
#' 
#' @return Dataset with the WoE created in a new variable
model_woe_categorical <- function(df, bin, var, newvar) {
  ivtable <- bin$ivtable
  nr <- nrow(ivtable)
  df[[newvar]] <- NA_real_
  sql_str <- ""
  sql_str <- paste0(sql_str, "UPDATE df SET ", newvar, " =")
  sql_str <- paste0(sql_str, "(CASE ")
  for (i in 1:(nr - 2)) {
    cat <- ivtable[["Cutpoint"]][i]
    cat <- paste0("('", gsub("/", "','", cat, fixed = TRUE), "')")
    sql_str <- paste0(sql_str, " WHEN ", var, " IN ", cat,
                      " THEN ", as.character(ivtable[["WoE"]][i]))
  }
  misswoe <- ivtable[["WoE"]][nr - 1]
  if (is.nan(misswoe) || is.infinite(misswoe) || is.na(misswoe)) {
    misswoe <- 0
  }
  sql_str <- paste0(sql_str, " WHEN ", var, " IS NULL",
                    " THEN ", as.character(misswoe))
  sql_str <- paste0(sql_str, " ELSE 0 END)")
  sql_str <- c(sql_str, paste0("SELECT * from df"))
  sqldf(sql_str)
}

#' Add good rate, bad rate and good-bad log adds to a dataset
#'
#' @param df Data set where to add predictions
#' @param logistic A logistic model object created using glm
#' @param scale_slope The slope to convert the log odds to a score
#' @param scale_intercept The intercept to convert the log odds to a score
#' @param pgood Name of the variable for good rate
#' @param pbad  Name of the variable for bad rate
#' @param ln_gb Name of the variable for good-bad log odds
#' @param score Name of the final score variable
#' 
#' @return Dataset after adding the good rate, bad rate, good-bad log odds
#'     and score
model_add_predictions <- function(df, logistic, scale_slope,
                                  scale_intercept, pgood = "p_good",
                                  pbad = "p_bad", ln_gb = "log_gb_odds",
                                  score = "score") {
  df[[pgood]] <- predict(logistic, df, type = "response")
  df[[pbad]] <- 1 - df[[pgood]]
  df[[ln_gb]] <- predict(logistic, df, type = "link")
  df[[score]] <- scale_slope * df[[ln_gb]] + scale_intercept
  df
}

#' Return the slope and intercept of the scaling function
#'
#' @param pdo Points to double the odds
#' @param score Base score
#' @param odds Odds at base score
#' 
#' @return A vector containing the scaling slope and intercept
model_scaling_parameters <- function(pdo, score, odds) {
  slope <- pdo / log(2)
  intercept <- score - slope * log(odds)
  return(c("scale_slope" = slope, "scale_intercept" = intercept))
}

#' Construct the score points for a single variable
#'
#' @param bin The binning object created for this variable
#' @param coef The logistic coefficient for this variable
#' @param intcpt The logistic intercept
#' @param scale_slope The slope to convert the log odds to a score
#' @param scale_intercept The intercept to convert the log odds to a score
#' @param n_predictors The number of predictors in the model
#' @param vname The name of the variable
#'
#' @return Data frame of bin values and score points
model_score_table <- function(bin, coef, intcpt, scale_slope, scale_intercept,
                              n_predictors, vname) {
  ivtable <- bin$ivtable
  nr <- nrow(ivtable)
  ivtable <- ivtable[1:(nr - 1), ]
  ivtable[["Scorepoint"]] <- ivtable[["WoE"]] * coef * scale_slope +
    (scale_slope * intcpt + scale_intercept) / n_predictors
  ivtable[["Variable"]] <- vname
  ivtable[, c("Variable", "Cutpoint", "Scorepoint")]
}

# Model Module - UI -----------------------------------------------------

#' Interface to build the scorecard
#'
#' @param id character used to specify namespace,
#'     see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
model_ui <- function(id) {
  
  ns <- NS(id)
  tagList(
    column(
      9,
      uiOutput(ns("modelwindow"))
    ),
    column(3, includeMarkdown("qh_model.md"))
  )
  
}

# Model Module - Server -------------------------------------------------

#' Model module server-side processing
#'
#' @param input, output, session standard \code{shiny} boilerplate
#' @param modeltraintest data frame containing the credit scoring data split
#'     into training and testing data
#' @param modeldataspecs data specification table for credit scoring data
#' @param binning bin specifications for all variables
#' @param goodbad_var good/bad variable
#'
#' @return list with following components
#' \describe{
#'   \item{scorecard}{Scorecard object}
#' }
model_server <- function(input, output, session, modeltraintest,
                         modeldataspecs, binning, goodbad_var) {
  
  # Return the list of potential model variables and their IVs.
  potential_model_vars <- reactive({
    md_vnames <- req(names(binning()))
    bins <- binning()
    md_valid <- vapply(md_vnames, function(vname) {
      if (!is.null(bins[[vname]]) &&
          class(bins[[vname]]$bin_train) != "character") {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }, logical(1))
    
    if (!any(md_valid)) {
      return(NULL)
    }
    
    md_vnames <- md_vnames[md_valid]
    train_ivs <- vapply(md_vnames, function(vname) {
      bins[[vname]]$bin_train$iv
    }, numeric(1))
    test_ivs <- vapply(md_vnames, function(vname) {
      bins[[vname]]$bin_test$iv
    }, numeric(1))
    
    train <- do.call(cbind, as.list(train_ivs))
    test <- do.call(cbind, as.list(test_ivs))
    rbind(train, test)
  })
  
  # Return the generated scorecard along with other relevant objects.
  scorecard <- eventReactive(input$buildscore, {
    svars <- as.character(req(input$scorevars))
    validate(need(svars, message = FALSE))
    if (length(svars) < 2) {
      showNotification("At least 2 variables required.", type = "error")
      return(NULL)
    }
    req(input$scalepdo, input$scalescore, input$scaleodds)
    
    mds <- req(modeldataspecs())
    mtt <- req(modeltraintest())
    bins <- req(binning())
    
    traindata <- mtt$train
    testdata <- mtt$test
    
    withProgress({
    
    nvars <- mds$vnames[mds$vtypes == "numeric" | mds$vtypes == "integer"]
    nvars <- intersect(svars, nvars)
    for (nvar in nvars) {
      traindata <- model_woe_numeric(traindata, bins[[nvar]]$bin_train,
                                     nvar, paste0(nvar, "_WOE"))
      testdata <- model_woe_numeric(testdata, bins[[nvar]]$bin_train,
                                    nvar, paste0(nvar, "_WOE"))
    }
    
    setProgress(value = 0.4, message = "Creating character bins in data.")
    cvars <- mds$vnames[mds$vtypes == "character"]
    cvars <- intersect(svars, cvars)
    for (cvar in cvars) {
      traindata <- model_woe_categorical(traindata, bins[[cvar]]$bin_train,
                                         cvar, paste0(cvar, "_WOE"))
      testdata <- model_woe_categorical(testdata, bins[[cvar]]$bin_train,
                                        cvar, paste0(cvar, "_WOE"))
    }
    
    setProgress(value = 0.6, message = "Running logistic regression.")
    mvars <- c(nvars, cvars)
    logisitic_f <- reformulate(termlabels = paste0(mvars, "_WOE"),
                               response = goodbad_var())
    logistic_train <- glm(logisitic_f, traindata, family = binomial())
    logistic_test <- glm(logisitic_f, testdata, family = binomial())
    
    setProgress(value = 0.8, message = paste0("Scoring datasets and ",
                                              "generating probabilities"))
    msp <- model_scaling_parameters(input$scalepdo, input$scalescore,
                                    input$scaleodds)
    traindata <- model_add_predictions(traindata, logistic_train,
                                       msp[1], msp[2])
    testdata <- model_add_predictions(testdata, logistic_train,
                                      msp[1], msp[2])
    scorecard_table <- list()
    for (mvar in mvars) {
      scorecard_table[[mvar]] <- model_score_table(
        bins[[mvar]]$bin_train,
        coef(logistic_train)[[paste0(mvar, "_WOE")]],
        coef(logistic_train)[["(Intercept)"]],
        msp[1],
        msp[2],
        length(mvars),
        mvar
      )
    }
    vif <- vif(logistic_train)
    
    setProgress(value = 1, message = "Completed.")
    Sys.sleep(1)
    
    }, value = 0.2, message = "Creating numeric bins in data.")
    
    list(
      traindata = traindata,
      testdata = testdata,
      logistic_train = logistic_train,
      logistic_test = logistic_test,
      scorecard_table = scorecard_table,
      vif = vif,
      scaling_parameters = msp
    )
  })
  
  # Render the modelling window.
  output$modelwindow <- renderUI({
    pmv <- req(potential_model_vars())
    sidebarLayout(
      sidebarPanel(
        div(helpText("Scaling parameters"), style = "font-style:italic;"),
        numericInput(session$ns("scalescore"), "Base Score", value = 400),
        numericInput(session$ns("scaleodds"), "Base Odds", value = 100),
        numericInput(session$ns("scalepdo"), "PDO", value = 20),
        br(),
        div(helpText("Scorecard variables"),
            style = "font-style:italic;"),
        varSelectInput(session$ns("scorevars"), "Choose variables", pmv,
                       selected = colnames(pmv), multiple = TRUE),
        br(),
        actionButton(session$ns("buildscore"), "Build Scorecard")
      ),
      mainPanel(
        div(helpText("Information Value (Selected Variables)"),
            style = "font-weight:bold;"),
        rHandsontableOutput(session$ns("ivtable")),
        br(),
        div(helpText("Variance Inflation Factor"),
            style = "font-weight:bold;"),
        rHandsontableOutput(session$ns("vif")),
        br(),
        div(helpText("Logistic Regression"),
            style = "font-weight:bold;"),
        div(helpText("Training"), style = "font-style:italic;"),
        rHandsontableOutput(session$ns("coeftrain")),
        div(helpText("Test"), style = "font-style:italic;"),
        rHandsontableOutput(session$ns("coeftest")),
        br(),
        div(helpText("Score to Log-Odds Relationship"),
            style = "font-weight:bold;"),
        div(helpText("Training"), style = "font-style:italic;"),
        plotOutput(session$ns("slotrain")),
        div(helpText("Test"), style = "font-style:italic;"),
        plotOutput(session$ns("slotest")),
        br(),
        div(helpText("Final Scorecard"),
            style = "font-weight:bold;"),
        rHandsontableOutput(session$ns("scoretable")),
        br(),
        helpText("")
      )
    )
  })
  
  # Render the IV table.
  output$ivtable <- renderRHandsontable({
    pmv <- req(potential_model_vars())
    svars <- as.character(req(input$scorevars))
    validate(need(svars, message = FALSE))
    pmv <- as.data.frame(pmv)
    pmv <- pmv[, intersect(colnames(pmv), svars), drop = FALSE]
    pmv1 <- gather(pmv[1, , drop = FALSE], "Variable", "IV")
    pmv2 <- gather(pmv[2, , drop = FALSE], "Variable", "IV2")
    pmv <- cbind(pmv1, pmv2[, 2])
    pmv <- arrange(pmv, desc(IV))
    colnames(pmv) <- c("Variable", "IV (Training)", "IV (Test)")
    pmv %>%
      rhandsontable(
        contextMenu = FALSE,
        stretchH = "all",
        readOnly = TRUE,
        rowHeaders = NULL
      ) %>%
      hot_cols(columnSorting = FALSE) %>%
      hot_col("IV (Training)", format = "0.00%") %>%
      hot_col("IV (Test)", format = "0.00%")
  })
  
  # Render the VIF table.
  output$vif <- renderRHandsontable({
    scr <- req(scorecard())
    vif <- data.frame(Variable = names(scr$vif), VIF = scr$vif,
                      row.names = NULL, stringsAsFactors = FALSE)
    vif %>%
      rhandsontable(
        contextMenu = FALSE,
        stretchH = "all",
        readOnly = TRUE,
        rowHeaders = NULL
      ) %>%
      hot_cols(columnSorting = FALSE) %>%
      hot_col("VIF", format = "0.0000")
  })
  
  # Render the logistic coefficients.
  prepare_HOT <- function(coeftable) {
    coeftable <- as.data.frame(coeftable)
    coeftable[["Variable"]] <- rownames(coeftable)
    coeftable <- coeftable[, c(5, 1, 2, 3, 4)]
    coeftable %>%
      rhandsontable(
        contextMenu = FALSE,
        stretchH = "all",
        readOnly = TRUE,
        rowHeaders = NULL
      ) %>%
      hot_cols(columnSorting = FALSE) %>%
      hot_col("Estimate", format = "0.0000") %>%
      hot_col("Pr(>|z|)", format = "0.000000")
  }
  output$coeftrain <- renderRHandsontable({
    scr <- req(scorecard())
    coef <- coef(summary(scr$logistic_train))
    prepare_HOT(coef)
  })
  output$coeftest <- renderRHandsontable({
    scr <- req(scorecard())
    coef <- coef(summary(scr$logistic_test))
    prepare_HOT(coef)
  })
  
  # Render the score to log-odds charts.
  prepare_GG <- function(df, msp, tt = "Training") {
    probs <- seq(0.05, 0.95, by = 0.05)
    brks <- unique(c(-Inf, quantile(df[["score"]], probs = probs), Inf))
    gb <- sym(goodbad_var())
    df <- df %>%
      mutate(score_band = cut(score, breaks = brks, include.lowest = TRUE)) %>%
      group_by(score_band) %>%
      summarise(
        ntot = n(),
        ngood = sum(!!gb),
        nbad = ntot - ngood,
        realised = log(ngood / nbad),
        mean_score = mean(score),
        predicted = (mean_score - msp[2]) / msp[1]
      ) %>%
      select(mean_score, realised, predicted)
    df <- gather(df, rvp, value, -mean_score)
    ggplot(df, aes(mean_score, value, colour = rvp, group = rvp)) +
      theme_bw() +
      geom_point(size = 5, alpha = 0.5) +
      geom_line(size = 1) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
      ggtitle(paste0("Score to Log Odds Relationship", " - ", tt)) +
      xlab(NULL) +
      ylab(NULL) +
      theme(
        plot.title = element_text(size = 20, hjust = 0.5, vjust = 0.5),
        axis.text = element_text(size = 15),
        panel.border = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 15)
      )
  }
  output$slotrain <- renderPlot({
    scr <- req(scorecard())
    prepare_GG(scr$traindata, scr$scaling_parameters)
  })
  output$slotest <- renderPlot({
    scr <- req(scorecard())
    prepare_GG(scr$testdata, scr$scaling_parameters, tt = "Test")
  })
  
  # Render the final scorecard.
  output$scoretable <- renderRHandsontable({
    scr <- req(scorecard())
    stable <- bind_rows(scr$scorecard_table)
    stable %>%
      rhandsontable(
        contextMenu = FALSE,
        stretchH = "all",
        readOnly = TRUE,
        rowHeaders = NULL
      ) %>%
      hot_cols(columnSorting = FALSE) %>%
      hot_col("Scorepoint", format = "0.0")
  })
  
  # Return the scorecard object.
  list(scorecard = scorecard)
  
}