# Validation Module - Helpers ------------------------------------------------

#' Create underlying table for validation metrics
#'
#' @param df Dataset containing the required variables
#' @param score Score variable
#' @param pbad Probability of bad
#' @param gb Actual good/bad flag with value 1 for goods
#' @param n_groups Number of groups to divide the data while calculating metrics
#' @param brks Pre-determined set of breaks to cut the score
#'
#' @return a list of the metrics table along with the breaks for the score
#'     bands
validation_metrics_table <- function(df, score = "score", pbad = "p_bad",
                                     gb = "f_good", n_groups = 20,
                                     brks = NULL) {
  if (is.null(brks)) {
    probs <- seq(1 / n_groups, 1 - 1 / n_groups, by = 1 / n_groups)
    brks <- unique(c(-Inf, quantile(df[["score"]], probs = probs), Inf))
  }
  score_s <- sym(score)
  gb_s <- sym(gb)
  pbad_s <- sym(pbad)
  df_summ <- df %>%
    mutate(
      score_band = cut(!!score_s, breaks = brks, include.lowest = TRUE)
    ) %>%
    group_by(score_band) %>%
    summarise(
      nrec = n(),
      ngood = sum(!!gb_s),
      nbad = nrec - ngood,
      pbad = sum(!!pbad_s),
      pgood = nrec - pbad,
      realised = nbad / nrec,
      predicted = pbad / nrec,
      percentage_error = if_else(realised <= 0, NA_real_,
                                 (predicted - realised) / realised)
    ) %>%
    ungroup() %>%
    mutate(
      nrec_pct = nrec / sum(nrec),
      cumgood = cumsum(ngood),
      cumbad = cumsum(nbad),
      cumgood_pct = cumgood / sum(ngood),
      cumbad_pct = cumbad / sum(nbad),
      ks = abs(cumgood_pct - cumbad_pct),
      gini = (cumgood_pct - lag(cumgood_pct)) *
        (cumbad_pct + lag(cumbad_pct)) / 2
    )
  df_summ[1, "gini"] <- df_summ[1, "cumgood_pct"] * df_summ[1, "cumbad_pct"] / 2
  list(summ = df_summ, brks = brks)
}

# Validation Module - UI -----------------------------------------------------

#' Interface to display scorecard validation metrics
#'
#' @param id character used to specify namespace,
#'     see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
validation_ui <- function(id) {
  
  ns <- NS(id)
  tagList(
    column(
      9,
      uiOutput(ns("validationwindow"))
    ),
    column(3, includeMarkdown("qh_validation.md"))
  )
  
}

# Validation Module - Server -------------------------------------------------

#' Validation module server-side processing
#'
#' @param input, output, session standard \code{shiny} boilerplate
#' @param scorecard scorecard object created in model module
#' @param goodbad_var good/bad variable
#'
#' @return list with following components
#' \describe{
#'   \item{validation}{Object with the validation datasets}
#' }
validation_server <- function(input, output, session, scorecard, goodbad_var) {
  
  # Return a validation object.
  validation <- reactive({
    scr <- req(scorecard())
    gb <- req(goodbad_var())
    withProgress({
      train_summ <- validation_metrics_table(scr$traindata, gb = gb)
      test_summ <- validation_metrics_table(scr$testdata, gb = gb,
                                            brks = train_summ$brks)
      setProgress(value = 1, "Completed.")
      Sys.sleep(1)
    }, value = 0.5, message = "Preparing validation metrics.")
    list(
      train_summ = train_summ$summ,
      test_summ = test_summ$summ
    )
  })
  
  # Render the validation window.
  output$validationwindow <- renderUI({
    req(validation())
    tagList(
      column(10, div(helpText("Discrimination"), style = "font-weight:bold;")),
      br(),
      column(5, plotOutput(session$ns("roccurve"))),
      column(
        5,
        div(helpText("Metrics"), style = "font-style:italic;"),
        rHandsontableOutput(session$ns("ginikstable"))
      ),
      br(),
      br(),
      column(10, div(helpText("Accuracy"), style = "font-weight:bold;")),
      br(),
      column(5, plotOutput(session$ns("rvptrain"))),
      column(5, plotOutput(session$ns("rvptest"))),
      br(),
      br(),
      column(10, div(helpText("Stability"), style = "font-weight:bold;")),
      br(),
      column(10, plotOutput(session$ns("psiplot")))
    )
  })
  
  # ROC Curve.
  output$roccurve <- renderPlot({
    vl <- req(validation())
    roc <- vl$train_summ[, c("cumgood_pct", "cumbad_pct")]
    zero <- data.frame(cumgood_pct = 0, cumbad_pct = 0)
    roc <- bind_rows(zero, roc)
    ggplot(roc, aes(cumgood_pct, cumbad_pct)) +
      theme_bw() +
      geom_point(size = 5, colour = "lightblue") +
      geom_abline(slope = 1, intercept = 0) +
      scale_x_continuous(labels = scales::percent) +
      scale_y_continuous(labels = scales::percent) +
      ggtitle("ROC Curve - Training Data") +
      xlab("Cumulative Good %") +
      ylab("Cumulative Bad %") +
      theme(
        plot.title = element_text(size = 20, hjust = 0.5, vjust = 0.5),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        panel.border = element_blank()
      )
  })
  
  # Gini / KS table.
  output$ginikstable <- renderRHandsontable({
    vl <- req(validation())
    train <- summarise(vl$train_summ, ks = max(ks), gini = 2 * sum(gini) - 1)
    test <- summarise(vl$test_summ, ks = max(ks), gini = 2 * sum(gini) - 1)
    train["ds"] <- "Training"
    test["ds"] <- "Test"
    tt <- select(bind_rows(train, test), ds, ks, gini)
    tt %>%
      rhandsontable(
        contextMenu = FALSE,
        stretchH = "all",
        readOnly = TRUE,
        rowHeaders = NULL,
        colHeaders = c("Data", "KS", "Gini")
      ) %>%
      hot_cols(columnSorting = FALSE) %>%
      hot_col("KS", format = "0.00%") %>%
      hot_col("Gini", format = "0.00%")
  })
  
  # RvP charts.
  prepare_RVP <- function(df_summ, wpe, tt = "Training") {
    df <- gather(select(df_summ, score_band, predicted, realised), rvp,
                 value, -score_band)
    ggplot(df, aes(score_band, value, colour = rvp, group = rvp)) +
      theme_bw() +
      geom_point(size = 5, alpha = 0.5) +
      geom_line(size = 1) +
      scale_y_continuous(labels = scales::percent) +
      expand_limits(y = 0) +
      annotate("text", x = nrow(df_summ) - 2,
               y = max(df_summ[["predicted"]], df_summ[["realised"]],
                       na.rm = TRUE) + 0.01,
               label = paste0("Wtd. % Error = ",
                              scales::percent(wpe, accuracy = 0.01))) +
      ggtitle(paste0("Realised vs. Predicted Bad Rate", " - ", tt)) +
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
  }
  output$rvptrain <- renderPlot({
    vl <- req(validation())
    wpe <- weighted.mean(vl$train_summ$percentage_error,
                         vl$train_summ$nrec, na.rm = TRUE)
    prepare_RVP(vl$train_summ, wpe, tt = "Training")
  })
  output$rvptest <- renderPlot({
    vl <- req(validation())
    wpe <- weighted.mean(vl$test_summ$percentage_error,
                         vl$test_summ$nrec, na.rm = TRUE)
    prepare_RVP(vl$test_summ, wpe, tt = "Test")
  })
  
  # PSI.
  output$psiplot <- renderPlot({
    vl <- req(validation())
    train_summ <- vl$train_summ
    test_summ <- vl$test_summ
    psi_table <- train_summ %>%
      select(score_band, nrec_pct) %>%
      rename(nrec_pct_train = nrec_pct) %>%
      left_join(
        select(test_summ, score_band, nrec_pct) %>%
          rename(nrec_pct_test = nrec_pct),
        by = c("score_band" = "score_band")
      ) %>%
      mutate(
        psi = (nrec_pct_test - nrec_pct_train) *
          log(nrec_pct_test / nrec_pct_train)
      )
    psi <- scales::percent(sum(psi_table$psi, na.rm = TRUE), accuracy = 0.01)
    psi_table <- psi_table %>%
      select(score_band, nrec_pct_train, nrec_pct_test) %>%
      rename(Training = nrec_pct_train, Test = nrec_pct_test) %>%
      gather(ds, value, -score_band)
    ggplot(psi_table, aes(score_band, value, colour = ds, group = ds)) +
      theme_bw() +
      geom_point(size = 5, alpha = 0.5) +
      geom_line(size = 1) +
      scale_y_continuous(labels = scales::percent) +
      expand_limits(y = 0) +
      annotate("text", x = nrow(psi_table) / 4,
               y = max(psi_table[["value"]] + 0.01),
               label = paste0("PSI = ", psi)) +
      ggtitle(paste0("% of Records - Training vs. Test")) +
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
  
  # Return the validation object.
  list(validation = validation)
}