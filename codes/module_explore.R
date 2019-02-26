# Explore Module - Helpers ---------------------------------------------------

#' Modes of a numeric or categorical variable
#'
#' @param x A vector of numeric or categorical values
#' @param na.rm Logical to indicate if missing values should be removed
#' @param max_modes Maximum number of modes to return, if there are multiple
#'
#' @return A vector containing the modes of x
explore_modes <- function(x, na.rm = TRUE, max_modes = 5L) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  modes <- ux[tab == max(tab)]
  names(modes) <- vapply(1:length(modes), function(i) {
    paste("Mode", i)
  }, character(1))
  modes[1:max_modes]
}

#' Descriptive statistics for a numeric variable
#'
#' @param x A vector of numeric values
#' @param round_dec Number of decimal places to round the results
#'
#' @return A named vector of descriptive statistics
explore_descstat_numeric <- function(x, round_dec = 2) {
  x_n <- length(unique(x))
  x_nmiss <- sum(is.na(x))
  x_mean <- mean(x, na.rm = TRUE)
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  x_sd <- sd(x, na.rm = TRUE)
  x_var <- var(x, na.rm = TRUE)
  x_quantiles <- quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95,
                                       0.99, 0.999), na.rm = TRUE)
  names(x_quantiles) <- vapply(names(x_quantiles), function(name) {
    paste("Quantile", "-", name)
  }, character(1))
  round(
    c(
      "n_unique" = x_n, "nmiss" = x_nmiss, "mean" = x_mean, "min" = x_min,
      "max" = x_max, "sd" = x_sd, "var" = x_var, x_quantiles
    ),
    round_dec
  )
}

#' Distribution plot for a numeric vector
#'
#' @param x A vector of numeric values
#'
#' @return A ggplot2 object showing the distribution of the variable
explore_distribution_numeric <- function(x, vname = "numeric") {
  x <- x[!is.na(x)]
  nuniq <- length(unique(x))
  if (nuniq < 30) {
    nbins <- 10
  } else if (nuniq >= 30 && nuniq < 300) {
    nbins <- 20
  } else {
    nbins <- 30
  }
  d <- data.frame(var = x)
  g <- ggplot(d, aes(x)) +
    theme_bw() +
    geom_histogram(aes(y = ..density..), bins = nbins, fill = "lightblue") +
    geom_density(size = 1) +
    ggtitle(paste0("Distribution - ", vname)) +
    xlab(NULL) +
    ylab(NULL) +
    theme(
      plot.title = element_text(size = 20, hjust = 0.5, vjust = 0.5),
      axis.text = element_text(size = 15),
      panel.border = element_blank()
    )
  g
}

#' Frequency and percentage of values for a categorical variable
#'
#' @param x A vector of character values
#' @param max_values Maximum number of distinct values to return; if the number
#'     of values exceeds this then the remaining are clubbed as OTHERS
#'
#' @return A data frame of frequencies and percentages for each value
explore_countperc_categorical <- function(x, max_values = 30) {
  df <- data.frame(Value = x, stringsAsFactors = FALSE)
  df <- df %>%
    group_by(Value) %>%
    summarise(Frequency = n()) %>%
    mutate(Proportion = Frequency / sum(Frequency)) %>%
    ungroup() %>%
    mutate(Value = ifelse(is.na(Value), "MISSING", Value)) %>%
    arrange(desc(Frequency))
  if (nrow(df) > max_values) {
    frequency_o <- sum(df[max_values:nrow(df), 2])
    proportion_o <- sum(df[max_values:nrow(df), 3])
    df <- bind_rows(df[1:(max_values - 1), ],
                    data.frame(Value = "OTHERS", Frequency = frequency_o,
                               Proportion = proportion_o,
                               stringsAsFactors = FALSE))
  }
  df
}

#' Distribution plot for a categorical vector
#'
#' @param x A vector of character values
#'
#' @return A list containing the ggplot2 object showing the distribution of
#'     the variable as well as the data frame of frequencies and percentages
explore_distribution_categorical <- function(x, vname = "categorical") {
  d <- explore_countperc_categorical(x)
  g <- ggplot(d, aes(factor(Value), Proportion)) +
    theme_bw() +
    geom_col(width = 0.3, fill = "lightblue", colour = "black") +
    geom_text(aes(y = Proportion + .02,
                  label = sprintf("%.2f%%", Proportion * 100)), size = 3) +
    scale_y_continuous(
      limits = c(0, 1.1),
      breaks = seq(0, 1.1, by = 0.1),
      labels = scales::percent
    ) +
    ggtitle(paste0("Distribution - ", vname)) +
    xlab(NULL) +
    ylab(NULL) +
    theme(
      plot.title = element_text(size = 20, hjust = 0.5, vjust = 0.5),
      axis.text = element_text(size = 15),
      panel.border = element_blank()
    )
  list(disttable = d, distplot = g)
}

# Explore Module - UI --------------------------------------------------------

#' Interface to explore the data based on data specifications
#'
#' @param id character used to specify namespace,
#'     see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
explore_ui <- function(id) {
  
  ns <- NS(id)
  tagList(
    column(
      9,
      radioButtons(ns("exploreonesumm"), "Choose view", inline = TRUE,
                   choices = c("All Variables", "Single Variable"),
                   selected = "All Variables"),
      uiOutput(ns("explorewindow"))
    ),
    column(3, includeMarkdown("qh_explore.md"))
  )
  
}

# Explore Module - Server ----------------------------------------------------

#' Explore data module server-side processing
#'
#' @param input, output, session standard \code{shiny} boilerplate
#' @param modeldata data frame containing the credit scoring data
#' @param modeldataspecs data specification table for credit scoring data
explore_server <- function(input, output, session, modeldata, modeldataspecs) {
  
  # Summary report of all variables.
  summary_report <- reactive({
    md <- modeldata()
    withProgress({
      sr <- dfSummary(md, style = 'grid', plain.ascii = FALSE,
                      graph.magnif = 1, silent = TRUE)
      Sys.sleep(1)
      setProgress(message = "Completed", value = 1)
    }, message = "Generating summary report of all variables", value = 0.5)
    sr
  })
  
  # Change the display depending on whether the user wants to see all variables
  # or a single variable.
  output$explorewindow <- renderUI({
    if (req(input$exploreonesumm) == "All Variables") {
      print(summary_report(), method = "render", headings = FALSE)
    } else {
      chcs <- modeldataspecs()$vnames
      chcs <- chcs[modeldataspecs()$vtypes != "Date"]
      tagList(
        selectInput(session$ns("exploreone"), label = "Choose variable",
                    choices = chcs),
        column(10, div(helpText("Distribution Plot"),
                      style = "font-weight: bold;")),
        br(),
        column(10, plotOutput(session$ns("exploreonedist"))),
        br(),
        column(5, div(helpText("Descriptive Statistics"),
                      style = "font-weight: bold;")),
        column(5, div(helpText("Modes (Upto 5 values)"),
                      style = "font-weight: bold;")),
        br(),
        column(5, rHandsontableOutput(session$ns("exploreoneds"))),
        column(5, rHandsontableOutput(session$ns("exploreonemodes")))
      )
    }
  })
  
  # Generate the tables and chart for a single variable report.
  variable_report <- eventReactive(input$exploreone, {
    vtype <- modeldataspecs()$vtypes[modeldataspecs()$vnames ==
                                       input$exploreone]
    if (vtype == "numeric") {
      ds <- explore_descstat_numeric(modeldata()[[input$exploreone]])
      ds <- data.frame(Statistic = names(ds), Value = ds, row.names = NULL,
                       stringsAsFactors = FALSE)
      dist <- explore_distribution_numeric(modeldata()[[input$exploreone]],
                                           vname = input$exploreone)
    } else {
      distobj <- explore_distribution_categorical(
        modeldata()[[input$exploreone]],
        vname = input$exploreone
      )
      ds <- distobj$disttable
      dist <- distobj$distplot
    }
    
    modes <- explore_modes(modeldata()[[input$exploreone]])
    modes <- data.frame(Modes = names(modes), Value = modes, row.names = NULL,
                        stringsAsFactors = FALSE)
    
    list(ds = ds, modes = modes, dist = dist, type = vtype)
  })
  
  # Render the distribution plot.
  output$exploreonedist <- renderPlot({
    variable_report()$dist
  })
  
  # Render the descriptive statistics or frequency / proportion table,
  # depending on whether the chosen variable is numeric or categorical.
  output$exploreoneds <- renderRHandsontable({
    if (variable_report()$type == "numeric") {
      rhandsontable(variable_report()$ds,
                    height = 500,
                    contextMenu = FALSE,
                    rowHeaders = NULL) %>%
        hot_col("Statistic", readOnly = TRUE) %>%
        hot_col("Value", readOnly = TRUE) %>%
        hot_cols(columnSorting = FALSE)
    } else {
      rhandsontable(variable_report()$ds,
                    height = 500,
                    contextMenu = FALSE,
                    rowHeaders = NULL) %>%
        hot_col("Value", readOnly = TRUE) %>%
        hot_col("Frequency", readOnly = TRUE, format = "0,0") %>%
        hot_col("Proportion", readOnly = TRUE, format = "0.00%") %>%
        hot_cols(columnSorting = FALSE)
    }
  })
  
  # Render the table of modes.
  output$exploreonemodes <- renderRHandsontable({
    rhandsontable(variable_report()$modes,
                  height = 500,
                  contextMenu = FALSE,
                  rowHeaders = NULL) %>%
      hot_col("Modes", readOnly = TRUE) %>%
      hot_col("Value", readOnly = TRUE) %>%
      hot_cols(columnSorting = FALSE)
  })
  
}