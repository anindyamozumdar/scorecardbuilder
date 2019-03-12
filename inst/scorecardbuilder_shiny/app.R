# Source Files ------------------------------------------------------------

# source("codes/libs.R")
# source("codes/utils.R")
# source("codes/module_data.R")
# source("codes/module_explore.R")
# source("codes/module_sample.R")
# source("codes/module_binning.R")
# source("codes/module_model.R")
# source("codes/module_validation.R")
# source("codes/module_download.R")
# source("codes/server.R")
# source("codes/ui.R")

# Help Files --------------------------------------------------------------

# qh_files <- c("help/qh_data.Rmd", "help/qh_explore.Rmd",
#               "help/qh_sample.Rmd", "help/qh_binning.Rmd",
#               "help/qh_model.Rmd", "help/qh_validation.Rmd")
# sapply(qh_files, knit, quiet = TRUE)
# 
# h_files <- c("help/h_data.Rmd", "help/h_explore.Rmd", "help/h_sample.Rmd",
#              "help/h_binning.Rmd", "help/h_model.Rmd", "help/h_validation.Rmd",
#              "help/h_download.Rmd")
# sapply(h_files, knit, quiet = TRUE)

# Run Application ---------------------------------------------------------

# Options -----------------------------------------------------------------

options(shiny.maxRequestSize = Inf)
options(shiny.minified = TRUE)

scorecardbuilder::scorecardbuilder()