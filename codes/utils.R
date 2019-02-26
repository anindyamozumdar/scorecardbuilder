quotify <- function(x, split = ",") {
  x <- strsplit(x, split = split)
  sapply(x, function(x) {
    paste(paste0("'", x, "'"), collapse = ",")
  })
}

infinite_woes <- function(woes, cnt_recs) {
  woes <- woes[cnt_recs > 0]
  any(is.infinite(woes))
}

missnan_woes <- function(woes, cnt_recs) {
  woes <- woes[cnt_recs > 0]
  any(is.na(woes)) || any(is.nan(woes))
}

notify_bin_errors <- function(bin) {
  if (class(bin$bin_train) == "character") {
    showNotification(paste0("Error occurred while binning. The error was - ",
                            bin$bin_train, "."),
                     duration = NULL, type = "error")
    return(TRUE)
  } else {
    return(FALSE)
  }
}

notify_bin_warnings <- function(bin) {
  woes <- c(bin$bin_train$ivtable$WoE, bin$bin_test$ivtable$WoE)
  cnt_recs <- c(bin$bin_train$ivtable$CntRec, bin$bin_test$ivtable$CntRec)
  if (infinite_woes(woes, cnt_recs) || missnan_woes(woes, cnt_recs)) {
    msg <- "Infinite or missing WoEs detected."
    showNotification(msg, duration = NULL, type = "warning")
    return(TRUE)
  } else {
    return(FALSE)
  }
}