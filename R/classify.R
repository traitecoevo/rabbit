# Function to calculate mode
calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' summary_by_time
#'
#' @param data A data frame containing the data.
#' @param time_col time col
#' @param behavior_col behavior col
#' @param window_minutes how to chunk
#' @return A data frame with a summary
#' @rdname summary_by_time
#' @export
#'
summary_by_time <- function(data, time_col,
                                         behavior_col,
                                         window_minutes) {
  data %>%
    dplyr::mutate(time = lubridate::ymd_hms(!!rlang::sym(time_col)),
                  # Ensure the time column is in POSIXct format
                  window = lubridate::floor_date(time, unit = paste0(window_minutes, " mins"))) %>%
    dplyr::group_by(window) %>%
    dplyr::summarise(mode_behavior = calculate_mode(!!rlang::sym(behavior_col))) %>%
    dplyr::filter(!is.na(mode_behavior)) -> mode_sum
  return(mode_sum)
}

#' classify_behaviors
#'
#' This function takes a data frame and a classifier and predicts across the data frame assuming everything lines up
#'
#' @param dat A data frame containing the data.
#' @param MSOM  a classifier
#' @param quiet chill out
#' @return A data frame with a column of behaviors
#' @rdname classify_behaviors
#' @export
#'
classify_behaviors <- function(
                               dat,
                               MSOM = readRDS("tests/testthat/MSOM2.rds"),
                               quiet = FALSE) {
  
  ssom.pred <- kohonen:::predict.kohonen(MSOM, newdata = as.matrix(dat[, -1]), whatmap = 1) # this is slow part
  
  dat$behavior <- ssom.pred$predictions$activity
  if (quiet)
    print(table(dat$behavior))
  
  return(dat)
}
