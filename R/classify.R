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
    dplyr::mutate(time = lubridate::ymd_hms(!!sym(time_col)),
                  # Ensure the time column is in POSIXct format
                  window = lubridate::floor_date(time, unit = paste0(window_minutes, " mins"))) %>%
    dplyr::group_by(window) %>%
    dplyr::summarise(mode_behavior = calculate_mode(!!sym(behavior_col))) %>%
    dplyr::filter(!is.na(mode_behavior)) -> mode_sum
  return(mode_sum)
}

#' classify_behaviors
#'
#' This function takes a data frame and a classifier and predicts across the data frame assuming everything lines up
#'
#' @param dat A data frame containing the data.
#' @param MSOM_path classifier
#' @param quiet chill out
#' @return A data frame with a column of behaviors
#' @rdname classify_behaviors
#' @export
#'
classify_behaviors <- function(
                               dat,
                               MSOM_path = "tests/testthat/MSOM_8by7.rda",
                               quiet = FALSE) {
  #dat <- moving_window_calcs_2(acc_dat) #this might move out of the function
  dd <- as.matrix(dat[, -1])
  load(file = MSOM_path)
  ssom.pred <- kohonen:::predict.kohonen(MSOM, newdata = dd, whatmap = 1) # this is slow part
  
  dat$behavior <- ssom.pred$predictions$activity
  if (quiet)
    print(table(dat$behavior))
  
  return(dat)
}
