
#' Crop data to specified start and end times, to remove records at the beginning and end of each file.
#' This is useful for removing records collected before the device was attached or after it was removed. crop_diurnal defaults to cropping dataframe to the first instance of 01:00:00 and the last instance of 01:00:00. crop_nocturnal defaults to cropping dataframe to the first instance of 12:00:00 and the last instance of 12:00:00. crop_ends is a more general function that allows users to specify any start and end times. crop_diurnal
#' @param df A dataframe containing the data.
#' @param start_time The desired start time
#' @param end_time The desired end time
#' @rdname crop_ends
#' @export
crop_ends <- function(df, start_time = "00:00:00", end_time   = "24:00:00") {

  # first rows with time > start_time and last rows with time < end_time are retained. 
  time <- format(df$time, "%H:%M:%S")

  start_i <- which(!is.na(time) & time >= start_time)
  if(length(start_i) == 0) {
    start_i <- nrow(df)
  } else {
    start_i <- min(start_i)
  }

  # if there are no times after end_time, then end_i is set to nrow(df) to retain all rows after start_i. Otherwise, end_i is set to the last row with time < end_time.
  end_i <- which(!is.na(time) &  time <= end_time)
  if(length(end_i) == 0) {
    end_i <- nrow(df)
  } else {
    end_i <- max(end_i)
  }

  df |>
    dplyr::slice(start_i:end_i)
}

#' @rdname crop_ends
#' @export
crop_ends_diurnal <- function(df, start_time = "01:00:00", end_time = "01:00:00") {
  crop_ends(df, start_time = start_time, end_time = end_time)
}

#' @rdname crop_ends
#' @export
crop_ends_nocturnal <- function(df, start_time = "12:00:00", end_time = "12:00:00") {
  crop_ends(df, start_time = start_time, end_time = end_time)
}
