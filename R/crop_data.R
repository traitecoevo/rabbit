

#' This function Crops the start and end of each file to a specified time. Use crop_diurnal for diurnal data and crop_nocturnal for nocturnal data.
#'
#' @param df A dataframe containing the data.
#' @param start_time The desired start time
#' @param end_time The desired end time
#' @rdname crop_times
#' @export
crop_times <- function(df, start_time = "00:00:00", end_time   = "24:00:00") {
  
  if(end_time <= start_time) {
    stop("in crop_times: end_time must be after start_time")
  }

  df |>
    dplyr::filter(
      dplyr::between(
        format(df$time, "%H:%M:%S"), start_time, end_time)
    )
}

#' @rdname crop_times
#' @export
crop_diurnal <- function(df, start_time = "06:00:00", end_time = "18:00:00") {
  crop_times(df, start_time = start_time, end_time = end_time)
}

#' @rdname crop_times
#' @export
crop_nocturnal <- function(df, start_time = "18:00:00", end_time = "06:00:00") {
  df |>
    dplyr::filter(
      !dplyr::between(
        format(df$time, "%H:%M:%S"), end_time, start_time
      )
    )
}
