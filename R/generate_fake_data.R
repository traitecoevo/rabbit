#' Generate fake ample accelerometer data
#'
#' Creates synthetic triaxial accelerometer data with 1-second timestamps.
#' The `accZ` axis is centered around 1 (gravity-like baseline), while `accX` and `accY`
#' are centered around 0.
#'
#' @param n Integer. Number of rows to generate.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{timestamp}{Character timestamp in format `"%d/%m/%Y %H:%M:%S"` (UTC).}
#'   \item{accX}{Simulated x-axis acceleration values.}
#'   \item{accY}{Simulated y-axis acceleration values.}
#'   \item{accZ}{Simulated z-axis acceleration values.}
#' }
#' @export
#' @examples
#' df <- generate_fake_data(1000)
#' head(df)
generate_fake_data <- function(n) {
  set.seed(123)

  timestamp <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + (seq_len(n) - 1)

  data.frame(
    timestamp = format(timestamp, "%d/%m/%Y %H:%M:%S"),
    accX = rnorm(n, mean = 0, sd = 0.1),
    accY = rnorm(n, mean = 0, sd = 0.1),
    accZ = rnorm(n, mean = 1, sd = 0.1)
  )
}
