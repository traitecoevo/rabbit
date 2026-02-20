
#' This function takes standardised accelerometer data and caluclates 
#' a set of metrics of movement dynamics across a sliding window of specified number of datapoints. Caluclate metrics include including means, variances, covariances, of xyz, as well as overall dynamic body acceleration (ODBA) and vectorial dynamic body acceleration (VDBA). The function is designed to be fast, using the RcppRoll package to calculate rolling means and sums in C++.
#' 
#' @param df A data frame containing the data.
#' @param window_size An integer specifying the size of the rolling window.
#' @return A data frame with movement dynamics metrics calculated across the sliding window.
#' @rdname extract_movement_dynamics
#' @export
#' @examples 
#' #' @examples
#' file_in = system.file("extdata", "raw_Pic2Jan_50000.parquet", package = "rabbit")
#' df <-
#'   standardize_data(file_in = file_in, vars = c("Timestamp", "accX", "accY", "accZ")) |>
#'   extract_movement_dynamics()
extract_movement_dynamics <- function(df, window_size=50) {

  n <- window_size

  x <- df$x
  y <- df$y
  z <- df$z

  abs_x <- abs(x)
  abs_y <- abs(y)
  abs_z <- abs(z)

  x_2 <- x^2
  y_2 <- y^2
  z_2 <- z^2

  # Calculate means
  mean_x <- roll_sum(x, n) / window_size
  mean_y <- roll_sum(y, n) / window_size
  mean_z <- roll_sum(z, n) / window_size
  
  # Calculate variances
  variance_x <- (roll_sum(x_2, n) - n * mean_x ^ 2) / n
  variance_y <- (roll_sum(y_2, n) - n * mean_y ^ 2) / n
  variance_z <- (roll_sum(z_2, n) - n * mean_z ^ 2) / n
  
   # Calculate covariance
  cov_xy <- (roll_sum(x * y, n) - n * mean_x * mean_y) / n
  cov_xz <- (roll_sum(x * z, n) - n * mean_x * mean_z) / n
  cov_yz <- (roll_sum(y * z, n) - n * mean_y * mean_z) / n
   
  # Calculate Overall Dynamic Body Acceleration (ODBA) and Vectorial Dynmic Body Acceleration (VDBA)
  ODBA = abs_x + abs_y + abs_z
  VDBA = sqrt(x_2 + y_2 + z_2)
  
  out <- 
    dplyr::tibble(
      time = roll_date(df$time, n),
      meanX = mean_x,
      meanY = mean_y,
      meanZ = mean_z,
      maxx = roll_max(x, n),
      maxy = roll_max(y, n),
      maxz = roll_max(z, n),
      minx = roll_min(x, n),
      miny = roll_min(y, n),
      minz = roll_min(z, n),
      sdx = sqrt(variance_x * n / (n-1) ),
      sdy = sqrt(variance_y * n / (n-1) ),
      sdz = sqrt(variance_z * n / (n-1) ),
      SMA = (roll_sum(abs_x, n) + roll_sum(abs_y, n) + roll_sum(abs_z, n))/n,
      minODBA = roll_min(ODBA, n),
      maxODBA = roll_max(ODBA, n),
      minVDBA = roll_min(VDBA, n),
      maxVDBA = roll_max(VDBA, n),
      sumODBA = roll_sum(ODBA, n),
      sumVDBA = roll_sum(VDBA, n),
      meanODBA = roll_sum(ODBA, n) / n,
      meanVDBA = roll_sum(VDBA, n) / n,
      corXY = cov_xy / sqrt(variance_x * variance_y),
      corXZ = cov_xz / sqrt(variance_x * variance_z),  
      corYZ = cov_yz / sqrt(variance_y * variance_z),
    
      # Calculate skewness, using formula from https://en.wikipedia.org/wiki/Skewness
      # https://wikimedia.org/api/rest_v1/media/math/render/svg/77a8f1e4f233c410e85698ca11d163f6f81c5e5f
      skx = (roll_mean(x^3, n) - 3 * mean_x * variance_x - mean_x^3) / sdx ^ 3,
      sky = (roll_mean(y^3, n) - 3 * mean_y * variance_y - mean_y^3) / sdy ^ 3,    
      skz = (roll_mean(z^3, n) - 3 * mean_z * variance_z - mean_z^3) / sdz ^ 3,
  )

  return(out)
}

# Define functions to use for rolling means
# We use the package RcppRoll, as functions are written in C++
roll_mean <- function(x, n) {
  RcppRoll::roll_mean(x,
    n = n, fill = NA, align = "right"
  )
}

roll_sum <- function(x, n) {
  RcppRoll::roll_sum(x,
    n = n, fill = NA, align = "right"
  )
}

roll_min <- function(x, n) {
  RcppRoll::roll_min(x,
    n = n, fill = NA, align = "right"
  )
}

roll_max <- function(x, n) {
  RcppRoll::roll_max(x,
    n = n, fill = NA, align = "right"
  )
}

# Assume input is already POSIXct — no need to re-parse
roll_date <- function(x, n) {
  origin <- min(x, na.rm = TRUE)
  # Convert dates for rolling mean calculation, then convert back to POSIXct
  x |>
    as.numeric() |>
    RcppRoll::roll_mean(n = n, fill = NA, align = "right") |>
    # Convert back to POSIXct
    lubridate::as_datetime(origin = origin)
}
