#' moving_window_calcs
#'
#' This function takes a data frame and computes sliding window calculations to set up for classification
#'
#' @param df A data frame containing the data.
#' @param window_size An integer specifying the size of the rolling window.
#' @return A data frame with names following doAccloop.R
#' @rdname moving-window-calcs
#' @export
#'

moving_window_calcs_2 <- function(df, window_size=50) {

  # Define functions to use for rolling means
  # We use the package RcppRoll, as functions are written in C++
  roll_mean <- function(x) RcppRoll::roll_mean(x, 
    n = window_size, fill = NA, align = "right") 

  roll_sum <- function(x) RcppRoll::roll_sum(x, 
      n = window_size, fill = NA, align = "right") 

  roll_min <- function(x) RcppRoll::roll_min(x, 
    n = window_size, fill = NA, align = "right") 

  roll_max <- function(x) RcppRoll::roll_max(x, 
      n = window_size, fill = NA, align = "right") 

  x <- df$X
  y <- df$Y
  z <- df$Z

  abs_x <- abs(x)
  abs_y <- abs(y)
  abs_z <- abs(z)

  x_2 <- x^2
  y_2 <- y^2
  z_2 <- z^2

  # Calculate means
  mean_x <- roll_sum(x) / window_size
  mean_y <- roll_sum(y) / window_size
  mean_z <- roll_sum(z) / window_size
    
  # Calculate variances
  variance_x <- (roll_sum(x_2) - window_size * mean_x ^ 2) / window_size
  variance_y <- (roll_sum(y_2) - window_size * mean_y ^ 2) / window_size
  variance_z <- (roll_sum(z_2) - window_size * mean_z ^ 2) / window_size
  
   # Calculate covariance
  cov_xy <- (roll_sum(x * y) - window_size * mean_x * mean_y) / window_size
  cov_xz <- (roll_sum(x * z) - window_size * mean_x * mean_z) / window_size
  cov_yz <- (roll_sum(y * z) - window_size * mean_y * mean_z) / window_size
   
  # Calculate Overall Dynamic Body Acceleration (ODBA) and Vectorial Dynmic Body Acceleration (VDBA)
  ODBA = abs_x + abs_y + abs_z
  VDBA = sqrt(x_2 + y_2 + z_2)
  
  # Extract the ID from the file name
  file_name <- basename(parquet_file)
  name <- str_split(file_name, "_")[[1]][1]

  out <- 
    dplyr::tibble(
      ID = name,
      time = rolling_mean_time_date(df$Timestamp, window_size),
      meanX = mean_x,
      meanY = mean_y,
      meanZ = mean_z,
      maxx = roll_max(x),
      maxy = roll_max(y),
      maxz = roll_max(z),
      minx = roll_min(x),
      miny = roll_min(y),
      minz = roll_min(z),
      sdx = sqrt(variance_x * window_size / (window_size-1) ),
      sdy = sqrt(variance_y * window_size / (window_size-1) ),
      sdz = sqrt(variance_z * window_size / (window_size-1) ),
      SMA = (roll_sum(abs_x) + roll_sum(abs_y) + roll_sum(abs_z))/window_size,
      minODBA = roll_min(ODBA),
      maxODBA = roll_max(ODBA),
      minVDBA = roll_min(VDBA),
      maxVDBA = roll_max(VDBA),
      sumODBA = roll_sum(ODBA),
      sumVDBA = roll_sum(VDBA),
      meanODBA = roll_sum(ODBA) / window_size,
      meanVDBA = roll_sum(VDBA) / window_size,
      corXY = cov_xy / sqrt(variance_x * variance_y),
      corXZ = cov_xz / sqrt(variance_x * variance_z),  
      corYZ = cov_yz / sqrt(variance_y * variance_z),
    
      # Calculate skewness, using formula from https://en.wikipedia.org/wiki/Skewness
      # https://wikimedia.org/api/rest_v1/media/math/render/svg/77a8f1e4f233c410e85698ca11d163f6f81c5e5f
      skx = (roll_mean(x^3) - 3 * mean_x * variance_x - mean_x^3) / sdx ^ 3,
      sky = (roll_mean(y^3) - 3 * mean_y * variance_y - mean_y^3) / sdy ^ 3,    
      skz = (roll_mean(z^3) - 3 * mean_z * variance_z - mean_z^3) / sdz ^ 3,
  )

  return(out)
}
