


roll_cor <- function(x, y, window_size) {
  n <- length(x)
  
  # Calculate rolling sums
  sum_x <- RcppRoll::roll_sum(x,
                              n = window_size,
                              fill = NA,
                              align = "right")
  sum_y <- RcppRoll::roll_sum(y,
                              n = window_size,
                              fill = NA,
                              align = "right")
  
  # Calculate rolling sums of squares
  sum_x_sq <- RcppRoll::roll_sum(x ^ 2,
                                 n = window_size,
                                 fill = NA,
                                 align = "right")
  sum_y_sq <- RcppRoll::roll_sum(y ^ 2,
                                 n = window_size,
                                 fill = NA,
                                 align = "right")
  
  # Calculate rolling sum of products
  sum_xy <- RcppRoll::roll_sum(x * y,
                               n = window_size,
                               fill = NA,
                               align = "right")
  
  # Calculate means
  mean_x <- sum_x / window_size #could use roll_mean here
  mean_y <- sum_y / window_size #could use roll_mean here
  
  # Calculate covariance
  covariance <- (sum_xy - window_size * mean_x * mean_y) / window_size
  
  # Calculate variances
  variance_x <- (sum_x_sq - window_size * mean_x ^ 2) / window_size
  variance_y <- (sum_y_sq - window_size * mean_y ^ 2) / window_size
  
  # Calculate correlation
  correlation <- covariance / sqrt(variance_x * variance_y)
  
  return(correlation)
}

#not in C++, I'm not clever enough to get that to work
roll_skewness <- function(x, n) {
  # Use rollapply to extract windows and calculate skewness
  skewness_vals <- zoo::rollapply(x, width = n, FUN = function(window) {
    # Calculate skewness using e1071::skewness for the current window
    skewness <- e1071::skewness(window, na.rm = TRUE)
    return(skewness)
  }, fill = NA, align = "right")
    return(skewness_vals)
}


#not in C++, I'm not clever enough to get that to work
roll_kurtosis <- function(x, n) {
  # Use rollapply to extract windows and calculate skewness
  skewness_vals <- zoo::rollapply(x, width = n, FUN = function(window) {
    # Calculate skewness using e1071::skewness for the current window
    skewness <- e1071::kurtosis(window, na.rm = TRUE)
    return(skewness)
  }, fill = NA, align = "right")
  return(skewness_vals)
}


#' hard coding column names for now
#' @noRd
rolling_mean_time_date <- function(date_time_vec,window_size) {
  # Convert to numeric
  numeric_dates <- as.numeric(lubridate::dmy_hms(date_time_vec))
  # Calculate the mean
  mean_numeric <- RcppRoll::roll_mean(numeric_dates,
                                      n = window_size,
                                      fill = NA,
                                      align = "right")
  # Convert back to POSIXct
  mean_date <- lubridate::as_datetime(mean_numeric)
  return(mean_date)
}  

