


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

roll_skewness <- function(x, n) {
  
  sum_x <- RcppRoll::roll_sum(x,
    n = n,
    fill = NA,
    align = "right")
  
  sum_x_2 <- RcppRoll::roll_sum(x^2,
    n = n,
    fill = NA,
    align = "right")

  mean_x <- sum_x / n
    
  # Calculate variances
  variance_x <- (sum_x_2 - n * mean_x ^ 2) / n
  
  sdx = sqrt(variance_x * n / (n-1) )
  
  skx <- (RcppRoll::roll_mean(x^3,
    n = n,
    fill = NA,
    align = "right") -
        3 * mean_x * variance_x - mean_x^3) / sdx ^ 3
  
  return(skx)
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

