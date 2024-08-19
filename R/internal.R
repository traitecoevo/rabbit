

rolling_correlation <- function(x, y, window_size) {
  n <- length(x)
  
  # Calculate rolling sums
  sum_x <- RcppRoll::roll_sum(x, n = window_size, fill = NA, align = "right")
  sum_y <- RcppRoll::roll_sum(y, n = window_size, fill = NA, align = "right")
  
  # Calculate rolling sums of squares
  sum_x_sq <- RcppRoll::roll_sum(x^2, n = window_size, fill = NA, align = "right")
  sum_y_sq <- RcppRoll::roll_sum(y^2, n = window_size, fill = NA, align = "right")
  
  # Calculate rolling sum of products
  sum_xy <- RcppRoll::roll_sum(x * y, n = window_size, fill = NA, align = "right")
  
  # Calculate means
  mean_x <- sum_x / window_size #could use roll_mean here
  mean_y <- sum_y / window_size #could use roll_mean here
  
  # Calculate covariance
  covariance <- (sum_xy - window_size * mean_x * mean_y) / window_size
  
  # Calculate variances
  variance_x <- (sum_x_sq - window_size * mean_x^2) / window_size
  variance_y <- (sum_y_sq - window_size * mean_y^2) / window_size
  
  # Calculate correlation
  correlation <- covariance / sqrt(variance_x * variance_y)
  
  return(correlation)
}
