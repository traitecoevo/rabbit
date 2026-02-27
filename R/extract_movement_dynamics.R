
#' This function takes standardised accelerometer data and caluclates 
#' a set of metrics of movement dynamics across a sliding window of specified number of datapoints. Caluclate metrics include including means, variances, covariances, of xyz, as well as overall dynamic body acceleration (ODBA) and vectorial dynamic body acceleration (VDBA). The function is designed to be fast, using the RcppRoll package to calculate rolling means and sums in C++.
#' 
#' @param df A data frame containing the data.
#' @param window_size An integer specifying the size of the rolling window.
#' @param method A string specifying the method to use for rolling calculations. Options are "fast" (using RcppRoll) or "base" (using pure R implementations). The default is "fast".
#' @return A data frame with movement dynamics metrics calculated across the sliding window.
#' @rdname extract_movement_dynamics
#' @export
#' @examples 
#' #' @examples
#' file_in = system.file("extdata", "raw_Pic2Jan_50000.parquet", package = "rabbit")
#' df <-
#'   standardize_data(file_in = file_in, vars = c("Timestamp", "accX", "accY", "accZ")) |>
#'   extract_movement_dynamics()
extract_movement_dynamics <- function(df, window_size=50, method = c("fast", "base")) {

  method <- match.arg(method)

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
  mean_x <- roll_sum(x, n, method = method[1]) / window_size
  mean_y <- roll_sum(y, n, method = method[1]) / window_size
  mean_z <- roll_sum(z, n, method = method[1]) / window_size
  
  # Calculate variances
  variance_x <- (roll_sum(x_2, n, method = method[1]) - n * mean_x ^ 2) / n
  variance_y <- (roll_sum(y_2, n, method = method[1]) - n * mean_y ^ 2) / n
  variance_z <- (roll_sum(z_2, n, method = method[1]) - n * mean_z ^ 2) / n
  
   # Calculate covariance
  cov_xy <- (roll_sum(x * y, n, method = method[1]) - n * mean_x * mean_y) / n
  cov_xz <- (roll_sum(x * z, n, method = method[1]) - n * mean_x * mean_z) / n
  cov_yz <- (roll_sum(y * z, n, method = method[1]) - n * mean_y * mean_z) / n
   
  # Calculate Overall Dynamic Body Acceleration (ODBA) and Vectorial Dynmic Body Acceleration (VDBA)
  ODBA = abs_x + abs_y + abs_z
  VDBA = sqrt(x_2 + y_2 + z_2)
  
  out <- 
    dplyr::tibble(
      time = roll_date(df$time, n, method = method),
      meanX = mean_x,
      meanY = mean_y,
      meanZ = mean_z,
      maxx = roll_max(x, n, method = method),
      maxy = roll_max(y, n, method = method),
      maxz = roll_max(z, n, method = method),
      minx = roll_min(x, n, method = method),
      miny = roll_min(y, n, method = method),
      minz = roll_min(z, n, method = method),
      sdx = sqrt(variance_x * n / (n-1) ),
      sdy = sqrt(variance_y * n / (n-1) ),
      sdz = sqrt(variance_z * n / (n-1) ),
      SMA = (roll_sum(abs_x, n, method = method) + roll_sum(abs_y, n, method = method) + roll_sum(abs_z, n, method = method))/n,
      minODBA = roll_min(ODBA, n, method = method),
      maxODBA = roll_max(ODBA, n, method = method),
      minVDBA = roll_min(VDBA, n, method = method),
      maxVDBA = roll_max(VDBA, n, method = method),
      sumODBA = roll_sum(ODBA, n, method = method),
      sumVDBA = roll_sum(VDBA, n, method = method),
      meanODBA = roll_sum(ODBA, n, method = method) / n,
      meanVDBA = roll_sum(VDBA, n, method = method) / n,
      corXY = cov_xy / sqrt(variance_x * variance_y),
      corXZ = cov_xz / sqrt(variance_x * variance_z),  
      corYZ = cov_yz / sqrt(variance_y * variance_z),
    
      # Calculate skewness, using formula from https://en.wikipedia.org/wiki/Skewness
      # https://wikimedia.org/api/rest_v1/media/math/render/svg/77a8f1e4f233c410e85698ca11d163f6f81c5e5f
      skx = (roll_mean(x^3, n, method = method) - 3 * mean_x * variance_x - mean_x^3) / sdx ^ 3,
      sky = (roll_mean(y^3, n, method = method) - 3 * mean_y * variance_y - mean_y^3) / sdy ^ 3,    
      skz = (roll_mean(z^3, n, method = method) - 3 * mean_z * variance_z - mean_z^3) / sdz ^ 3
    )

  return(out)
}

# Define functions to use for rolling means
# We use the package RcppRoll, as functions are written in C++
roll_mean <- function(x, n, method = "fast") {
  method <- match.arg(method, c("fast", "base"))
  if (method == "fast") {
    RcppRoll::roll_mean(x,
      n = n, fill = NA, align = "right"
    )
  } else {
    roll_mean_base(x, n)
  }
}

roll_sum <- function(x, n, method = "fast") {
  method <- match.arg(method, c("fast", "base"))
  if (method == "fast") {
    RcppRoll::roll_sum(x,
      n = n, fill = NA, align = "right"
    )
  } else {
    roll_sum_base(x, n)
  }
}

roll_min <- function(x, n, method = "fast") {
  method <- match.arg(method, c("fast", "base"))
  if (method == "fast") {
    RcppRoll::roll_min(x,
      n = n, fill = NA, align = "right"
    )
  } else {
    roll_min_base(x, n)
  }
}

roll_max <- function(x, n, method = "fast") {
  method <- match.arg(method, c("fast", "base"))
  if (method == "fast") {
    RcppRoll::roll_max(x,
      n = n, fill = NA, align = "right"
    )
  } else {
    roll_max_base(x, n)
  }
}

# Assume input is already POSIXct — no need to re-parse
roll_date <- function(x, n, method = "fast") {
  tz <- attr(x, "tzone")
  if (is.null(tz) || length(tz) == 0 || is.na(tz) || tz == "") {
    tz <- "UTC"
  }

  numeric_time <- as.numeric(x)
  mean_time <- roll_mean(numeric_time, n = n, method = method)
  as.POSIXct(mean_time, origin = "1970-01-01", tz = tz)
}

roll_validate_inputs <- function(x, n) {
  if (!is.numeric(x)) {
    stop("`x` must be numeric.")
  }

  if (length(n) != 1 || !is.numeric(n) || is.na(n) || n < 1 || n != as.integer(n)) {
    stop("`n` must be a single positive integer.")
  }

  as.integer(n)
}

roll_sum_base <- function(x, n) {
  n <- roll_validate_inputs(x, n)
  len <- length(x)
  out <- rep(NA_real_, len)

  if (n > len) {
    return(out)
  }

  na_mask <- is.na(x)
  x_no_na <- ifelse(na_mask, 0, x)

  cs <- c(0, cumsum(x_no_na))
  na_cs <- c(0, cumsum(na_mask))

  sums <- cs[(n + 1):(len + 1)] - cs[1:(len - n + 1)]
  na_counts <- na_cs[(n + 1):(len + 1)] - na_cs[1:(len - n + 1)]

  out[n:len] <- ifelse(na_counts > 0, NA_real_, sums)
  out
}

roll_mean_base <- function(x, n) {
  roll_sum_base(x, n) / as.integer(n)
}

roll_min_base <- function(x, n) {
  n <- roll_validate_inputs(x, n)
  len <- length(x)
  out <- rep(NA_real_, len)

  if (n > len) {
    return(out)
  }

  for (index in n:len) {
    window <- x[(index - n + 1):index]
    out[index] <- if (any(is.na(window))) NA_real_ else min(window)
  }

  out
}

roll_max_base <- function(x, n) {
  n <- roll_validate_inputs(x, n)
  len <- length(x)
  out <- rep(NA_real_, len)

  if (n > len) {
    return(out)
  }

  for (index in n:len) {
    window <- x[(index - n + 1):index]
    out[index] <- if (any(is.na(window))) NA_real_ else max(window)
  }

  out
}
