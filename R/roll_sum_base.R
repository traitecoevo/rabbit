
# Pure-R rolling sum equivalent (right aligned, fill = NA)
roll_sum_base <- function(x, n) {
  if (!is.numeric(x)) {
    stop("`x` must be numeric.")
  }

  if (length(n) != 1 || !is.numeric(n) || is.na(n) || n < 1 || n != as.integer(n)) {
    stop("`n` must be a single positive integer.")
  }

  n <- as.integer(n)
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

roll_sum_mean <- function(x, n) {
  roll_sum_base(x, n) / n
}

# Pure-R rolling sum equivalent (right aligned, fill = NA)
roll_min_base <- function(x, n) {
  if (!is.numeric(x)) {
    stop("`x` must be numeric.")
  }

  if (length(n) != 1 || !is.numeric(n) || is.na(n) || n < 1 || n != as.integer(n)) {
    stop("`n` must be a single positive integer.")
  }

  n <- as.integer(n)
  len <- length(x)
  out <- rep(NA_real_, len)

  if (n > len) {
    return(out)
  }

  na_mask <- is.na(x)
  x_no_na <- ifelse(na_mask, Inf, x)

  cs <- c(0, cumsum(x_no_na))
  na_cs <- c(0, cumsum(na_mask))

  mins <- sapply(1:(len - n + 1), function(i) min(x_no_na[i:(i + n - 1)]))
  
  out[n:len] <- mins
  out
}

# Pure-R rolling max equivalent (right aligned, fill = NA)
roll_max_base <- function(x, n) {
  if (!is.numeric(x)) {
    stop("`x` must be numeric.")
  }

  if (length(n) != 1 || !is.numeric(n) || is.na(n) || n < 1 || n != as.integer(n)) {
    stop("`n` must be a single positive integer.")
  }

  n <- as.integer(n)
  len <- length(x)
  out <- rep(NA_real_, len)

  if (n > len) {
    return(out)
  }

  na_mask <- is.na(x)
  x_no_na <- ifelse(na_mask, -Inf, x)

  cs <- c(0, cumsum(x_no_na))
  na_cs <- c(0, cumsum(na_mask))

  maxs <- sapply(1:(len - n + 1), function(i) max(x_no_na[i:(i + n - 1)]))
  
  out[n:len] <- maxs
  out
}
