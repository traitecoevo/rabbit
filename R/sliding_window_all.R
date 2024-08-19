

moving_window_calcs_df <- function(df, window_size=50) {

  x <- df$accX
  y <- df$accY
  z <- df$accZ

  abs_x <- abs(x)
  abs_y <- abs(y)
  abs_z <- abs(z)

  x_2 <- x^2
  y_2 <- y^2
  z_2 <- z^2

  # Calculate rolling sums
  sum_x <- RcppRoll::roll_sum(x,
                              n = window_size,
                              fill = NA,
                              align = "right")
  sum_y <- RcppRoll::roll_sum(y,
                              n = window_size,
                              fill = NA,
                              align = "right")
  sum_z <- RcppRoll::roll_sum(z,
                              n = window_size,
                              fill = NA,
                              align = "right")
  
  # Calculate rolling sums of squares
  sum_x_2 <- RcppRoll::roll_sum(x_2,
                                 n = window_size,
                                 fill = NA,
                                 align = "right")
  sum_y_2 <- RcppRoll::roll_sum(y_2,
                                 n = window_size,
                                 fill = NA,
                                 align = "right")
  sum_z_2 <- RcppRoll::roll_sum(z_2,
                                 n = window_size,
                                 fill = NA,
                                 align = "right")
  
  # Calculate means
  mean_x <- sum_x / window_size
  mean_y <- sum_y / window_size
  mean_z <- sum_z / window_size
    
  # Calculate variances
  variance_x <- (sum_x_2 - window_size * mean_x ^ 2) / window_size
  variance_y <- (sum_y_2 - window_size * mean_y ^ 2) / window_size
  variance_z <- (sum_z_2 - window_size * mean_z ^ 2) / window_size
                   

  # Calculate rolling sums of quads
  sum_x_4 <- RcppRoll::roll_sum(x ^ 4,
                                 n = window_size,
                                 fill = NA,
                                 align = "right")
  
  sum_y_4 <- RcppRoll::roll_sum(y ^ 4,
                                 n = window_size,
                                 fill = NA,
                                 align = "right")
  
  sum_z_4 <- RcppRoll::roll_sum(z ^ 4,
                                 n = window_size,
                                 fill = NA,
                                 align = "right")
                                              
  # Calculate rolling sum of products
  sum_xy <- RcppRoll::roll_sum(x * y,
                               n = window_size,
                               fill = NA,
                               align = "right")
  
  sum_xz <- RcppRoll::roll_sum(x * z,
                               n = window_size,
                               fill = NA,
                               align = "right")
  
  sum_yz <- RcppRoll::roll_sum(y * z,
                               n = window_size,
                               fill = NA,
                               align = "right")

  
   # Calculate covariance
  cov_xy <- (sum_xy - window_size * mean_x * mean_y) / window_size
  cov_xz <- (sum_xz - window_size * mean_x * mean_z) / window_size
  cov_yz <- (sum_yz - window_size * mean_y * mean_z) / window_size
   
  # ???
  ODBA = abs_x + abs_y + abs_z

  VDBA = sqrt(x_2 + y_2 + z_2)

  out <- 
  dplyr::tibble(
    time = rolling_mean_time_date(df$Timestamp,
                                  window_size)
    ) %>% 
  dplyr::mutate(
    meanX = mean_x,
    meanY = mean_y,
    meanZ = mean_z,

    maxx = RcppRoll::roll_max(x,
                               n = window_size,
                               fill = NA,
                               align = "right"),
    maxy = RcppRoll::roll_max(y,
                               n = window_size,
                               fill = NA,
                               align = "right"),
    maxz = RcppRoll::roll_max(z,
                               n = window_size,
                               fill = NA,
                               align = "right"),
    minx = RcppRoll::roll_min(x,
                               n = window_size,
                               fill = NA,
                               align = "right"),
    miny = RcppRoll::roll_min(y,
                               n = window_size,
                               fill = NA,
                               align = "right"),
    minz = RcppRoll::roll_min(z,
                               n = window_size,
                               fill = NA,
                               align = "right"),
    sdx = sqrt(variance_x * window_size / (window_size-1) ),
    sdy = sqrt(variance_y * window_size / (window_size-1) ),
    sdz = sqrt(variance_z * window_size / (window_size-1) ),

    SMA = (RcppRoll::roll_sum(abs_x,
                          n = window_size,
                          fill = NA,
                          align = "right") +
           RcppRoll::roll_sum(abs_y,
                          n = window_size,
                          fill = NA,
                          align = "right") +
           RcppRoll::roll_sum(abs_z,
                          n = window_size,
                          fill = NA,
                          align = "right"))/window_size,
    
    minODBA = RcppRoll::roll_min(ODBA,
                          n = window_size,
                          fill = NA,
                          align = "right"),

    maxODBA = RcppRoll::roll_max(ODBA,
                          n = window_size,
                          fill = NA,
                          align = "right"),
  
    minVDBA = RcppRoll::roll_min(VDBA,
                          n = window_size,
                          fill = NA,
                          align = "right"),
  
    maxVDBA = RcppRoll::roll_max(VDBA,
                          n = window_size,
                          fill = NA,
                          align = "right"),
  
    sumODBA = RcppRoll::roll_sum(ODBA,
                          n = window_size,
                          fill = NA,
                          align = "right"),
  
    sumVDBA = RcppRoll::roll_sum(VDBA,
                          n = window_size,
                          fill = NA,
                          align = "right"),
  
    corXY = cov_xy / sqrt(variance_x * variance_y),
    corXZ = cov_xz / sqrt(variance_x * variance_z),  
    corYZ = cov_yz / sqrt(variance_y * variance_z),
  
    # skx = roll_skewness(x, window_size),
    # sky = roll_skewness(y, window_size),
    # skz = roll_skewness(z, window_size),
  
    # Calculate skewness, using formula from https://en.wikipedia.org/wiki/Skewness
    # https://wikimedia.org/api/rest_v1/media/math/render/svg/77a8f1e4f233c410e85698ca11d163f6f81c5e5f
    skx = (RcppRoll::roll_mean(x^3,
                          n = window_size,
                          fill = NA,
                          align = "right") -
            3 * mean_x * variance_x - mean_x^3) / sdx ^ 3,
    
    sky = (RcppRoll::roll_mean(y^3,
                          n = window_size,
                          fill = NA,
                          align = "right") -
            3 * mean_y * variance_y - mean_y^3) / sdy ^ 3,
    
    skz = (RcppRoll::roll_mean(z^3,
                          n = window_size,
                          fill = NA,
                          align = "right") -
            3 * mean_z * variance_z - mean_z^3) / sdz ^ 3,

    kux = roll_kurtosis(x, window_size),
    kuy = roll_kurtosis(y, window_size),
    kuz = roll_kurtosis(z, window_size)
  )

  return(out)
}


