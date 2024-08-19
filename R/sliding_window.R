#' moving_window_calcs
#'
#' This function takes a data frame and computes sliding window calculations to set up for classification
#'
#' @param df A data frame containing the data.
#' @param window_size An integer specifying the size of the rolling window.
#' @return A data frame with names following doAccloop.R
#' @export
#'

moving_window_calcs <- function(df, window_size=50) {
  
  dat_temp_matrix <- dplyr::tibble(
    time = rolling_mean_time_date(df$Timestamp,
                                  window_size),
    meanX = RcppRoll::roll_mean(df$accX,
                                 n = window_size,
                                 fill = NA,
                                 align = "right"),
    meanY = RcppRoll::roll_mean(df$accY,
                                 n = window_size,
                                 fill = NA,
                                 align = "right"),
    meanZ = RcppRoll::roll_mean(df$accZ,
                                 n = window_size,
                                 fill = NA,
                                 align = "right"),
    maxx = RcppRoll::roll_max(df$accX,
                               n = window_size,
                               fill = NA,
                               align = "right"),
    maxy = RcppRoll::roll_max(df$accY,
                               n = window_size,
                               fill = NA,
                               align = "right"),
    maxz = RcppRoll::roll_max(df$accZ,
                               n = window_size,
                               fill = NA,
                               align = "right"),
    minx = RcppRoll::roll_min(df$accX,
                               n = window_size,
                               fill = NA,
                               align = "right"),
    miny = RcppRoll::roll_min(df$accY,
                               n = window_size,
                               fill = NA,
                               align = "right"),
    minz = RcppRoll::roll_min(df$accZ,
                               n = window_size,
                               fill = NA,
                               align = "right"),
    sdx = RcppRoll::roll_sd(df$accX,
                             n = window_size,
                             fill = NA,
                             align = "right"),
    sdy = RcppRoll::roll_sd(df$accY,
                             n = window_size,
                             fill = NA,
                             align = "right"),
    sdz = RcppRoll::roll_sd(df$accZ,
                             n = window_size,
                             fill = NA,
                             align = "right"),
    SMA = (RcppRoll::roll_sum(abs(df$accX),
                          n = window_size,
                          fill = NA,
                          align = "right") +
           RcppRoll::roll_sum(abs(df$accY),
                          n = window_size,
                          fill = NA,
                          align = "right") +
           RcppRoll::roll_sum(abs(df$accZ),
                          n = window_size,
                          fill = NA,
                          align = "right"))/window_size)
  ODBA <- abs(df$accX) + abs(df$accY) + abs(df$accZ) #direct from doAccloop.R, no change
  VDBA <- sqrt(df$accX^2+df$accY^2+df$accZ^2)  #direct from doAccloop.R, no change
  
  #not sure if this is efficient, but need new line here; alternatively move ODBA and VDBA above
  #to fit with the tibble, allocate all memory at once vibe
  
  dat_temp_matrix$minODBA <- RcppRoll::roll_min(ODBA,
                                              n = window_size,
                                              fill = NA,
                                              align = "right")
  
  dat_temp_matrix$maxODBA <- RcppRoll::roll_max(ODBA,
                                                n = window_size,
                                                fill = NA,
                                                align = "right")
  
  dat_temp_matrix$minVDBA <- RcppRoll::roll_min(VDBA,
                                                n = window_size,
                                                fill = NA,
                                                align = "right")
  
  dat_temp_matrix$maxVDBA <- RcppRoll::roll_max(VDBA,
                                                n = window_size,
                                                fill = NA,
                                                align = "right")
  
  dat_temp_matrix$sumODBA <- RcppRoll::roll_sum(ODBA,
                                                n = window_size,
                                                fill = NA,
                                                align = "right")
  
  dat_temp_matrix$sumVDBA <- RcppRoll::roll_sum(VDBA,
                                                n = window_size,
                                                fill = NA,
                                                align = "right")
  
  dat_temp_matrix$corXY <- roll_cor(df$accX,df$accY,window_size)
  
  dat_temp_matrix$corXZ <- roll_cor(df$accX,df$accZ,window_size)
  
  dat_temp_matrix$corYZ <- roll_cor(df$accY,df$accZ,window_size)
  
  dat_temp_matrix$skx <- roll_skewness(df$accX,window_size)
  
  dat_temp_matrix$sky <- roll_skewness(df$accY,window_size)
  
  dat_temp_matrix$skz <- roll_skewness(df$accZ,window_size)
  
  dat_temp_matrix$kux <- roll_kurtosis(df$accX,window_size)
  
  dat_temp_matrix$kuy <- roll_kurtosis(df$accY,window_size)
  
  dat_temp_matrix$kuz <- roll_kurtosis(df$accZ,window_size)
  
  return(dat_temp_matrix)
}



