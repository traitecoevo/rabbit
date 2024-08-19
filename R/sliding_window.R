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
 
   dat_temp_matrix <- cbind(
        time = rolling_mean_time_date(df$Timestamp),
        meanX <- RcppRoll::roll_mean(df$accX,
                                  n = window_size,
                                  fill = NA,
                                  align = "right"),
        meanY <- RcppRoll::roll_mean(df$accY,
                           n = window_size,
                           fill = NA,
                           align = "right"),
        meanZ <- RcppRoll::roll_mean(df$accZ,
                           n = window_size,
                           fill = NA,
                           align = "right"),
        maxx <- RcppRoll::roll_max(df$accX,
                           n = window_size,
                           fill = NA,
                           align = "right"),
        maxy <- RcppRoll::roll_max(df$accY,
                         n = window_size,
                         fill = NA,
                         align = "right"),
        maxz <- RcppRoll::roll_max(df$accZ,
                         n = window_size,
                         fill = NA,
                         align = "right"),
        minx <- RcppRoll::roll_min(df$accX,
                                   n = window_size,
                                   fill = NA,
                                   align = "right"),
        miny <- RcppRoll::roll_min(df$accY,
                                   n = window_size,
                                   fill = NA,
                                   align = "right"),
        minz <- RcppRoll::roll_min(df$accZ,
                                   n = window_size,
                                   fill = NA,
                                   align = "right"),
        sdx <- RcppRoll::roll_sd(df$accX,
                                   n = window_size,
                                   fill = NA,
                                   align = "right"),
        sdy <- RcppRoll::roll_sd(df$accY,
                                   n = window_size,
                                   fill = NA,
                                   align = "right"),
        sdz <- RcppRoll::roll_sd(df$accZ,
                                   n = window_size,
                                   fill = NA,
                                   align = "right")
   )
   return(dat_temp_matrix)
    # SMA=SMA,  minODBA=minODBA, maxODBA=maxODBA, minVDBA=minVDBA, maxVDBA=maxVDBA, 
    # sumODBA=sumODBA, sumVDBA=sumVDBA, 
    # corXY=corXY, corXZ=corXZ, corYZ=corYZ, 
    # skx=skx,  sky=sky, skz=skz, 
    # kux=kux,  kuy=kuy, kuz=kuz
}


#' hard coding column names for now
#' @noRd
rolling_mean_time_date <- function(date_time_vec) {
  # Convert to numeric
  numeric_dates <- as.numeric(lubridate::dmy_hms(date_time_vec))
  # Calculate the mean
  mean_numeric <- RcppRoll::roll_mean(numeric_dates,na.rm=TRUE)
  # Convert back to POSIXct
  mean_date <- lubridate::as_datetime(mean_numeric)
  return(mean_date)
}  
  
  

#' hard coding column names for now
#' @noRd
moving_window_cors <- function(df, window_size=50) {
  df$cor_xy <- rolling_correlation(df$accX,df$accY,window_size)
  df$cor_xz <- rolling_correlation(df$accX,df$accZ,window_size)
  df$cor_yz <- rolling_correlation(df$accY,df$accZ,window_size)
  return(df)
}