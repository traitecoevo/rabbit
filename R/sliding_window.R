#' Calculate Rolling Mean and SD for a Specified Column
#'
#' This function takes a data frame and computes the rolling mean for a specified column using a specified window size.
#' The resulting rolling mean values are added as a new column in the data frame.
#'
#' @param df A data frame containing the data.
#' @param column_name A string specifying the name of the column for which the rolling mean is to be calculated.
#' @param window_size An integer specifying the size of the rolling window.
#' @return A data frame with an additional column containing the rolling mean values.
#' The new column will be named as `column_name_rolling_mean_window_size`.
#' @importFrom RcppRoll roll_mean
#' @export
#' @examples
#' df <- data.frame(Time = 1:10, Value = c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19))
#' df <- calculate_rolling_mean(df, "Value", 3)
#' print(df)

calculate_rolling_mean <- function(df, column_name, window_size) {
  # Check if the specified column exists
  if (!(column_name %in% colnames(df))) {
    stop("The specified column does not exist in the data frame.")
  }
  
  # Calculate the rolling mean
  rolling_mean <- roll_mean(df[[column_name]], n = window_size, fill = NA)
  rolling_sd <- roll_sd(df[[column_name]], n = window_size, fill = NA)
  
  # Create a new column with the rolling mean
  new_column_name <- paste0(column_name, "_rolling_mean_", window_size)
  new_column_name_2 <- paste0(column_name, "_rolling_sd_", window_size)
  df[[new_column_name]] <- rolling_mean
  df[[new_column_name_2]] <- rolling_sd
  
  # Return the updated data frame
  return(df)
}
