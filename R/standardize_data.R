#' Read CSV with Date Checking and Summary Stats
#'
#' This function reads in a CSV file, checks that the date column can be properly processed,
#' and prints out summary statistics about the file, including the number of rows, columns, and
#' a summary of the date column.
#'
#' @param file_path A string representing the path to the CSV file.
#' @param date_column A string representing the name of the date column in the CSV file.
#' @param date_format A string representing the expected format of the date column (e.g., "%Y-%m-%d").
#' 
#' @return A data frame containing the CSV data, with the date column converted to Date type.
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- read_csv_with_dates("data.csv", "date", "%Y-%m-%d")
#' }
read_csv_with_dates <- function(file_path, date_column, date_format) {
  # Check if the file exists
  if (!file.exists(file_path)) {
    stop("The file does not exist.")
  }
  
  # Read in the CSV file
  df <- data.table::fread(file_path)
  
  # Check if the date column exists
  if (!(date_column %in% colnames(df))) {
    stop("The specified date column does not exist in the file.")
  }
  
  # Convert the date column to Date type
  # using base R for now, this will have to switch to lubridate at some point
  df[[date_column]] <- as.Date(df[[date_column]], format = date_format)
  
  # Check if there are any NA values after conversion
  if (any(is.na(df[[date_column]]))) {
    stop("Some dates could not be processed. Please check the date format.")
  }
  
  # Print out summary statistics
  cat("Summary Statistics:\n")
  cat("Number of Rows: ", nrow(df), "\n")
  cat("Number of Columns: ", ncol(df), "\n")
  cat("Date Column Summary:\n")
  print(summary(df[[date_column]]))
  
  # save parquet goes here
  # Return the data frame
  return(df)
}