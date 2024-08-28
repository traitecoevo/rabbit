#' Read CSV with Date Checking and Summary Stats
#'
#' This function reads in a CSV file, checks that the date column can be properly processed,
#' and prints out summary statistics about the file, including the number of rows, columns, and
#' a summary of the date column.
#'
#' @param file_path A string representing the path to the CSV file.
#' @param date_column A string representing the name of the date column in the CSV file.
#' @param timezone Timezone that the accelerometer is set to
#' @param ... Other arguments to pass into read in function
#' @param output_file path of output file
#' @rdname read_csv_with_dates
#' @return A data frame containing the CSV data, with the date column converted to Date type.
#' @export
#'


read_csv_with_dates <- function(file_path, date_column, 
                                timezone="Australia/Adelaide", 
                                output_file = gsub(".csv", ".parquet", file_path, fixed = TRUE), ...) {
  # Check if the file exists
  if (!file.exists(file_path)) {
    stop("The file does not exist.")
  }
  
  # Read in the CSV file
  df <- data.table::fread(file_path, ...)
  
  # Check if the date column exists
  if (!(date_column %in% colnames(df))) {
    stop("The specified date column does not exist in the file.")
  }
  
  # Convert the date column to POSIXct (date-time) type using lubridate
  df[[date_column]] <- lubridate::mdy_hms(df[[date_column]], tz = timezone)
  
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
  
  # Save the data as a Parquet file
  arrow::write_parquet(df, output_file)
  cat("File saved as parquet format for future use:", output_file, "\n")
  
  # Return the data frame
  return(df)
}

read_parquet_process_datetime <- function(file_path,timezone="Australia/Adelaide") {
  df <- arrow::read_parquet(file_path)
  df$Timestamp <- lubridate::dmy_hms(df$Timestamp, tz = timezone)
  return(df)
}


