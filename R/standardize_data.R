#' Standardize accelerometer data
#'
#' This function accpets either a dataframe or filename (*.csv or *.parquet), checks that the time and x,y,z columncan be properly processed,
#' and prints out summary statistics about the data. 
#' @param df A data frame containing the data
#' @param file_in A string representing the path to the input file (CSV or parquet).
#' @param file_out path where the output file should be saved. If NULL, the data will not be saved to a file. The output format will be determined by the file extension (CSV or parquet).
#' @param vars A character vector of length 4 specifying the names of the columns in df that contain the variables `time`, `X`, `Y`, `Z` from the  accelerometer. The default is c("Timestamp","X","Y","Z").
#' @param timezone Timezone of where accelerometer was used
#' @param ... Other arguments to pass into read in function
#' @rdname standardize_data 
#' @return A data frame containing the standardised data, with the time column converted to POSIXct type.
#' @export
#' @examples
#' file_in = system.file("extdata", "raw_Pic2Jan_50000.parquet", package = "rabbit")
#' df <-
#'   standardize_data(file_in = file_in, vars = c("Timestamp", "accX", "accY", "accZ"))

standardize_data <- function(df, 
                                file_in = NULL, file_out = NULL, 
                                vars = c("Timestamp","X","Y","Z"), 
                                timezone="Australia/Adelaide", 
                                ...) {

  # Read in the CSV file
  if(!is.null(file_in)) {
    # Check if the file exists
    if (!file.exists(file_in)) {
      stop("The file does not exist.")
    }

    df <- switch(tools::file_ext(file_in),
      "csv" = data.table::fread(file_in, ...),
      "parquet" = arrow::read_parquet(file_in, ...)
      )
  }

  if (!is.character(vars) || length(vars) != 4) {
    stop("`vars` must be a character vector of length 4 naming timestamp, x, y, and z columns.")
  }
  
  # Check if the date column exists
  if (!(vars[1] %in% colnames(df))) {
    stop("The specified timestamp column does not exist in the file.")
  }

  missing_vars <- setdiff(vars, colnames(df))
  if (length(missing_vars) > 0) {
    stop("The following vars columns do not exist in the data: ", paste(missing_vars, collapse = ", "))
  }

  df <- dplyr::rename(
    df,
    dplyr::all_of(c(
      time = vars[1],
      x = vars[2],
      y = vars[3],
      z = vars[4]
    ))
  )
  
  # Convert the date column to POSIXct (date-time) type using lubridate
  df$time <- lubridate::dmy_hms(df$time, tz = timezone)

  # Check if there are any NA values after conversion
  if (any(is.na(df$time))) {
    stop("Some dates could not be processed. Please check the date format.")
  }
  
  if(!is.null(file_out)) {
    switch(tools::file_ext(file_out),
      "csv" = data.table::fwrite(df, file_out, ...),
      "parquet" = arrow::write_parquet(df, file_out, ...)
    )
    cat("File saved as:", file_out, "\n")
  }

  # Return the data frame
  return(df)
}

#' @rdname standardize_data
#' @export
standardise_data <- standardize_data
