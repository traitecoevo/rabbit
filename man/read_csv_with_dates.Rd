% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardize_data.R
\name{read_csv_with_dates}
\alias{read_csv_with_dates}
\title{Read CSV with Date Checking and Summary Stats}
\usage{
read_csv_with_dates(
  file_path,
  date_column,
  timezone = "Australia/Adelaide",
  output_file = gsub(".csv", ".parquet", file_path, fixed = TRUE),
  ...
)
}
\arguments{
\item{file_path}{A string representing the path to the CSV file.}

\item{date_column}{A string representing the name of the date column in the CSV file.}

\item{timezone}{Timezone that the accelerometer is set to}

\item{output_file}{path of output file}

\item{...}{Other arguments to pass into read in function}
}
\value{
A data frame containing the CSV data, with the date column converted to Date type.
}
\description{
This function reads in a CSV file, checks that the date column can be properly processed,
and prints out summary statistics about the file, including the number of rows, columns, and
a summary of the date column.
}
