#' Save a CSV or TSV file in parquet format
#'
#' @param output_file path of output file
#' @inheritParams read_csv_with_dates
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- convert_to_parquet("data.csv")
#' }
convert_to_parquet <- function(file_path, output_file = gsub(".csv", ".parquet", file_path, fixed = TRUE), ...){
  
  file_path |> read_csv_with_dates(...) |> arrow::write_parquet(output_file)
}
