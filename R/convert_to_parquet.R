#' Save a CSV or TSV file in parquet format
#'
#' @param file path of input file
#' @param output_file path of output file
#' @param delim delimiter (default = "\t")
#'
#' @export
#'
#' @examples
convert_to_parquet <- function(file, output_file = gsub(".csv", ".parquet", file, fixed = TRUE), delim="\t"){
  file |> readr::read_delim(delim=delim) |> arrow::write_parquet(output_file)
}
