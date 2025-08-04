#' Step 2
#'
#' This function Crops the start and end of each file to a specified time
#'
#' @param path A string representing the path to the .parquet files
#' @param df A dataframe containing the data. 
#' @param start_time The desired start time, uses the same end time. Defaults to 12:00:00
#' @rdname read_parquet_crop_files A function to crop the start and end of each file
#' 

library(dplyr)
library(lubridate)
library(tibble)

read_parquet_crop_files <- function(df, start_time = "12:00:00") {
  df <- df %>%
    mutate(TimeOnly = format(time, "%H:%M:%S"))
  
  matching_times <- df$time[df$TimeOnly == start_time]
  
  first_time <- min(matching_times, na.rm = TRUE)
  last_time <- max(matching_times, na.rm = TRUE)
  
  df %>%
    filter(time >= first_time & time <= last_time) %>%
    select(-TimeOnly)
}

# Create an empty table
summary_tibble <- tibble()

# List all parquet files in folder
parquet_files <- list.files(path = "", 
                            pattern = "*.parquet", full.names = TRUE)

# Loop through each .parquet file, crop files, display summary in tibble table and save
for(parquet_file in parquet_files) {
  # Load the data
  df <- arrow::read_parquet(parquet_file)
  df[complete.cases(df[,c("time")]), ]
  gc()  # Trigger garbage collection after reading the file
  
  # Crop each file
  result2 <- read_parquet_crop_files(df)
  gc()  # Trigger garbage collection after calculations
  
  # Extract first and lsat rows
  first_row <- result2[1, ]
  last_row <- result2[nrow(result2), ]
  
  # Combine first and last rows into summary tibble
  summary_tibble <- bind_rows(summary_tibble, first_row, last_row)
  
  # Generate the new filename
  filename <- basename(parquet_file)
  new_filename <- sub(".parquet", "_crop.parquet", filename)
  
  # Save the result to the Calculations folder
  arrow::write_parquet(result2, paste0("Cropped/", new_filename))
  gc()  # Trigger garbage collection after saving the file
  
  print(summary_tibble)
}

