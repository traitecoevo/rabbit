# Function to calculate mode
calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Function to calculate mode behavior for each window of N minutes
plot_mode_behavior_by_window <- function(data,
                                         time_col,
                                         behavior_col,
                                         window_minutes) {
  data %>%
    dplyr::mutate(time = ymd_hms(!!sym(time_col)),
                  # Ensure the time column is in POSIXct format
                  window = floor_date(time, unit = paste0(window_minutes, " mins"))) %>%
    dplyr::group_by(window) %>%
    dplyr::summarise(mode_behavior = calculate_mode(!!sym(behavior_col))) %>%
    dplyr::filter(!is.na(mode_behavior)) -> mode_sum
  mode_sum %>%
    ggplot2::ggplot(aes(x = window, y = mode_behavior, color = mode_behavior)) +
    geom_point(size = 3) +
    labs(
      x = "Time",
      y = "Mode Behavior",
      title = paste0("Mode Behavior Every ", window_minutes, " Minutes")
    ) +
    theme_minimal() -> plotting
  print(plotting)
  return(mode_sum)
}


classify_behaviors <- function(acc_dat = arrow::read_parquet("tests/testthat/raw_Pic2Jan_50000.parquet"),
                               MSOM_path = "tests/testthat/MSOM_8by7.rda",
                               quiet = FALSE) {
  dat <- moving_window_calcs_2(acc_dat) #this might move out of the function
  dd <- as.matrix(dat[, -1])
  load(file = MSOM_path)
  
  ssom.pred <- kohonen::predict.kohonen(MSOM, newdata = dd, whatmap = 1) # this is slow part
  
  dat$behavior <- ssom.pred$predictions$activity
  if (quiet)
    print(table(dat$behavior))
  
  return(dat)
}
