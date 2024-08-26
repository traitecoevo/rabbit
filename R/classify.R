
classify_and_plot<-function(window_minutes){
library(arrow)
library(rabbit)
aa<-read_parquet("tests/testthat/raw_Pic2Jan_50000.parquet")

dat<-moving_window_calcs_2(aa)
load(file = "MSOM_8by7.rda")
dd<-as.matrix(dat[,-1])

ssom.pred <- predict(MSOM, newdata = dd, whatmap = 1)

dat$behavior<-ssom.pred$predictions$activity
print(table(dat$behavior))

# Function to calculate mode
calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Function to calculate mode behavior for each window of N minutes
mode_behavior_by_window <- function(data, time_col, behavior_col, window_minutes) {
  data %>%
    mutate(time = ymd_hms(!!sym(time_col)),  # Ensure the time column is in POSIXct format
           window = floor_date(time, unit = paste0(window_minutes, " mins"))) %>%
    group_by(window) %>%
    summarise(mode_behavior = calculate_mode(!!sym(behavior_col))) %>%
    filter(!is.na(mode_behavior)) ->mode_sum
    mode_sum %>%
    ggplot(aes(x = window, y = mode_behavior, color = mode_behavior)) +
    geom_point(size = 3) +
    labs(x = "Time", y = "Mode Behavior", title = paste0("Mode Behavior Every ", window_minutes, " Minutes")) +
    theme_minimal()->plotting
    print(plotting)
  return(mode_sum)
}

mode_sum<-mode_behavior_by_window(dat, "time", "behavior", window_minutes)
return(mode_sum)
}
