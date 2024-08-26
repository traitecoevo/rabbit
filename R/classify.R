
classify_and_plot<-function(){
library(arrow)
library(rabbit)
aa<-read_parquet("tests/testthat/raw_Pic2Jan_50000.parquet")

dat<-moving_window_calcs_2(aa)
load(file = "MSOM_8by7.rda")
dd<-as.matrix(dat[,-1])

ssom.pred <- predict(MSOM, newdata = dd, whatmap = 1)
tibble(time_date=dat$time,ssom.pred$predictions)
table(ssom.pred$predictions$activity)
dat$behavior<-ssom.pred$predictions$activity
table(dat$behavior)
summary(dat$time)
ggplot(dat,aes(x=time,y=behavior,col=behavior))+geom_point(size=1,alpha=0.1)


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
    ggplot(aes(x = window, y = mode_behavior, color = mode_behavior)) +
    geom_point(size = 3) +
    labs(x = "Time", y = "Mode Behavior", title = paste0("Mode Behavior Every ", window_minutes, " Minutes")) +
    theme_minimal()
}

mode_behavior_by_window(dat, "time", "behavior", 0.1)
}