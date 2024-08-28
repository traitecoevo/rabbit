
# This script creates a version of the MSOM object with much smaller size, for icnlusion in the package

# The original file is 13Mb, which is too big to include

# Load original file and convert to rds
load("tests/testthat/MSOM_8by7.rda")
MSOM |> saveRDS("tests/testthat/MSOM_8by7.rds")

# Create a smaller item, by reducing the amount of data

# Load full sized object
MSOM <- readRDS("tests/testthat/MSOM_8by7.rds")
nrow(MSOM$data$measurements) 
# 115529

# Split data by behaviors, choose 200 rows of data for each
i <- 
  tibble(
  ii = seq_len(nrow(MSOM2$data$activity)), 
  
  # in the matrix of activities, find which column is == 1 in each row
  activities = purrr::map_int(ii, 
~which(MSOM2$data$activity[.x,] == 1))
  ) %>% 
  # choose 100 random rows within each
  group_by(activities) %>% 
  sample_n(min(n(), 100)) %>% 
  pull(ii)

# now choose the selected rows to create a new object
MSOM2 <- MSOM
MSOM2$data$measurements <- MSOM$data$measurements[i,]
MSOM2$data$activity <- MSOM$data$activity[i,]
MSOM2$unit.classif <- MSOM$unit.classif[i]
MSOM2$distances <- MSOM$distances[i]

nrow(MSOM2$data$measurements) # 1233

# check it runs
df <- arrow::read_parquet("tests/testthat/raw_Pic2Jan_50000.parquet")
dat <- moving_window_calcs_2(df)
ssom.pred <- kohonen:::predict.kohonen(MSOM2, newdata = as.matrix(dat[, -1]), whatmap = 1)

# Save
MSOM2 |> saveRDS("tests/testthat/MSOM_8by7_small.rds")
