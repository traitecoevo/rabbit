
df <- arrow::read_parquet("tests/testthat/raw_Pic2Jan_50000.parquet")
dat <- moving_window_calcs_2(df)

load("tests/testthat/MSOM_8by7.rda")

MSOM |> saveRDS("tests/testthat/MSOM_8by7.rds")

# smaller item with less data

MSOM <- readRDS("tests/testthat/MSOM_8by7.rds")

MSOM2 <- MSOM
#i <- 1:1000

i <- 
  tibble(
  i = seq_len(nrow(MSOM2$data$activity)), 
  activities = purrr::map_int(i, 
~which(MSOM2$data$activity[.x,] == 1))
  ) %>% group_by(activities) %>% 
  sample_n(min(n(), 100)) %>% 
  pull(i)


MSOM2$data$measurements <- MSOM$data$measurements[i,]

MSOM2$data$activity <- MSOM$data$activity[i,]


MSOM2$unit.classif <- MSOM$unit.classif[i]
MSOM2$distances <- MSOM$distances[i]

MSOM2 |> saveRDS("tests/testthat/MSOM2.rds")

ssom.pred <- kohonen:::predict.kohonen(MSOM2, newdata = as.matrix(dat[, -1]), whatmap = 1)

