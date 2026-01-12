
# Compare results of this package to results from original function

source("doAccloop.R")

test_that("whole thing doesn't error out and matches older version", {
  if (file.exists("raw_Pic2Jan_50000.parquet")) {
    df <- arrow::read_parquet("raw_Pic2Jan_50000.parquet")
    expect_s3_class(df, "data.frame")
  } else {
    skip("File raw_Pic2Jan_50000.parquet not found")
  }
  
  new <- sliding_window(df[1:50,])
  
  expect_type(new, "list")
  
  # Comparee new and old caluclations, 
  # excluding the date column that we're handling different on purpose
  # Try several startign points
  window_size <- 50
  
  # check for some specific rows
  for(i in c(1, 101, 151)) {
    ii <- seq(i, i+49)
    new <- sliding_window(df[ii,])[50, -1]
    old <- doAccloop(as.matrix(df[ii,c(6,3:5)]))[,-1]
  
    expect_equal(new, old, tolerance=0.0001) 
  }

  # Bigger check 

  df2 <- df[1:300, ]

  old <- doAccloop_all(df2)
  new <- sliding_window(df2)[-(1:49),-1]

  ii <- 49:249
  expect_equal(new[ii,], old[ii,], tolerance=0.0001)
  
  MSOM <- readRDS("MSOM_8by7_small.rds")
  expect_type(nighttime_activities <- classify_behaviors(sliding_window(df), MSOM,quiet=TRUE),"list")
})
