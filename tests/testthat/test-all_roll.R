test_that("whole thing doesn't error out", {
  df <- arrow::read_parquet("../../data-raw/raw_Pic2Jan_50000.parquet")
  expect_type(moving_window_calcs(df),"list")
})