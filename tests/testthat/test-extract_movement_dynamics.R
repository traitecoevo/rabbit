test_that("extract_movement_dynamics computes rolling metrics", {
  df <- data.frame(
    time = as.POSIXct("2020-02-01 00:00:00", tz = "UTC") + 0:4,
    x = 1:5,
    y = 2:6,
    z = 3:7
  )

  out <- extract_movement_dynamics(df, window_size = 3)

  expect_equal(nrow(out), nrow(df))
  expect_true(inherits(out$time, "POSIXct"))

  expect_equal(out$meanX[5], 4)
  expect_equal(out$meanY[5], 5)
  expect_equal(out$meanZ[5], 6)
  expect_equal(out$maxx[5], 5)
  expect_equal(out$minx[5], 3)
  expect_equal(out$meanODBA[5], 15)
  expect_equal(out$corXY[5], 1, tolerance = 1e-12)
})
