test_that("extract_movement_dynamics computes rolling metrics", {
  df <- data.frame(
    time = as.POSIXct("2020-02-01 00:00:00", tz = "UTC") + 0:4,
    x = 1:5,
    y = 2:6,
    z = 3:7
  )

  expect_no_error(
    out <- extract_movement_dynamics(df, window_size = 3)
  )

  expect_equal(nrow(out), nrow(df))
  expect_true(inherits(out$time, "POSIXct"))

  expect_equal(out$meanX[5], 4)
  expect_equal(out$meanY[5], 5)
  expect_equal(out$meanZ[5], 6)
  expect_equal(out$maxx[5], 5)
  expect_equal(out$minx[5], 3)
  expect_equal(out$meanODBA[5], 15)
  expect_equal(out$corXY[5], 1, tolerance = 1e-6)
})

test_that("roll_sum_base matches roll_sum", {
  x <- rnorm(1000)
  n <- 3

  expect_equal(roll_sum_base(x, n), roll_sum(x, n))
  expect_equal(roll_min_base(x, n), roll_min(x, n))
  expect_equal(roll_max_base(x, n), roll_max(x, n))
 
  expect_error(roll_sum_base(c(x, "NA"), n))
  expect_error(roll_sum_base("not numeric", n))
  expect_error(roll_sum_base(x, "not numeric"))
  expect_error(roll_sum_base(x, NA))
  expect_error(roll_sum_base(x, 0))
})

test_that("extract_movement_dynamics base matches fast", {

  expect_no_error({
    df <- generate_fake_data(50) |>
      standardize_data(vars = c("timestamp", "accX", "accY", "accZ"))

    out_fast <- extract_movement_dynamics(df, window_size = 5, method = "fast")
    out_base <- extract_movement_dynamics(df, window_size = 5, method = "base")
  })

  expect_equal(out_base, out_fast, tolerance = 1e-6)
})

test_that("benchmark_movement_dynamics returns expected structure", {
  expect_no_error({
    df <- generate_fake_data(50) |>
      standardize_data(vars = c("timestamp", "accX", "accY", "accZ"))
  
    out <- benchmark_movement_dynamics(df, window_size = 5, iterations = 2)
  })

  expect_equal(nrow(out), 6)
  expect_true(all(c("task", "method", "iteration", "elapsed_sec") %in% names(out)))
  expect_true(all(out$elapsed_sec >= 0))
})
