test_that("standardise_data renames columns and parses time", {

  expect_no_error({
    
  df <- generate_fake_data(5)

  out <- 
  standardise_data(
    df,
    vars = c("timestamp", "accX", "accY", "accZ"),
    timezone = "UTC"
    )
  })

  expect_equal(names(out), c("time", "x", "y", "z"))
  expect_true(inherits(out$time, "POSIXct"))
  expect_false(any(is.na(out$time)))
  expect_equal(out$x, df$accX)
})

test_that("standardise_data supports file_in", {
  df <- generate_fake_data(4)
  file_in <- tempfile(fileext = ".csv")

  data.table::fwrite(df, file_in)

  out <- standardise_data(
    df = df[0, ],
    file_in = file_in,
    vars = c("timestamp", "accX", "accY", "accZ"),
    timezone = "UTC"
  )

  expect_equal(nrow(out), nrow(df))
})

test_that("standardise_data supports custom parser for ymd timestamp format", {
  df <- make_sample_df(4)
  df$Timestamp <- format(
    lubridate::dmy_hms(df$Timestamp, tz = "UTC"),
    "%Y-%m-%d %H:%M:%S"
  )

  out <- standardise_data(
    df,
    vars = c("Timestamp", "X", "Y", "Z"),
    time_function = lubridate::ymd_hms,
    timezone = "UTC"
  )

  expect_true(inherits(out$time, "POSIXct"))
  expect_false(any(is.na(out$time)))
})

test_that("standardise_data supports custom parser for unix epoch milliseconds", {
  base_time <- as.POSIXct("2020-02-01 00:00:00", tz = "UTC") + seq(0, by = 60, length.out = 4)
  df <- data.frame(
    Timestamp = as.numeric(base_time) * 1000,
    X = 1:4,
    Y = 2:5,
    Z = 3:6
  )

  out <- standardise_data(
    df,
    vars = c("Timestamp", "X", "Y", "Z"),
    time_function = function(x, tz) as.POSIXct(x / 1000, origin = "1970-01-01", tz = tz),
    timezone = "UTC"
  )

  expect_equal(out$time, base_time)
})

test_that("standardise_data does not reparse POSIXct timestamps", {
  base_time <- as.POSIXct("2020-02-01 00:00:00", tz = "UTC") + seq(0, by = 60, length.out = 4)
  df <- data.frame(
    Timestamp = base_time,
    X = 1:4,
    Y = 2:5,
    Z = 3:6
  )

  out <- standardise_data(
    df,
    vars = c("Timestamp", "X", "Y", "Z"),
    timezone = "UTC",
    time_function = function(...) stop("time_function should not be called")
  )

  expect_equal(out$time, base_time)
  expect_true(inherits(out$time, "POSIXct"))
})

test_that("standardize_data is an alias", {
  df <- make_sample_df(2)

  out <- standardize_data(
    df,
    vars = c("Timestamp", "X", "Y", "Z"),
    timezone = "UTC"
  )

  expect_equal(names(out), c("time", "x", "y", "z"))
})
