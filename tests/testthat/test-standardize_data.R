test_that("standardize_data renames columns and parses time", {
  df <- make_sample_df(5)

  out <- standardize_data(
    df,
    vars = c("Timestamp", "X", "Y", "Z"),
    timezone = "UTC"
  )

  expect_equal(names(out), c("time", "x", "y", "z"))
  expect_true(inherits(out$time, "POSIXct"))
  expect_false(any(is.na(out$time)))
  expect_equal(out$x, df$X)
})

test_that("standardize_data supports file_in and file_out", {
  df <- make_sample_df(4)
  file_in <- tempfile(fileext = ".csv")
  file_out <- tempfile(fileext = ".csv")

  data.table::fwrite(df, file_in)

  out <- standardize_data(
    df = df[0, ],
    file_in = file_in,
    file_out = file_out,
    vars = c("Timestamp", "X", "Y", "Z"),
    timezone = "UTC"
  )

  expect_equal(nrow(out), nrow(df))
  expect_true(file.exists(file_out))
})

test_that("standardize_data validates inputs", {
  df <- make_sample_df(3)

  expect_error(
    standardize_data(df, vars = c("Timestamp", "X")),
    "vars"
  )

  df$Timestamp[2] <- "bad"
  expect_error(
    suppressWarnings(
      standardize_data(df, vars = c("Timestamp", "X", "Y", "Z"), timezone = "UTC")
    ),
    "could not be processed"
  )
})

test_that("standardise_data is an alias", {
  df <- make_sample_df(2)

  out <- standardise_data(
    df,
    vars = c("Timestamp", "X", "Y", "Z"),
    timezone = "UTC"
  )

  expect_equal(names(out), c("time", "x", "y", "z"))
})
