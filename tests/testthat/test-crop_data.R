test_that("crop_ends keeps rows between first and last matched times", {
  df <- make_time_df(n = 48)

  expect_no_error(
    out <- crop_ends(df, start_time = "12:00:00", end_time = "13:00:00")
  )

  time <- format(df$time, "%H:%M:%S")
  expected_idx <- min(which(time >= "12:00:00")):max(which(time <= "13:00:00"))

  expect_equal(nrow(out), length(expected_idx))
  expect_equal(out$time, df$time[expected_idx])
  expect_equal(format(out$time[1], "%H:%M:%S"), "12:00:00")
  expect_equal(format(out$time[nrow(out)], "%H:%M:%S"), "13:00:00")
})

test_that("crop_ends uses fallback indices when no times match", {
  df <- make_time_df(n = 48)

  out <- crop_ends(df, start_time = "25:00:00", end_time = "26:00:00")

  expect_equal(nrow(out), 1)
  expect_equal(out$time, df$time[48])
})

test_that("crop_ends wrappers call crop_ends with expected defaults", {
  df <- make_time_df(n = 48)

  out_diurnal <- crop_ends_diurnal(df)
  expected_diurnal <- crop_ends(df, start_time = "01:00:00", end_time = "01:00:00")
  expect_equal(out_diurnal, expected_diurnal)

  out_nocturnal <- crop_ends_nocturnal(df)
  expected_nocturnal <- crop_ends(df, start_time = "12:00:00", end_time = "12:00:00")
  expect_equal(out_nocturnal, expected_nocturnal)
})
