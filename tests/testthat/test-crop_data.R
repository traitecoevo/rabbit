test_that("crop_times trims to matching time window", {
  df <- make_time_df(n = 48)

  # any times
  expect_no_error(
    out <- crop_times(df, start_time = "12:00:00", end_time = "13:00:00")
  )

  time <- out$time |> format("%H:%M:%S")
  expect_true(all(time >= "12:00:00"))
  expect_true(all(time <= "13:00:00"))

  # dirunal
  expect_no_error(
    out <- crop_diurnal(df)
  )
  time <- out$time |> format("%H:%M:%S")
  expect_true(all(time >= "06:00:00"))
  expect_true(all(time <= "18:00:00"))
  
  expect_no_error(
    out <- crop_diurnal(df, start_time = "11:00", end_time = "13:00")
  )
  time <- out$time |> format("%H:%M:%S")
  expect_true(all(time >= "11:00:00"))
  expect_true(all(time <= "13:00:00"))
  
  ## nocturnal
  expect_no_error(
    out <- crop_nocturnal(df)
  )
  time <- out$time |> format("%H:%M:%S")
  expect_true(all((time >= "18:00:00") | (time <= "06:00:00")))

  expect_no_error(
    out <- crop_nocturnal(df, start_time = "22:00:00", end_time = "04:00:00")
  )
  time <- out$time |> format("%H:%M:%S")
  expect_true(all((time >= "22:00:00") | (time <= "04:00:00")))

})
