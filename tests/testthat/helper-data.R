make_sample_df <- function(n = 10, start = "01/02/2020 00:00:00", tz = "UTC") {
  time_posix <- lubridate::dmy_hms(start, tz = tz) + seq(0, by = 3600, length.out = n)

  data.frame(
    Timestamp = format(time_posix, "%d/%m/%Y %H:%M:%S"),
    X = seq_len(n),
    Y = seq_len(n) * 2,
    Z = seq_len(n) * 3,
    stringsAsFactors = FALSE
  )
}

make_time_df <- function(n = 48, start = "2020-02-01 00:00:00", tz = "UTC") {
  time <- as.POSIXct(start, tz = tz) + seq(0, by = 3600, length.out = n)

  data.frame(
    time = time,
    x = seq_len(n),
    y = seq_len(n),
    z = seq_len(n)
  )
}
