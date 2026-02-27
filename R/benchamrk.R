
#' Benchmark rolling methods used by movement dynamics
#'
#' Compare elapsed runtime for fast (RcppRoll) and base (pure R) methods for
#' both `roll_sum()` and `extract_movement_dynamics()`.
#'
#' @param df A data frame containing columns `time`, `x`, `y`, `z`.
#' @param window_size An integer specifying the size of the rolling window.
#' @param iterations Number of repeated timings per task/method.
#' @return A tibble with columns `task`, `method`, `iteration`, and `elapsed_sec`.
#' @export
benchmark_movement_dynamics <- function(df, window_size = 50, iterations = 3) {
  if (!all(c("time", "x", "y", "z") %in% names(df))) {
    stop("`df` must contain columns: time, x, y, z.")
  }

  time_expr <- function(expr) {
    unname(system.time(force(expr))["elapsed"])
  }

  rows <- list()
  for (i in seq_len(iterations)) {
    rows[[(i-1)*3+1]] <- data.frame(task = "extract_movement_dynamics", method = "fast", iteration = i, 
                      elapsed_sec = time_expr(extract_movement_dynamics(df, window_size = window_size, method = "fast")))
    rows[[(i-1)*3+2]] <- data.frame(task = "extract_movement_dynamics", method = "base", iteration = i, 
                      elapsed_sec = time_expr(extract_movement_dynamics(df, window_size = window_size, method = "base")))
    rows[[(i-1)*3+3]] <- data.frame(task = "extract_movement_dynamics", method = "orig", iteration = i, 
                      elapsed_sec = time_expr(doAccloop_all(df, window_size)))
  }

  out <- dplyr::bind_rows(rows) |> dplyr::tibble()
  out$method <- factor(out$method, levels = c("fast", "base", "orig"))

  out
}
