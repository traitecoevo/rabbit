test_that("correlation works", {
  x <- rnorm(50)
  y <- rnorm(50)
  test_value <- rolling_correlation(x, y, 50)[50]
  expect_equal(cor(x, y), test_value)
  
  x2 <- rnorm(100)
  y2 <- rnorm(100)
  test_value2 <- rolling_correlation(x2, y2, 50)[100]
  expect_equal(cor(x2[51:100], y2[51:100]), test_value2)
})
