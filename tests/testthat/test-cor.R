test_that("correlation works", {
  x <- rnorm(50)
  y <- rnorm(50)
  test_value <- roll_cor(x, y, 50)[50]
  expect_equal(cor(x, y), test_value)
  
  x2 <- rnorm(100)
  y2 <- rnorm(100)
  test_value2 <- roll_cor(x2, y2, 50)[100]
  expect_equal(cor(x2[51:100], y2[51:100]), test_value2)
})


#possibly trival test, given how current skewness function works
test_that("skewness works", {
  x <- rnorm(50)
  test_value <- roll_skewness(x, 50)[50]
  expect_equal(e1071::skewness(x), test_value)
  
  x2 <- rnorm(100)
  test_value2 <- roll_skewness(x2, 50)[100]
  expect_equal(e1071::skewness(x2[51:100]), test_value2)
})
