test_that("Zero calculation works via fourier coefficients with basis of period 2*pi", {
  func_a <- c(1, 1, 1, rep(0, times = 11), 0.5)

  func_b <- c(1, 1, 1, rep(0, times = 11), -0.5)

  # calculate zeroes of the difference
  zeroes <- fourier_zeroes(func_a = func_a, func_b = func_b, domain = c(0, 2 * pi))

  zeroes_true <- unlist(purrr::map(
    .x = 1:14,
    .f = ~ (pi * .x) / 7 - pi / 14
  ))

  # expect similar values
  expect_equal(object = zeroes, expected = zeroes_true)
})

test_that("Zero calculation works via fourier coefficients with basis of period 1", {
  func_a <- c(1, 1, 1, rep(0, times = 11), 0.5)

  func_b <- c(1, 1, 1, rep(0, times = 11), -0.5)

  # calculate zeroes of the difference
  zeroes <- fourier_zeroes(func_a = func_a, func_b = func_b, domain = c(0, 1))

  zeroes_true <- unlist(purrr::map(
    .x = 1:14,
    .f = ~ (.x) / 14 - 1 / 28
  ))

  # expect similar values
  expect_equal(object = zeroes, expected = zeroes_true)
})

test_that("Zero calculation works via fourier coefficients with basis of period 2*pi if the last coefficients of the difference are zero", {

  func_a <- c(1, 1, 1, rep(0, times = 11), 0.5)

  func_b <- c(1, 1, -1, rep(0, times = 11), 0.5)

  # calculate zeroes of the difference
  zeroes <- fourier_zeroes(func_a = func_a, func_b = func_b, domain = c(0, 2 * pi))

  zeroes_true <- c(pi / 2, 3 * pi / 2)

  # expect similar values
  expect_equal(object = zeroes, expected = zeroes_true)
})

test_that("Zero calculation works via fourier coefficients with basis of period 1 if the last coefficients of the difference are zero", {
  func_a <- c(1, 1, 1, rep(0, times = 11), 0.5)

  func_b <- c(1, 1, -1, rep(0, times = 11), 0.5)

  # calculate zeroes of the difference
  zeroes <- fourier_zeroes(func_a = func_a, func_b = func_b, domain = c(0, 1))

  zeroes_true <- c(1 / 4, 3 / 4)

  # expect similar values
  expect_equal(object = zeroes, expected = zeroes_true)
})

test_that("Function Comparison works via fourier coefficients with basis of period 2*pi", {

  func_a <- c(4, 1, 1, rep(0, times = 11), 0.5)

  func_b <- c(1, 1, 1, rep(0, times = 11), -0.5)

  # check if function a is always bigger
  comp <- func_comparison_fourier(func_a = func_a, func_b = func_b, domain = c(0, 2 * pi))

  # expect similar values
  expect_equal(object = comp, expected = FALSE)
})

test_that("Function Comparison works via fourier coefficients with basis of period 1", {

  func_a <- c(4, 1, 1, rep(0, times = 11), 0.5)

  func_b <- c(1, 1, 1, rep(0, times = 11), -0.5)

  # check if function a is always bigger
  comp <- func_comparison_fourier(func_a = func_a, func_b = func_b, domain = c(0, 1))

  # expect similar values
  expect_equal(object = comp, expected = FALSE)
})

test_that(paste0(
  "Function Comparison works via fourier coefficients with basis of period 2*pi ",
  "when there are zeroes but func_b is still always weakly bigger."
), {

  func_a <- c(0, 0, 0, 0, 1 * sqrt(pi))

  func_b <- c(2 * sqrt(2 * pi), 0, 0, 0, -1 * sqrt(pi))

  # check if function a is always bigger
  comp_1 <- func_comparison_fourier(func_a = func_a, func_b = func_b, domain = c(0, 2 * pi))
  comp_2 <- func_comparison_fourier(func_a = func_b, func_b = func_a, domain = c(0, 2 * pi))

  # expect similar values
  expect_equal(object = comp_1, expected = TRUE)
  expect_equal(object = comp_2, expected = FALSE)
})

test_that(paste0(
  "Function Comparison works via fourier coefficients with basis of period 1 ",
  "when there are zeroes but func_b is still always weakly bigger."
), {

  func_a <- c(0, 0, 0, 0, 1 / sqrt(2))

  func_b <- c(2, 0, 0, 0, -1 / sqrt(2))

  # check if function a is always bigger
  comp_1 <- func_comparison_fourier(func_a = func_a, func_b = func_b, domain = c(0, 1))
  comp_2 <- func_comparison_fourier(func_a = func_b, func_b = func_a, domain = c(0, 1))

  # expect similar values
  expect_equal(object = comp_1, expected = TRUE)
  expect_equal(object = comp_2, expected = FALSE)
})
