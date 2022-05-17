test_that(paste0(
  "Fourier Version of Empirical Distribution Function works for ",
  "Period of 2*pi"
), {

  comparison_function <- c(2, rep(0, times = 13), 0.5)

  # test sample where all functions are always bigger or always smaller
  test_coefs_1 <- t(matrix(
    data = c(
      c(5, rep(0, times = 13), 1),
      c(4, rep(0, times = 13), 1),
      c(0, rep(0, times = 13), 1),
      c(-1, rep(0, times = 13), 1)
    ),
    nrow = 4, ncol = 15, byrow = TRUE
  ))

  dist_1 <- empirical_dist_func_fourier(
    sample = test_coefs_1, func = comparison_function,
    domain = c(0, 2 * pi)
  )

  # add one function that where the difference has zeroes
  test_coefs_2 <- t(matrix(
    data = c(
      c(5, rep(0, times = 13), 1),
      c(4, rep(0, times = 13), 1),
      c(2, rep(0, times = 13), -0.5),
      c(0, rep(0, times = 13), 1),
      c(-1, rep(0, times = 13), 1)
    ),
    nrow = 5, ncol = 15, byrow = TRUE
  ))

  dist_2 <- empirical_dist_func_fourier(
    sample = test_coefs_2, func = comparison_function,
    domain = c(0, 2 * pi)
  )

  # expect similar values
  expect_equal(object = dist_1, expected = 0.5)
  expect_equal(object = dist_2, expected = 0.4)
})

test_that(paste0(
  "Fourier Version of Empirical Distribution Function works for ",
  "Period of 1"
), {

  comparison_function <- c(2, rep(0, times = 13), 0.5)

  # test sample where all functions are always bigger or always smaller
  test_coefs_1 <- t(matrix(
    data = c(
      c(5, rep(0, times = 13), 1),
      c(4, rep(0, times = 13), 1),
      c(0, rep(0, times = 13), 1),
      c(-1, rep(0, times = 13), 1)
    ),
    nrow = 4, ncol = 15, byrow = TRUE
  ))

  dist_1 <- empirical_dist_func_fourier(
    sample = test_coefs_1, func = comparison_function,
    domain = c(0, 1)
  )

  # add one function that where the difference has zeroes
  test_coefs_2 <- t(matrix(
    data = c(
      c(5, rep(0, times = 13), 1),
      c(4, rep(0, times = 13), 1),
      c(2, rep(0, times = 13), -0.5),
      c(0, rep(0, times = 13), 1),
      c(-1, rep(0, times = 13), 1)
    ),
    nrow = 5, ncol = 15, byrow = TRUE
  ))

  dist_2 <- empirical_dist_func_fourier(
    sample = test_coefs_2, func = comparison_function,
    domain = c(0, 1)
  )

  # expect similar values
  expect_equal(object = dist_1, expected = 0.5)
  expect_equal(object = dist_2, expected = 0.4)
})
