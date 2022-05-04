test_that(paste0(
  "Fourier Version of Empirical Distribution Function works for ",
  "Period of 2*pi"
), {
  fourier_basis <- fda::create.fourier.basis(rangeval = c(0, 2 * pi), nbasis = 15)
  comparison_function <- fda::fd(coef = c(2, rep(0, times = 13), 0.5), basisobj = fourier_basis)

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

  test_sample_1 <- purrr::map(
    .x = 1:4,
    .f = ~ fda::fd(coef = test_coefs_1[, .x], basisobj = fourier_basis)
  )

  dist_1 <- empirical_dist_func_fourier(
    sample = test_sample_1, func = comparison_function,
    domain = c(0, 2 * pi), n_fourier_basis = 15
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

  test_sample_2 <- purrr::map(
    .x = 1:5,
    .f = ~ fda::fd(coef = test_coefs_2[, .x], basisobj = fourier_basis)
  )

  dist_2 <- empirical_dist_func_fourier(
    sample = test_sample_2, func = comparison_function,
    domain = c(0, 2 * pi), n_fourier_basis = 15
  )

  # expect similar values
  expect_equal(object = dist_1, expected = 0.5)
  expect_equal(object = dist_2, expected = 0.4)
})

test_that(paste0(
  "Fourier Version of Empirical Distribution Function works for ",
  "Period of 1"
), {
  fourier_basis <- fda::create.fourier.basis(rangeval = c(0, 1), nbasis = 15)
  comparison_function <- fda::fd(coef = c(2, rep(0, times = 13), 0.5), basisobj = fourier_basis)

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

  test_sample_1 <- purrr::map(
    .x = 1:4,
    .f = ~ fda::fd(coef = test_coefs_1[, .x], basisobj = fourier_basis)
  )

  dist_1 <- empirical_dist_func_fourier(
    sample = test_sample_1, func = comparison_function,
    domain = c(0, 1), n_fourier_basis = 15
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

  test_sample_2 <- purrr::map(
    .x = 1:5,
    .f = ~ fda::fd(coef = test_coefs_2[, .x], basisobj = fourier_basis)
  )

  dist_2 <- empirical_dist_func_fourier(
    sample = test_sample_2, func = comparison_function,
    domain = c(0, 1), n_fourier_basis = 15
  )

  # expect similar values
  expect_equal(object = dist_1, expected = 0.5)
  expect_equal(object = dist_2, expected = 0.4)
})

test_that("Grid Version of Empirical Distribution Function works", {
  test_func_1 <- function(x) {
    sin(x)
  }
  test_sample_1 <- list()
})
