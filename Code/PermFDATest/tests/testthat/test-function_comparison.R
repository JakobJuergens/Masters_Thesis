test_that("Zero calculation works via fourier coefficients with basis of period 2*pi", {
  fourier_basis <- fda::create.fourier.basis(rangeval = c(0, 2*pi), nbasis = 15,
                                             period = 2*pi)

  func_a <- fda::fd(coef = c(1,1,1, rep(0, times = 11), 0.5),
                    basisobj = fourier_basis)

  func_b <- fda::fd(coef = c(1,1,1, rep(0, times = 11), -0.5),
                    basisobj = fourier_basis)

  # calculate zeroes of the difference
  zeroes <- fourier_zeroes(func_a = func_a, func_b = func_b, domain = c(0, 2*pi))

  zeroes_true <- unlist(purrr::map(.x = 1:14,
                            .f = ~ (pi*.x)/7 - pi/14))

  # expect similar values
  expect_equal(object = zeroes, expected = zeroes_true)
})

test_that("Zero calculation works via fourier coefficients with basis of period 1", {
  fourier_basis <- fda::create.fourier.basis(rangeval = c(0, 1), nbasis = 15,
                                             period = 1)

  func_a <- fda::fd(coef = c(1,1,1, rep(0, times = 11), 0.5),
                    basisobj = fourier_basis)

  func_b <- fda::fd(coef = c(1,1,1, rep(0, times = 11), -0.5),
                    basisobj = fourier_basis)

  # calculate zeroes of the difference
  zeroes <- fourier_zeroes(func_a = func_a, func_b = func_b, domain = c(0, 1))

  zeroes_true <- unlist(purrr::map(.x = 1:14,
                                   .f = ~ (.x)/14 - 1/28))

  # expect similar values
  expect_equal(object = zeroes, expected = zeroes_true)
})

test_that("Function Comparison works via fourier coefficients with basis of period 2*pi", {
  fourier_basis <- fda::create.fourier.basis(rangeval = c(0, 2*pi), nbasis = 15,
                                             period = 2*pi)

  func_a <- fda::fd(coef = c(4,1,1, rep(0, times = 11), 0.5),
                    basisobj = fourier_basis)

  func_b <- fda::fd(coef = c(1,1,1, rep(0, times = 11), -0.5),
                    basisobj = fourier_basis)

  # check if function a is always bigger
  comp <- func_comparison_fourier(func_a = func_a, func_b = func_b, domain = c(0, 2*pi))

  # expect similar values
  expect_equal(object = comp, expected = FALSE)
})

test_that("Function Comparison works via fourier coefficients with basis of period 1", {
  fourier_basis <- fda::create.fourier.basis(rangeval = c(0, 1), nbasis = 15,
                                             period = 1)

  func_a <- fda::fd(coef = c(4,1,1, rep(0, times = 11), 0.5),
                    basisobj = fourier_basis)

  func_b <- fda::fd(coef = c(1,1,1, rep(0, times = 11), -0.5),
                    basisobj = fourier_basis)

  # check if function a is always bigger
  comp <- func_comparison_fourier(func_a = func_a, func_b = func_b, domain = c(0, 1))

  # expect similar values
  expect_equal(object = comp, expected = FALSE)
})

test_that(paste0("Function Comparison works via fourier coefficients with basis of period 2*pi ",
                 "when there are zeroes but func_b is still always weakly bigger."), {
                   fourier_basis <- fda::create.fourier.basis(rangeval = c(0, 2*pi), nbasis = 5,
                                                              period = 2*pi)

                   func_a <- fda::fd(coef = c(0,0,0,0,1*sqrt(pi)),
                                     basisobj = fourier_basis)

                   func_b <- fda::fd(coef = c(2*sqrt(2*pi),0,0,0,-1*sqrt(pi)),
                                     basisobj = fourier_basis)

                   # check if function a is always bigger
                   comp_1 <- func_comparison_fourier(func_a = func_a, func_b = func_b, domain = c(0, 2*pi))
                   comp_2 <- func_comparison_fourier(func_a = func_b, func_b = func_a, domain = c(0, 2*pi))

                   # expect similar values
                   expect_equal(object = comp_1, expected = TRUE)
                   expect_equal(object = comp_2, expected = FALSE)
                 })

test_that(paste0("Function Comparison works via fourier coefficients with basis of period 1 ",
                 "when there are zeroes but func_b is still always weakly bigger."), {
  fourier_basis <- fda::create.fourier.basis(rangeval = c(0, 1), nbasis = 5,
                                             period = 1)

  func_a <- fda::fd(coef = c(0,0,0,0,1/sqrt(2)),
                    basisobj = fourier_basis)

  func_b <- fda::fd(coef = c(2,0,0,0,-1/sqrt(2)),
                    basisobj = fourier_basis)

  # check if function a is always bigger
  comp_1 <- func_comparison_fourier(func_a = func_a, func_b = func_b, domain = c(0, 1))
  comp_2 <- func_comparison_fourier(func_a = func_b, func_b = func_a, domain = c(0, 1))

  # expect similar values
  expect_equal(object = comp_1, expected = TRUE)
  expect_equal(object = comp_2, expected = FALSE)
})

