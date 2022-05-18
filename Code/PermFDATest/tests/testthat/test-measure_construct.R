test_that("The calculation of a mean for the fourier coefficient work for the case of the fourier basis", {
  # create the weight function
  w_func <- function(x) {
    return(1)
  }
  # create the fourier basis
  fourier_basis <- fda::create.fourier.basis(rangeval = c(0, 1), nbasis = 15)
  # create a basis_func object for the calculation
  basis_func <- function(x) {
    fda::eval.basis(evalarg = x, basisobj = fourier_basis)[3]
  }
  # calculate the mean of the fourier coefficient
  coef_mean <- fourier_coef_mean(w_func = w_func, basis_func = basis_func, domain = c(0, 1))

  expect_equal(object = round(x = coef_mean, digits = 8), expected = 0)
})

test_that("The calculation of the means of the fourier coefficients works for the case of the fourier basis", {
  # create the weight function
  w_func <- function(x) {
    return(1)
  }
  # calculate means of fourier coefficients
  coef_means <- fourier_basis_coef_means(w_func = w_func, n_basis = 15, domain = c(0, 1))

  expect_equal(object = round(x = coef_means, digits = 8), expected = c(1, rep(0, times = 14)))
})
