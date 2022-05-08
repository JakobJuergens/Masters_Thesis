test_that("The calculation of a mean for the fourier coefficient work for the case of the fourier basis",{
  # create the weight function
  w_func <- function(x){return(1)}
  # create the fourier basis
  fourier_basis <- fda::create.fourier.basis(rangeval = c(0,1), nbasis = 15)
  # create a basis_func object for the calculation
  basis_func <- function(x){
    fda::eval.basis(evalarg = x, basisobj = fourier_basis)[3]
  }
  # calculate the mean of the fourier coefficient
  coef_mean <- fourier_coef_mean(w_func = w_func, basis_func = basis_func, domain = c(0,1))

  expect_equal(object = round(x = coef_mean, digits = 8), expected = 0)
})

test_that("The calculation of the means of the fourier coefficients works for the case of the fourier basis", {
  # create the weight function
  w_func <- function(x){return(1)}
  # create the fourier basis
  fourier_basis <- fda::create.fourier.basis(rangeval = c(0,1), nbasis = 15)
  # calculate means of fourier coefficients
  coef_means <- fourier_basis_coef_means(w_func = w_func, basis = fourier_basis, n_basis = 15, domain = c(0, 1))

  expect_equal(object = round(x = coef_means, digits = 8), expected = c(1, rep(0, times = 14)))
})

test_that("The generation of random fourier coefficients works", {
  # create the weight function
  w_func <- function(x){return(1)}
  # choose n_basis
  n_basis <- 15
  # create vector of rho's
  rho <- seq(from = 2, to = 0.25, length.out = n_basis)
  # create the fourier basis
  fourier_basis <- fda::create.fourier.basis(rangeval = c(0,1), nbasis = 15)
  # draw random function
  rand_coef <- fourier_coef_sample(w_func = w_func, basis = fourier_basis, n_basis = n_basis,
                               rho = rho, u_sample_func = u_unif, domain = c(0,1))

  # check properties of returned object
  expect_equal(object = length(rand_coef), expected = n_basis)
})

test_that("The generation of random functions works", {
  # create the weight function
  w_func <- function(x){return(1)}
  # choose n_basis
  n_basis <- 15
  # create vector of rho's
  rho <- seq(from = 2, to = 0.25, length.out = n_basis)
  # draw random function
  rand_func <- function_sample(w_func = w_func, basis = "fourier", n_basis = n_basis,
                               rho = rho, u_sample_func = u_unif, domain = c(0,1))

  # check properties of returned object
  expect_equal(object = fda::is.fd(rand_func), expected = TRUE)
  expect_equal(object = nrow(rand_func$coefs), expected = n_basis)
})
