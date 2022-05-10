test_that("Cramer-von Mises Test gives output in the correct format", {
  # choose weight function
  w_func <- function(x){return(1)}
  # choose rho
  rho <- seq(from = 10, to = 1, length.out = 15)
  # generate basis
  basis <- fda::create.fourier.basis(rangeval = c(0,1), nbasis = 15, period = 1)
  # generate test_samples:
  grid = seq(0,1, length.out = 100)
  sample_1 <- quick_funcify(sample = rep(
    x = list(list(
      args = grid,
      vals = rep(1, times = length(grid))
    )), times = 20), domain = c(0,1), n_fourier_basis = 15)

  sample_2 <- quick_funcify(sample = rep(
    x = list(list(
      args = grid,
      vals = rep(-5, times = length(grid))
    )), times = 20), domain = c(0,1), n_fourier_basis = 15)

  # calculate t-stat for cramer-von mises type test
  # for n_func way(!) too low
  tau_hat <- cramer_von_mises_tstat_fourier(sample1 = sample_1, sample2 = sample_2, domain = c(0,1),
                                            basis = basis, w_func = w_func, rho = rho, u_sample_func = u_unif, n_func = 20)

  # check some simple theoretical properties
  expect_equal(object = is.numeric(tau_hat), expected = TRUE)
  expect_equal(object = (tau_hat >= 0), expected = TRUE)
})
