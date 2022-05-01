test_that(paste0("Fourier Version of Empirical Distribution Function works for",
          "Period of 2*pi"), {

  set.seed(100)
  test_sample <- sample_generator_2(n_obs = 100, domain = c(0,2*pi))
  test_sample_f <- quick_funcify(sample = test_sample, domain = c(0, 2*pi), n_fourier_basis = 15)

  fourier_basis <- fda::create.fourier.basis(rangeval = c(0, 2*pi), nbasis = 15)
  comparison_function <- fda::fd(coef = c(-100, rep(0, times = 13), 1), basisobj = fourier_basis)

  dist <- empirical_dist_func_fourier(sample = test_sample_f, func = comparison_function,
                                      domain = c(0,2*pi), n_fourier_basis = 15)
})
