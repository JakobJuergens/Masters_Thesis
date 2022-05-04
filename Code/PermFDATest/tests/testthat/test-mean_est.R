test_that("Linear Interpolation Mean Estimator works", {
  grid <- seq(0, 1, length.out = 100)
  des_outcome <- list(
    args = grid,
    vals = rep(0.5, times = 100)
  )

  sample <- list(
    obs_1 <- list(
      args = seq(0, 1, length.out = 20),
      vals = rep(1, times = 20)
    ),
    obs_2 <- list(
      args = seq(0, 1, length.out = 20),
      vals = rep(0, times = 20)
    )
  )

  expect_equal(
    linear_mean_estimator(sample = sample, domain = c(0, 1), grid = grid),
    des_outcome
  )
})

test_that("Fourier Mean Estimator works", {
  grid <- seq(0, 1, length.out = 20)

  des_outcome <- list(
    args = grid,
    vals = 0.5 * sin(grid * 2 * pi) + 0.5 * cos(grid * 2 * pi)
  )

  test_sample <- list(
    rep(x = list(
      args = grid,
      vals = sin(grid * 2 * pi)
    ), times = 50),
    rep(x = list(
      args = grid,
      vals = cos(grid * 2 * pi)
    ), times = 50)
  )

  mean_func <- fourier_mean_estimator(sample = test_sample, domain = c(0, 1), n_basis = 15)
  mean_func_vals <- unlist(
    purrr::map(
      .x = grid,
      .f = function(x) unname(mean_func(x))[1]
    )
  )

  expect_equal(
    mean_func_vals,
    des_outcome$vals
  )
})

