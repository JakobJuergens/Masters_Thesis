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
