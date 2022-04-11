test_that("Calculation of Means based test statistic works", {

  grid = seq(0,1, length.out = 100)

  sample_1 <- rep(
    x = list(list(
      args = grid,
      vals = rep(1, times = length(grid))
    )), times = 20)

  sample_2 <- rep(
    x = list(list(
      args = grid,
      vals = rep(0, times = length(grid))
    )), times = 20)

  sample_3 <- rep(
    x = list(list(
      args = grid,
      vals = seq(0, 1, length.out = length(grid))
    )), times = 20)

  output_1 <- means_t_stat(sample1 = sample_1, sample2 = sample_2, interpolation_mode = 'linear',
                         domain = c(0,1), grid = grid)

  output_2 <- means_t_stat(sample1 = sample_1, sample2 = sample_3, interpolation_mode = 'bspline',
                              domain = c(0,1), grid = grid, n_basis = 11)

  expect_equal(output_1, 1)
  expect_equal(output_2, 1/3)
})
