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

  output_1 <- means_tstat(sample1 = sample_1, sample2 = sample_2, interpolation_mode = 'linear',
                         domain = c(0,1), grid = grid)

  output_2 <- means_tstat(sample1 = sample_1, sample2 = sample_3, interpolation_mode = 'bspline',
                              domain = c(0,1), grid = grid, n_basis = 11)

  expect_equal(object = output_1, expected = 1)
  expect_equal(object = output_2, expected = 1/3)
})

test_that("Calculation of Critical Value for Means-based Test works", {
  grid = seq(0,1, length.out = 100)

  sample_1 <- rep(
    x = list(list(
      args = grid,
      vals = rep(1, times = length(grid))
    )), times = 4)

  sample_2 <- rep(
    x = list(list(
      args = grid,
      vals = rep(0, times = length(grid))
    )), times = 4)

  tstats <- means_tstats(full = TRUE, sample1 = sample_1,
                               sample2 = sample_2, interpolation_mode = 'linear',
                               domain = c(0,1), grid = seq(0,1, length.out = 100))

  crit_value <- crit_val(realizations = tstats, alpha = 0.05)

  expect_equal(object = 0, expected = 1)
})

test_that("Approximation of Critical Value for Means-based Test works", {
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

  tstats <- means_tstats(full = FALSE, approxQ = 100, sample1 = sample_1,
                               sample2 = sample_2, interpolation_mode = 'linear',
                               domain = c(0,1), grid = seq(0,1, length.out = 100))

  crit_value <- crit_val(realizations = tstats, alpha = 0.05)

  expect_equal(object = 0, expected = 1)
})
