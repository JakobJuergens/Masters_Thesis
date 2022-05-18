test_that("Calculation of Means based test statistic works", {
  grid <- seq(0, 1, length.out = 100)

  sample_1 <- rep(
    x = list(list(
      args = grid,
      vals = rep(1, times = length(grid))
    )), times = 20
  )

  sample_2 <- rep(
    x = list(list(
      args = grid,
      vals = rep(0, times = length(grid))
    )), times = 20
  )

  sample_3 <- rep(
    x = list(list(
      args = grid,
      vals = seq(0, 1, length.out = length(grid))
    )), times = 20
  )

  output_1 <- means_tstat(
    sample1 = sample_1, sample2 = sample_2, interpolation_mode = "linear",
    domain = c(0, 1), grid = grid
  )

  output_2 <- means_tstat(
    sample1 = sample_1, sample2 = sample_3, interpolation_mode = "bspline",
    domain = c(0, 1), grid = grid, n_basis = 11
  )

  expect_equal(object = output_1, expected = 1)
  expect_equal(object = output_2, expected = 1 / 3)
})

test_that("Calculation of Critical Value for Means-based Test works", {
  grid <- seq(0, 1, length.out = 100)

  sample_1 <- rep(
    x = list(list(
      args = grid,
      vals = rep(1, times = length(grid))
    )), times = 3
  )

  sample_2 <- rep(
    x = list(list(
      args = grid,
      vals = rep(0, times = length(grid))
    )), times = 3
  )

  tstats <- means_tstats(
    full = TRUE, sample1 = sample_1,
    sample2 = sample_2, interpolation_mode = "linear",
    domain = c(0, 1), grid = seq(0, 1, length.out = 100)
  )

  crit_value <- crit_val(realizations = tstats, alpha = 0.05)

  expect_equal(object = 0, expected = 1)
})

test_that("Approximation of Critical Value for Means-based Test works", {
  grid <- seq(0, 1, length.out = 100)

  sample_1 <- rep(
    x = list(list(
      args = grid,
      vals = rep(1, times = length(grid))
    )), times = 20
  )

  sample_2 <- rep(
    x = list(list(
      args = grid,
      vals = rep(0, times = length(grid))
    )), times = 20
  )

  tstats <- means_tstats(
    full = FALSE, approxQ = 250, sample1 = sample_1,
    sample2 = sample_2, interpolation_mode = "linear",
    domain = c(0, 1), grid = seq(0, 1, length.out = 100)
  )

  crit_value <- crit_val(realizations = tstats, alpha = 0.05)

  expect_equal(object = 0, expected = 1)
})

test_that("Full Permutation Procedure works for mean based test for interpolation_mode = 'bspline'.", {
  grid <- seq(from = 0, to = 1, length.out = 100)

  # create two sample with two observations each for a trivial case
  sample1 <- list(
    list(
      args = grid,
      vals = rep(0, times = length(grid))
    ),
    list(
      args = grid,
      vals = rep(0.5, times = length(grid))
    )
  )
  sample2 <- list(
    list(
      args = grid,
      vals = rep(1, times = length(grid))
    ),
    list(
      args = grid,
      vals = rep(1.5, times = length(grid))
    )
  )
  sample3 <- list(
    list(
      args = grid,
      vals = grid
    ),
    list(
      args = grid,
      vals = grid
    )
  )

  Q_1 <- choose(n = length(sample1) + length(sample2), k = length(sample1))

  t_vals <- perm_tstats_full(
    Q = Q_1, sample1 = sample1, sample2 = sample2,
    t_stat_func = means_tstat, interpolation_mode = "bspline", domain = c(0, 1),
    n_basis = 15
  )
  crit_value_1 <- crit_val(realizations = t_vals, alpha = 0.05)

  diff_1 <- c(-1, -0.5, 0, 0, 0.5, 1)^2
  t_real_1 <- quantile(x = diff_1, probs = 0.95, names = FALSE)

  expect_equal(object = crit_value_1, expected = t_real_1)
})

test_that("Full Permutation Procedure works for mean based test for interpolation_mode = 'linear'.", {
  grid <- seq(from = 0, to = 1, length.out = 100)

  # create two sample with two observations each for a trivial case
  sample1 <- list(
    list(
      args = grid,
      vals = rep(0, times = length(grid))
    ),
    list(
      args = grid,
      vals = rep(0.5, times = length(grid))
    )
  )
  sample2 <- list(
    list(
      args = grid,
      vals = rep(1, times = length(grid))
    ),
    list(
      args = grid,
      vals = rep(1.5, times = length(grid))
    )
  )
  sample3 <- list(
    list(
      args = grid,
      vals = grid
    ),
    list(
      args = grid,
      vals = grid
    )
  )

  Q_1 <- choose(n = length(sample1) + length(sample2), k = length(sample1))

  t_vals_1 <- perm_tstats_full(
    Q = Q_1, sample1 = sample1, sample2 = sample2,
    t_stat_func = means_tstat, interpolation_mode = "linear", domain = c(0, 1),
    grid = seq(0, 1, length.out = 1000)
  )
  crit_value_1 <- crit_val(realizations = t_vals_1, alpha = 0.05)

  diff_1 <- c(-1, -0.5, 0, 0, 0.5, 1)^2
  t_real_1 <- quantile(x = diff_1, probs = 0.95, names = FALSE)

  expect_equal(object = crit_value_1, expected = t_real_1)
})

test_that("Means Test gives output for both variants of permutation procedure.", {
  grid <- seq(from = 0, to = 1, length.out = 100)

  # create two sample with two observations each for a trivial case
  sample1 <- list(
    list(
      args = grid,
      vals = rep(0, times = length(grid))
    ),
    list(
      args = grid,
      vals = rep(0.5, times = length(grid))
    )
  )
  sample2 <- list(
    list(
      args = grid,
      vals = rep(1, times = length(grid))
    ),
    list(
      args = grid,
      vals = rep(1.5, times = length(grid))
    )
  )


  Q_1 <- choose(n = length(sample1) + length(sample2), k = length(sample1))

  t_vals_full <- means_tstats(
    full = TRUE, sample1 = sample1, sample2 = sample2,
    interpolation_mode = "bspline", domain = c(0, 1),
    n_basis = 15
  )
  crit_value_1 <- crit_val(realizations = t_vals_full, alpha = 0.05)

  diff_1 <- c(-1, -0.5, 0, 0, 0.5, 1)^2
  t_real_1 <- quantile(x = diff_1, probs = 0.95, names = FALSE)

  t_vals_approx <- means_tstats(
    full = FALSE, approxQ = 3, sample1 = sample1, sample2 = sample2,
    interpolation_mode = "bspline", domain = c(0, 1),
    n_basis = 15
  )

  expect_equal(object = crit_value_1, expected = t_real_1)
  expect_equal(object = length(t_vals_approx), expected = 3)
})
