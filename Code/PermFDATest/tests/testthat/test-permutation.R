test_that("Full Permutation Procedure works for trivial setting.", {

  trivial_tstat_func <- function(sample1, sample2){
    return(0.5)
  }

  sample1 <- as.list(1:4)
  sample2 <- as.list(5:8)

  Q <- choose(n = length(sample1) + length(sample2), k = length(sample1))

  t_vals <- perm_tstats_full(Q = Q, sample1 = sample1,
                             sample2 = sample2, t_stat_func = trivial_tstat_func)

  crit_value <- crit_val(realizations = t_vals, alpha = 0.05)
  expect_equal(object = t_val, expected = 0.5)
})

test_that("Full Permutation Procedure works for scalar mean setting.", {

  mean_tstat_func <- function(sample1, sample2){
    return(mean(unlist(sample2)) - mean(unlist(sample1)))
  }

  sample1 <- as.list(1:3)
  sample2 <- as.list(4:6)

  Q <- choose(n = length(sample1) + length(sample2), k = length(sample1))

  t_vals <- perm_tstats_full(Q = Q, sample1 = sample1,
                                sample2 = sample2, t_stat_func = mean_tstat_func)

  crit_value <- crit_val(realizations = t_vals, alpha = 0.05)
  diff <-  7 - 2*c(2, 7/3, 8/3, 3, 8/3, 3, 10/3, 10/3, 11/3, 12/3, 3, 10/3, 11/3, 11/3, 12/3, 13/3, 12/3, 13/3, 14/3, 15/3)
  exp <- quantile(x = diff, probs = 0.95, names = FALSE)
  expect_equal(object = crit_value, expected = exp)
})

test_that("Full Permutation Procedure works for mean based test for interpolation_mode = 'bspline'.",{

  grid = seq(from = 0, to = 1, length.out = 100)

  # create two sample with two observations each for a trivial case
  sample1 <- list(
    list(args = grid,
         vals = rep(0, times = length(grid))),
    list(args = grid,
         vals = rep(0.5, times = length(grid)))
  )
  sample2 <- list(
    list(args = grid,
         vals = rep(1, times = length(grid))),
    list(args = grid,
         vals = rep(1.5, times = length(grid)))
  )
  sample3 <- list(
    list(args = grid,
         vals = grid),
    list(args = grid,
         vals = grid)
  )

  Q_1 <- choose(n = length(sample1) + length(sample2), k = length(sample1))

  t_vals <- perm_tstats_full(Q = Q_1, sample1 = sample1, sample2 = sample2,
                                  t_stat_func = means_tstat, interpolation_mode = "bspline", domain = c(0,1),
                                  n_basis = 15)
  crit_value_1 <- crit_val(realizations = t_vals, alpha = 0.05)

  diff_1 <- c(-1, -0.5, 0, 0, 0.5, 1)^2
  t_real_1 <- quantile(x = diff_1, probs = 0.95, names = FALSE)

  expect_equal(object = crit_value_1, expected = t_real_1)

})

test_that("Full Permutation Procedure works for mean based test for interpolation_mode = 'linear'.",{

  grid = seq(from = 0, to = 1, length.out = 100)

  # create two sample with two observations each for a trivial case
  sample1 <- list(
    list(args = grid,
         vals = rep(0, times = length(grid))),
    list(args = grid,
         vals = rep(0.5, times = length(grid)))
  )
  sample2 <- list(
    list(args = grid,
         vals = rep(1, times = length(grid))),
    list(args = grid,
         vals = rep(1.5, times = length(grid)))
  )
  sample3 <- list(
    list(args = grid,
         vals = grid),
    list(args = grid,
         vals = grid)
  )

  Q_1 <- choose(n = length(sample1) + length(sample2), k = length(sample1))

  t_vals_1 <- perm_tstats_full(Q = Q_1, sample1 = sample1, sample2 = sample2,
                                  t_stat_func = means_tstat, interpolation_mode = "linear", domain = c(0,1),
                                  grid = seq(0,1, length.out = 1000))
  crit_value_1 <- crit_val(realizations = t_vals_1, alpha = 0.05)

  diff_1 <- c(-1, -0.5, 0, 0, 0.5, 1)^2
  t_real_1 <- quantile(x = diff_1, probs = 0.95, names = FALSE)

  expect_equal(object = crit_value_1, expected = t_real_1)
})

test_that("Approximation Permutation Procedure works for trivial setting.", {

  trivial_tstat_func <- function(sample1, sample2){
    return(0.5)
  }

  sample1 <- as.list(1:4)
  sample2 <- as.list(5:8)

  t_vals <- perm_tstats_approx(approxQ = 10, sample1 = sample1,
                                sample2 = sample2, t_stat_func = trivial_tstat_func)
  crit_value <- crit_val(realizations = t_vals, alpha = 0.05)
  expect_equal(object = crit_value, expected = 0.5)
})
