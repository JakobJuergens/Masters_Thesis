test_that("Full Permutation Procedure works for trivial setting.", {

  trivial_tstat_func <- function(sample1, sample2){
    return(0.5)
  }

  sample1 <- as.list(1:4)
  sample2 <- as.list(5:8)

  Q <- choose(n = length(sample1) + length(sample2), k = length(sample1))

  t_val <- perm_crit_value_full(alpha = 0.05, Q = Q, sample1 = sample1,
                                sample2 = sample2, t_stat_func = trivial_tstat_func)

  expect_equal(object = t_val, expected = 0.5)
})

test_that("Full Permutation Procedure works for scalar mean setting.", {

  mean_tstat_func <- function(sample1, sample2){
    return(mean(unlist(sample2)) - mean(unlist(sample1)))
  }

  sample1 <- as.list(1:3)
  sample2 <- as.list(4:6)

  Q <- choose(n = length(sample1) + length(sample2), k = length(sample1))

  t_val <- perm_crit_value_full(alpha = 0.05, Q = Q, sample1 = sample1,
                                sample2 = sample2, t_stat_func = mean_tstat_func)

  diff <-  7 - 2*c(2, 7/3, 8/3, 3, 8/3, 3, 10/3, 10/3, 11/3, 12/3, 3, 10/3, 11/3, 11/3, 12/3, 13/3, 12/3, 13/3, 14/3, 15/3)
  exp <- quantile(x = diff, probs = 0.95, names = FALSE)
  expect_equal(object = t_val, expected = exp)
})

