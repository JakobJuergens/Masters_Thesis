#' This function derives / approximates the critical value for the
#' permutation test
#'
#' @param alpha: Size of the test
#' @param full: Boolean that decides whether all combinations are considered
#' if FALSE, approxQ has to be provided
#' @param approxQ: integer specifying the number of combinations to be used for
#' the approximation of the critical value
#' @param sample1: first sample, specified as a list where each element is one observation
#' @param sample2: second sample, specified as a list where each element is one observation
#' @param t_stat_func: function that calculates the test statistic on a given sample
#' @param ...: additional arguments that are relayed to t_stat_func
#'
#' @export
perm_crit_value <- function(alpha = 0.05, full = TRUE, approxQ = NULL,
                            sample1, sample2, t_stat_func, ...) {
  if (full == FALSE & missing(approxQ)) {
    stop("If full == FALSE is chosen, approxQ has to be provided.")
  }

  if (full == TRUE & !missing(approxQ)) {
    warning("full == TRUE and approxQ has been provided. approxQ is ignored
             and all combinations are considered.")
  }

  # set Q to the number of combinations with the chosen sample sizes
  Q <- choose(n = length(sample1) + length(sample2), k = length(sample1))

  if (!missing(approxQ) && (approxQ > Q)) {
    warning("approxQ is larger than the number of distinct combinations.
             Instead, all combinations are considered.")
    full <- TRUE
  }

  # if full == TRUE, call function for non-approximated case
  if (full == TRUE) {
    crit_value <- perm_crit_value_full(
      alpha = alpha, Q = Q,
      sample1 = sample1, sample2 = sample2,
      t_stat_func, ...
    )
  }
  # if full == FALSE, call function for approximated case
  if (full == FALSE) {
    crit_value <- perm_crit_value_approx(
      alpha = alpha, Q = Q, approxQ = approxQ,
      sample1 = sample1, sample2 = sample2,
      t_stat_func, ...
    )
  }

  return(crit_value)
}

#' This function derives the critical value for the permutation test by considering
#' all combinations of the data of the specified sample sizes
#'
#' @param alpha: Size of the test
#' @param Q: possible number of combinations
#' @param sample1: first sample, specified as a list where each element is one observation
#' @param sample2: second sample, specified as a list where each element is one observation
#' @param t_stat_func: function that calculates the test statistic on a given sample
#' @param ...: additional arguments that are relayed to t_stat_func
#'
#' @export
perm_crit_value_full <- function(alpha = 0.05, Q, sample1, sample2, t_stat_func, ...) {
  # concatenate samples to form one data set
  data <- append(sample1, sample2)
  # Initialize vector for realizations of t_stat
  t_stats <- rep(x = NA, times = Q)
  # Initialize current combination as NULL
  cur_cbn <- NULL
  # set n_1 and n_2
  n_1 <- length(sample1)
  n_2 <- length(sample2)

  # iterate over combinations
  for (i in 1:Q) {
    # message(paste0('Now in Iteration:', i))
    # get next combination
    cur_cbn <- next_cbn(cbn = cur_cbn, n = n_1 + n_2, k = n_1)
    # get list of sample1 and sample2
    cur_samples <- sample_inds(n = n_1 + n_2, smpl1 = cur_cbn)
    # create samples
    cur_sample1 <- data[cur_samples$smpl1]
    cur_sample2 <- data[cur_samples$smpl2]
    # calculate test statistic
    t_stats[i] <- t_stat_func(
      sample1 = cur_sample1, sample2 = cur_sample2, ...
    )
  }

  # calculate critical value
  crit_value <- quantile(x = t_stats, probs = 1 - alpha, names = FALSE)

  # return critical value
  return(crit_value)
}

#' This function derives the critical value for the permutation test by considering
#' a specified number of combinations. The specific combinations that are considered
#' are chosen randomly.
#'
#' @param alpha: Size of the test
#' @param Q: possible number of combinations
#' @param approxQ: integer specifying the number of combinations to be used for
#' the approximation of the critical value
#' @param sample1: first sample, specified as a list where each element is one observation
#' @param sample2: second sample, specified as a list where each element is one observation
#' @param t_stat_func: function that calculates the test statistic on a given sample
#' @param ...: additional arguments that are relayed to t_stat_func
#'
#' @export
perm_crit_value_approx <- function(alpha = 0.05, Q, approxQ, sample1, sample2, t_stat_func, ...) {
  # choose which combinations to use for approximation by sampling with replacement
  combination_index <- sample(x = 1:Q, size = approxQ, replace = FALSE)

  # concatenate samples to form one data set
  data <- append(sample1, sample2)
  # Initialize vector for realizations of t_stat
  t_stats <- rep(x = NA, times = approxQ)
  # Initialize current combination as NULL
  cur_cbn <- NULL
  # set n_1 and n_2
  n_1 <- length(sample1)
  n_2 <- length(sample2)
  # initialize running variable for assigning elements of t_stats
  j <- 1

  # iterate over combinations
  for (i in 1:Q) {
    if (i %in% combination_index) {
      # get multiplicity of combination in vector combination index
      cur_mult <- length(which(combination_index == i))
      # get next combination
      cur_cbn <- next_cbn(cbn = cur_cbn, n = n_1 + n_2, k = n_1)
      # get list of sample1 and sample2
      cur_samples <- sample_inds(n = n_1 + n_2, smpl1 = cur_cbn)
      # create samples
      cur_sample1 <- data[cur_samples$smpl1]
      cur_sample2 <- data[cur_samples$smpl2]
      # calculate test statistic
      t_stats[j:(j + cur_mult - 1)] <- t_stat_func(
        sample1 = cur_sample1, sample2 = cur_sample2, ...
      )
      # update running variable j
      j <- j + cur_mult
    }
  }

  # calculate critical value
  crit_value <- quantile(x = t_stats, probs = 1 - alpha, names = FALSE)

  # return critical value
  return(crit_value)
}
