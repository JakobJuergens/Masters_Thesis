#' This function performs the task of the permutation test function (called phi
#' in the corresponding paper)
#'
#' @param alpha: Significance Level of the test
#' @param realizations: vector of realizations of the test statistic over
#' permutations
#' @param realized_value: realized value of the test statistic in original permutation
#'
#' @return: The value of the permutation test function
#' @export
perm_test <- function(alpha, realizations, realized_value) {
  # calculate critical value
  crit_value <- crit_val(realizations = realizations, alpha = alpha)
  # determine Q values
  Q <- length(realizations)
  Q_pl <- sum(realizations > crit_value)
  Q_0 <- sum(realizations == crit_value)
  # calculate a return value
  a <- (Q * alpha - Q_pl) / Q_0
  # return appropriate values
  if (realized_value > crit_value) {
    return(1)
  } else if (realized_value == crit_value) {
    return(a)
  } else if (realized_value < crit_value) {
    return(0)
  }
}

#' This function returns a critical value for a chosen significance level
#' given a vector of realizations of the test statistic
#'
#' @param realizations: vector of realizations of the test statistic
#' @param alpha: significance level of the test
#'
#' @return: Critical Value for the test
#' @export
crit_val <- function(realizations, alpha = 0.05) {
  # returns the fitting quantile
  return(quantile(x = realizations, probs = 1 - alpha, names = FALSE))
}

#' This function calculates realizations of the test statistic over a specified number
#' of permutations of the data
#'
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
perm_tstats <- function(full = TRUE, approxQ = NULL,
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
    tstats <- perm_tstats_full(
      Q = Q,
      sample1 = sample1, sample2 = sample2,
      t_stat_func, ...
    )
  }
  # if full == FALSE, call function for approximated case
  if (full == FALSE) {
    tstats <- perm_tstats_approx(
      approxQ = approxQ,
      sample1 = sample1, sample2 = sample2,
      t_stat_func, ...
    )
  }
  # return realizations of test statistic
  return(tstats)
}

#' This function calculates all realizations of the test statistic by considering
#' all combinations of the data of the specified sample sizes
#'
#' @param Q: possible number of combinations
#' @param sample1: first sample, specified as a list where each element is one observation
#' @param sample2: second sample, specified as a list where each element is one observation
#' @param t_stat_func: function that calculates the test statistic on a given sample
#' @param ...: additional arguments that are relayed to t_stat_func
#'
#' @export
perm_tstats_full <- function(Q, sample1, sample2, t_stat_func, ...) {
  # concatenate samples to form one data set
  if(is.matrix(sample1) & is.matrix(sample2)){
    data <- cbind(sample1, sample2)
  } else{
    data <- append(sample1, sample2)
  }
  # set n_1 and n_2
  if(is.matrix(sample1) & is.matrix(sample2)){
    n_1 <- ncol(sample1)
    n_2 <- ncol(sample2)
  } else{
    n_1 <- length(sample1)
    n_2 <- length(sample2)
  }
  # determine number of combinations
  Q <- choose(n = n_1 + n_2, k = n_1)

  # Initialize vector for realizations of t_stat
  t_stats <- rep(x = NA, times = Q)

  cur_cbn <- NULL
  # iterate over combinations
  for (i in 1:Q) {
    # message(paste0('Now in Iteration:', i))
    # get next combination
    cur_cbn <- next_cbn(cbn = cur_cbn, n = n_1 + n_2, k = n_1)
    # get list of sample1 and sample2
    cur_samples <- sample_inds(n = n_1 + n_2, smpl1 = cur_cbn)
    # create samples
    if(is.matrix(sample1) & is.matrix(sample2)){
      cur_sample1 <- data[,cur_samples$smpl1]
      cur_sample2 <- data[,cur_samples$smpl2]
    } else{
      cur_sample1 <- data[cur_samples$smpl1]
      cur_sample2 <- data[cur_samples$smpl2]
    }
    # calculate test statistic
    t_stats[i] <- t_stat_func(
      sample1 = cur_sample1, sample2 = cur_sample2, ...
    )
  }

  # return realizations of test statistic
  return(t_stats)
}

#' This function calculates realizations of the test statistic by considering
#' a specified number of combinations. The specific combinations that are considered
#' are chosen randomly with replacement.
#'
#' @param approxQ: integer specifying the number of combinations to be used for
#' the approximation of the critical value
#' @param sample1: first sample, specified as a list where each element is one observation
#' @param sample2: second sample, specified as a list where each element is one observation
#' @param t_stat_func: function that calculates the test statistic on a given sample
#' @param ...: additional arguments that are relayed to t_stat_func
#'
#' @export
perm_tstats_approx <- function(approxQ, sample1, sample2, t_stat_func, ...) {
  # concatenate samples to form one data set
  if(is.matrix(sample1) & is.matrix(sample2)){
    data <- cbind(sample1, sample2)
  } else{
    data <- append(sample1, sample2)
  }

  # Initialize vector for realizations of t_stat
  t_stats <- rep(x = NA, times = approxQ)
  # set n_1 and n_2
  if(is.matrix(sample1) & is.matrix(sample2)){
    n_1 <- ncol(sample1)
    n_2 <- ncol(sample2)
  } else{
    n_1 <- length(sample1)
    n_2 <- length(sample2)
  }
  # iterate over combinations
  # previously, this worked as in the full version and sampled combinations
  # without replacement. However, this resulted in large performance problems as
  # the loop iterated over unreasonably many elements which were not used in the
  # actual approximation.
  for (i in 1:approxQ) {
    # generate a random combination
    index_1 <- sample(x = 1:(n_1+n_2), size = n_1, replace = FALSE)
    index_2 <- setdiff(x = 1:(n_1+n_2), y = index_1)
    # create samples
    if(is.matrix(sample1) & is.matrix(sample2)){
      cur_sample1 <- data[,index_1]
      cur_sample2 <- data[,index_2]
    } else{
      cur_sample1 <- data[index_1]
      cur_sample2 <- data[index_2]
    }
    # calculate test statistic
    t_stats[i] <- t_stat_func(
      sample1 = cur_sample1, sample2 = cur_sample2, ...
    )
  }

  # return realizations of test statistic
  return(t_stats)
}
