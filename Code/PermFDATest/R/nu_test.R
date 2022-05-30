#' This function calculates realizations of the cramer-von mises type test
#' on two given samples for a specified number of permutations
#'
#' @param sample1: first sample, specified as a list where each element is one observation
#' @param sample2: second sample, specified as a list where each element is one observation
#' @param full: Boolean that decides whether all combinations are considered
#' if FALSE, approxQ has to be provided
#' @param approxQ: integer specifying the number of combinations to be used for
#' the approximation of the critical value
#' @param domain: Domain of the functional observations
#'
#' @return: Realizations of the t-statistic for the Cramer-von Mises test
#' @export
nu_realizations <- function(sample1, sample2, full = FALSE, approxQ, domain) {
  if (nrow(sample1) != nrow(sample2)) {
    stop("The samples have to be expressed in terms of the same basis.")
  }

  if (full == TRUE) {
    return(nu_real_full(
      sample1 = sample1, sample2 = sample2, type = type,
      domain = domain
    ))
  } else {
    return(nu_real_approx(
      sample1 = sample1, sample2 = sample2, approxQ = approxQ,
      domain = domain
    ))
  }
}

nu_real_full <- function(sample1, sample2, domain) {
  # determine sample sizes
  n_1 <- ncol(sample1)
  n_2 <- ncol(sample2)
  # calculate number of combinations
  Q <- choose(n = n_1 + n_2, k = n_1)
  # create container for realizations of nu_hat
  nu_hat <- rep(x = NA, times = Q)
  # create combined data set
  comb_data <- cbind(sample1, sample2)
  # determine number of basis functions
  n_basis <- nrow(sample1)

  # set initial combination to NULL
  cur_cbn <- NULL
  # iterate over combinations
  for (i in 1:Q) {
    # get next combination
    cur_cbn <- next_cbn(cbn = cur_cbn, n = n_1 + n_2, k = n_1)
    # get list of sample1 and sample2
    cur_samples <- sample_inds(n = n_1 + n_2, smpl1 = cur_cbn)
    # determine mean functions of samples
    mean_s1 <- rowMeans(comb_data[, cur_samples$smpl1])
    mean_s2 <- rowMeans(comb_data[, cur_samples$smpl2])
    # determine difference function of mean functions
    diff_coefs <- mean_s2 - mean_s1
    diff_func <- function(x) {
      fourier_eval(x = x, coefs = diff_coefs, domain = domain)^2
    }
    # calculate nu_hat
    nu_hat[i] <- (n_1 + n_2) * integrate(
      f = diff_func, lower = domain[1], upper = domain[2], subdivisions = 1000
    )$value
  }

  mean_s1 <- rowMeans(sample1)
  mean_s2 <- rowMeans(sample2)
  # determine difference function of mean functions
  diff_coefs <- mean_s2 - mean_s1
  diff_func <- function(x) {
    fourier_eval(x = x, coefs = diff_coefs, domain = domain)^2
  }
  nu_realized <- (n_1 + n_2) * integrate(
    f = diff_func, lower = domain[1], upper = domain[2], subdivisions = 1000
  )$value

  return(list(nu_hat = nu_hat, nu_realized = nu_realized))
}

nu_real_approx <- function(sample1, sample2, approxQ, domain) {
  # determine sample sizes
  n_1 <- ncol(sample1)
  n_2 <- ncol(sample2)
  # create container for realizations of nu_hat
  nu_hat <- rep(x = NA, times = approxQ)
  # create combined data set
  comb_data <- cbind(sample1, sample2)
  # determine number of basis functions
  n_basis <- nrow(sample1)

  # iterate over combinations
  for (i in 1:approxQ) {
    index_1 <- sample(x = 1:(n_1 + n_2), size = n_1, replace = FALSE)
    index_2 <- setdiff(x = 1:(n_1 + n_2), y = index_1)
    # determine mean functions of samples
    mean_s1 <- rowMeans(comb_data[, index_1])
    mean_s2 <- rowMeans(comb_data[, index_2])
    # determine difference function of mean functions
    diff_coefs <- mean_s2 - mean_s1
    diff_func <- function(x) {
      fourier_eval(x = x, coefs = diff_coefs, domain = domain)^2
    }
    # calculate nu_hat
    nu_hat[i] <- (n_1 + n_2) * integrate(
      f = diff_func, lower = domain[1], upper = domain[2], subdivisions = 1000
    )$value
  }

  mean_s1 <- rowMeans(sample1)
  mean_s2 <- rowMeans(sample2)
  # determine difference function of mean functions
  diff_coefs <- mean_s2 - mean_s1
  diff_func <- function(x) {
    fourier_eval(x = x, coefs = diff_coefs, domain = domain)^2
  }
  nu_realized <- (n_1 + n_2) * integrate(
    f = diff_func, lower = domain[1], upper = domain[2], subdivisions = 1000
  )$value

  return(list(nu_hat = nu_hat, nu_realized = nu_realized))
}
