#' This function is aimed at the application for my master's thesis and is
#' experimental. It performs the approximation means based test parallelized
#' over the permutations.
#'
#' @param cl: cluster object created by parallel package
#' @param seeds: Seeds for reproducibility (vector of length approxQ)
#' @param approxQ: integer specifying the number of combinations to be used for
#' the approximation of the critical value
#' @param sample1: first sample, specified as a list where each element is one observation
#' @param sample2: second sample, specified as a list where each element is one observation
#' @param interpolation_mode: string that determines the mode of interpolation between
#' discrete measurement points. (Recommendation: 'linear')
#' @param domain: vector with beginning and endpoint of the closed interval
#' that is the domain of the stochastic processes
#' @param n_basis: if interpolation mode is choses as a basis based method,
#' this determines the number of basis functions used in the approximation
#' @param grid: grid used for the approximation of the mean function
#'
#' @return Realizations of the t-statistic for the Mean based test
#'
#' @export
nu_test_par <- function(cl, seeds, approxQ = NULL, sample1, sample2, domain = c(0, 1)) {
  # determine sample sizes
  n_1 <- ncol(sample1)
  n_2 <- ncol(sample2)
  # create combined data set
  comb_data <- cbind(sample1, sample2)
  # determine number of basis functions
  n_basis <- nrow(sample1)

  helper_func <- function(seed, comb_data, n_1, n_2) {
    set.seed(seed)

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
    nu_hat <- (n_1 + n_2) * integrate(
      f = diff_func, lower = domain[1], upper = domain[2], subdivisions = 1000
    )$value

    return(nu_hat)
  }

  parallel::clusterExport(cl = cl, varlist = "comb_data", envir = environment())

  tstats <- unlist(parallel::clusterApply(
    cl = cl, x = seeds,
    fun = function(x){helper_func(seed = x, comb_data = comb_data, n_1 = n_1, n_2 = n_2)}
  ))

  # determine mean functions of samples
  mean_s1 <- rowMeans(sample1)
  mean_s2 <- rowMeans(sample2)

  # determine difference function of mean functions
  diff_coefs <- mean_s2 - mean_s1
  diff_func <- function(x) {
    fourier_eval(x = x, coefs = diff_coefs, domain = domain)^2
  }

  # calculate nu_hat
  nu_realized <- (n_1 + n_2) * integrate(
    f = diff_func, lower = domain[1], upper = domain[2], subdivisions = 1000
  )$value

  return(list(nu_hat = tstats, nu_realized = nu_realized))
}

#' This function is aimed at the application for my master's thesis and is
#' experimental. It performs the approximation CvM test parallelized
#' over the permutations.
#'
#' @param cl: cluster object created by parallel package
#' @param approxQ: integer specifying the number of combinations to be used for
#' the approximation of the critical value
#' @param sample1: first sample, specified as a list where each element is one observation
#' @param sample2: second sample, specified as a list where each element is one observation
#' @param domain: Domain of the functional observations
#' @param w_func: weight function for construction of measure
#' @param rho: a vector of length n_basis from a square summable sequence that is used
#' as scaling terms for the error terms
#' @param u_sample_func: function that can be used to sample from for the error terms
#' around the specified mean
#' @param n_func: number of functions used for the Monte-Carlo Integration in the
#' approximation of tau
#' @param ...: additional arguments given to u_sample_func
#'
#' @return Realizations of the t-statistic for the Cramer-von Mises test
#'
#' @export
tau_test_par <- function(cl, approxQ = NULL, sample1, sample2, domain = c(0, 1),
                         w_func, rho, u_sample_func, n_func, ...) {
  # determine sample sizes
  n_1 <- ncol(sample1)
  n_2 <- ncol(sample2)
  # create container for realizations of tau_hat
  tau_hat <- rep(x = NA, times = approxQ)
  # create combined data set
  comb_data <- cbind(sample1, sample2)
  # determine number of basis functions
  n_basis <- nrow(sample1)
  # determine means of the fourier coefficients for random functions
  coef_means <- matrix(
    data = rep(
      x = fourier_basis_coef_means(w_func = w_func, n_basis = n_basis, domain = domain),
      times = n_func
    ), nrow = n_basis, ncol = n_func, byrow = FALSE
  )
  # draw error terms from chosen distribution
  coef_errors <- matrix(
    data = unlist(
      purrr::map(
        .x = 1:n_func,
        .f = function(i) {
          rho * u_sample_func(n_basis = n_basis, ...)
        }
      )
    ), nrow = n_basis, ncol = n_func, byrow = FALSE
  )
  # determine fourier coefficients of comparison sample
  func_coefs <- coef_means + coef_errors
  message('Random Functions Generated.')
  # compare all functions in comb_data with all functions in func_coefs (!)
  # create container for indicators
  weakly_bigger <- matrix(data = NA, nrow = ncol(comb_data), ncol = n_func)

  parallel::clusterExport(cl = cl, varlist = "comb_data", envir = environment())

  for (i in 1:(n_1 + n_2)) {
    message(paste0('Currently in function ', i, ' out of ', n_1 + n_2))
    weakly_bigger[i, ] <- unlist(
      parallel::clusterApply(
        cl = cl, x = 1:n_func,
        fun = function(x) {
          PermFDATest::func_comparison_fourier(
            func_a = comb_data[, i], func_b = func_coefs[, x], domain = domain
          )
        }
      )
    )
  }
  # iterate over combinations
  for (i in 1:approxQ) {
    index_1 <- sample(x = 1:(n_1 + n_2), size = n_1, replace = FALSE)
    index_2 <- setdiff(x = 1:(n_1 + n_2), y = index_1)
    # create container for values of empirical distribution functions
    emp_dist_vals_s1 <- colMeans(weakly_bigger[index_1, ])
    emp_dist_vals_s2 <- colMeans(weakly_bigger[index_2, ])
    # calculate test statistic
    tau_hat[i] <- (n_1 + n_2) * mean(x = (emp_dist_vals_s1 - emp_dist_vals_s2)^2, na.rm = FALSE)
  }

  tau_realized <- (n_1 + n_2) * mean(
    x = (colMeans(weakly_bigger[1:n_1, ]) - colMeans(weakly_bigger[(n_1 + 1):(n_1 + n_2), ]))^2,
    na.rm = FALSE
  )

  return(list(tau_hat = tau_hat, tau_realized = tau_realized))
}
