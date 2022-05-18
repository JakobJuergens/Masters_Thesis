#' This function calculates realizations of the cramer-von mises type test
#' on two given samples for a specified number of permutations
#'
#' @param sample1: first sample, specified as a list where each element is one observation
#' @param sample2: second sample, specified as a list where each element is one observation
#' @param full: Boolean that decides whether all combinations are considered
#' if FALSE, approxQ has to be provided
#' @param approxQ: integer specifying the number of combinations to be used for
#' the approximation of the critical value
#' @param type: type of computation to use (options are: 'fourier', 'eigen', and 'grid' )
#' @param grid: grid used for comparison if type == 'grid' is chosen
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
#' @return: Realizations of the t-statistic for the Cramer-von Mises test
#' @export
tau_realizations <- function(sample1, sample2, full = FALSE, approxQ, type = "fourier",
                             grid = NULL, domain, w_func, rho, u_sample_func, n_func, ...) {
  if (ncol(sample1) != ncol(sample2)) {
    stop("The samples have to be expressed in terms of the same basis.")
  }

  if (full == TRUE) {
    return(tau_real_full(
      sample1 = sample1, sample2 = sample2, type = type, grid = grid,
      domain = domain, w_func = w_func, rho = rho,
      u_sample_func = u_sample_func, n_func = n_func, ...
    ))
  } else {
    return(tau_real_approx(
      sample1 = sample1, sample2 = sample2, approxQ = approxQ, type = type, grid = grid,
      domain = domain, w_func = w_func, rho = rho,
      u_sample_func = u_sample_func, n_func = n_func, ...
    ))
  }
}

tau_real_full <- function(sample1, sample2, type = "fourier",
                          domain, w_func, rho, u_sample_func, n_func, ...) {
  # determine sample sizes
  n_1 <- ncol(sample1)
  n_2 <- ncol(sample2)
  # calculate number of combinations
  Q <- choose(n = n_1 + n_2, k = n_1)
  # create container for realizations of tau_hat
  tau_hat <- rep(x = NA, times = Q)
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
        .f = function(i){rho * u_sample_func(n_basis = n_basis, ...)}
      )
    ), nrow = n_basis, ncol = n_func, byrow = FALSE
  )
  # determine fourier coefficients of comparison sample
  func_coefs <- coef_means + coef_errors
  # compare all functions in comb_data with all functions in func_coefs (!)
  # create container for indicators
  weakly_bigger <- matrix(data = NA, nrow = ncol(comb_data), ncol = n_func)
  # compare functions with chosen method
  if(type == 'fourier'){
    for (i in 1:n_func) {
      weakly_bigger[, i] <- unlist(
        purrr::map(
          .x = 1:ncol(comb_data),
          .f = ~ func_comparison_fourier(func_a = comb_data[, .x], func_b = func_coefs[, i], domain = domain)
        )
      )
    }
  }
  else if(type == 'grid'){
    # evaluate functional objects at chosen grid
    data_grid <- fourier_eval(x = grid, coefs = comb_data, domain = domain)
    comp_grid <- fourier_eval(x = grid, coefs = func_coefs, domain = domain)
    for (i in 1:n_func) {
      weakly_bigger[, i] <- unlist(
        purrr::map(
          .x = 1:(n_1 + n_2),
          .f = ~ all(comp_grid[,i] >= data_grid[, .x])
        )
      )
    }
  }

  # set initial combination to NULL
  cur_cbn <- NULL
  # iterate over combinations
  for (i in 1:Q) {
    # get next combination
    cur_cbn <- next_cbn(cbn = cur_cbn, n = n_1 + n_2, k = n_1)
    # get list of sample1 and sample2
    cur_samples <- sample_inds(n = n_1 + n_2, smpl1 = cur_cbn)
    # create container for values of empirical distribution functions
    emp_dist_vals_s1 <- colMeans(weakly_bigger[cur_samples$smpl1, ])
    emp_dist_vals_s2 <- colMeans(weakly_bigger[cur_samples$smpl2, ])
    # calculate test statistic
    tau_hat[i] <- (n_1 + n_2) * mean(x = (emp_dist_vals_s1 - emp_dist_vals_s2)^2, na.rm = FALSE)
  }

  tau_realized <- (n_1 + n_2) * mean(
    x = (colMeans(weakly_bigger[1:n_1, ]) - colMeans(weakly_bigger[(n_1 + 1):(n_1 + n_2), ]))^2,
    na.rm = FALSE
  )

  return(list(tau_hat = tau_hat, tau_realized = tau_realized))
}

tau_real_approx <- function(sample1, sample2, approxQ, type = "fourier", grid = NULL,
                            domain, w_func, rho, u_sample_func, n_func, ...) {
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
        .f = function(i){rho * u_sample_func(n_basis = n_basis, ...)}
      )
    ), nrow = n_basis, ncol = n_func, byrow = FALSE
  )
  # determine fourier coefficients of comparison sample
  func_coefs <- coef_means + coef_errors
  # compare all functions in comb_data with all functions in func_coefs (!)
  # create container for indicators
  weakly_bigger <- matrix(data = NA, nrow = ncol(comb_data), ncol = n_func)
  # compare functions with chosen method
  if(type == 'fourier'){
    for (i in 1:n_func) {
      weakly_bigger[, i] <- unlist(
        purrr::map(
          .x = 1:ncol(comb_data),
          .f = ~ func_comparison_fourier(func_a = comb_data[, .x], func_b = func_coefs[, i], domain = domain)
        )
      )
    }
  }
  else if(type == 'grid'){
    # evaluate functional objects at chosen grid
    data_grid <- fourier_eval(x = grid, coefs = comb_data, domain = domain)
    comp_grid <- fourier_eval(x = grid, coefs = func_coefs, domain = domain)
    for (i in 1:n_func) {
      weakly_bigger[, i] <- unlist(
        purrr::map(
          .x = 1:(n_1 + n_2),
          .f = ~ all(comp_grid[,i] >= data_grid[, .x])
        )
      )
    }
  }

  # iterate over combinations
  for (i in 1:approxQ) {
    index_1 <- sample(x = 1:(n_1+n_2), size = n_1, replace = FALSE)
    index_2 <- setdiff(x = 1:(n_1+n_2), y = index_1)
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
