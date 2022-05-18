#' This function translates a sample generated with the sample_generator
#' functions into a functional data set
#'
#' @param sample: Sample to translate
#' @param domain: beginning and endpoint of domain (closed interval)
#' @param n_fourier_basis: number of fourier basis functions to use
#'
#' @return: A sample in the typical format
quick_funcify <- function(sample, domain = c(0, 1), n_fourier_basis) {
  fourier_basis <- fda::create.fourier.basis(rangeval = domain, nbasis = n_fourier_basis)

  data_fd <- matrix(
    data = unlist(
      purrr::map(
        .x = sample,
        .f = function(obs) {
          fda::smooth.basis(
            argvals = obs$args, y = obs$vals, fdParobj = fourier_basis
          )$fd$coef
        }
      )
    ), nrow = n_fourier_basis, ncol = length(sample), byrow = FALSE
  )

  return(data_fd)
}

#' This function generates two samples for the simulation of type 1
#' as described in my master's thesis.
#'
#' @param n_basis: number of fourier basis functions used
#' @param n_obs_1: Number of observations in sample 1
#' @param n_obs_2: Number of observations in sample 2
#' @param mean: vector of same length as grid, that specifies the mean function
#' @param rho: vector describing the autocorrelation of the process
#' same length as vector grid
#' @param sigma: vector of same length as grid, that specifies the square root
#' of the variance function
#' @param grid: grid at which observations are generated
#'
#' @return: list containing the two samples in a functional format
#' @export
sim_1_generator <- function(n_basis, n_obs_1, n_obs_2, mean, rho, sigma, grid) {
  # Generate xi's as described in the paper
  XI_1 <- matrix(
    data = rnorm(n = n_obs_1 * length(grid), mean = 0, sd = 1),
    nrow = n_obs_1, ncol = length(grid)
  )
  XI_2 <- matrix(
    data = rnorm(n = n_obs_1 * length(grid), mean = 0, sd = 1),
    nrow = n_obs_1, ncol = length(grid)
  )
  # Set up containers for the x's
  X_1_tilde <- matrix(data = NA, nrow = n_obs_1, ncol = length(grid))
  X_2_tilde <- matrix(data = NA, nrow = n_obs_1, ncol = length(grid))

  X_1 <- matrix(data = NA, nrow = n_obs_1, ncol = length(grid))
  X_2 <- matrix(data = NA, nrow = n_obs_1, ncol = length(grid))

  # fill first column with the first column of XI matrices
  X_1_tilde[, 1] <- XI_1[, 1]
  X_2_tilde[, 1] <- XI_2[, 1]

  X_1[, 1] <- mean[1] + sigma[1] * X_1_tilde[, 1]
  X_2[, 1] <- mean[1] + sigma[1] * X_2_tilde[, 1]

  # iterate through rest of the entries:
  for (i in 2:length(grid)) {
    X_1_tilde[, i] <- rho[i] * X_1_tilde[, i - 1] + XI_1[, i] * sqrt(1 - rho[i]^2)
    X_2_tilde[, i] <- rho[i] * X_2_tilde[, i - 1] + XI_2[, i] * sqrt(1 - rho[i]^2)

    X_1[,i] <- mean[i] + sigma[i] * X_1_tilde[, i]
    X_2[,i] <- mean[i] + sigma[i] * X_2_tilde[, i]
  }
  # bring into format for quick_funcify
  sample_1 <- purrr::map(
    .x = 1:n_obs_1,
    .f = function(i) {
      list(args = grid, vals = X_1[i, ])
    }
  )
  sample_2 <- purrr::map(
    .x = 1:n_obs_1,
    .f = function(i) {
      list(args = grid, vals = X_2[i, ])
    }
  )
  # bring into functional format
  sample_1_f <- quick_funcify(sample = sample_1, domain = c(0, 1), n_fourier_basis = n_basis)
  sample_2_f <- quick_funcify(sample = sample_2, domain = c(0, 1), n_fourier_basis = n_basis)
  # return as list
  return(list(
    sample_1 = sample_1, sample_2 = sample_2,
    sample_1_f = sample_1_f, sample_2_f = sample_2_f
  ))
}

#' This function generates two samples for the simulation of type 2
#' as described in my master's thesis.
#'
#' @param n_basis: number of fourier basis functions used
#' @param n_obs_1: Number of observations in sample 1
#' @param n_obs_2: Number of observations in sample 2
#' @param mean: vector of same length as grid, that specifies the mean function
#' @param mean_shift: vector of same length as grid, that is added to every
#' realization of sample2
#' @param rho: vector describing the autocorrelation of the process
#' same length as vector grid
#' @param sigma: vector of same length as grid, that specifies the square root
#' of the variance function
#' @param grid: grid at which observations are generated
#'
#' @return: list containing the two samples in a functional format
#' @export
sim_2_generator <- function(n_basis, n_obs_1, n_obs_2, mean, mean_shift, rho, sigma, grid) {
  # Generate xi's as described in the paper
  XI_1 <- matrix(
    data = rnorm(n = n_obs_1 * length(grid), mean = 0, sd = 1),
    nrow = n_obs_1, ncol = length(grid)
  )
  XI_2 <- matrix(
    data = rnorm(n = n_obs_1 * length(grid), mean = 0, sd = 1),
    nrow = n_obs_1, ncol = length(grid)
  )
  # Set up containers for the x's
  X_1_tilde <- matrix(data = NA, nrow = n_obs_1, ncol = length(grid))
  X_2_tilde <- matrix(data = NA, nrow = n_obs_1, ncol = length(grid))

  X_1 <- matrix(data = NA, nrow = n_obs_1, ncol = length(grid))
  X_2 <- matrix(data = NA, nrow = n_obs_1, ncol = length(grid))

  # fill first column with the first column of XI matrices
  X_1_tilde[, 1] <- XI_1[, 1]
  X_2_tilde[, 1] <- XI_2[, 1]

  X_1[, 1] <- mean[1] + sigma[1] * X_1_tilde[, 1]
  X_2[, 1] <- mean[1] + sigma[1] * X_2_tilde[, 1]

  # iterate through rest of the entries:
  for (i in 2:length(grid)) {
    X_1_tilde[, i] <- rho[i] * X_1_tilde[, i - 1] + XI_1[, i] * sqrt(1 - rho[i]^2)
    X_2_tilde[, i] <- rho[i] * X_2_tilde[, i - 1] + XI_2[, i] * sqrt(1 - rho[i]^2)

    X_1[,i] <- mean[i] + sigma[i] * X_1_tilde[, i]
    X_2[,i] <- mean[i] + sigma[i] * X_2_tilde[, i]
  }
  # bring into format for quick_funcify
  sample_1 <- purrr::map(
    .x = 1:n_obs_1,
    .f = function(i) {
      list(args = grid, vals = X_1[i, ])
    }
  )
  sample_2 <- purrr::map(
    .x = 1:n_obs_1,
    .f = function(i) {
      list(args = grid, vals = X_2[i, ] + mean_shift)
    }
  )
  # bring into functional format
  sample_1_f <- quick_funcify(sample = sample_1, domain = c(0, 1), n_fourier_basis = n_basis)
  sample_2_f <- quick_funcify(sample = sample_2, domain = c(0, 1), n_fourier_basis = n_basis)
  # return as list
  return(list(
    sample_1 = sample_1, sample_2 = sample_2,
    sample_1_f = sample_1_f, sample_2_f = sample_2_f
  ))
}

#' This function generates two samples for the simulation of type 3
#' as described in my master's thesis.
#'
#' @param n_basis: number of fourier basis functions used
#' @param n_obs_1: Number of observations in sample 1
#' @param n_obs_2: Number of observations in sample 2
#' @param mean: vector of same length as grid, that specifies the mean function
#' @param rho: vector describing the autocorrelation of the process
#' same length as vector grid
#' @param rho_shift: vector describing the difference in autocorrelation between
#' the two processes
#' @param sigma: vector of same length as grid, that specifies the square root
#' of the variance function
#' @param grid: grid at which observations are generated
#'
#' @return: list containing the two samples in a functional format
#' @export
sim_3_generator <- function(n_basis, n_obs_1, n_obs_2, mean, rho, rho_shift, sigma, grid) {
  # Generate xi's as described in the paper
  XI_1 <- matrix(
    data = rnorm(n = n_obs_1 * length(grid), mean = 0, sd = 1),
    nrow = n_obs_1, ncol = length(grid)
  )
  XI_2 <- matrix(
    data = rnorm(n = n_obs_1 * length(grid), mean = 0, sd = 1),
    nrow = n_obs_1, ncol = length(grid)
  )
  # Set up containers for the x's
  X_1_tilde <- matrix(data = NA, nrow = n_obs_1, ncol = length(grid))
  X_2_tilde <- matrix(data = NA, nrow = n_obs_1, ncol = length(grid))

  X_1 <- matrix(data = NA, nrow = n_obs_1, ncol = length(grid))
  X_2 <- matrix(data = NA, nrow = n_obs_1, ncol = length(grid))

  # fill first column with the first column of XI matrices
  X_1_tilde[, 1] <- XI_1[, 1]
  X_2_tilde[, 1] <- XI_2[, 1]

  X_1[, 1] <- mean[1] + sigma[1] * X_1_tilde[, 1]
  X_2[, 1] <- mean[1] + sigma[1] * X_2_tilde[, 1]

  # iterate through rest of the entries:
  for (i in 2:length(grid)) {
    X_1_tilde[, i] <- rho[i] * X_1_tilde[, i - 1] + XI_1[, i] * sqrt(1 - rho[i]^2)
    X_2_tilde[, i] <- (rho[i] + rho_shift[i]) * X_2_tilde[, i - 1] + XI_2[, i] * sqrt(1 - (rho[i] + rho_shift[i])^2)

    X_1[,i] <- mean[i] + sigma[i] * X_1_tilde[, i]
    X_2[,i] <- mean[i] + sigma[i] * X_2_tilde[, i]
  }
  # bring into format for quick_funcify
  sample_1 <- purrr::map(
    .x = 1:n_obs_1,
    .f = function(i) {
      list(args = grid, vals = X_1[i, ])
    }
  )
  sample_2 <- purrr::map(
    .x = 1:n_obs_1,
    .f = function(i) {
      list(args = grid, vals = X_2[i, ])
    }
  )
  # bring into functional format
  sample_1_f <- quick_funcify(sample = sample_1, domain = c(0, 1), n_fourier_basis = n_basis)
  sample_2_f <- quick_funcify(sample = sample_2, domain = c(0, 1), n_fourier_basis = n_basis)
  # return as list
  return(list(
    sample_1 = sample_1, sample_2 = sample_2,
    sample_1_f = sample_1_f, sample_2_f = sample_2_f
  ))
}

#' This function generates two samples for the simulation of type 4
#' as described in my master's thesis.
#'
#' @param n_basis: number of fourier basis functions used
#' @param n_obs_1: Number of observations in sample 1
#' @param n_obs_2: Number of observations in sample 2
#' @param mean: vector of same length as grid, that specifies the mean function
#' @param rho: vector describing the autocorrelation of the process
#' same length as vector grid
#' @param sigma: vector of same length as grid, that specifies the square root
#' of the variance function
#' @param sigma_shift: vector describing the difference in variance between
#' the two processes
#' @param grid: grid at which observations are generated
#'
#' @return: list containing the two samples in a functional format
#' @export
sim_4_generator <- function(n_basis, n_obs_1, n_obs_2, mean, rho, sigma, sigma_shift, grid) {
  # Generate xi's as described in the paper
  XI_1 <- matrix(
    data = rnorm(n = n_obs_1 * length(grid), mean = 0, sd = 1),
    nrow = n_obs_1, ncol = length(grid)
  )
  XI_2 <- matrix(
    data = rnorm(n = n_obs_1 * length(grid), mean = 0, sd = 1),
    nrow = n_obs_1, ncol = length(grid)
  )
  # Set up containers for the x's
  X_1_tilde <- matrix(data = NA, nrow = n_obs_1, ncol = length(grid))
  X_2_tilde <- matrix(data = NA, nrow = n_obs_1, ncol = length(grid))

  X_1 <- matrix(data = NA, nrow = n_obs_1, ncol = length(grid))
  X_2 <- matrix(data = NA, nrow = n_obs_1, ncol = length(grid))

  # fill first column with the first column of XI matrices
  X_1_tilde[, 1] <- XI_1[, 1]
  X_2_tilde[, 1] <- XI_2[, 1]

  X_1[, 1] <- mean[1] + sigma[1] * X_1_tilde[, 1]
  X_2[, 1] <- mean[1] + (sigma[1] + sigma_shift[1]) * X_2_tilde[, 1]

  # iterate through rest of the entries:
  for (i in 2:length(grid)) {
    X_1_tilde[, i] <- rho[i] * X_1_tilde[, i - 1] + XI_1[, i] * sqrt(1 - rho[i]^2)
    X_2_tilde[, i] <- rho[i] * X_2_tilde[, i - 1] + XI_2[, i] * sqrt(1 - rho[i]^2)

    X_1[,i] <- mean[i] + sigma[i] * X_1_tilde[, i]
    X_2[,i] <- mean[i] + (sigma[i] + sigma_shift[i]) * X_2_tilde[, i]
  }
  # bring into format for quick_funcify
  sample_1 <- purrr::map(
    .x = 1:n_obs_1,
    .f = function(i) {
      list(args = grid, vals = X_1[i, ])
    }
  )
  sample_2 <- purrr::map(
    .x = 1:n_obs_1,
    .f = function(i) {
      list(args = grid, vals = X_2[i, ])
    }
  )
  # bring into functional format
  sample_1_f <- quick_funcify(sample = sample_1, domain = c(0, 1), n_fourier_basis = n_basis)
  sample_2_f <- quick_funcify(sample = sample_2, domain = c(0, 1), n_fourier_basis = n_basis)
  # return as list
  return(list(
    sample_1 = sample_1, sample_2 = sample_2,
    sample_1_f = sample_1_f, sample_2_f = sample_2_f
  ))
}
