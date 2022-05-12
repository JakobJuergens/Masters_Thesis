#' This function generates a sample for testing purposes
#'
#' @param n_obs: Number of observations in sample
#' @param domain: beginning and endpoint of domain (closed interval)
#'
#' @return: A sample in the typical format
sample_generator_1 <- function(n_obs = 100, domain = c(0, 1)) {
  # generate random number of measurement points for each observation
  n_points <- unlist(purrr::map(.x = n_obs, .f = ~ sample(x = 50:150, replace = TRUE)))

  return(purrr::map(
    .x = 1:n_obs,
    .f = function(i) obs_generator_1(domain = domain, n_points = n_points[i])
  ))
}

#' This function generates an observation for testing purposes
#'
#' @param domain: beginning and endpoint of domain (closed interval)
#' @param n_points: number of measurement points in observation
#'
#' @return: An observation in the typical format
obs_generator_1 <- function(domain = c(0, 1), n_points = 100) {
  # generate points for sampling
  sample_points <- seq(
    from = domain[1] + (domain[2] - domain[1]) / (5 * n_points),
    to = domain[2] - (domain[2] - domain[1]) / (5 * n_points),
    length.out = 5 * n_points
  )
  # find arguments
  args <- c(
    domain[1],
    sort(sample(x = sample_points, size = n_points - 2, replace = FALSE)),
    domain[2]
  )
  # generate some values
  vals <- 10 + 15 * args + rnorm(n = n_points)

  # return observation
  return(list(args = args, vals = vals))
}

#' This function generates a sample for testing purposes
#'
#' @param n_obs: Number of observations in sample
#' @param domain: beginning and endpoint of domain (closed interval)
#'
#' @return: A sample in the typical format
sample_generator_2 <- function(n_obs = 100, domain = c(0, 1)) {
  # generate random number of measurement points for each observation
  n_points <- unlist(purrr::map(.x = n_obs, .f = ~ sample(x = 50:150, replace = TRUE)))

  return(purrr::map(
    .x = 1:n_obs,
    .f = function(i) obs_generator_2(domain = domain, n_points = n_points[i])
  ))
}

#' This function generates an observation for testing purposes
#'
#' @param domain: beginning and endpoint of domain (closed interval)
#' @param n_points: number of measurement points in observation
#'
#' @return: An observation in the typical format
obs_generator_2 <- function(domain = c(0, 1), n_points = 100) {
  # generate points for sampling
  sample_points <- seq(
    from = domain[1] + (domain[2] - domain[1]) / (5 * n_points),
    to = domain[2] - (domain[2] - domain[1]) / (5 * n_points),
    length.out = 5 * n_points
  )
  # find arguments
  args <- c(
    domain[1],
    sort(sample(x = sample_points, size = n_points - 2, replace = FALSE)),
    domain[2]
  )
  # generate some values
  vals <- -10 + 45 * args + rnorm(n = n_points)

  # return observation
  return(list(args = args, vals = vals))
}


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

  data_fd <- purrr::map(
    .x = sample,
    .f = function(obs) {
      fda::smooth.basis(argvals = obs$args, y = obs$vals, fdParobj = fourier_basis)$fd
    }
  )

  return(data_fd)
}

#' This function transforms a sample of functional data as it is typically generated
#' by functions from the package fda into the format that is used for example
#' for the function empirical_dist_func
#'
#' @param orgnl_sample: fda object representing multiple observations;
#' one observation per column of the coef object
#'
#' @return: sample in the format that is used by this package;
#' list containing elements, each of which is a single observation
#' @export
func_sample_transform <- function(orgnl_sample) {
  # ectract basis object from original sample
  obj_basis <- orgnl_sample$basis
  # find number of observatioons
  n_obs <- ncol(orgnl_sample$coefs)
  # generate new sample by transforming each observation into its own
  # fd object
  new_sample <- purrr::map(
    .x = 1:n_obs,
    .f = ~ fda::fd(
      coef = orgnl_sample$coef[, .x],
      basisobj = obj_basis
    )
  )
  # return new sample
  return(new_sample)
}

#' This function transforms a sample of functional data as it is used by this package
#' into the format that is used by the package fda
#'
#' @param orgnl_sample: fda object representing multiple observations;
#' as a list of individual fda objects
#'
#' @return: sample in the format that is used by fda
#' @export
func_sample_transform2 <- function(orgnl_sample) {
  # ectract basis object from original sample
  obj_basis <- orgnl_sample[[1]]$basis
  # find number of observatioons
  n_obs <- length(orgnl_sample)
  # generate new sample by transforming each observation into its own
  # fd object
  coefs <- matrix(data = unlist(
    purrr::map(
    .x = 1:n_obs,
    .f = ~ orgnl_sample[[.x]]$coefs
  )), nrow = obj_basis$nbasis, ncol = n_obs, byrow = FALSE)
  # return new sample
  return(fda::fd(coef = coefs, basisobj = obj_basis))
}

#' This function generates two samples for the simulation of type 1
#' as described in my master's thesis.
#'
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
sim_1_generator <- function(n_obs_1, n_obs_2, mean, rho, sigma, grid) {
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
  sample_1_f <- quick_funcify(sample = sample_1, domain = c(0, 1), n_fourier_basis = 25)
  sample_2_f <- quick_funcify(sample = sample_2, domain = c(0, 1), n_fourier_basis = 25)
  # return as list
  return(list(
    sample_1 = sample_1, sample_2 = sample_2,
    sample_1_f = sample_1_f, sample_2_f = sample_2_f
  ))
}

#' This function generates two samples for the simulation of type 2
#' as described in my master's thesis.
#'
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
sim_2_generator <- function(n_obs_1, n_obs_2, mean, mean_shift, rho, sigma, grid) {
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
  sample_1_f <- quick_funcify(sample = sample_1, domain = c(0, 1), n_fourier_basis = 25)
  sample_2_f <- quick_funcify(sample = sample_2, domain = c(0, 1), n_fourier_basis = 25)
  # return as list
  return(list(
    sample_1 = sample_1, sample_2 = sample_2,
    sample_1_f = sample_1_f, sample_2_f = sample_2_f
  ))
}

#' This function generates two samples for the simulation of type 3
#' as described in my master's thesis.
#'
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
sim_3_generator <- function(n_obs_1, n_obs_2, mean, rho, rho_shift, sigma, grid) {
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
  sample_1_f <- quick_funcify(sample = sample_1, domain = c(0, 1), n_fourier_basis = 25)
  sample_2_f <- quick_funcify(sample = sample_2, domain = c(0, 1), n_fourier_basis = 25)
  # return as list
  return(list(
    sample_1 = sample_1, sample_2 = sample_2,
    sample_1_f = sample_1_f, sample_2_f = sample_2_f
  ))
}

#' This function generates two samples for the simulation of type 4
#' as described in my master's thesis.
#'
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
sim_4_generator <- function(n_obs_1, n_obs_2, mean, rho, sigma, sigma_shift, grid) {
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
  sample_1_f <- quick_funcify(sample = sample_1, domain = c(0, 1), n_fourier_basis = 25)
  sample_2_f <- quick_funcify(sample = sample_2, domain = c(0, 1), n_fourier_basis = 25)
  # return as list
  return(list(
    sample_1 = sample_1, sample_2 = sample_2,
    sample_1_f = sample_1_f, sample_2_f = sample_2_f
  ))
}
