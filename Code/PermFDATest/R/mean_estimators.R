#' This function finds the estimated mean function from a given sample
#'
#' @param sample: sample, specified as a list where each element is one observation
#' @param interpolation_mode: string that determines the mode of interpolation between
#' discrete measurement points. Options: 'linear', 'fourier', 'eigen'
#' @param domain: vector with beginning and endpoint of the closed interval
#' that is the domain of the stochastic processes
#' @param n_basis: if interpolation mode is a basis based method,
#' this determines the number of basis functions used in the approximation
#' @param grid: grid used for the approximation of the mean function
#'
#' @return Placeholder
mean_estimator <- function(sample, interpolation_mode = "linear",
                           domain = c(0, 1), n_basis = NA, grid) {
  # These functions can be found in the file mean_estimators.R
  if (interpolation_mode == "linear") {
    return(linear_mean_estimator(sample = sample, domain = domain, grid = grid))
  }
  if (interpolation_mode == "fourier") {
    return(fourier_mean_estimator(sample = sample, domain = domain, n_basis = n_basis, grid = grid))
  }
  if (interpolation_mode == "eigen") {
    return(eigenbasis_mean_estimator(sample = sample, domain = domain, n_basis = n_basis, grid = grid))
  } else {
    (
      stop("Chosen Interpolation Mode is not available.")
    )
  }
}

#' This function finds the estimated mean function from a given sample using
#' linear interpolation between the discrete measurement points
#'
#' @param sample: sample, specified as a list where each element is one observation
#' @param domain: vector with beginning and endpoint of the closed interval
#' that is the domain of the stochastic processes
#' @param grid: grid used for the approximation of the mean function
#'
#' @return Placeholder
linear_mean_estimator <- function(sample, domain, grid) {
  # for all observations in the sample, use linear interpolation to
  # bring them to the specified grid
  grid_sample <- purrr::map(
    .x = sample,
    .f = function(obs) {
      linear_interpolation(observation = obs, grid = grid)
    }
  )
  # create contaioner for mean function
  est_mean <- list(
    args = grid,
    vals = rep(NA_real_, times = length(grid))
  )

  # fill vals with means of approximated observations
  est_mean$vals <- unlist(
    purrr::map(
      .x = 1:length(grid),
      .f = function(i) {
        mean(x = unlist(
          purrr::map(
            .x = 1:length(sample),
            .f = function(j) grid_sample[[j]]$vals[i]
          )
        ), na.rm = TRUE)
      }
    )
  )
  # return estimated mean object
  return(est_mean)
}

#' This is a helper function that performs linear interpolation for the
#' linear_mean_estimator function.
#'
#' @param observation: Single Functional Observation
#' @param grid: grid to be used for the approximation by linear interpolation
#'
#' @return An approximated observation at the desired grid points.
linear_interpolation <- function(observation, grid) {
  # Extract objects
  args <- observation$args
  vals <- observation$vals
  # Check if args are compatible with chosen grid
  if (min(args) > min(grid) | max(args) < max(grid)) {
    stop(paste0(
      "In linear interpolation: the end points of the observation lie within",
      "the desired grid. Linear Interpolation not possible."
    ))
  }
  # Create container for approximation values at grid
  grid_vals <- rep(x = NA_real_, times = length(grid))
  for (i in 1:length(grid)) {
    # if point occurs in sample, take the value
    if (grid[i] %in% args) {
      grid_vals[i] <- vals[which(args == grid[i])]
    } else {
      # determine left and right measurement point for interpolation
      left_arg <- max(args[which(args < grid[i])])
      right_arg <- min(args[which(args > grid[i])])
      # determine correpsonding values
      left_val <- vals[which(args == left_arg)]
      right_val <- vals[which(args == right_arg)]
      # calculate slope between points
      slope <- (right_val - left_val) / (right_arg - left_arg)
      # calculate linear interpolation for grid point
      grid_vals[i] <- left_val + (grid[i] - left_arg) * slope
    }
  }
  # return object in the typical format of an observation
  return(list(args = grid, vals = grid_vals))
}

#' This function finds the estimated mean function from a given sample using
#' an approximation using a truncated Fourier basis
#'
#' @param sample: sample, specified as a list where each element is one observation
#' @param domain: vector with beginning and endpoint of the closed interval
#' that is the domain of the stochastic processes
#' @param n_basis: the number of basis functions used in the approximation
#' @param grid: grid used for the approximation of the mean function
#'
#' @return An estimate of the mean function in the usual observation format
fourier_mean_estimator <- function(sample, domain, n_basis, grid) {
  # Create Fourier Basis of chosen size
  fourier_basis <- fda::create.fourier.basis(
    rangeval = domain, nbasis = n_basis,
    period = domain[2] - domain[1]
  )
  # Evaluate basis at grid points
  fourier_basis_eval <- fda::eval.basis(evalarg = grid, basisobj = fourier_basis)
  # Fit sample using Fourier Basis
  fitted_sample <- purrr::map(
    .x = sample,
    .f = function(obs) {
      fda::smooth.basis(
        argvals = obs$args,
        y = obs$vals,
        fdParobj = fourier_basis
      )
    }
  )
  # extract coefficients from fitted sample
  fitted_coefficients <- matrix(
    data = unlist(purrr::map(
      .x = fitted_sample,
      .f = function(fitted_obs) {
        fitted_obs$fd$coefs
      }
    )), nrow = n_basis, ncol = length(sample), byrow = FALSE
  )
  # get matrix with approximated values of functions
  sample_approx <- fourier_basis_eval %*% fitted_coefficients
  # get column-wise mean of matrix
  mean_vals <- rowMeans(sample_approx)
  # return mean object
  return(list(args = grid, vals = mean_vals))
}

#' This function finds the estimated mean function from a given sample using
#' an approximation using a truncated Eigenbasis (Karhunen-Loeve Representation)
#' Currently non-functional!
#'
#' @param sample: sample, specified as a list where each element is one observation
#' @param domain: vector with beginning and endpoint of the closed interval
#' that is the domain of the stochastic processes
#' @param n_basis: the number of basis functions used in the approximation
#' @param grid: grid used for the approximation of the mean function
#'
#' @return An estimate of the mean function in the usual observation format
eigenbasis_mean_estimator <- function(sample, domain, n_basis, grid) {
  # Create cubic bspline Basis with many functions
  bspline_basis <- fda::create.bspline.basis(rangeval = domain, nbasis = 100, norder = 4)
  # Fit sample using Fourier Basis
  fitted_sample <- purrr::map(
    .x = sample,
    .f = function(obs) {
      fda::smooth.basis(
        argvals = obs$args,
        y = obs$vals,
        fdParobj = bspline_basis
      )$fd
    }
  )
  # combine fd objects for fpca
  combined_fd_obj <- fda::fd(
    coef = matrix(
      data = unlist(
        purrr::map(.x = fitted_sample, .f = function(obs) obs$coefs)
      ),
      nrow = 100, ncol = length(sample), byrow = FALSE
    ),
    basis = fitted_sample[[1]]$basis
  )
  # conduct fpca (centerfns doesn't make sense here because we're looking for
  # the mean, but this is just for testing purposes)
  sample_fpca <- fda::pca.fd(fdobj = combined_fd_obj, nharm = n_basis, centerfns = TRUE)
  # return mean object
  return(list(
    args = grid,
    vals = fda::eval.fd(evalarg = grid, fdobj = sample_fpca$meanfd)
  ))
}
