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
  if (interpolation_mode == "bspline") {
    return(bspline_mean_estimator(sample = sample, domain = domain, n_basis = n_basis, grid = grid))
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
      approx(x = obs$args, y = obs$vals, xout = grid, method = "linear")
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
            .f = function(j) grid_sample[[j]]$y[i]
          )
        ), na.rm = TRUE)
      }
    )
  )
  # return estimated mean object
  return(est_mean)
}

#' This function finds the estimated mean function from a given sample using
#' an approximation using a truncated Fourier basis
#'
#' @param sample: sample, specified as a list where each element is one observation
#' @param domain: vector with beginning and endpoint of the closed interval
#' that is the domain of the stochastic processes
#' @param n_basis: the number of basis functions used in the approximation
#'
#' @return A function describing the mean function
fourier_mean_estimator <- function(sample, domain, n_basis, grid) {
  # Create Fourier Basis of chosen size
  fourier_basis <- fda::create.fourier.basis(
    rangeval = domain, nbasis = n_basis,
    period = domain[2] - domain[1]
  )
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
  # get average cofficients to calculate mean function as fd object
  avg_coefs <- rowMeans(fitted_coefficients)
  # create mean fd object
  mean_fd_obj <- fda::fd(
    coef = avg_coefs,
    basis = fitted_sample[[1]]$fd$basis
  )
  # return mean object
  return(mean_function = function(x) {
    fda::eval.fd(evalarg = x, fdobj = mean_fd_obj)
  })
}

#' This function finds the estimated mean function from a given sample using
#' an approximation using a truncated cubic bspline basis
#'
#' @param sample: sample, specified as a list where each element is one observation
#' @param domain: vector with beginning and endpoint of the closed interval
#' that is the domain of the stochastic processes
#' @param n_basis: the number of basis functions used in the approximation
#'
#' @return A function describing the mean function
bspline_mean_estimator <- function(sample, domain, n_basis, grid) {
  # Create Fourier Basis of chosen size
  bspline_basis <- fda::create.bspline.basis(
    rangeval = domain, nbasis = n_basis, norder = 4
  )
  # Fit sample using Fourier Basis
  fitted_sample <- purrr::map(
    .x = sample,
    .f = function(obs) {
      fda::smooth.basis(
        argvals = obs$args,
        y = obs$vals,
        fdParobj = bspline_basis
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
  # get average cofficients to calculate mean function as fd object
  avg_coefs <- rowMeans(fitted_coefficients)
  # create mean fd object
  mean_fd_obj <- fda::fd(
    coef = avg_coefs,
    basis = fitted_sample[[1]]$fd$basis
  )
  # return mean object
  return(mean_function = function(x) {
    fda::eval.fd(evalarg = x, fdobj = mean_fd_obj)
  })
}

#' This function finds the estimated mean function from a given sample using
#' an approximation using a truncated Eigenbasis (Karhunen-Loeve Representation)
#' Currently non-functional!
#'
#' @param sample: sample, specified as a list where each element is one observation
#' @param domain: vector with beginning and endpoint of the closed interval
#' that is the domain of the stochastic processes
#' @param n_basis: the number of basis functions used in the approximation
#'
#' @return A function describing the mean function
eigenbasis_mean_estimator <- function(sample, domain, n_basis, grid) {
  # figure out number of basis functions to use for the construction of the
  # functional principal components
  nbasis <- min(c(100, unlist(purrr::map(.x = sample, .f = ~ length(.x$args)))))
  # Create cubic bspline Basis with many functions
  bspline_basis <- fda::create.bspline.basis(rangeval = domain, nbasis = nbasis, norder = 4)
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
  # return mean function
  return(mean_function = function(x) {
    fda::eval.fd(evalarg = x, fdobj = sample_fpca$meanfd)
  })
}
