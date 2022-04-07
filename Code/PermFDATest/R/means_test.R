#' This function finds the estimated mean function from a given sample
#'
#' @param sample: sample, specified as a list where each element is one observation
#' @param interpolation_mode: string that determines the mode of interpolation between
#' discrete measurement points. Options: 'linear', 'fourier'
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
  } else {
    (
      stop("Chosen Interpolation Mode is not available.")
    )
  }
}

#' This function performs the mean-based test for two samples with a chosen
#' set of parameters.
#'
#' @param sample1: first sample, specified as a list where each element is one observation
#' @param sample2: second sample, specified as a list where each element is one observation
#' @param interpolation_mode: string that determines the mode of interpolation between
#' discrete measurement points.
#' @param domain: vector with beginning and endpoint of the closed interval
#' that is the domain of the stochastic processes
#' @param n_basis: if interpolation mode is choses as a basis based method,
#' this determines the number of basis functions used in the approximation
#' @param ...: additional arguments that are relayed to t_stat_func
#'
#' @return Placeholder
#'
#' @export
means_test <- function(sample1, sample2, interpolation_mode = "linear", domain = c(0, 1),
                       n_basis = NA) {
  # calculate sample mean functions for both samples
  smpl1_mean <- mean_estimator(
    sample = sample1, interpolation_mode = interpolation_mode,
    domain = domain, n_basis = n_basis
  )
  smpl2_mean <- mean_estimator(
    sample = sample2, interpolation_mode = interpolation_mode,
    domain = domain, n_basis = n_basis
  )
}
