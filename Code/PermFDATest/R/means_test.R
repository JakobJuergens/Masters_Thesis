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
