#' Returns the value of an empirical distribution function of a given sample
#' for a given function
#'
#' @param sample: Sample of functional data
#' @param func: Function to evaluate the empirical distribution function at
#' @param domain: Domain of the functions in both the sample and the comparison
#' function
#' @param grid: grid used to decide whether the functions are always bigger
#' @param fourier: Boolean that decided whether the method for fourier zeroes is
#' used to compare the functions
#' @param n_fourier_basis: if fourier == TRUE, this decides the number of
#' fourier basis functions to use
#'
#' @return An approximated value of the empirical distribution function
empirical_dist_func <- function(sample, func, domain = c(0, 1), grid = NULL,
                                fourier = FALSE, n_fourier_basis = NULL) {
  # check for all functions in the sample whether they are always bigger than
  # func for all points in the domain
  if (fourier == TRUE) {
    return(empirical_dist_func_fourier(
      sample = sample, func = func,
      domain = domain, n_fourier_basis = n_fourier_basis
    ))
  } else {
    return(empirical_dist_func_grid(
      sample = sample, func = func,
      domain = domain, grid = grid
    ))
  }
}

#' Returns the value of an empirical distribution function of a given sample
#' for a given function
#'
#' @param sample: Sample of functional data
#' @param func: Function to evaluate the empirical distribution function at
#' @param domain: Domain of the functions in both the sample and the comparison
#' function
#' @param n_fourier_basis: if fourier == TRUE, this decides the number of
#' fourier basis functions to use
#'
#' @return An approximated value of the empirical distribution function
empirical_dist_func_fourier <- function(sample, func, domain = c(0, 1), n_fourier_basis) {
  # determine functions that are bigger than the comparison function everywhere
  bigger <- unlist(purrr::map(
    .x = sample,
    .f = ~ func_comparison_fourier(func_a = .x, func_b = func, domain = domain)
  ))
  # return mean
  return(mean(bigger))
}

#' Returns the value of an empirical distribution function of a given sample
#' for a given function
#'
#' @param sample: Sample of functional data
#' @param func: Function to evaluate the empirical distribution function at
#' @param domain: Domain of the functions in both the sample and the comparison
#' function
#' @param grid: grid used to decide whether the functions are always bigger
#'
#' @return An approximated value of the empirical distribution function
empirical_dist_func_grid <- function(sample, func, domain = c(0, 1), grid = NULL) {

}
