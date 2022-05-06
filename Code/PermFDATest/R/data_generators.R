#' This function generates a sample for testing purposes
#'
#' @param n_obs: Number of observations in sample
#' @param domain: beginning and endpoint of domain (closed interval)
#'
#' @return: A sample in the typical format
sample_generator_1 <- function(n_obs = 100, domain = c(0, 1)) {
  # generate random number of measurement points for each observation
  n_points <- unlist(purrr::map(.x = n_obs, .f = ~sample(x = 50:150, replace = TRUE)))

  return(purrr::map(.x = 1:n_obs,
                    .f = function(i) obs_generator_1(domain = domain, n_points = n_points[i]))
         )
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
  n_points <- unlist(purrr::map(.x = n_obs, .f = ~sample(x = 50:150, replace = TRUE)))

  return(purrr::map(.x = 1:n_obs,
                    .f = function(i) obs_generator_2(domain = domain, n_points = n_points[i]))
  )
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
quick_funcify <- function(sample, domain = c(0,1), n_fourier_basis){
  fourier_basis <- fda::create.fourier.basis(rangeval = domain, nbasis = n_fourier_basis)

  data_fd <- purrr::map(
    .x = sample,
    .f = function(obs){
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
