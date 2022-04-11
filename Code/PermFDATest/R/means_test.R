#' This function calculates the t statistic for the means based test
#'
#' @param sample1: first sample, specified as a list where each element is one observation
#' @param sample2: second sample, specified as a list where each element is one observation
#' @param interpolation_mode: string that determines the mode of interpolation between
#' discrete measurement points. (Recommendation: 'bspline')
#' @param domain: vector with beginning and endpoint of the closed interval
#' that is the domain of the stochastic processes
#' @param n_basis: if interpolation mode is choses as a basis based method,
#' this determines the number of basis functions used in the approximation
#' @param grid: grid used for the approximation of the mean function
#'
#' @return The value of the t statistic for means based test
#' for the chosen samples.
#'
#' @export
means_t_stat <- function(sample1, sample2, interpolation_mode = "linear", domain = c(0, 1),
                         n_basis = NA, grid) {
  if (interpolation_mode == "eigen") {
    stop(paste0(
      "interpolation_mode == 'eigen' is currently not functional for the means-based test. ",
      "However, in the context of the means test it does not add useful functionality."
    ))
  }

  # calculate sample mean functions for both samples
  smpl1_mean <- mean_estimator(
    sample = sample1, interpolation_mode = interpolation_mode,
    domain = domain, n_basis = n_basis, grid = grid
  )
  smpl2_mean <- mean_estimator(
    sample = sample2, interpolation_mode = interpolation_mode,
    domain = domain, n_basis = n_basis, grid = grid
  )
  # if interpolation_mode == 'linear' create functional object describing the
  # squared difference of the mean functions
  if (interpolation_mode == "linear") {
    # calculate squared difference of mean functions
    mean_diff_sq <- (smpl2_mean$vals - smpl1_mean$vals)^2
    # calculate area of triangles (linear interpolation)
    interpolation_func <- approxfun(x = grid, y = mean_diff_sq, method = "linear")
    # integration over squared difference of mean functions
  }
  # if functional methods were chosen, directly use the provided functions
  # to create the squared difference of the means
  else if (interpolation_mode %in% c("fourier", "bspline")) {
    interpolation_func <- function(x) {
      (smpl1_mean(x) - smpl2_mean(x))^2
    }
  } else {
    stop("Chosen interpolation_mode is not implemented.")
  }
  # calculate the test value via integration over the created function
  test_value <- integrate(f = interpolation_func, lower = domain[1], upper = domain[2])
  # return the value of the integral
  return(test_value$value)
}

#' This function calculates / approximates the critical value for the means based test
#'
#' @param sample1: first sample, specified as a list where each element is one observation
#' @param sample2: second sample, specified as a list where each element is one observation
#' @param interpolation_mode: string that determines the mode of interpolation between
#' discrete measurement points. (Recommendation: 'bspline')
#' @param domain: vector with beginning and endpoint of the closed interval
#' that is the domain of the stochastic processes
#' @param n_basis: if interpolation mode is choses as a basis based method,
#' this determines the number of basis functions used in the approximation
#' @param grid: grid used for the approximation of the mean function
#'
#' @return The value of the t statistic for means based test
#' for the chosen samples.
#'
#' @export
means_crit_value <- function(alpha = 0.05, full = TRUE, approxQ, sample1, sample2,
                             interpolation_mode = "linear", domain = c(0, 1),
                             n_basis = NA, grid){

  if(missing(approxQ)){
    critical_value <- perm_crit_value(alpha = alpha, full = full,
                                      sample1 = sample1, sample2 = sample2, t_stat_func = means_t_stat,
                                      interpolation_mode = interpolation_mode, domain = domain,
                                      n_basis = n_basis, grid = grid)
  } else{
    critical_value <- perm_crit_value(alpha = alpha, full = full, approxQ = approxQ,
                                      sample1 = sample1, sample2 = sample2, t_stat_func = means_t_stat,
                                      interpolation_mode = interpolation_mode, domain = domain,
                                      n_basis = n_basis, grid = grid)
  }

  return(critical_value)
}
