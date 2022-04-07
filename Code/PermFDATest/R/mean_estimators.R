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
  # return list of approximated observations
  return(grid_sample)
}

#' This function approximates an observation on a specified grid
#' using linear interpolation
#'
#' @param observation: Placeholder
#' @param grid: Placeholder
#'
#' @return Placeholder
linear_interpolation <- function(observation, grid) {
  # Extract objects
  args <- observation$args
  vals <- observation$vals
  # Create container for approximation values at grid
  grid_vals <- rep(x = NA_real_, times = length(grid))
  for (i in 1:length(grid)) {
    # if point occurs in sample, take the value
    if (grid[i] %in% args) {
      grid_vals[i] <- vals[which(args == grid[i])]
    } else {
      # determine left and right measurement point for interpolation
      left_arg <- max(args[which[args < grid[i]]])
      right_arg <- min(args[which[args > grid[i]]])
      # determine correpsonding values
      left_val <- vals[which(args == left_arg)]
      right_val <- vals[which(args == right_arg)]
      # calculate slope between points
      slope <- (right_val - left_val) / (right_arg - left_arg)
      # calculate linear interpolation for grid point
      grid_vals[i] <- left_val + (grid - left_arg) * slope
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
#' @return Placeholder
fourier_mean_estimator <- function(sample, domain, n_basis, grid) {

}
