#' Returns the value of an empirical distribution function of a given sample
#' for a given function
#'
#' @param sample: Sample of functional data
#' @param func: Function to evaluate the empirical distribution function at
#' @param domain: Domain of the functions in both the sample and the comparison
#' function
#'
#' @return An approximated value of the empirical distribution function
empirical_dist_func_fourier <- function(sample, func, domain = c(0, 1)) {
  # determine functions that are bigger than the comparison function everywhere
  wkl_smaller <- unlist(purrr::map(
    .x = 1:ncol(sample),
    .f = ~ func_comparison_fourier(func_a = sample[,.x], func_b = func, domain = domain)
  ))
  # return mean
  return(mean(wkl_smaller))
}

#' This function checks if one function is weakly smaller than another function for all
#' points in a closed interval
#'
#' @param func_a: fd coefficient matrix for Function one
#' @param func_b: fd coefficient matrix for Function two
#' @param domain: interval to check
#'
#' @return TRUE or FALSE depending on wheter func_a is always weakly smaller than
#' func_b
func_comparison_fourier <- function(func_a, func_b, domain = c(0, 1)) {
  # find zeroes of the difference function
  zeroes <- fourier_zeroes(func_a = func_a, func_b = func_b, domain = domain)
  # if zeroes exists on the domain, then return false
  if (length(zeroes != 0)) {
    # create grid of points to check between the zeroes
    if (length(zeroes > 1)) {
      inner_points <- unlist(purrr::map(
        .x = 2:length(zeroes),
        .f = ~ mean(c(zeroes[.x - 1], zeroes[.x]))
      ))
    }
    checking_grid <- c(domain[1], inner_points, domain[2])
    # evaluate functions on checking grid, to determine if one function is always
    # weakly bigger
    check_val_a <- fourier_eval(x = checking_grid, coefs = func_a, domain = domain)
    check_val_b <- fourier_eval(x = checking_grid, coefs = func_b, domain = domain)
  }
  # if no zeroes exists on the domain check on an arbitrary point if func_a
  # is bigger then func b
  else {
    check_val_a <- fourier_eval(x = mean(domain), coefs = func_a, domain = domain)
    check_val_b <- fourier_eval(x = mean(domain), coefs = func_b, domain = domain)
    # if return whether function a is bigger than function b for all points in
    # the specified domain
  }
  return(all(check_val_b >= check_val_a))
}

#' This function calculates the zeroes of the difference of two functions
#' given in terms of the fourier coefficients of the same fourier basis
#' based on https://math.stackexchange.com/questions/370996/roots-of-a-finite-fourier-series
#'
#' @param func_a: fd coefficient matrix for Function one
#' @param func_b: fd coefficient matrix for Function two
#' @param domain: interval to check
#'
#' @return The zeroes of the difference function
fourier_zeroes <- function(func_a, func_b, domain = c(0, 1)) {
  diff_coefs <- func_a - func_b
  cos_coefs <- diff_coefs[which(1:length(diff_coefs) %% 2 == 1)]
  sin_coefs <- diff_coefs[which(1:length(diff_coefs) %% 2 == 0)]
  N <- length(sin_coefs)

  # check if all entries of coefficient vector are zero
  # Then the function is weakly bigger
  if (all(diff_coefs == 0)) {
    return(c())
  }

  # This function currently only works if the last coefficient is non-zero
  # so implement a check for that case
  while (cos_coefs[N + 1] == 0 & sin_coefs[N] == 0) {
    cos_coefs <- cos_coefs[-(N + 1)]
    sin_coefs <- sin_coefs[-N]
    N <- N - 1
  }

  h_coefs <- c(
    # the first N coefficients
    unlist(purrr::map(
      .x = 0:(N - 1),
      .f = function(i) {
        complex(real = cos_coefs[N + 1 - i], imaginary = sin_coefs[N - i])
      }
    )),
    # N+1 coefficient
    2 * cos_coefs[1],
    # the last N coefficients
    unlist(purrr::map(
      .x = (N + 1):(2 * N),
      .f = function(i) {
        complex(real = cos_coefs[i + 1 - N], imaginary = -sin_coefs[i - N])
      }
    ))
  )

  # define 2N x 2N matrix
  B <- matrix(data = NA, nrow = 2 * N, ncol = 2 * N)
  # fill matrix with appropriate entries
  for (k in 1:(2 * N)) {
    for (j in 1:(2 * N - 1)) {
      B[j, k] <- ifelse(test = (j == k - 1), yes = 1, no = 0)
    }
    j <- 2 * N
    B[j, k] <- -h_coefs[k] / complex(real = cos_coefs[N + 1], imaginary = -sin_coefs[N])
  }
  # calculate complex eigenvalues of matrix
  eigen_b <- eigen(x = B)$values
  # calculate zeroes
  zeroes_b <- complex(real = 0, imaginary = -1) * log(eigen_b)
  # real zeroes
  real_zeroes <- sort((Re(zeroes_b[which((abs(Im(zeroes_b)) < 10^(-8)))]) / (2 * pi) * (domain[2] - domain[1]))
                      %% (domain[2] - domain[1]))
  # return the real zeroes
  return(real_zeroes)
}
