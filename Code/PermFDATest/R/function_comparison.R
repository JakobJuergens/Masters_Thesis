#' This function checks if one function is smaller than another function for all
#' points in a closed interval
#'
#' @param func_a: Function one
#' @param func_b: Function two
#' @param domain: interval to check
#' @param fourier: If true: func_a and func_b are interpreted as vectors of
#' fourier coefficients for the same fourier basis. This can be used to solve this
#' problem more efficiently.
#' @param grid: if fourier_coef == FALSE, this decides the grid that is used to check
#' if one function is always bigger than the other
#'
#' @return TRUE or FALSE depending on wheter func_a is always bigger than
#' func_b
func_comparison <- function(func_a, func_b, domain = c(0, 1), fourier = FALSE,
                            grid = NULL) {
  # This could be done via evaluation at a fine grid...
  # or more sophisticated by finding the zeros of the difference
  # There is literature for the case of a Fourier series
  # https://math.stackexchange.com/questions/370996/roots-of-a-finite-fourier-series

  # currently only works for basis with period 2*pi
  if (fourier == TRUE) {
    return(func_comparison_fourier(func_a = func_a, func_b = func_b, domain = domain))
  }
  # otherwise use grid_based approach
  else {
    return(func_comparison_grid(
      func_a = func_a, func_b = func_b,
      domain = domain, grid = grid
    ))
  }
}

#' This function checks if one function is weakly smaller than another function for all
#' points in a closed interval
#'
#' @param func_a: fd object for Function one
#' @param func_b: fd object for Function two
#' @param domain: interval to check
#'
#' @return TRUE or FALSE depending on wheter func_a is always weakly smaller than
#' func_b
func_comparison_fourier <- function(func_a, func_b, domain = c(0, 1)) {
  # find zeroes of the difference function
  zeroes <- fourier_zeroes(func_a = func_a, func_b = func_b, domain = domain)
  # if zeroes exists on the domain, then return false
  if(length(zeroes != 0)){
    # create grid of points to check between the zeroes
    if(length(zeroes > 1)){
      inner_points <- unlist(purrr::map(.x = 2:length(zeroes),
                                        .f = ~ mean(c(zeroes[.x - 1], zeroes[.x]))))
    }
    checking_grid <- c(domain[1], inner_points, domain[2])
    # evaluate functions on checking grid, to determine if one function is always
    # weakly bigger
    check_val_a <- fda::eval.fd(evalarg = checking_grid, fdobj = func_a)
    check_val_b <- fda::eval.fd(evalarg = checking_grid, fdobj = func_b)
    # return
    if(all(check_val_b >= check_val_a)){
      # this is the case we want to check for: there are zeroes but function_b
      # is still always weakly bigger
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  # if no zeroes exists on the domain check on an arbitrary point if func_a
  # is bigger then func b
  else{
    check_val_a <- fda::eval.fd(evalarg = mean(domain), fdobj = func_a)
    check_val_b <- fda::eval.fd(evalarg = mean(domain), fdobj = func_b)
    # if return whether function a is bigger than function b for all points in
    # the specified domain
    return(all(check_val_a <= check_val_b))
  }

}

#' This function calculates the zeroes of the difference of two functions
#' given in terms of the fourier coefficients of the same fourier basis
#' based on https://math.stackexchange.com/questions/370996/roots-of-a-finite-fourier-series
#'
#' @param func_a: fd object for Function one
#' @param func_b: fd object for Function two
#' @param domain: interval to check
#'
#' @return The zeroes of the difference function
fourier_zeroes <- function(func_a, func_b, domain = c(0, 1)) {
  diff_coefs <- func_a$coefs - func_b$coefs
  cos_coefs <- diff_coefs[which(1:length(diff_coefs) %% 2 == 1)]
  sin_coefs <- diff_coefs[which(1:length(diff_coefs) %% 2 == 0)]
  N <- length(sin_coefs)

  # This function currently only works if the last coefficient is non-zero
  # so implement a check for that case

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
  real_zeroes <- sort((Re(zeroes_b[which((abs(Im(zeroes_b)) < 10^(-8)))]) / (2*pi) * (domain[2] - domain[1]))
                      %% (domain[2] - domain[1]))
  # return the real zeroes
  return(real_zeroes)
}

#' This function checks if one function is smaller than another function for all
#' points in a closed interval
#'
#' @param func_a: Fourier coefficients for Function one
#' @param func_b: Fourier coefficients for Function two
#' @param domain: interval to check
#' @param grid: grid of points used to checks
#'
#' @return TRUE or FALSE depending on wheter func_a is always bigger than
#' func_b
func_comparison_grid <- function(func_a, func_b, domain = c(0, 1), grid) {
  # check if grid contains point outside the domain
  if(any(grid < domain[1] | grid > domain[2])){
    stop('grid for function comparison contains points outside the domain.')
  }
  # check if functions are fd objects
  if(fda::is.fd(func_a) & fda::is.fd(func_b)){
    # if both functions are fd objects, then use eval.fd for comparison on grid
  } else if(xor(fda::is.fd(func_a), fda::is.fd(func_b))){
    # this is currently not implemented because it's tedious and a weird usecase
    stop(paste0('It is currently not implemented that one function in the ',
                'function comparison is an fd object'))
  } else{
    # here I assume that both functions are just functions that can be evaluated
    # at a single point of the real line
    grid_a <- unlist(purrr::map(.x = grid, .f = ~ func_a(.x)))
    grid_b <- unlist(purrr::map(.x = grid, .f = ~ func_b(.x)))
    # compare grid values
    grid_compare <- grid_a <= grid_b
    # check if all points in grid_b are weakly bigger than in grid_a
    # and return correpsonding boolean
    return(all(grid_compare))
  }
  # quick debugging message
  stop("You shouldn't arrive here... (func_comparison_grid)")
}
