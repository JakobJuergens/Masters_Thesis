#' This function checks if one function is bigger than another function for all
#' points in a closed interval
#'
#' @param func_a: Function one
#' @param func_b: Function two
#' @param domain: interval to check
#' @param fourier_coef: If true: func_a and func_b are interpreted as vectors of
#' fourier coefficients for the same fourier basis. This can be used to solve this
#' problem more efficiently.
#' @param grid: if fourier_coef == FALSE, this decides the grid that is used to check
#' if one function is always bigger than the other
#'
#' @return TRUE or FALSE depending on wheter func_a is always bigger than
#' func_b
func_comparison <- function(func_a, func_b, domain = c(0,1), fourier_coef = FALSE,
                            grid = NULL){
  # This could be done via evaluation at a fine grid...
  # or more sophisticated by finding the zeros of the difference
  # There is literature for the case of a Fourier series
  # https://math.stackexchange.com/questions/370996/roots-of-a-finite-fourier-series

  # currently only works for basis with period 2*pi
  if(fourier_coef == TRUE){
    return(func_comparison_fourier(func_a = func_a, func_b = func_b, domain = domain))
  }
  # otherwise use grid_based approach
  else{
    return(func_comparison_grid(func_a = func_a, func_b = func_b,
                                domain = domain, grid = grid))
  }

}

#' This function checks if one function is bigger than another function for all
#' points in a closed interval
#'
#' @param func_a: Fourier coefficients for Function one
#' @param func_b: Fourier coefficients for Function two
#' @param domain: interval to check
#'
#' @return TRUE or FALSE depending on wheter func_a is always bigger than
#' func_b
func_comparison_fourier <- function(func_a, func_b, domain = c(0,1)){
  zeroes <- fourier_zeroes(func_a = func_a, func_b = func_b, domain = domain)
}

#' This function calculates the zeroes of the difference of two functions
#' given in terms of the fourier coefficients of the same fourier basis
#'
#' @param func_a: Fourier coefficients for Function one
#' @param func_b: Fourier coefficients for Function two
#' @param domain: interval to check
#'
#' @return The zeroes of the difference function
fourier_zeroes <- function(func_a, func_b, domain = c(0,1)){
  diff_coefs <- func_a - func_b
  cos_coefs <- diff_coefs[which(1:length(diff_coefs) %% 2 == 1)]
  sin_coefs <- diff_coefs[which(1:length(diff_coefs) %% 2 == 0)]
  N <- length(sin_coefs)

  h_coefs <- c(
    # the first N coefficients
    unlist(purrr::map(.x = 0:(N-1),
                      .f = function(i){
                        complex(real = cos_coefs[N+1-i], imaginary = sin_coefs[N-i])
                      }
    )
    ),
    # N+1 coefficient
    2*cos_coefs[1],
    # the last N coefficients
    unlist(purrr::map(.x = (N+1):(2*N),
                      .f = function(i){
                        complex(real = cos_coefs[i+1-N], imaginary = -sin_coefs[i-N])
                      }
    )
    )
  )

  # define 2N x 2N matrix
  B <- matrix(data = NA, nrow = 2*N, ncol = 2*N)
  # fill matrix with appropriate entries
  for(k in 1:(2*N)){
    for(j in 1:(2*N - 1)){
      B[j,k] <- ifelse(test = (j == k - 1), yes = 1, no = 0)
    }
    j <- 2*N
    B[j,k] <- - h_coefs[k] / complex(real = cos_coefs[N+1], imaginary = -sin_coefs[N])
  }
  # calculate complex eigenvalues of matrix
  eigen_b <- eigen(x = B)$values
  # calculate zeroes
  zeroes_b <- complex(real = 0, imaginary = -1)*log(eigen_b)
  # real zeroes
  real_zeroes <- sort(Re(zeroes_b[which((abs(Im(zeroes_b)) < 10^(-12)))]))
  # return the real zeroes
  return(real_zeroes)
}

#' This function checks if one function is bigger than another function for all
#' points in a closed interval
#'
#' @param func_a: Fourier coefficients for Function one
#' @param func_b: Fourier coefficients for Function two
#' @param domain: interval to check
#' @param grid: grid of points used to checks
#'
#' @return TRUE or FALSE depending on wheter func_a is always bigger than
#' func_b
func_comparison_grid <- function(func_a, func_b, domain = c(0,1), grid){

}
