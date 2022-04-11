#' This function calculates the means of the fourier coefficients as described
#' in section 5 of Bugni, Horowitz (2021) for a chosen basis and a function  w
#'
#' @param w_func: function that is large in the areas of the domain the
#' investigator expects differences in the distribution functions
#' @param basis: type of functional basis to use (has to be an orthogonal basis)
#' @param n_basis: number of basis functions
#' @param domain: vector of two points (start and endpoint of the closed interval)
#'
#' @return: Vector of length n_basis containing the resulting means of the
#' fourier coefficients
fourier_basis_coef_means <- function(w_func, basis = "fourier", n_basis, domain = c(0, 1)) {
  # create chosen basis (has to be orthogonal basis!)
  if (basis == "fourier") {
    my_basis <- fda::create.fourier.basis(
      rangeval = domain, nbasis = n_basis,
      period = domain[2] - domain[1]
    )
  } else {
    stop("Chosen basis not implemented.")
  }
  # extract basis functions as function objects
  basis_functions <- purrr::map(
    .x = 1:n_basis,
    .f = function(i) {
      basis_func <- function(x) {
        fda::eval.basis(evalarg = x, basisobj = my_basis)[1, i]
      }
    }
  )
  # calculate means of fourier coefficients
  coef_means <- purrr::map(
    .x = basis_functions,
    .f = function(f) {
      fourier_coef_mean(w_func = w_func, basis_func = f, domain = domain)
    }
  )
  # return vector of means of fourier coefficients
  return(unlist(coef_means))
}

#' This function calculates the desired mean of a fourier coefficient for
#' a single basis function
#'
#' @param w_func: function that is large in the areas of the domain the
#' investigator expects differences in the distribution functions
#' @param basis_func: basis function to calculate the fourier coefficient for
#' @param domain: vector of two points (start and endpoint of the closed interval)
#'
#' @return: Double that is the desired mean of the fourier coefficient
fourier_coef_mean <- function(w_func, basis_func, domain = c(0, 1)) {
  # define function to integrate over
  prod_func <- function(x) {
    return(w_func(x) * basis_func(x))
  }
  # integrate over product function
  coef_mean <- integrate(f = prod_func, lower = domain[1], upper = domain[2])$value
  # return appropriate mean object
  return(coef_mean)
}
