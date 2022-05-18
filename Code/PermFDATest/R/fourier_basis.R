#' This is a helper function to not use the fda package in the function evaluation and comparison
#'
#' @param x: point or vector of points to evaluate the function at
#' @param domain: domain of the basis
#' @param num_func: Which fourier function to evaluate
fourier_func <- function(x, domain = c(0, 1), num_func) {
  if (num_func == 1) {
    return(rep(x = 1 / sqrt(domain[2] - domain[1]), times = length(x)))
  } else if (num_func %% 2 == 0) {
    return(sqrt(2) / sqrt((domain[2] - domain[1])) * sin((num_func %/% 2) * (2 * pi) / (domain[2] - domain[1]) * (x - domain[1])))
  } else {
    return(sqrt(2) / sqrt((domain[2] - domain[1])) * cos((num_func %/% 2) * (2 * pi) / (domain[2] - domain[1]) * (x - domain[1])))
  }
}

#' This is a helper function that evaluates a functional object given as its
#' fourier coefficients at points
#'
#' @param x: point or vector of points to evaluate the function at
#' @param coefs: matrix or vector of coefficients identifying the functional objects
#' @param domain: domain of the basis
#' @param num_func: Which fourier function to evaluate
fourier_eval <- function(x, coefs, domain = c(0, 1)) {
  # check if all points lie in the domain
  if (!all(x >= domain[1] & x <= domain[2])) {
    stop("Evaluating points outside basis domain.")
  }

  if (is.matrix(coefs)) {
    fourier_eval_matrix(x, coefs, domain = domain)
  } else if (is.vector(coefs)) {
    fourier_eval_vec(x, coefs, domain = domain)
  } else {
    stop("Argument coefs is misshaped.")
  }
}

#' This is a helper function that evaluates a functional object given as its
#' fourier coefficients at points
#'
#' @param x: point or vector of points to evaluate the function at
#' @param coefs: matrix of coefficients identifying the functional objects
#' @param domain: domain of the basis
#' @param num_func: Which fourier function to evaluate
fourier_eval_matrix <- function(x, coefs, domain = c(0, 1)) {
  # determine number of functions
  num_func <- ncol(coefs)
  # determine number of basis functions
  num_basis <- nrow(coefs)
  # determine number of points
  num_points <- length(x)
  # calculate values of basis functions at points
  basis_values <- matrix(
    data = unlist(
      purrr::map(
        .x = 1:num_basis,
        .f = ~ fourier_func(x = x, domain = domain, num_func = .x)
      )
    ), nrow = num_points, ncol = num_basis, byrow = FALSE)
  # calculate values of functional objects
  func_values <- basis_values %*% coefs
  # return values of the functional object
  return(func_values)
}

#' This is a helper function that evaluates a functional object given as its
#' fourier coefficients at points
#'
#' @param x: point or vector of points to evaluate the function at
#' @param coefs: vector of coefficients identifying the functional object
#' @param domain: domain of the basis
#' @param num_func: Which fourier function to evaluate
fourier_eval_vec <- function(x, coefs, domain = c(0, 1)) {
  # determine number of basis functions
  num_basis <- length(coefs)
  # determine number of points
  num_points <- length(x)
  # calculate values of basis functions at points
  basis_values <-  matrix(
    data = unlist(
      purrr::map(
        .x = 1:num_basis,
        .f = ~ fourier_func(x = x, domain = domain, num_func = .x)
      )
    ), nrow = num_points, ncol = num_basis, byrow = FALSE)
  # calculate values of functional objects
  func_values <- as.vector(coefs %*% t(basis_values))
  # return values of the functional object
  return(func_values)
}
