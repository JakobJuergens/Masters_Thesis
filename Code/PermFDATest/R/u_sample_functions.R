#' This function is used to sample from multivariate standard normals
#' and acts as a wrapper around MASS::mvrnorm to interface with the functions
#' that construct the measure on L2.
#'
#' @param n_basis: number of basis functions used in the derivation of
#' the measure. Specifies the number of error terms that are drawn.
#'
#' @return: A specified number of jointly normal error terms for the construction
#' of the measure
u_norm <- function(n_basis){
  errors <- MASS::mvrnorm(n = 1, mu = rep(0, times = n_basis),
                          Sigma = diag(rep(1, times = n_basis)))
  return(errors)
}

#' This function is used to sample from independent uniform errors with support [-3,3]
#' to interface with the functions that construct the measure on L2.
#'
#' @param n_basis: number of basis functions used in the derivation of
#' the measure. Specifies the number of error terms that are drawn.
#'
#' @return: A specified number of independent uniform error terms for the construction
#' of the measure
u_unif <- function(n_basis){
  errors <- runif(n = n_basis, min = -3^(1/3), max = 3^(1/3))
  return(errors)
}

#' This function is used to sample from independent t-distributed random variables
#'
#' @param n_basis: number of basis functions used in the derivation of
#' the measure. Specifies the number of error terms that are drawn.
#' @param df: degrees of freedom for the t distribution
#'
#' @return: A specified number of t distributed error terms
u_t <- function(n_basis, df = 2){
  errors <- rt(n = n_basis, df = df)

  return(errors)
}

