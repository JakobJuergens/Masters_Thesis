#' This function calculates the means of the fourier coefficients as described
#' in section 5 of Bugni, Horowitz (2021) for a chosen basis and a function  w
#'
#' @param w_func: function that is large in the areas of the domain the
#' investigator expects differences in the distribution functions
#' @param n_basis: number of basis functions
#' @param domain: vector of two points (start and endpoint of the closed interval)
#'
#' @return: Vector of length n_basis containing the resulting means of the
#' fourier coefficients
#' @export
fourier_basis_coef_means <- function(w_func, n_basis, domain = c(0, 1)) {

  # extract basis functions as function objects
  basis_functions <- purrr::map(
    .x = 1:n_basis,
    .f = function(i) {
      basis_func <- function(x) {
        fourier_func(x = x, domain = domain, num_func = i)
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
  prod_func <- Vectorize(FUN = function(x) {
    return(w_func(x) * basis_func(x))
  })
  # integrate over product function
  coef_mean <- cubature::cubintegrate(
    f = prod_func,
    lower = domain[1],
    upper = domain[2],
    method = "hcubature",
    # these tolerances are currently hardcoded... Adjust in future iterations
    absTol = 1e-3,
    relTol = 1e-2
  )$integral
  # return appropriate mean object
  return(coef_mean)
}

#' This function implements method 2 of choosing the weight function
#' described in my thesis
#'
#' @param sample: sample in the list format where each observation contains two
#' vectors args and vals containing the x and y values
#'
#' @export
w_func_construct_1 <- function(sample) {
  ret_f <- function(x) {
    return(
      median(
        x = unlist(
          purrr::map(
            .x = sample,
            .f = ~ max(.x$vals)
          )
        )
      )
    )
  }
  return(ret_f)
}

#' This function implements method 3 of choosing the weight function
#' described in my thesis
#'
#' @param sample: functional sample in the fda format
#' @param domain: domain of the functional observations
#' @param q: chosen quantile for the mean function
#'
#' @export
w_func_construct_2 <- function(sample, domain, q) {
  ret_f <- function(x) {
    unname(quantile(
      x = fourier_eval(x = x, coefs = sample, domain = domain),
      na.rm = FALSE, probs = q
    ))
  }
  return(ret_f)
}

#' This function implements the method of choosing the sequence of rhos
#' described in my thesis
#'
#' @param sample: sample as a matrix of fourier coefficients
#' @param factor: factor to multiplt the empirical standard deviation with
#'
#' @export
rho_construct <- function(sample, factor) {
  n_basis <- nrow(sample)
  rho <- unlist(
    purrr::map(
      .x = 1:n_basis,
      .f = ~ sd(sample[.x, ])
    )
  ) * factor
  return(rho)
}
