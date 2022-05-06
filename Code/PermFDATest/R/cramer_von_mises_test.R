#' This function calculates the realization of the cramer-von mises type test
#' on two given samples
#'
#' @param sample1: first sample, specified as a list where each element is one observation
#' @param sample2: second sample, specified as a list where each element is one observation
#' @param domain: Domain of the functional observations
#' @param type: Method used for the calculation of the test statistic tau;
#' possible values: 'fourier', 'grid'
#' @param n_basis: if basis based method is chosen in type, this determines the
#' number of basis functions used for the calculation
#' @param grid: if type grid is chosen, this specifies the grid for the function
#' comparison
#' @param mu_measure: function used for the generation of realizations of the random
#' function corresponding to the measure mu
#' @param n_func: number of functions used for the Monte-Carlo Integration in the
#' approximation of tau
#'
#' @return: The value of the t-statistic for the Cramer-von Mises test
cramer_von_mises_tstat <- function(sample1, sample2, domain = c(0,1), type = 'fourier',
                                   n_basis = NULL, grid = NULL, mu_measure, n_func){

}
