#' This function performs Monte-Carlo integration over L2 for the test function
#' and two original samples for a given number of evaluation points (this is meant
#' for the Cramer-von Mises test statistic)
#'
#' @param t_stat_func: function that calculates the test statistic on a given sample
#' @param n_points: number of random points used for the approximation
#' @param sample1: first sample, specified as a list where each element is one observation
#' @param sample2: second sample, specified as a list where each element is one observation
#' @param ...: additional arguments for the function t_stat_func
#'
#' @return:
monte_carlo_integration <- function(t_stat_func, n_points, sample1, sample2, ...){

}
