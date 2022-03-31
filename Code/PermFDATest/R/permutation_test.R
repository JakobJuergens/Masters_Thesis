#' This function derives / approximates the critical value for the
#' permutation test
#'
#' @param alpha: Size of the test
#' @param full: Boolean that decides whether all combinations are considered
#' if FALSE, approxQ has to be provided
#' @param approxQ: integer specifying the number of combinations to be used for
#' the approximation of the critical value
#' @param sample1: first sample, specified as a list where each element is one observation
#' @param sample2: second sample, specified as a list where each element is one observation
#' @export
perm_crit_value <- function(alpha = 0.05, full = TRUE, approxQ, sample1, sample2) {
  if (full == FALSE & missing(approxQ)) {
    stop("If full == FALSE is chosen, approxQ has to be provided.")
  }

  if (full == TRUE & !missing(approxQ)) {
    warning("full == TRUE and approxQ has been provided. approxQ is ignored
             and all combinations are considered.")
  }

  # set Q to the number of combinations with the chosen sample sizes
  Q <- choose(n = length(sample1) + length(sample2), k = length(sample1))

  if (approxQ > Q) {
    warning("approxQ is larger than the number of distinct combinations.
             Instead, all combinations are considered.")
    full <- TRUE
  }

  # if full == TRUE, call function for non-approximated case
  if (full == TRUE) {
    crit_value <- perm_crit_value_full(
      alpha = alpha, Q = Q,
      sample1 = sample1, sample2 = sample2
    )
  }
  # if full == FALSE, call function for approximated case
  if (full == FALSE) {
    crit_value <- perm_crit_value_approx(
      alpha = alpha, Q = Q, approxQ = approxQ,
      sample1 = sample1, sample2 = sample2
    )
  }

  return(crit_value)
}

#' This function derives the critical value for the permutation test by considering
#' all combinations of the data of the specified sample sizes
#'
#' @param alpha: Size of the test
#' @param Q: possible number of combinations
#' @param sample1: first sample, specified as a list where each element is one observation
#' @param sample2: second sample, specified as a list where each element is one observation
#' @export
perm_crit_value_full <- function(alpha = 0.05, Q, sample1, sample2) {

}

#' This function derives the critical value for the permutation test by considering
#' a specified number of combinations. The specific combinations that are considered
#' are chosen randomly.
#'
#' @param alpha: Size of the test
#' @param Q: possible number of combinations
#' @param approxQ: integer specifying the number of combinations to be used for
#' the approximation of the critical value
#' @param sample1: first sample, specified as a list where each element is one observation
#' @param sample2: second sample, specified as a list where each element is one observation
#' @export
perm_crit_value_approx <- function(alpha = 0.05, Q, approxQ, sample1, sample2) {
  # choose which combinations to use for approximation by sampling with replacement
  combination_index <- sample(x = 1:Q, size = approxQ, replace = TRUE)
}
