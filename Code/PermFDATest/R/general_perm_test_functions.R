#' This function performs the task of the permutation test function (called phi
#' in the corresponding paper)
#'
#' @param alpha: Significance Level of the test
#' @param realizations: vector of realizations of the test statistic over
#' permutations
#' @param realized_value: realized value of the test statistic in original permutation
#'
#' @return: The value of the permutation test function
#' @export
perm_test <- function(alpha, realizations, realized_value) {
  # calculate critical value
  crit_value <- crit_val(realizations = realizations, alpha = alpha)
  # determine Q values
  Q <- length(realizations)
  Q_pl <- sum(realizations > crit_value)
  Q_0 <- sum(realizations == crit_value)
  # calculate a return value
  a <- (Q * alpha - Q_pl) / Q_0
  # return appropriate values
  if (realized_value > crit_value) {
    return(1)
  } else if (realized_value == crit_value) {
    return(a)
  } else if (realized_value < crit_value) {
    return(0)
  }
}

#' This function returns a critical value for a chosen significance level
#' given a vector of realizations of the test statistic
#'
#' @param realizations: vector of realizations of the test statistic
#' @param alpha: significance level of the test
#'
#' @return: Critical Value for the test
#' @export
crit_val <- function(realizations, alpha = 0.05) {
  # returns the fitting quantile
  return(quantile(x = realizations, probs = 1 - alpha, names = FALSE))
}
