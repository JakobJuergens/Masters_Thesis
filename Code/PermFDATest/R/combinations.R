#' Get the next combination of size k from 1:n given cbn as an argument
#' describing the last combination. (Thanks to user Quantibex on stackoverflow
#' https://stackoverflow.com/users/2513407/quantibex)
#'
#' @param cbn The last combination. If Null: return the first combination.
#' @param n The size of the full data set
#' @param k The size of sample 1
#' @return The next combination.
#' @examples
#' next_cbn(cbn = c(1, 2, 3), n = 6, k = 3)
next_cbn <- function(cbn = NULL, n, k) {
  # check if size of combination is smaller than data set
  if (k > n) {
    stop("Chosen size of combination larger than data set.")
  }

  # if no previous combination is provided return the first combination
  if (is.null(cbn) | missing(cbn)) {
    return(1:k)
  }

  # check if cbn has valid length
  if (length(cbn) != k) {
    stop(paste0(
      "Invalid size of argument cbn. Your argument has length: ",
      length(cbn), " but next_cbn expected argument of length ",
      k
    ))
  }

  # check if cbn contains no duplicates
  if (any(duplicated(cbn))) {
    stop("cbn contains duplicates.")
  }

  # check if all entries of cbn are integers in the correct range
  if (!all(cbn %in% 1:n)) {
    stop("cbn contains entries that are not in 1:n.")
  }

  # if no further combination can be generated throw error
  if (all(cbn == (n - k + 1):n)) {
    stop(paste0(
      "Last combination reached. You should limit your loop ",
      "to choose(n, k) combinations."
    ))
  }

  # Initialize container
  cbn.bin <- rep(0, n)
  # set n'th entry to 1
  cbn.bin[cbn] <- 1

  if (tail(cbn.bin, 1) == 0) {
    ind <- tail(which(cbn.bin == 1), 1)
    cbn.bin[c(ind, ind + 1)] <- c(0, 1)
  } else {
    ind <- 1 + tail(which(diff(cbn.bin) == -1), 1)
    nb <- sum(cbn.bin[-c(1:ind)] == 1)
    cbn.bin[c(ind - 1, (n - nb + 1):n)] <- 0
    cbn.bin[ind:(ind + nb)] <- 1
  }
  cbn <- which(cbn.bin == 1)

  return(cbn)
}

#' Given an integer n and a vector smpl1 containing elements of 1:n,
#' return a list containing smpl1 and its counterpart smpl2
#'
#' @param n An integer specifying the sample 1:n to be split
#' @param smpl1 a vector containing elements of 1:n
#' @return A list containing two vectors smpl1 and smpl2. These are disjunct
#' and their union is 1:n.
#' @examples
#' sample_inds(n = 10, smpl1 = c(1,4,3,6))
sample_inds <- function(n, smpl1) {
  # check if smpl1 has compatible length
  if (length(smpl1) > n) {
    stop(paste0(
      "smpl1 has length: ", length(smpl1),
      " but n is chosen as: ", n
    ))
  }

  # check if smpl1 contains no duplicates
  if (any(duplicated(smpl1))) {
    stop("smpl1 contains duplicates.")
  }

  # check if all entries of smpl1 are integers in the correct range
  if (!all(smpl1 %in% 1:n)) {
    stop("smpl1 contains entries that are not in 1:n.")
  }

  return(list("smpl1" = smpl1, "smpl2" = setdiff(1:n, smpl1)))
}
