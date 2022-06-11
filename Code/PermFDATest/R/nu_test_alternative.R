#' This function calculates realizations of the cramer-von mises type test
#' on two given samples for a specified number of permutations
#'
#' @param sample1: first sample, specified as a list where each element is one observation
#' @param sample2: second sample, specified as a list where each element is one observation
#' @param full: Boolean that decides whether all combinations are considered
#' if FALSE, approxQ has to be provided
#' @param approxQ: integer specifying the number of combinations to be used for
#' the approximation of the critical value
#' @param domain: Domain of the functional observations
#'
#' @return: Realizations of the t-statistic for the Cramer-von Mises test
#' @export
nu_realizations_alt <- function(sample1, sample2, full = FALSE, approxQ, domain) {

  if (full == TRUE) {
    return(nu_real_full_alt(
      sample1 = sample1, sample2 = sample2, type = type,
      domain = domain
    ))
  } else {
    return(nu_real_approx_alt(
      sample1 = sample1, sample2 = sample2, approxQ = approxQ,
      domain = domain
    ))
  }
}

nu_real_full_alt <- function(sample1, sample2, domain) {
  # determine sample sizes
  n_1 <- length(sample1)
  n_2 <- length(sample2)
  # create container for realizations of nu_hat
  nu_hat <- rep(x = NA, times = approxQ)
  # create combined data set
  comb_data <- append(sample1, sample2)

  # for each observation create a function using approxfun
  sample_funcs <- purrr::map(
    .x = comb_data,
    .f = ~ approxfun(x = .x$args, y = .x$vals, method = "linear")
  )

  # set initial combination to NULL
  cur_cbn <- NULL
  # iterate over combinations
  for (i in 1:Q) {
    # get next combination
    cur_cbn <- next_cbn(cbn = cur_cbn, n = n_1 + n_2, k = n_1)
    # get list of sample1 and sample2
    cur_samples <- sample_inds(n = n_1 + n_2, smpl1 = cur_cbn)
    # determine mean functions of samples
    mean_funcs_1 <- sample_funcs[cur_samples$smpl1]
    mean_funcs_2 <- sample_funcs[cur_samples$smpl2]
    # determine difference function of mean functions
    diff_func <- function(x) {
      s1_val <- mean(
        x = unlist(
          purrr::map(
            .x = mean_funcs_1,
            .f = ~ .x(x)
          )
        ), na.rm = FALSE
      )
      s2_val <- mean(
        x = unlist(
          purrr::map(
            .x = mean_funcs_2,
            .f = ~ .x(x)
          )
        ), na.rm = FALSE
      )

      return((s2_val - s1_val)^2)
    }
    # calculate nu_hat
    nu_hat[i] <- tryCatch(
      {
        (n_1 + n_2) * integrate(
          f = Vectorize(diff_func), lower = domain[1], upper = domain[2], subdivisions = 1000
        )$value
      },
      error = function(cond){
        return(NA_real_)
      }
    )
  }

  mean_funcs_1 <- sample_funcs[1:n_1]
  mean_funcs_2 <- sample_funcs[(n_1 + 1):(n_1 + n_2)]

  diff_func <- function(x) {
    s1_val <- mean(
      x = unlist(
        purrr::map(
          .x = mean_funcs_1,
          .f = ~ .x(x)
        )
      ), na.rm = FALSE
    )
    s2_val <- mean(
      x = unlist(
        purrr::map(
          .x = mean_funcs_2,
          .f = ~ .x(x)
        )
      ), na.rm = FALSE
    )

    return((s2_val - s1_val)^2)
  }
  nu_realized <- (n_1 + n_2) * integrate(
    f = Vectorize(diff_func), lower = domain[1], upper = domain[2], subdivisions = 1000
  )$value

  return(list(nu_hat = nu_hat, nu_realized = nu_realized))
}

nu_real_approx_alt <- function(sample1, sample2, approxQ, domain) {
  # determine sample sizes
  n_1 <- length(sample1)
  n_2 <- length(sample2)
  # create container for realizations of nu_hat
  nu_hat <- rep(x = NA, times = approxQ)
  # create combined data set
  comb_data <- append(sample1, sample2)

  # for each observation create a function using approxfun
  sample_funcs <- purrr::map(
    .x = comb_data,
    .f = ~ approxfun(x = .x$args, y = .x$vals, method = "linear")
  )

  # iterate over combinations
  for (i in 1:approxQ) {
    index_1 <- sample(x = 1:(n_1 + n_2), size = n_1, replace = FALSE)
    index_2 <- setdiff(x = 1:(n_1 + n_2), y = index_1)
    # determine mean functions of samples
    mean_funcs_1 <- sample_funcs[index_1]
    mean_funcs_2 <- sample_funcs[index_2]
    # determine difference function of mean functions
    diff_func <- function(x) {
      s1_val <- mean(
        x = unlist(
          purrr::map(
            .x = mean_funcs_1,
            .f = ~ .x(x)
          )
        ), na.rm = FALSE
      )
      s2_val <- mean(
        x = unlist(
          purrr::map(
            .x = mean_funcs_2,
            .f = ~ .x(x)
          )
        ), na.rm = FALSE
      )

      return((s2_val - s1_val)^2)
    }
    # calculate nu_hat
    nu_hat[i] <- tryCatch(
      {
        (n_1 + n_2) * integrate(
          f = Vectorize(diff_func), lower = domain[1], upper = domain[2], subdivisions = 1000
        )$value
      },
      error = function(cond){
        return(NA_real_)
      }
    )
  }

  mean_funcs_1 <- sample_funcs[1:n_1]
  mean_funcs_2 <- sample_funcs[(n_1 + 1):(n_1 + n_2)]

  diff_func <- function(x) {
    s1_val <- mean(
      x = unlist(
        purrr::map(
          .x = mean_funcs_1,
          .f = ~ .x(x)
        )
      ), na.rm = FALSE
    )
    s2_val <- mean(
      x = unlist(
        purrr::map(
          .x = mean_funcs_2,
          .f = ~ .x(x)
        )
      ), na.rm = FALSE
    )

    return((s2_val - s1_val)^2)
  }
  nu_realized <- (n_1 + n_2) * integrate(
    f = Vectorize(diff_func), lower = domain[1], upper = domain[2], subdivisions = 1000
  )$value

  return(list(nu_hat = nu_hat, nu_realized = nu_realized))
}
