# create a plot that illustrates that the fourier series diverges at zero
# for the shown function

test_f <- Vectorize(FUN = function(x) {
  res <- sum(unlist(
      purrr::map(
        .x = 1:100,
        .f = function(i) {
          (1/(i^2)) * sin(0.5 * x * (2^(i^3) + 1))
        }
      )
    ), na.rm = TRUE
  )

  return(res)
})

# create grid and function values
grid <- seq(from = 0, to = 0.2, length.out = 10000)
f_vals <- test_f(grid)

