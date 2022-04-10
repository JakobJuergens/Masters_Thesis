test_that("Combination generation works", {
  n <- 5
  k <- 3

  output <- as.list(rep(NA, times = choose(n = n, k = k)))
  cur_cbn <- NULL
  for(i in 1:length(output)){
    output[[i]] <- next_cbn(cbn = cur_cbn, n = n, k = k)
    cur_cbn <- output[[i]]
  }

  expected_output <- list(
    c(1,2,3), c(1,2,4), c(1,2,5),
    c(1,3,4), c(1,3,5),
    c(1,4,5),
    c(2,3,4), c(2,3,5),
    c(2,4,5),
    c(3,4,5)
  )

  expect_equal(object = output, expected = expected_output)
})
