test_that("Function u_norm works", {
  #set.seed(100)
  test_u <- u_norm(n_basis = 15)

  expect_equal(object = length(test_u), expected = 15)
})

test_that("Function u_unif works", {
  #set.seed(100)
  test_u <- u_unif(n_basis = 15)

  expect_equal(object = length(test_u), expected = 15)
})

test_that("Function u_t works", {
  #set.seed(100)
  test_u <- u_t(n_basis = 15, df = 2)

  expect_equal(object = length(test_u), expected = 15)
})
