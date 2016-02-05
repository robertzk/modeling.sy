test_that("it leaves the dataframe invariant", {
  bit <- resource()          
  expect_equal(bit$run(iris), iris)
  expect_equal(bit$run(iris), iris)
})

