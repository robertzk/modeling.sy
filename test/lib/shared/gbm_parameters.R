test_that('it correctly sets trees', {
  expect_equal(resource()(10000)$number_of_trees, 10000)
})

test_that('it correctly sets shrinkage', {
  expect_equal(resource()(shrinkage = 0.05)$shrinkage_factor, 0.05)
})

test_that('it correctly sets additional options', {
  expect_equal(resource()(distribution = 'gaussian')$distribution, 'gaussian')
})

