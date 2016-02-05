test_that("it can only take numeric number_of_trees", {
  mock_gbm  <- resource()(default_args = list(number_of_trees = "explode"))
  mock_data <- data.frame(dep_var = 1, a = 2)
  expect_error(mock_gbm$train(mock_data), "is.numeric")
})

