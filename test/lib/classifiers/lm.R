# Test that the lm classifier works as expected.

test_that("it can create a simple lm tundra container", {
  expect_is(resource()(), "tundraContainer")
  expect_is(resource()(), "tundraContainer")
})

test_that("it can train a simple lm model", {
  model <- resource()()
  model$train(within(iris, dep_var <- as.integer(Sepal.Length > 5)))
  expect_true(model$trained)
})

test_that("it can predict on a simple lm model", {
  model <- resource()()
  data <- within(iris, dep_var <- as.integer(Sepal.Length > 5))
  model$train(data[-1])
  expect_equal(unname(stats::predict(lm(dep_var ~ ., data[-1]))),
               model$predict(data[-1]))
})


