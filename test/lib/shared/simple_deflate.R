test_that("it can deflate a simple example", {
  fn <- local({ x <- 1; y <- 2; z <- 3; function(w) x + y + w })
  expect_equal(sort(ls(environment(fn))), c("x", "y", "z"))
  fn <- resource()(fn)
  expect_equal(sort(ls(environment(fn))), c("x", "y"))
})

test_that("it can deflate the transformation attribute (for column_transformations)", {
  fn <- local({ x <- 1; y <- 2; w <- 3
    transformation <- local({ x <- 1; y <- 2; z <- 3; function(w) x + y + w })
    function(w) x + y + transformation(1)
  })
  
  expect_equal(sort(ls(environment(environment(fn)$transformation))), c("x", "y", "z"))
  fn <- resource()(fn)
  expect_equal(sort(ls(environment(environment(fn)$transformation))), c("transformation", "x", "y"))
})

test_that("it can deflate helpers of a function", {
  problem <- local({
    helper <- function(x) x + 1
    true_function <- function(x, y) { x + y + helper(x) }
  })
  
  expect_equal(sort(ls(environment(environment(problem)$helper))), c("helper", "true_function"))
  problem <- resource()(problem)
  expect_equal(sort(ls(environment(environment(problem)$helper))), c("helper"))
})

test_that("it can deflate a chain of helpers", {
  problem <- local({
    fn1 <- function(x) fn2(x)
    fn2 <- function(y) fn3(y)
    z <- 1
    fn3 <- function(w) w + 1
    fn1
  })

  expect_equal(sort(ls(environment(problem))), c("fn1", "fn2", "fn3", "z"))
  problem <- resource()(problem)
  expect_equal(sort(ls(environment(problem))), c("fn2", "fn3"))
})

#test_that("it does not deflate sure_independence_screen", {
#  expect_identical(environment(resource()(syberiaMungebits::sure_independence_screen)),
#                   environment(syberiaMungebits::sure_independence_screen))
#})

