test_that("it can fully deflate a simple example", {
  fn <- list(foo = local({ x <- 1; y <- 2; z <- 3; function(w) x + y + w }))
  expect_equal(sort(ls(environment(fn$foo))), c("x", "y", "z"))
  fn <- resource()(fn)
  expect_equal(sort(ls(environment(fn$foo))), c("x", "y"))
})

test_that("it does not ruin a simple chain when fully deflating", {
  fn <- local({ bar <- function() "foo"; foo <- function() bar();
    list(foo = foo, bar = bar) })
  fn <- resource()(fn)
  expect_equal(fn$foo(), "foo")
})

test_that("it destroys chains bigger than length 1", {
  fn <- local({ bar <- function() "foo"; local({ 
    foo <- function() bar(); list(foo = foo) }) })
  fn <- resource()(fn)
  expect_error(fn$foo(), "could not find function")
})


