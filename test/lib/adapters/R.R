test_that("it can read a global variable correctly", {
  expect_identical(iris, resource()$read("iris"))
})

test_that("it can write to a global variable correctly", {
  on.exit(base::rm("*tmpvar*", envir = globalenv()))
  resource()$write(iris, "*tmpvar*")
  expect_identical(get("*tmpvar*", envir = globalenv()), iris)
})

