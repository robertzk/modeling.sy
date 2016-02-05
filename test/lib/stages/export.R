library(s3mpi)
library(testthatsomemore)

parameters_yield <- function(parameters, expr, model = TRUE) {
  tmp <- new.env(parent = emptyenv())
  if (isTRUE(model)) {
    if ("s3" %in% names(parameters))
      tmp$model_stage$model <- list(1)
    else
      tmp$model_stage$model <- 1
  } else { tmp$data <- 2 }
  sr <- stagerunner::stageRunner$new(tmp, resource()(parameters))
  sr$run()
  if (!missing(expr)) eval.parent(substitute(expr))
}

test_that("it can use the s3 option correctly", {
  marked <- FALSE
  opts <- options(list("s3mpi.path" = ""))
  on.exit(options(opts))
  testthatsomemore::package_stub("s3mpi", "s3store", function(...) marked <<- ..2, {
    parameters_yield(list(s3 = "boo"))
    expect_identical(marked, "boo")
  })
})

test_that("it can use the file option correctly", {
  marked <- FALSE
  package_stub("base", "saveRDS", function(...) marked <<- ..2, {
    parameters_yield(list(file = "bop"))
    expect_identical(marked, "bop")
  })
})

test_that("it can use the R option correctly", {
  parameters_yield(list(R = "*tempvar*"))
  expect_identical(get("*tempvar*", envir = globalenv()), 1)
  rm("*tempvar*", envir = globalenv())
})

test_that("it can use the R option without a model stage", {
  parameters_yield(list(R = list("*tempvar*", .type = "data")), model = FALSE)
  expect_identical(get("*tempvar*", envir = globalenv()), 2)
  rm("*tempvar*", envir = globalenv())
})

