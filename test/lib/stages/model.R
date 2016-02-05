test_that("it can build a trivial classifier container", {
  test_env <- new.env()
  test_env$data <- data.frame(person_id = integer(0))
  sr <- stageRunner$new(test_env, resource()(test_env, list("test", .id_var = "person_id")))
  expect_output(sr$run(), "^Trained$")
  expect_true(is.element("model", names(sr$context$model_stage)))
  expect_output(sr$context$model_stage$model$predict("Plop", verbose = TRUE), "^Predicted")
})

