# Any variables defined in this file will become global variables
# as soon as this project is loaded. They are meant as shortcuts / mnemonics.

`%||%` <- function(x, y) if (is.null(x)) y else x

models <- function(...) unique(gsub("^[^/]+\\/", "", syberia::active_project()$find(..., method = "wildcard", base = "models")))
syberia_models <- models
mungebits <- function(...) syberia::active_project()$find(..., method = 'substring', '^lib/mungebits')
classifiers <- function(...) syberia::active_project()$find(..., method = 'substring', '^lib/classifiers')
adapters <- function(...) syberia::active_project()$find(..., method = 'substring', '^lib/adapters')
controllers <- function(...) syberia::active_project()$find(..., method = 'substring', '^lib/controllers')
shared <- function(...) syberia::active_project()$find(..., method = 'substring', '^lib/shared')
stages <- function(...) syberia::active_project()$find(..., method = 'substring', '^lib/stages')

runner <- function(version) {
  res <- Filter(function(x) grepl('^/?models/', x), syberia::active_project()$find(version))[1]
  if (is.na(res)) stop("No model version ", sQuote(version), call. = FALSE)
  syberia::active_project()$resource(res)
}

stest <- function(filter) {
  library(testthat)
  filter <- gsub("^\\/?test/", "", filter)
  if (missing(filter)) test_project(root()) # Run all tests in this repo
  else {
    tests <- syberia::active_project()$find(filter, base = 'test')
    for (test in tests) syberia::active_project()$resource(test, recompile = TRUE, recompile. = TRUE) # Run test
  }
}

reload_syberia <- function(...) {
  unloadNamespace('syberia')
  unloadNamespace('syberiaStages')
  library(syberia)
  syberia_project(root())
  invisible(TRUE)
}

test_engine <- test_project <- function(..., as.ci = FALSE, as.travis = as.ci) {
  if (isTRUE(as.ci) || isTRUE(as.travis)) {
    Sys.setenv(CI = "TRUE")
    on.exit(Sys.setenv(CI = ""), add = TRUE)
  }
  syberia::test_engine(...)
}

last_model <- function() { syberia::active_project()$cache_get("last_model") }
last_run <- function() { syberia::active_project()$cache_get("last_run") }
active_runner <- function() { syberia::active_project()$cache_get("last_model_runner") }

# Any variables defined in this file will become global variables
# as soon as this project is loaded. They are meant as shortcuts / mnemonics.

`%||%` <- function(x, y) if (is.null(x)) y else x

models <- function(...) unique(gsub("^[^/]+\\/", "", syberia::active_project()$find(..., method = "wildcard", base = "models")))
syberia_models <- models
mungebits <- function(...) syberia::active_project()$find(..., method = 'substring', '^lib/mungebits')
classifiers <- function(...) syberia::active_project()$find(..., method = 'substring', '^lib/classifiers')
adapters <- function(...) syberia::active_project()$find(..., method = 'substring', '^lib/adapters')
controllers <- function(...) syberia::active_project()$find(..., method = 'substring', '^lib/controllers')
shared <- function(...) syberia::active_project()$find(..., method = 'substring', '^lib/shared')
stages <- function(...) syberia::active_project()$find(..., method = 'substring', '^lib/stages')

runner <- function(version) {
  res <- Filter(function(x) grepl('^/?models/', x), syberia::active_project()$find(version))[1]
  if (is.na(res)) stop("No model version ", sQuote(version), call. = FALSE)
  syberia::active_project()$resource(res)
}

stest <- function(filter) {
  library(testthat)
  filter <- gsub("^\\/?test/", "", filter)
  if (missing(filter)) test_project(root()) # Run all tests in this repo
  else {
    tests <- syberia::active_project()$find(filter, base = 'test')    
    for (test in tests) syberia::active_project()$resource(test, recompile = TRUE, recompile. = TRUE) # Run test
  }
}

reload_syberia <- function(...) {
  unloadNamespace('syberia')
  unloadNamespace('syberiaStages')
  library(syberia)
  syberia_project(root())
  invisible(TRUE)
}

run_model <- Ramd::define("run_model")[[1L]]
run <- run_model

makeActiveBinding("A", function() last_run()$after$data, env = globalenv())
makeActiveBinding("B", function() last_run()$before$data, env = globalenv())
makeActiveBinding("M", function() last_run()$after$model_stage$model, env = globalenv())
makeActiveBinding("S", function() active_runner(), env = globalenv())

