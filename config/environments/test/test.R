optional_tests <- c("lib/controllers", "models", "lib/shared/gbm_parameters",
                    "lib/shared/lexicals", "lib/shared/source_mungebits",
                    "lib/shared/default_adapter", "lib/classifiers/test",
                    "lib/shared/munge_data")

# Test setup hooks
setup <- Ramd::define("setup_import_data", function(setup_import_data) {
  list("Announce tests"                     = function(env) cat("Running tests...\n"),
       "Setup import_data for models"       = setup_import_data(director, optional_tests)
  )
})

# Test teardown hooks
teardown <- list(
  "All tests ran" = function(env) {
    cat(crayon::magenta("All tests ran.\n"))
  }
)


pROCoption <- getOption("pROCProgress")
msvc_option <- getOption("mgcv.vc.logrange")
survfit <- list (mean = getOption("survfit.print.mean"),
                 n    = getOption("survfit.print.n"))

single_setup <- list(
  "Hotfix leaky options" = function(e) {
    options(mgcv.vc.logrange = msvc_option, survfit.print.mean = survfit$mean,
            survfit.print.n = survfit$n, pROCProgress = pROCoption,
            lme4.summary.cor.max = NULL)
  }
)

single_teardown <- list(
  "Hotfix leaky options" = function(e) {
    options(mgcv.vc.logrange = msvc_option, survfit.print.mean = survfit$mean,
            survfit.print.n = survfit$n, pROCProgress = pROCoption,
            lme4.summary.cor.max = NULL)
  }
)
