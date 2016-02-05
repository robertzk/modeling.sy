# The models preprocessor.
#
# Inject lexicals (convenient syntax shortcuts) and "output" helper.
preprocessor <- function(resource, director, source_env) {
  source_env$extending <- function(model_version, expr) {
    eval.parent(substitute(
      within(resource(file.path("models/", model_version), raw = TRUE), {
        expr
      })
    ))
  }

  source_env$model_version <- version <- gsub("^[^/]+\\/[^/]+\\/", "", resource)
  source_env$model_name    <- basename(version)
  source_env$output <-
    function(suffix = "", create = TRUE, dir = file.path(director$root(), "tmp")) {
      filename <- file.path(dir, version, suffix)
      if (create && !file.exists(dir <- dirname(filename)))
        dir.create(dir, recursive = TRUE)
      filename
    }

  lexicals <- director$resource("lib/shared/lexicals")
  for (x in ls(lexicals)) source_env[[x]] <- lexicals[[x]]

  director$resource("lib/shared/source_mungebits")(source_env, director)

  model <- source()
  if (nzchar(Sys.getenv("CI"))) {
    # Never parse import stage in CI.
    if (is.list(model)) { # Temporary workaround for director bug
      model$import <- NULL
    }
  }
  model
}
