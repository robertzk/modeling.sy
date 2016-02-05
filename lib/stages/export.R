# Export stage.

default_adapter <- resource("lib/shared/default_adapter")

#' Build a stagerunner for exporting data with backup sources.
#'
#' @param export_options list. Nested list, one adapter per list entry.
#'   These adapter parametrizations will get converted to legitimate
#'   IO adapters. (See the "adapter" reference class.)
build_export_stagerunner <- function(export_options) {
  stages <- lapply(seq_along(export_options), function(index) {
    adapter <- names(export_options)[index] %||% default_adapter
    adapter <- syberiaStages:::fetch_adapter(adapter)
    type <- as.list(export_options[[index]])$.type %||% quote(model_stage$model)
    type <-
      if (identical(type, "data")) { quote(data) }
      else if (identical(type, "model")) { quote(model) }
      else { type }
    opts <- export_options[[index]][[1]]

    function(modelenv) {
      env <- as.environment(modelenv)

      # Monkey patch the parent environment of the modeling environment
      # to be able to access standard operators like "$". Otherwise,
      # the parent environment is the empty environment, which will
      # break expressions like model_stage$model.
      parent_env <- parent.env(env) 
      parent.env(env) <- baseenv()
      on.exit(parent.env(env) <- parent_env, add = TRUE)

      adapter$write(eval(type, envir = as.environment(modelenv),
                         enclos = baseenv()), opts)
    }
  })
  names(stages) <- vapply(stages, function(stage)
    paste0("Export to ",
      gsub('/', '.', environment(stage)$adapter$.keyword, fixed = TRUE)),
    character(1))

  stages
}

#' Export data stage for Syberia model process.
#'
#' @param export_options list. The available export options. Will differ
#'    depending on the adapter. (default is file adapter)
#' @export
export_stage <- function(export_options) {
  if (!is.list(export_options)) # Coerce to a list using the default adapter
    export_options <- setNames(list(resource = export_options), default_adapter)

  build_export_stagerunner(export_options)
}

