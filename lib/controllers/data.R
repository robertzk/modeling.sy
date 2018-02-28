# The data controller.
preprocessor <- function(director, source_env) {
  # Add lexicals to local environment.
  # lexicals <- director$resource('lib/shared/lexicals')
  # for (x in ls(lexicals)) source_env[[x]] <- lexicals[[x]]
  # director$resource('lib/shared/source_mungebits', source_env, director)
  source()
}

function(director, args) {
  if (isTRUE(args$raw)) { output } # Return a list of munge_procedures
  else {
    data_stage <- director$resource('lib/stages/data')

    if (is.environment(args$env)) {
      env <- args$env
    } else {
      new.env(parent = emptyenv())
    }

    data_stage(env, output, remember = !identical(args$remember, FALSE))
  }
}
