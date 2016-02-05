simple_deflate <- resource("lib/shared/simple_deflate")

preprocess_munge_procedure <- function(munge_procedure) {
  # We need to make sure we're not storing the entire parent environment chain
  # when later serializing a tundraContainer.
  deflate <- function(obj) {
    if (is.list(obj)) lapply(obj, deflate)
    else if (is.function(obj)) simple_deflate(obj)
    else obj
  }
  for (i in seq_along(munge_procedure)) {
    munge_procedure[[i]] <- deflate(munge_procedure[[i]])
  }
  munge_procedure
}

#' Data stage for syberia models
#'
#' TODO: Document this more
#' 
#' @param modelenv an environment. The persistent modeling environment.
#' @param munge_procedure a list. A list of mungepiece arguments,
#'    first preprocessed then passed to munge.
#' @param remember logical. Whether or not to use a caching stageRunner.
#'    The default is \code{TRUE}.
#' @export
data_stage <- function(modelenv, munge_procedure, remember = TRUE) {
  munge_procedure <- preprocess_munge_procedure(munge_procedure)

  stagerunner <- mungebits2::munge(modelenv, munge_procedure,
    stagerunner = list(remember = remember)
  ) 

  stagerunner
}

