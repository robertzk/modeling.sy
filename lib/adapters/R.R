#' Read data from an R environment variable.
#'
#' @param name character. Name of the variable in the \code{env}
#'    environment.
#' @param env environment. By default \code{globalenv()}.
#' @param inherits logical. Whether or not to look up the
#'    enclosing environment chain over \code{env}.
read <- function(name, env = globalenv(), inherits = TRUE) {
  stopifnot(is.environment(env))
  get(name, envir = env, inherits = inherits)
}

#' Write data to an R environment variable.
#'
#' @param object ANY. The R object to write.
#' @param name character. Name of the global variable.
#' @param env environment. By default \code{globalenv()}.
write <- function(object, name, env = globalenv()) {
  stopifnot(is.environment(env))
  assign(name, object, envir = env)
}

