select_only_training_variables <- list(
  "Select only training variables" = function(modelenv) {
    piece <- mungebits2::mungepiece$new(mungebits2::mungebit$new(select_variables),
      colnames(modelenv$data), setdiff(colnames(modelenv$data), "dep_var"))
    modelenv$data <- mungebits2::munge(modelenv$data,
                                       list("Select only training variables" = piece))
  }
)

#' Generate a list of functions which munges data according to provided data sources.
#'
#' @param sources character. The data sources to use. See the documentation
#'   for process_manual_data_source for a list of available data sources.
#' @param resource_fetcher. A function that can fetch resources from this syberia
#'   project. Necessary to fetch the munge procedures for each data source.
#' @return a list of functions which munges the data according to the
#'   munge procedure for each data source. These munge procedures should be
#'   recorded in data/data_source.R
munge_data <- function(sources, env = new.env()) {
  data_steps <- setNames(lapply(sources, function(data_source) {
    data_resource <- file.path("data", data_source)
    if (resource_exists(data_resource)) {
      resource(data_resource, env = env, remember = TRUE)
    }
  }), paste("Munge", sources, "data"))

  data_steps <- Filter(Negate(is.null), data_steps)

  data_steps <- c(
    select_only_training_variables,
    data_steps
  )

  stagerunner::stageRunner$new(env, data_steps, remember = TRUE)
}
