fetch_model_container <- function(keyword) {
  keyword <- gsub('.', '/', keyword, fixed = TRUE)
  classifier <- file.path('lib', 'classifiers', keyword)
  if (resource_exists(classifier)) resource(classifier)
  else if (exists(tkeyword <- paste0('tundra_', keyword),
             envir = as.environment('package:tundra'), inherits = FALSE))
    getFunction(tkeyword, where = as.environment('package:tundra'))
  else stop("Did not find a tundra or custom classifier with keyword ",
            sQuote(keyword), call. = FALSE)
}

#' Model stage for syberia models
#'
#' TODO: Document this more
#'
#' @param modelenv environment. The modeling environment.
#' @param model_parameters a list. Model-specific parameters, with the first
#'    parameter always being the model keyword for the tundra container
#'    (e.g., glm, gbm, etc.)
#' @export
model_stage <- function(modelenv, model_parameters) {
  stopifnot(length(model_parameters) > 0 && is.character(model_parameters[[1]]))
  model_fn <- fetch_model_container(model_parameters[[1]])
  keyword <- model_parameters[[1]]
  model_parameters[[1]] <- NULL

  # get the identifying variable from the list of model parameters
  id_var <- model_parameters$.id_var %||% stop("Please provide an .id_var in model stage")
  model_parameters$.id_var <- NULL

  function(modelenv) {
    mungepieces <- attr(modelenv$data, "mungepieces")

    # Instantiate tundra container for model
    modelenv$model_stage$model <- model_fn(list(), model_parameters)

    # Remember train and test IDs.
    if (!is.element(id_var, colnames(modelenv$data))) {
      stop("Please do not drop ", director:::colourise(id_var, 'red'),
           " anymore so it is possible to compute the train IDs. ",
           "I will drop it for you. ;)\n", call. = FALSE)
    } else {
     modelenv$model_stage$model$internal$train_ids <- modelenv$data[[id_var]]
      modelenv$model_stage$id_var <- id_var

      # TODO: (RK) Only record this when model_card stage is present.
      modelenv$import_stage$env$test_data <-
        modelenv$import_stage$env$full_data[
          !modelenv$import_stage$env$full_data[[id_var]] %in% modelenv$data[[id_var]], ]
      modelenv$import_stage$env$train_data <-
        modelenv$import_stage$env$full_data[
          modelenv$import_stage$env$full_data[[id_var]] %in% modelenv$data[[id_var]], ]
    }

    predict_pre_munge_hook <- eval(bquote(function() {
      if (is.element(.(id_var), colnames(dataframe)) &&
          !isTRUE(predict_args$on_train)) {
        if (length(bads <- intersect(dataframe[[.(id_var)]], internal$train_ids))) {
          stop("You are predicting on some IDs that were used for training ",
               "this model. You can pass ", sQuote('list(on_train = TRUE)'),
               " as the second argument to predict, or remove these first ",
               "by consulting ", sQuote('container$internal$train_ids'),
               " where ", sQuote('container'), " is the variable holding ",
               "your tundraContainer object. For reference, here are some ",
               "IDs that were used for training that you are trying to predict: ",
               paste(collapse = ', ', bads[seq_len(min(5, length(bads)))]),
               call. = FALSE)
        }
      }
    }))
    environment(predict_pre_munge_hook) <- globalenv()
    # Prevent predicting on train IDs.
    modelenv$model_stage$model$add_hook('predict_pre_munge', predict_pre_munge_hook)

    # Train the model
    modelenv$model_stage$model$train(modelenv$data, verbose = TRUE)

    # Manually skip munge procedure since it was already done
    modelenv$model_stage$model$munge_procedure <- mungepieces %||% list()
  }
}

