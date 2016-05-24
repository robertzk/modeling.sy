# A reference class to abstract importing and exporting data.
adapter_class <- R6::R6Class("adapter",

  public = list(

    read_function   = "function",
    write_function  = "function",
    format_function = "function",
    default_options = "list",
    keyword         = "character",

    initialize = function(read_function, write_function,
                          format_function = identity, default_options = list(),
                          keyword = character(0)) { 
      self$read_function   <- read_function
      self$write_function  <- write_function
      self$format_function <- format_function
      self$default_options <- default_options
      self$keyword         <- keyword
    },

    read = function(...) {
      do.call(self$read_function, I(list(...)))
    },

    write = function(value, ...) {
      do.call(self$write_function, c(list(value), I(list(...))))
    },

    store = function(...) { write(...) },

    format = function(options) {
      if (!is.list(options)) options <- list(options)

      # Merge in default options if they have not been set.
      for (i in seq_along(self$default_options)) {
        if (!is.element(name <- names(self$default_options)[i], names(options))) {
          options[[name]] <- self$default_options[[i]]
        }
      }

      environment(self$format_function) <<- environment()
      self$format_function(options)
    },

    show = function() {
      has_default_options <- length(self$default_options) > 0
      cat("A syberia IO adapter of type ", sQuote(self$keyword), " with",
          if (has_default_options) "" else " no", " default options",
          if (has_default_options) ": " else ".", "\n", sep = "")
      if (has_default_options) print(self$default_options)
    }

  )

)
