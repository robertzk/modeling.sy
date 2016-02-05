# A reference class to abstract importing and exporting data.
adapter_class <- setRefClass("adapter",
  list(.read_function = "function", .write_function = "function",
       .format_function = "function", .default_options = "list",
       .keyword = "character"),

  methods = list(
    initialize = function(read_function, write_function,
                          format_function = identity, default_options = list(),
                          keyword = character(0)) { 
      .read_function   <<- read_function
      .write_function  <<- write_function
      .format_function <<- format_function
      .default_options <<- default_options
      .keyword         <<- keyword
    },

    read = function(...) {
      do.call(.read_function, format(list(...)))
    },

    write = function(value, ...) {
      do.call(.write_function, c(list(value), format(list(...))))
    },

    store = function(...) { write(...) },

    format = function(options) {
      if (!is.list(options)) options <- list(options)

      # Merge in default options if they have not been set.
      for (i in seq_along(.default_options)) {
        if (!is.element(name <- names(.default_options)[i], names(options))) {
          options[[name]] <- .default_options[[i]]
        }
      }

      environment(.format_function) <<- environment()
      .format_function(options)
    },

    show = function() {
      has_default_options <- length(.default_options) > 0
      cat("A syberia IO adapter of type ", sQuote(.keyword), " with",
          if (has_default_options) "" else " no", " default options",
          if (has_default_options) ": " else ".", "\n", sep = "")
      if (has_default_options) print(.default_options)
    }
  )
)

