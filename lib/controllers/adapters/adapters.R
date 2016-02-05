cache <- TRUE

Ramd::define("adapter_class", function(adapter_class) {
  force(adapter_class)
  function(input, output, resource, director) {
    # If an adapter does not specify a read and write function,
    # but returns a function, we assume this should be used instead of the
    # the default import stage.
    is_stage_generating_function <-
      (!base::exists("read",  env = input, inherits = FALSE) ||
       !base::exists("write", env = input, inherits = FALSE)) &&
      is.function(output)

    if (is(output, "adapter") || stagerunner::is.stagerunner(output) ||
        is_stage_generating_function) {
      return(output)
    }

    adapter_name <- gsub("^lib/adapters/", "", resource)
    for (method in c("read", "write")) {
      if (!is.function(input[[method]])) {
        stop("When defining the ", sQuote(crayon::yellow(adapter_name)), " adapter, ",
             "please provide a ", sQuote(crayon::yellow(method)), " function.")
      }
    }

    # Construct the adapter object.
    adapter_class$new(input$read, input$write,
                      keyword = adapter_name)
  }
})

