simple_merge <- function(list1, list2) {
  for (i in seq_along(list2)) {
    name <- names(list2)[i]
    if (is.character(name) && nzchar(name)) {
      list1[[name]] <- list2[[i]]
    } else {
      list1[[length(list1) + 1]] <- list2[[i]]
    }
  }
  list1
}

#' Read data from a file.
#'
#' @param name character. The name of the file.
#' @param ... Any additional arguments to \code{read.csv} or
#'    \code{readRDS}.
read <- function(name, ...) {
  # If the user provided any of the options below in their syberia model,
  # pass them along to read.csv
  if (tolower(tools::file_ext(name)) == "rds") {
    readRDS(name, ...)
  } else {
    read_csv_params <- c("header", "sep", "quote", "dec", "fill", "comment.char",
      "stringsAsFactors")
    args <- simple_merge(list(file = name, stringsAsFactors = FALSE),
                         list(...)[read_csv_params])
    do.call(read.csv, args)
  }
}

#' Write data to a file.
#'
#' @param object ANY. The R object to write (usually a data.frame).
#' @param name character. The name of the file.
#' @param ... Any additional arguments to \code{read.csv} or
#'    \code{readRDS}.
write <- function(object, name, ...) {
  # If the user provided any of the options below in their syberia model,
  # pass them along to write.csv
  if (is.data.frame(object)) {
    write_csv_params <- setdiff(names(formals(write.table)), c("x", "file"))
    args <- simple_merge(list(x = object, file = name, row.names = FALSE),
                         list(...)[write_csv_params])
    do.call(write.csv, args)
  } else {
    save_rds_params <- setdiff(names(formals(saveRDS)), c("object", "file"))
    args <- list_merge(list(object = object, file = name),
                       list(...)[save_rds_params])
    do.call(saveRDS, args)
  }
}

