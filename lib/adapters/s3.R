ensure_s3mpi <- function() {
  if (!requireNamespace("s3mpi", quietly = TRUE)) {
    stop("Please ensure the s3mpi package is installed to use the S3 adapter.")
  }
}

s3key <- function(name, bucket, prefix) {
  stopifnot(missing(bucket) + missing(prefix) < 2)
  stopifnot(is.character(name))
  if (!missing(bucket)) {
    prefix <- paste0("s3://", bucket, "/")
  } else if (is.null(prefix)) {
    stop("Please provide an S3 bucket or prefix.")
  }
  stopifnot(is.character(prefix))
  paste0(prefix, name)
}

#' Read data from Amazon's S3 storage.
#'
#' @param name character. The key to read.
#' @param bucket character. The S3 bucket to use. If missing,
#'    attempt to read from prefix instead.
#' @param prefix character. The S3 prefix to use. Only one
#'    of \code{bucket} and \code{prefix} may be non-empty.
#'    By default, \code{getOption("s3mpi.path")} as in the
#'    \href{https://github.com/robertzk/s3mpi}{S3mpi package}.
read <- function(name, bucket, prefix = getOption("s3mpi.path")) {
  ensure_s3mpi()
  s3mpi::s3read(s3key(name, bucket, prefix), "")
}

#' Write to Amazon's S3 storage.
#'
#' @param object ANY. The R object to save.
#' @param name character. The key to save to.
#' @param bucket character. The S3 bucket to use. If missing,
#'    attempt to read from prefix instead.
#' @param prefix character. The S3 prefix to use. Only one
#'    of \code{bucket} and \code{prefix} may be non-empty.
#'    By default, \code{getOption("s3mpi.path")} as in the
#'    \href{https://github.com/robertzk/s3mpi}{S3mpi package}.
write <- function(object, name, bucket, prefix = getOption("s3mpi.path")) {
  ensure_s3mpi()
  s3mpi::s3store(object, s3key(name, bucket, prefix), "")
}

