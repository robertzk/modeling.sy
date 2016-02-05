# An adapter consist of a "read" function and a "write"
# function. The read function takes arguments and returns
# an object, usually a dataset.
read <- function(name) {
  paste0("Adapter input: ", name)
}

# The write function takes an *object* as its first argument
# and parameters for where to write the object as its later arguments.
write <- function(object, name) {
  cat(paste0("Example adapter writing ", name))
  # In a full-fledged adapter, we would put actual code
  # here to export our object or our data.
}

