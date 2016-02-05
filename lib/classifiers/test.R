train <- function(...) { cat("Trained") }
predict <- function(dataframe, ...) {
  cat("Predicted")
  runif(NROW(dataframe), 0, 1) # Random score
}
