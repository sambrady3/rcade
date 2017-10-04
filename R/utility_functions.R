
#' Generate a uniform distribution bounded by the values of an input vector,
#' with the same length as the input vector.
#'
#' @param x A vector or dataframe.
#'
#' @return A vector representing a uniform distribution bounded by the values of \code{x},
#' with the same length as \code{x}, and the same type as \code{x}
#'
#' @examples
#' generate_uniform(df$x)
#' generate_uniform(0:10)
#' generate_uniform(c("a", "b", "b", "a", "c"))
generate_uniform <- function(x) {
  UseMethod("generate_uniform", x)
}

generate_uniform.numeric <- function(x) {
  runif(length(x), min(x), max(x))
}

generate_uniform.integer <- function(x) {
  sample(min(x):max(x), length(x), replace=TRUE)
}

generate_uniform.factor <- function(x) {
  sample(levels(x), length(x), replace=TRUE)
}

generate_uniform.character <- function(x) {
  sample(unique(x), length(x), replace=TRUE)
}

generate_uniform.data.frame <- function(x) {
  as_data_frame(lapply(x[complete.cases(x), ], generate_uniform))
}

generate_uniform.default <- function(x) {
  sample(unique(x), length(x), replace=TRUE)
}

generate_combined_dataset <- function(x, indicator_column = "fake") {
  x_copy <- data.table::copy(x)
  x_copy[indicator_column] <- 0

  fake <- generate_uniform(x)
  fake[indicator_column] <- 1

  rbind(x_copy, fake)
}
