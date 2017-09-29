
#' Generate a uniform distribution bounded by the values of an input vector,
#' with the same length as the input vector.
#'
#' @param x A vector.
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
  runif(length(x), min(x, max(x)))
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

generate_uniform.default <- function(x) {
  sample(unique(x), length(x), replace=TRUE)
  sample(unique(x), length(x), replace=TRUE)
}
