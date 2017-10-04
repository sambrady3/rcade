library(testthat)
library(rcade)
context("Generating Uniform Distributions")

test_that("length of input and output are the same", {
  input1 <- c(1, 2, 3, 4, 5)
  input2 <- c("a", "b", "c")

  expect_equal(length(input1), length(generate_uniform(input1)))
  expect_equal(length(input2), length(generate_uniform(input2)))
})

test_that("generate_uniform of empty column is empty column", {
  expect_equal(c(), generate_uniform(c()))
})

test_that("generate_combined_dataset has length of 2*length_of_input", {
  input_length <- iris %>% nrow
  output <- generate_combined_dataset(iris)
  output_length <- output %>% nrow
  expect_equal(input_length * 2, output_length)
})
