library(testthat)
library(metafor)

# Source the functions
source("../../R/functions.R")

context("Meta-Regression with Permutation Tests")

# Test data from metafor package
data(dat.bcg, package = "metafor")

# Calculate effect sizes and variances for testing
test_effects <- with(dat.bcg, log((tpos * (tneg + cpos)) / (tneg * (tpos + cpos))))
test_variances <- with(dat.bcg, 1/tpos + 1/tneg + 1/cpos + 1/cneg)
test_moderators <- dat.bcg$year - 1900  # Center year around 1900

test_that("run_meta_regression works with valid data", {
  result <- run_meta_regression(
    effect_sizes = test_effects[1:8], 
    variances = test_variances[1:8],
    moderators = test_moderators[1:8],
    n_permutations = 10  # Small number for testing
  )
  
  expect_s3_class(result, "meta_regression_perm")
  expect_true("model" %in% names(result))
  expect_true("perm" %in% names(result))
  expect_s3_class(result$model, "rma")
})

test_that("run_meta_regression handles insufficient data", {
  # Should throw error for insufficient studies
  expect_error(
    run_meta_regression(
      effect_sizes = c(0.1, 0.2), 
      variances = c(0.01, 0.02),
      moderators = c(1, 2)
    ),
    "Meta-regression requires at least 3 studies"
  )
})

test_that("run_meta_regression handles mismatched lengths", {
  # Should throw error for mismatched vector lengths
  expect_error(
    run_meta_regression(
      effect_sizes = c(0.1, 0.2, 0.3), 
      variances = c(0.01, 0.02),  # Different length
      moderators = c(1, 2, 3)
    ),
    "must have the same length"
  )
})

test_that("run_meta_regression handles missing values", {
  # Create data with missing values
  effects_with_na <- c(0.5, 0.8, 0.3, NA, 0.6)
  variances_with_na <- c(0.2, 0.3, 0.15, 0.25, 0.2)
  moderators_with_na <- c(1990, 1995, 2000, 2005, 2010)
  
  # Should work by removing missing values
  result <- run_meta_regression(
    effect_sizes = effects_with_na,
    variances = variances_with_na,
    moderators = moderators_with_na,
    n_permutations = 10
  )
  
  expect_equal(length(result$effect_sizes), 4)  # Should have 4 complete cases
})

test_that("run_meta_regression handles insufficient complete cases", {
  # Create data where too many values are missing
  effects_insufficient <- c(0.5, NA, NA)
  variances_insufficient <- c(0.2, 0.3, 0.15)
  moderators_insufficient <- c(1990, 1995, 2000)
  
  # Should throw error for insufficient complete cases
  expect_error(
    run_meta_regression(
      effect_sizes = effects_insufficient,
      variances = variances_insufficient,
      moderators = moderators_insufficient
    ),
    "Insufficient complete cases"
  )
})

test_that("print and summary methods work for meta_regression_perm", {
  # Create a simple meta-regression result
  result <- run_meta_regression(
    effect_sizes = test_effects[1:5],
    variances = test_variances[1:5],
    moderators = test_moderators[1:5],
    n_permutations = 10
  )
  
  # Test print method
  expect_output(print(result), "Meta-Regression with Permutation Tests")
  
  # Test summary method
  expect_output(summary(result), "Meta-Regression Analysis Summary")
})