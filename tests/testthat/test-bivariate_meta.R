library(testthat)
library(meta)

# Correct source path relative to test file location
source("../../R/bivariate_meta.R")

# Test data 
test_data <- data.frame(
  studlab = paste0("S", 1:18),
  event.e = c(22, 17, 35, 23, 24, 4, 45, 26, 41, 64, 71, 46, 159, 98, 55, 67, 46, 34),
  n.e = c(54, 45, 53, 37, 49, 20, 80, 98, 70, 109, 131, 113, 243, 186, 123, 106, 70, 48),
  event.c = c(21, 9, 12, 15, 16, 11, 12, 19, 4, 48, 51, 56, 26, 80, 57, 22, 34, 25),
  n.c = c(55, 43, 55, 35, 49, 26, 79, 102, 70, 109, 130, 116, 81, 189, 124, 47, 70, 49)
)

context("Bivariate Meta-Analysis Functions")

test_that("metabiv function runs and returns a valid object", {
  biv_result <- metabiv(event.e = test_data$event.e, n.e = test_data$n.e, 
                        event.c = test_data$event.c, n.c = test_data$n.c, 
                        studlab = test_data$studlab, sm = "RR")
  
  expect_s3_class(biv_result, "metabiv")
  expect_true(is.numeric(biv_result$mu) && is.finite(biv_result$mu))
  expect_true(is.numeric(biv_result$tau) && is.finite(biv_result$tau) && biv_result$tau >= 0)
})

test_that("forest.metabiv runs without error", {
  biv_result <- metabiv(event.e = test_data$event.e, n.e = test_data$n.e, 
                        event.c = test_data$event.c, n.c = test_data$n.c, 
                        studlab = test_data$studlab, sm = "RR")
  
  pdf(NULL) # Suppress plot generation
  expect_error(forest.metabiv(biv_result), NA)
  dev.off()
})

test_that("summary.metabiv method works", {
  biv_result <- metabiv(event.e = test_data$event.e, n.e = test_data$n.e, 
                        event.c = test_data$event.c, n.c = test_data$n.c, 
                        studlab = test_data$studlab, sm = "RR")
  
  summary_obj <- summary(biv_result)
  expect_s3_class(summary_obj, "summary.metabiv")
  expect_output(print(summary_obj), "Summary of Bivariate Meta-Analysis")
})

test_that("comp.mu.tau.dev.CDF.CI returns a list of vectors", {
  biv_result <- metabiv(event.e = test_data$event.e, n.e = test_data$n.e, 
                        event.c = test_data$event.c, n.c = test_data$n.c, 
                        studlab = test_data$studlab, sm = "RR")
                        
  cdf_result <- comp.mu.tau.dev.CDF.CI(biv_result$dev_pvals)
  
  expect_type(cdf_result, "list")
  expect_length(cdf_result, 4)
  expect_true(is.numeric(cdf_result[[1]]))
  expect_true(is.numeric(cdf_result[[2]]))
  expect_true(is.numeric(cdf_result[[3]]))
  expect_true(is.numeric(cdf_result[[4]]))
})