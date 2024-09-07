library(testthat)
library(meta)
library(ggplot2)
library(plotly)

source("../../R/functions.R")

# Test data
test_data <- data.frame(
  studlab = paste("Study", 1:3),
  event.e = c(10, 15, 20),
  n.e = c(100, 100, 100),
  event.c = c(5, 10, 15),
  n.c = c(100, 100, 100)
)

test_that("random_forest_plot works", {
  data <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                  event.c = test_data$event.c, n.c = test_data$n.c,
                  studlab = test_data$studlab,
                  sm = "RR", method = "Inverse")
  
  plot <- random_forest_plot(data)
  expect_s3_class(plot, "forest")
})

test_that("heterogeneity_plot works", {
  data <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                  event.c = test_data$event.c, n.c = test_data$n.c,
                  studlab = test_data$studlab,
                  sm = "RR", method = "Inverse")
  
  plot <- heterogeneity_plot(data)
  expect_s3_class(plot, "ggplot")
})

test_that("influence_analysis works", {
  data <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                  event.c = test_data$event.c, n.c = test_data$n.c,
                  studlab = test_data$studlab,
                  sm = "RR", method = "Inverse")
  
  result <- capture.output(influence_analysis(data))
  expect_true(any(grepl("Influence Analysis", result)))
})

test_that("outlier_detection_plot works", {
  data <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                  event.c = test_data$event.c, n.c = test_data$n.c,
                  studlab = test_data$studlab,
                  sm = "RR", method = "Inverse")
  
  plot <- outlier_detection_plot(data)
  expect_s3_class(plot, "ggplot")
})

test_that("effect_distribution_plot works", {
  data <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                  event.c = test_data$event.c, n.c = test_data$n.c,
                  studlab = test_data$studlab,
                  sm = "RR", method = "Inverse")
  
  plot <- effect_distribution_plot(data)
  expect_s3_class(plot, "ggplot")
})

test_that("gosh_plot works", {
  data <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                  event.c = test_data$event.c, n.c = test_data$n.c,
                  studlab = test_data$studlab,
                  sm = "RR", method = "Inverse")
  
  plot <- gosh_plot(data)
  expect_s3_class(plot, "ggplot")
})

test_that("model_fit_plot works", {
  data <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                  event.c = test_data$event.c, n.c = test_data$n.c,
                  studlab = test_data$studlab,
                  sm = "RR", method = "Inverse")
  
  plot <- model_fit_plot(data)
  expect_s3_class(plot, "ggplot")
})

test_that("model_fit_statistics works", {
  data <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                  event.c = test_data$event.c, n.c = test_data$n.c,
                  studlab = test_data$studlab,
                  sm = "RR", method = "Inverse")
  
  result <- capture.output(model_fit_statistics(data))
  expect_true(any(grepl("Model Fit Statistics", result)))
  expect_true(any(grepl("Q statistic", result)))
  expect_true(any(grepl("I^2", result)))
})

test_that("influence_plot works", {
  data <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                  event.c = test_data$event.c, n.c = test_data$n.c,
                  studlab = test_data$studlab,
                  sm = "RR", method = "Inverse")
  
  plot <- influence_plot(data)
  expect_s3_class(plot, "ggplot")
})

test_that("method_comparison_plot works", {
  random_result <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                           event.c = test_data$event.c, n.c = test_data$n.c,
                           studlab = test_data$studlab,
                           sm = "RR", method = "Inverse", random = TRUE)
  
  fixed_result <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                          event.c = test_data$event.c, n.c = test_data$n.c,
                          studlab = test_data$studlab,
                          sm = "RR", method = "Inverse", fixed = TRUE)
  
  bivariate_result <- metabiv(event.e = test_data$event.e, n.e = test_data$n.e,
                              event.c = test_data$event.c, n.c = test_data$n.c,
                              studlab = test_data$studlab, sm = "RR")
  
  plot <- method_comparison_plot(random_result, fixed_result, bivariate_result)
  expect_s3_class(plot, "ggplot")
})

test_that("compare_models works", {
  random_result <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                           event.c = test_data$event.c, n.c = test_data$n.c,
                           studlab = test_data$studlab,
                           sm = "RR", method = "Inverse", random = TRUE)
  
  fixed_result <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                          event.c = test_data$event.c, n.c = test_data$n.c,
                          studlab = test_data$studlab,
                          sm = "RR", method = "Inverse", fixed = TRUE)
  
  bivariate_result <- metabiv(event.e = test_data$event.e, n.e = test_data$n.e,
                              event.c = test_data$event.c, n.c = test_data$n.c,
                              studlab = test_data$studlab, sm = "RR")
  
  results <- list(random = random_result, fixed = fixed_result, bivariate = bivariate_result)
  comparison <- compare_models(results)
  
  expect_s3_class(comparison, "data.frame")
  expect_equal(nrow(comparison), 3)
  expect_true("Model" %in% colnames(comparison))
  expect_true("Effect_Size" %in% colnames(comparison))
})

test_that("interpret_results works", {
  random_result <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                           event.c = test_data$event.c, n.c = test_data$n.c,
                           studlab = test_data$studlab,
                           sm = "RR", method = "Inverse", random = TRUE)
  
  fixed_result <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                          event.c = test_data$event.c, n.c = test_data$n.c,
                          studlab = test_data$studlab,
                          sm = "RR", method = "Inverse", fixed = TRUE)
  
  bivariate_result <- metabiv(event.e = test_data$event.e, n.e = test_data$n.e,
                              event.c = test_data$event.c, n.c = test_data$n.c,
                              studlab = test_data$studlab, sm = "RR")
  
  results <- list(random = random_result, fixed = fixed_result, bivariate = bivariate_result)
  interpretation <- interpret_results(results)
  
  expect_type(interpretation, "character")
  expect_true(grepl("Random Effects Model", interpretation))
  expect_true(grepl("Fixed Effects Model", interpretation))
  expect_true(grepl("Bivariate Model", interpretation))
})

test_that("grade_assessment works", {
  data <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                  event.c = test_data$event.c, n.c = test_data$n.c,
                  studlab = test_data$studlab,
                  sm = "RR", method = "Inverse")
  
  result <- capture.output(grade_assessment(data, "Random Effects"))
  expect_true(any(grepl("GRADE Assessment", result)))
  expect_true(any(grepl("Risk of Bias", result)))
  expect_true(any(grepl("Inconsistency", result)))
  expect_true(any(grepl("Indirectness", result)))
  expect_true(any(grepl("Imprecision", result)))
  expect_true(any(grepl("Publication Bias", result)))
})

test_that("qq_plot_residuals works", {
  data <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                  event.c = test_data$event.c, n.c = test_data$n.c,
                  studlab = test_data$studlab,
                  sm = "RR", method = "Inverse")
  
  expect_error(qq_plot_residuals(data), NA)
})

test_that("calculate_residuals and calculate_random_residuals work", {
  data <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                  event.c = test_data$event.c, n.c = test_data$n.c,
                  studlab = test_data$studlab,
                  sm = "RR", method = "Inverse")
  
  fixed_residuals <- calculate_residuals(data)
  random_residuals <- calculate_random_residuals(data)
  
  expect_equal(length(fixed_residuals), nrow(test_data))
  expect_equal(length(random_residuals), nrow(test_data))
})

test_that("generate_report_content works", {
  report_content <- generate_report_content()
  
  expect_type(report_content, "character")
  expect_true(grepl("Comprehensive Meta-Analysis Report", report_content))
  expect_true(grepl("Overall Results", report_content))
  expect_true(grepl("Random Effects Analysis", report_content))
  expect_true(grepl("Fixed Effects Analysis", report_content))
  expect_true(grepl("Bivariate Approach", report_content))
})

test_that("render_report works", {
  random_result <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                           event.c = test_data$event.c, n.c = test_data$n.c,
                           studlab = test_data$studlab,
                           sm = "RR", method = "Inverse", random = TRUE)
  
  fixed_result <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                          event.c = test_data$event.c, n.c = test_data$n.c,
                          studlab = test_data$studlab,
                          sm = "RR", method = "Inverse", fixed = TRUE)
  
  bivariate_result <- metabiv(event.e = test_data$event.e, n.e = test_data$n.e,
                              event.c = test_data$event.c, n.c = test_data$n.c,
                              studlab = test_data$studlab, sm = "RR")
  
  output_file <- render_report(random_result, fixed_result, bivariate_result)
  
  expect_true(file.exists(output_file))
  expect_true(grepl("\\.html$", output_file))
})

test_that("ggplot_metainf works", {
  data <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                  event.c = test_data$event.c, n.c = test_data$n.c,
                  studlab = test_data$studlab,
                  sm = "RR", method = "Inverse")
  
  metainf_result <- metainf(data)
  plot <- ggplot_metainf(metainf_result)
  
  expect_s3_class(plot, "ggplot")
})