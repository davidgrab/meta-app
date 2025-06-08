library(testthat)
library(meta)
library(ggplot2)

# Correct source path relative to test file location
source("../../R/functions.R")
source("../../R/bivariate_meta.R")

# Test data
test_data <- data.frame(
  studlab = paste("Study", 1:5),
  event.e = c(10, 15, 20, 25, 30),
  n.e = c(100, 100, 100, 100, 100),
  event.c = c(5, 10, 15, 20, 25),
  n.c = c(100, 100, 100, 100, 100)
)

context("General Meta-Analysis Functions")

test_that("heterogeneity_plot creates a ggplot object", {
  random_result <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                           event.c = test_data$event.c, n.c = test_data$n.c,
                           studlab = test_data$studlab,
                           sm = "RR", method = "Inverse", random = TRUE)
                           
  plot <- heterogeneity_plot(random_result)
  expect_s3_class(plot, "ggplot")
})

test_that("Standardized Q-Q plot function runs without error", {
  bivariate_result <- metabiv(event.e = test_data$event.e, n.e = test_data$n.e,
                              event.c = test_data$event.c, n.c = test_data$n.c,
                              studlab = test_data$studlab, sm = "RR")
                              
  # Suppress plotting for tests
  pdf(NULL)
  
  expect_error(
    qq_plot_with_ci(y_k = bivariate_result$y.k, 
                    mu = bivariate_result$mu,
                    sigma_2_k = bivariate_result$sigma.2.k,
                    tau_2 = bivariate_result$tau^2,
                    title = "Test Standardized QQ"),
    NA
  )
  dev.off()
})

test_that("generate_report_content works", {
  report_content <- generate_report_content()
  
  expect_type(report_content, "character")
  expect_true(grepl("Comprehensive Meta-Analysis Report", report_content))
})