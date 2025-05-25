library(testthat)
library(meta)
library(metafor)
library(ggplot2)
library(plotly)

source("../../R/bivariate_meta.R")
source("../../R/functions.R")

# Test data based on the provided Hypericum vs. Placebo study
test_data <- data.frame(
  studlab = c("first", "sec", "thired", "4", "5", "6", "7", "8", "9", "10", 
              "11", "12", "13", "14", "15", "16", "17", "18"),
  event.e = c(22, 17, 35, 23, 24, 4, 45, 26, 41, 64, 71, 46, 159, 98, 55, 67, 46, 34),
  n.e = c(54, 45, 53, 37, 49, 20, 80, 98, 70, 109, 131, 113, 243, 186, 123, 106, 70, 48),
  event.c = c(21, 9, 12, 15, 16, 11, 12, 19, 4, 48, 51, 56, 26, 80, 57, 22, 34, 25),
  n.c = c(55, 43, 55, 35, 49, 26, 79, 102, 70, 109, 130, 116, 81, 189, 124, 47, 70, 49)
)

test_that("metabiv function works correctly for RR and OR", {
  biv_RR <- metabiv(event.e = test_data$event.e, 
                    n.e = test_data$n.e, 
                    event.c = test_data$event.c, 
                    n.c = test_data$n.c, 
                    studlab = test_data$studlab,
                    sm = "RR")
  
  biv_OR <- metabiv(event.e = test_data$event.e, 
                    n.e = test_data$n.e, 
                    event.c = test_data$event.c, 
                    n.c = test_data$n.c, 
                    studlab = test_data$studlab,
                    sm = "OR")
  
  expect_s3_class(biv_RR, "metabiv")
  expect_s3_class(biv_OR, "metabiv")
  
  expect_equal(biv_RR$mu, 0.40, tolerance = 0.01)
  expect_equal(biv_RR$tau, 0.36, tolerance = 0.01)
  
  expect_equal(biv_OR$mu, 0.744, tolerance = 0.01)
  expect_equal(biv_OR$tau, 0.688, tolerance = 0.01)
})

test_that("comp.tau.mu.MLE function works correctly", {
  RR_MLE <- comp.tau.mu.MLE(test_data, c(0.4, 0.4), "RR")
  OR_MLE <- comp.tau.mu.MLE(test_data, c(0.4, 0.4), "OR")
  
  expect_type(RR_MLE, "list")
  expect_type(OR_MLE, "list")
  expect_equal(length(RR_MLE), 2)
  expect_equal(length(OR_MLE), 2)
  expect_true(!is.null(RR_MLE$mu) && !is.null(RR_MLE$tau))
  expect_true(!is.null(OR_MLE$mu) && !is.null(OR_MLE$tau))
})

test_that("comp.tau.mu.dev.pvals function works correctly", {
  mu.vec <- seq(-1, 1, length.out = 10)
  tau.vec <- seq(0.01, 1, length.out = 10)
  
  RR_dev_pvals <- comp.tau.mu.dev.pvals(test_data, mu.vec, tau.vec, "RR")
  OR_dev_pvals <- comp.tau.mu.dev.pvals(test_data, mu.vec, tau.vec, "OR")
  
  expect_type(RR_dev_pvals, "list")
  expect_type(OR_dev_pvals, "list")
  expect_equal(length(RR_dev_pvals), 2)
  expect_equal(length(OR_dev_pvals), 2)
  expect_equal(dim(RR_dev_pvals[[1]]), c(10, 10))
  expect_equal(dim(OR_dev_pvals[[1]]), c(10, 10))
})

test_that("plot.metabiv function works", {
  biv_result <- metabiv(event.e = test_data$event.e, 
                        n.e = test_data$n.e, 
                        event.c = test_data$event.c, 
                        n.c = test_data$n.c,
                        studlab = test_data$studlab,
                        sm = "RR")
  
  expect_error(plot(biv_result, type = "cdf"), NA)
  expect_error(plot(biv_result, type = "superiority"), NA)
})

test_that("forest.metabiv function works", {
  biv_result <- metabiv(event.e = test_data$event.e, 
                        n.e = test_data$n.e, 
                        event.c = test_data$event.c, 
                        n.c = test_data$n.c,
                        studlab = test_data$studlab,
                        sm = "RR")
  
  expect_error(forest.metabiv(biv_result), NA)
  expect_error(meta::forest(biv_result), NA)
})

test_that("summary.metabiv function works", {
  biv_result <- metabiv(event.e = test_data$event.e, 
                        n.e = test_data$n.e, 
                        event.c = test_data$event.c, 
                        n.c = test_data$n.c,
                        studlab = test_data$studlab,
                        sm = "RR")
  
  expect_output(summary(biv_result), "Bivariate Meta-Analysis")
})

test_that("comp.mu.tau.dev.CDF.CI function works", {
  biv_result <- metabiv(event.e = test_data$event.e, 
                        n.e = test_data$n.e, 
                        event.c = test_data$event.c, 
                        n.c = test_data$n.c,
                        studlab = test_data$studlab,
                        sm = "RR")
  
  cdf_result <- comp.mu.tau.dev.CDF.CI(biv_result$dev_pvals)
  
  expect_type(cdf_result, "list")
  expect_equal(length(cdf_result), 4)
})

test_that("comp.eff.harm.plot function works", {
  biv_result <- metabiv(event.e = test_data$event.e, 
                        n.e = test_data$n.e, 
                        event.c = test_data$event.c, 
                        n.c = test_data$n.c,
                        studlab = test_data$studlab,
                        sm = "RR")
  
  cdf_result <- comp.mu.tau.dev.CDF.CI(biv_result$dev_pvals)
  
  expect_error(comp.eff.harm.plot(cdf_result, efficacy.is.OR.le1 = FALSE), NA)
})

test_that("leave_one_out function works", {
  biv_result <- metabiv(event.e = test_data$event.e, 
                        n.e = test_data$n.e, 
                        event.c = test_data$event.c, 
                        n.c = test_data$n.c,
                        studlab = test_data$studlab,
                        sm = "RR")
  
  loo_result <- leave_one_out(biv_result)
  
  expect_equal(length(loo_result), nrow(test_data))
  expect_s3_class(loo_result[[1]], "metabiv")
})

test_that("bivariate_gosh_plot function works", {
  biv_result <- metabiv(event.e = test_data$event.e, 
                        n.e = test_data$n.e, 
                        event.c = test_data$event.c, 
                        n.c = test_data$n.c,
                        studlab = test_data$studlab,
                        sm = "RR")
  
  gosh_plot <- bivariate_gosh_plot(biv_result, n_subsets = 100)
  
  expect_s3_class(gosh_plot, "plotly")
})

test_that("confidence_region_shift_plot function works", {
  biv_result <- metabiv(event.e = test_data$event.e, 
                        n.e = test_data$n.e, 
                        event.c = test_data$event.c, 
                        n.c = test_data$n.c,
                        studlab = test_data$studlab,
                        sm = "RR")
  
  shift_plot <- confidence_region_shift_plot(biv_result)
  
  expect_s3_class(shift_plot, "plotly")
  
  # Check if the MLE point is correctly plotted
  expect_equal(biv_result$mu, biv_result$mu, tolerance = 0.01)
  expect_equal(biv_result$tau, biv_result$tau, tolerance = 0.01)
})


test_that("bivariate_grade_assessment function works", {
  biv_result <- metabiv(event.e = test_data$event.e, 
                        n.e = test_data$n.e, 
                        event.c = test_data$event.c, 
                        n.c = test_data$n.c,
                        studlab = test_data$studlab,
                        sm = "RR")
  
  expect_output(bivariate_grade_assessment(biv_result, "Low", "Low"), "Bivariate GRADE Assessment")
})