library(testthat)
library(meta)
library(metafor)
library(ggplot2)
library(plotly)

source("../../R/bivariate_meta.R")

# Test data based on Saad et al. paper
test_data_RR <- data.frame(
  studlab = paste("Study", 1:20),
  event.e = c(42, 42, 96, 51, 47, 38, 390, 10, 165, 58, 72, 79, 48, 8, 44, 10, 11, 47, 144, 48),
  n.e = c(66, 59, 253, 137, 327, 584, 526, 28, 191, 86, 229, 153, 93, 40, 85, 100, 72, 80, 191, 85),
  event.c = c(24, 34, 32, 44, 39, 87, 323, 3, 126, 39, 60, 56, 35, 4, 21, 13, 9, 23, 116, 49),
  n.c = c(59, 65, 257, 144, 326, 588, 532, 30, 201, 94, 221, 144, 95, 40, 88, 107, 64, 74, 195, 85)
)

test_data_OR <- test_data_RR  # Use the same data for OR testing

test_that("metabiv function works correctly for RR and OR", {
  biv_RR <- metabiv(event.e = test_data_RR$event.e, 
                    n.e = test_data_RR$n.e, 
                    event.c = test_data_RR$event.c, 
                    n.c = test_data_RR$n.c, 
                    studlab = test_data_RR$studlab,
                    sm = "RR")
  
  biv_OR <- metabiv(event.e = test_data_OR$event.e, 
                    n.e = test_data_OR$n.e, 
                    event.c = test_data_OR$event.c, 
                    n.c = test_data_OR$n.c, 
                    studlab = test_data_OR$studlab,
                    sm = "OR")
  
  expect_s3_class(biv_RR, "metabiv")
  expect_s3_class(biv_OR, "metabiv")
  
  expect_equal(biv_RR$mu, 0.587, tolerance = 0.001)
  expect_equal(biv_RR$tau, 0.563, tolerance = 0.001)
  
  expect_equal(biv_OR$mu, 0.811, tolerance = 0.001)
  expect_equal(biv_OR$tau, 0.658, tolerance = 0.001)
  
  expect_equal(length(biv_RR$y.k), 20)
  expect_equal(length(biv_OR$y.k), 20)
})

test_that("comp.tau.mu.MLE function works", {
  result_RR <- comp.tau.mu.MLE(test_data_RR, c(0.4, 0.4), "RR")
  result_OR <- comp.tau.mu.MLE(test_data_OR, c(0.4, 0.4), "OR")
  
  expect_type(result_RR, "list")
  expect_type(result_OR, "list")
  expect_equal(length(result_RR), 2)
  expect_equal(length(result_OR), 2)
  expect_true(!is.null(result_RR$mu) && !is.null(result_RR$tau))
  expect_true(!is.null(result_OR$mu) && !is.null(result_OR$tau))
})

test_that("comp.tau.mu.dev.pvals function works", {
  mu.vec <- seq(-1, 1, length.out = 10)
  tau.vec <- seq(0.01, 1, length.out = 10)
  
  result_RR <- comp.tau.mu.dev.pvals(test_data_RR, mu.vec, tau.vec, "RR")
  result_OR <- comp.tau.mu.dev.pvals(test_data_OR, mu.vec, tau.vec, "OR")
  
  expect_type(result_RR, "list")
  expect_type(result_OR, "list")
  expect_equal(length(result_RR), 2)
  expect_equal(length(result_OR), 2)
  expect_equal(dim(result_RR[[1]]), c(10, 10))
  expect_equal(dim(result_OR[[1]]), c(10, 10))
})

test_that("plot.metabiv function works", {
  biv_result <- metabiv(event.e = test_data_RR$event.e, 
                        n.e = test_data_RR$n.e, 
                        event.c = test_data_RR$event.c, 
                        n.c = test_data_RR$n.c,
                        studlab = test_data_RR$studlab,
                        sm = "RR")
  
  expect_error(plot(biv_result, type = "region"), NA)
  expect_error(plot(biv_result, type = "cdf"), NA)
  expect_error(plot(biv_result, type = "superiority"), NA)
})

test_that("forest.metabiv function works", {
  biv_result <- metabiv(event.e = test_data_RR$event.e, 
                        n.e = test_data_RR$n.e, 
                        event.c = test_data_RR$event.c, 
                        n.c = test_data_RR$n.c,
                        studlab = test_data_RR$studlab,
                        sm = "RR")
  
  expect_error(forest(biv_result), NA)
})

test_that("summary.metabiv function works", {
  biv_result <- metabiv(event.e = test_data_RR$event.e, 
                        n.e = test_data_RR$n.e, 
                        event.c = test_data_RR$event.c, 
                        n.c = test_data_RR$n.c,
                        studlab = test_data_RR$studlab,
                        sm = "RR")
  
  expect_output(summary(biv_result), "Bivariate Meta-Analysis")
})

test_that("comp.mu.tau.dev.CDF.CI function works", {
  biv_result <- metabiv(event.e = test_data_RR$event.e, 
                        n.e = test_data_RR$n.e, 
                        event.c = test_data_RR$event.c, 
                        n.c = test_data_RR$n.c,
                        studlab = test_data_RR$studlab,
                        sm = "RR")
  
  cdf_result <- comp.mu.tau.dev.CDF.CI(biv_result$dev_pvals)
  
  expect_type(cdf_result, "list")
  expect_equal(length(cdf_result), 4)
})

test_that("comp.eff.harm.plot function works", {
  biv_result <- metabiv(event.e = test_data_RR$event.e, 
                        n.e = test_data_RR$n.e, 
                        event.c = test_data_RR$event.c, 
                        n.c = test_data_RR$n.c,
                        studlab = test_data_RR$studlab,
                        sm = "RR")
  
  cdf_result <- comp.mu.tau.dev.CDF.CI(biv_result$dev_pvals)
  
  expect_error(comp.eff.harm.plot(cdf_result, efficacy.is.OR.le1 = FALSE), NA)
})

test_that("leave_one_out function works", {
  biv_result <- metabiv(event.e = test_data_RR$event.e, 
                        n.e = test_data_RR$n.e, 
                        event.c = test_data_RR$event.c, 
                        n.c = test_data_RR$n.c,
                        studlab = test_data_RR$studlab,
                        sm = "RR")
  
  loo_result <- leave_one_out(biv_result)
  
  expect_equal(length(loo_result), nrow(test_data_RR))
  expect_s3_class(loo_result[[1]], "metabiv")
})

test_that("bivariate_gosh_plot function works", {
  biv_result <- metabiv(event.e = test_data_RR$event.e, 
                        n.e = test_data_RR$n.e, 
                        event.c = test_data_RR$event.c, 
                        n.c = test_data_RR$n.c,
                        studlab = test_data_RR$studlab,
                        sm = "RR")
  
  gosh_plot <- bivariate_gosh_plot(biv_result, n_subsets = 100)
  
  expect_s3_class(gosh_plot, "plotly")
})

test_that("confidence_region_shift_plot function works", {
  biv_result <- metabiv(event.e = test_data_RR$event.e, 
                        n.e = test_data_RR$n.e, 
                        event.c = test_data_RR$event.c, 
                        n.c = test_data_RR$n.c,
                        studlab = test_data_RR$studlab,
                        sm = "RR")
  
  shift_plot <- confidence_region_shift_plot(biv_result)
  
  expect_s3_class(shift_plot, "plotly")
})

test_that("bivariate_grade_assessment function works", {
  biv_result <- metabiv(event.e = test_data_RR$event.e, 
                        n.e = test_data_RR$n.e, 
                        event.c = test_data_RR$event.c, 
                        n.c = test_data_RR$n.c,
                        studlab = test_data_RR$studlab,
                        sm = "RR")
  
  expect_output(bivariate_grade_assessment(biv_result, "Low", "Low"), "Bivariate GRADE Assessment")
})