library(testthat)
library(meta)

source("../../R/bivariate_meta.R")

# Test data
test_data <- data.frame(
  studlab = c("Study 1", "Study 2", "Study 3"),
  event.e = c(10, 15, 20),
  n.e = c(100, 100, 100),
  event.c = c(5, 10, 15),
  n.c = c(100, 100, 100)
)

test_that("metabiv function works", {
  result <- metabiv(event.e = test_data$event.e, 
                    n.e = test_data$n.e, 
                    event.c = test_data$event.c, 
                    n.c = test_data$n.c,
                    studlab = test_data$studlab,
                    sm = "RR")
  
  expect_s3_class(result, "metabiv")
  expect_equal(length(result$y.k), 3)
  expect_equal(length(result$sigma.2.k), 3)
  expect_true(!is.null(result$mu))
  expect_true(!is.null(result$tau))
})

test_that("comp.tau.mu.MLE function works", {
  result <- comp.tau.mu.MLE(test_data, c(0.4, 0.4), "RR")
  
  expect_type(result, "list")
  expect_equal(length(result), 2)
  expect_true(!is.null(result$mu))
  expect_true(!is.null(result$tau))
})

test_that("comp.tau.mu.dev.pvals function works", {
  mu.vec <- seq(-1, 1, length.out = 10)
  tau.vec <- seq(0.01, 1, length.out = 10)
  
  result <- comp.tau.mu.dev.pvals(test_data, mu.vec, tau.vec, "RR")
  
  expect_type(result, "list")
  expect_equal(length(result), 2)
  expect_equal(dim(result[[1]]), c(10, 10))
  expect_equal(dim(result[[2]]), c(10, 10))
})

test_that("plot.metabiv function works", {
  biv_result <- metabiv(event.e = test_data$event.e, 
                        n.e = test_data$n.e, 
                        event.c = test_data$event.c, 
                        n.c = test_data$n.c,
                        studlab = test_data$studlab,
                        sm = "RR")
  
  expect_error(plot(biv_result, type = "region"), NA)
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
  
  expect_error(forest(biv_result), NA)
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

# Add more tests as needed for other functions in bivariate_meta.R