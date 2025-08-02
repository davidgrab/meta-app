library(testthat)
library(meta)
library(metafor)

# Source the bivariate meta-analysis functions
source("../../R/bivariate_meta.R")
source("../../R/functions.R")

# Test data specifically for OR testing
or_test_data <- data.frame(
  studlab = paste0("Study", 1:8),
  event.e = c(12, 8, 15, 20, 5, 18, 25, 10),
  n.e = c(50, 40, 60, 80, 30, 70, 100, 45),
  event.c = c(8, 12, 10, 15, 8, 12, 20, 15),
  n.c = c(50, 40, 60, 80, 30, 70, 100, 45)
)

# Test data with extreme OR values
extreme_or_data <- data.frame(
  studlab = c("Low_OR", "High_OR", "Medium_OR"),
  event.e = c(1, 45, 20),
  n.e = c(50, 50, 50),
  event.c = c(25, 5, 20),
  n.c = c(50, 50, 50)
)

context("Bivariate Meta-Analysis OR Functionality")

test_that("metabiv correctly calculates OR estimates", {
  biv_or <- metabiv(event.e = or_test_data$event.e, 
                    n.e = or_test_data$n.e,
                    event.c = or_test_data$event.c, 
                    n.c = or_test_data$n.c,
                    studlab = or_test_data$studlab,
                    sm = "OR")
  
  expect_s3_class(biv_or, "metabiv")
  expect_equal(biv_or$sm, "OR")
  expect_true(is.numeric(biv_or$mu))
  expect_true(is.finite(biv_or$mu))
  expect_true(length(biv_or$y.k) == nrow(or_test_data))
})

test_that("OR calculations match manual calculations", {
  # Simple case: Study with events (10,40) vs (5,45)
  # OR = (10*45)/(40*5) = 450/200 = 2.25
  # log(OR) = log(2.25) ≈ 0.811
  
  simple_data <- data.frame(
    studlab = "Test",
    event.e = 10,
    n.e = 50,
    event.c = 5,
    n.c = 50
  )
  
  biv_or <- metabiv(event.e = simple_data$event.e, 
                    n.e = simple_data$n.e,
                    event.c = simple_data$event.c, 
                    n.c = simple_data$n.c,
                    studlab = simple_data$studlab,
                    sm = "OR")
  
  expected_log_or <- log((10 * 45) / (40 * 5))
  expect_true(abs(biv_or$y.k[1] - expected_log_or) < 0.001)
})

test_that("OR variance calculations are correct", {
  # Variance of log(OR) = 1/a + 1/b + 1/c + 1/d
  # where a=event.e, b=n.e-event.e, c=event.c, d=n.c-event.c
  
  simple_data <- data.frame(
    studlab = "Test",
    event.e = 10,
    n.e = 50,
    event.c = 5,
    n.c = 50
  )
  
  biv_or <- metabiv(event.e = simple_data$event.e, 
                    n.e = simple_data$n.e,
                    event.c = simple_data$event.c, 
                    n.c = simple_data$n.c,
                    studlab = simple_data$studlab,
                    sm = "OR")
  
  expected_var <- 1/10 + 1/40 + 1/5 + 1/45
  expect_true(abs(biv_or$var.k[1] - expected_var) < 0.001)
})

test_that("OR handles extreme values appropriately", {
  biv_or <- metabiv(event.e = extreme_or_data$event.e, 
                    n.e = extreme_or_data$n.e,
                    event.c = extreme_or_data$event.c, 
                    n.c = extreme_or_data$n.c,
                    studlab = extreme_or_data$studlab,
                    sm = "OR")
  
  expect_s3_class(biv_or, "metabiv")
  expect_true(all(is.finite(biv_or$y.k)))
  expect_true(all(is.finite(biv_or$var.k)))
  expect_true(all(biv_or$var.k > 0))
})

test_that("OR and RR give different results for same data", {
  biv_or <- metabiv(event.e = or_test_data$event.e[1:5], 
                    n.e = or_test_data$n.e[1:5],
                    event.c = or_test_data$event.c[1:5], 
                    n.c = or_test_data$n.c[1:5],
                    studlab = or_test_data$studlab[1:5],
                    sm = "OR")
  
  biv_rr <- metabiv(event.e = or_test_data$event.e[1:5], 
                    n.e = or_test_data$n.e[1:5],
                    event.c = or_test_data$event.c[1:5], 
                    n.c = or_test_data$n.c[1:5],
                    studlab = or_test_data$studlab[1:5],
                    sm = "RR")
  
  # OR and RR should give different estimates
  expect_false(identical(biv_or$mu, biv_rr$mu))
  expect_false(identical(biv_or$y.k, biv_rr$y.k))
  
  # But both should be valid
  expect_true(is.finite(biv_or$mu) && is.finite(biv_rr$mu))
})

test_that("OR confidence intervals are valid", {
  biv_or <- metabiv(event.e = or_test_data$event.e, 
                    n.e = or_test_data$n.e,
                    event.c = or_test_data$event.c, 
                    n.c = or_test_data$n.c,
                    studlab = or_test_data$studlab,
                    sm = "OR")
  
  # Overall CI should be properly ordered
  expect_true(biv_or$lower.mu <= biv_or$mu)
  expect_true(biv_or$mu <= biv_or$upper.mu)
  
  # Individual study CIs should be properly ordered
  expect_true(all(biv_or$lower.k <= biv_or$y.k))
  expect_true(all(biv_or$y.k <= biv_or$upper.k))
  
  # CI widths should be positive
  expect_true(all(biv_or$upper.k - biv_or$lower.k > 0))
  expect_true(biv_or$upper.mu - biv_or$lower.mu > 0)
})

test_that("OR handles zero cells with continuity correction", {
  zero_data <- data.frame(
    studlab = c("Zero_E", "Zero_C", "Normal"),
    event.e = c(0, 10, 15),
    n.e = c(50, 50, 50),
    event.c = c(10, 0, 12),
    n.c = c(50, 50, 50)
  )
  
  # Should handle zero cells gracefully (with continuity correction)
  expect_no_error({
    biv_or <- metabiv(event.e = zero_data$event.e, 
                      n.e = zero_data$n.e,
                      event.c = zero_data$event.c, 
                      n.c = zero_data$n.c,
                      studlab = zero_data$studlab,
                      sm = "OR")
  })
})

test_that("OR meta-analysis produces reasonable heterogeneity estimates", {
  biv_or <- metabiv(event.e = or_test_data$event.e, 
                    n.e = or_test_data$n.e,
                    event.c = or_test_data$event.c, 
                    n.c = or_test_data$n.c,
                    studlab = or_test_data$studlab,
                    sm = "OR")
  
  expect_true(is.numeric(biv_or$tau))
  expect_true(biv_or$tau >= 0)
  expect_true(is.numeric(biv_or$I2))
  expect_true(biv_or$I2 >= 0 && biv_or$I2 <= 100)
})