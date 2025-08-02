library(testthat)
library(jcrmeta)

# Test data 
test_data <- data.frame(
  studlab = paste0("S", 1:18),
  event.e = c(22, 17, 35, 23, 24, 4, 45, 26, 41, 64, 71, 46, 159, 98, 55, 67, 46, 34),
  n.e = c(54, 45, 53, 37, 49, 20, 80, 98, 70, 109, 131, 113, 243, 186, 123, 106, 70, 48),
  event.c = c(21, 9, 12, 15, 16, 11, 12, 19, 4, 48, 51, 56, 26, 80, 57, 22, 34, 25),
  n.c = c(55, 43, 55, 35, 49, 26, 79, 102, 70, 109, 130, 116, 81, 189, 124, 47, 70, 49)
)

# Small test dataset for edge cases
small_data <- data.frame(
  studlab = paste0("S", 1:3),
  event.e = c(5, 10, 15),
  n.e = c(20, 30, 40),
  event.c = c(3, 8, 12),
  n.c = c(20, 30, 40)
)

# Test data with zero cells
zero_cell_data <- data.frame(
  studlab = paste0("S", 1:5),
  event.e = c(0, 5, 10, 0, 8),
  n.e = c(20, 25, 30, 15, 25),
  event.c = c(2, 0, 8, 3, 0),
  n.c = c(20, 25, 30, 15, 25)
)

context("Bivariate Meta-Analysis Functions")

test_that("metabiv function runs and returns a valid object", {
  biv_result <- metabiv(event.e = test_data$event.e, n.e = test_data$n.e, 
                        event.c = test_data$event.c, n.c = test_data$n.c, 
                        studlab = test_data$studlab, sm = "RR")
  
  expect_s3_class(biv_result, "metabiv")
  expect_true(is.list(biv_result))
  expect_true("mu" %in% names(biv_result))
  expect_true("tau" %in% names(biv_result))
  expect_true("y.k" %in% names(biv_result))
  expect_true("sigma.2.k" %in% names(biv_result))
})

test_that("metabiv works with OR summary measure", {
  biv_result <- metabiv(event.e = test_data$event.e, n.e = test_data$n.e, 
                        event.c = test_data$event.c, n.c = test_data$n.c, 
                        studlab = test_data$studlab, sm = "OR")
  
  expect_s3_class(biv_result, "metabiv")
  expect_equal(biv_result$sm, "OR")
  expect_true(is.numeric(biv_result$mu))
  expect_true(is.numeric(biv_result$tau))
})

test_that("metabiv handles zero cells correctly", {
  biv_result <- metabiv(event.e = zero_cell_data$event.e, n.e = zero_cell_data$n.e, 
                        event.c = zero_cell_data$event.c, n.c = zero_cell_data$n.c, 
                        studlab = zero_cell_data$studlab, sm = "OR")
  
  expect_s3_class(biv_result, "metabiv")
  expect_true(all(is.finite(biv_result$y.k)))
  expect_true(all(is.finite(biv_result$sigma.2.k)))
  expect_true(all(biv_result$sigma.2.k > 0))
})

test_that("metabiv input validation works", {
  # Test negative event counts
  expect_error(metabiv(event.e = c(-1, 5), n.e = c(10, 20), 
                       event.c = c(2, 3), n.c = c(10, 20), sm = "RR"),
               "Event counts cannot be negative")
  
  # Test zero sample sizes
  expect_error(metabiv(event.e = c(1, 5), n.e = c(0, 20), 
                       event.c = c(2, 3), n.c = c(10, 20), sm = "RR"),
               "Sample sizes must be positive")
  
  # Test events exceeding sample sizes
  expect_error(metabiv(event.e = c(15, 5), n.e = c(10, 20), 
                       event.c = c(2, 3), n.c = c(10, 20), sm = "RR"),
               "Event counts cannot exceed sample sizes")
})

test_that("metabiv works with pre-calculated effect sizes (SMD)", {
  # Generate some SMD data
  y <- c(0.2, 0.5, -0.1, 0.8, 0.3)
  sigma2 <- c(0.1, 0.15, 0.12, 0.18, 0.11)
  
  biv_result <- metabiv(y = y, sigma2 = sigma2, sm = "SMD")
  
  expect_s3_class(biv_result, "metabiv")
  expect_equal(biv_result$sm, "SMD")
  expect_equal(biv_result$y.k, y)
  expect_equal(biv_result$sigma.2.k, sigma2)
})

test_that("log_rr function works correctly", {
  result <- log_rr(small_data)
  
  expect_true(is.list(result))
  expect_length(result, 2)
  expect_true(all(is.finite(result[[1]])))
  expect_true(all(is.finite(result[[2]])))
  expect_true(all(result[[2]] > 0))
})

test_that("log_or function works correctly", {
  result <- log_or(small_data)
  
  expect_true(is.list(result))
  expect_length(result, 2)
  expect_true(all(is.finite(result[[1]])))
  expect_true(all(is.finite(result[[2]])))
  expect_true(all(result[[2]] > 0))
})

test_that("variance_log_rr function works correctly", {
  variance <- variance_log_rr(10, 100, 5, 100)
  
  expect_true(is.numeric(variance))
  expect_true(is.finite(variance))
  expect_true(variance > 0)
})

test_that("variance_log_or function works correctly", {
  variance <- variance_log_or(10, 100, 5, 100)
  
  expect_true(is.numeric(variance))
  expect_true(is.finite(variance))
  expect_true(variance > 0)
})

test_that("comp_tau_mu_mle function works correctly", {
  initial_values <- c(0, 0.1)
  result <- comp_tau_mu_mle(small_data, initial_values, "OR")
  
  expect_true(is.list(result))
  expect_true("mu" %in% names(result))
  expect_true("tau" %in% names(result))
  expect_true(is.finite(result$mu))
  expect_true(is.finite(result$tau))
  expect_true(result$tau >= 0)
})

test_that("comp_tau_mu_dev_pvals function works correctly", {
  mu_vec <- seq(-0.5, 0.5, length.out = 10)
  tau_vec <- seq(0.01, 0.5, length.out = 10)
  
  result <- comp_tau_mu_dev_pvals(small_data, mu_vec, tau_vec, "OR")
  
  expect_true(is.list(result))
  expect_length(result, 2)
  expect_true(is.matrix(result[[1]]))  # deviance matrix
  expect_true(is.matrix(result[[2]]))  # p-value matrix
  expect_equal(dim(result[[1]]), c(10, 10))
  expect_equal(dim(result[[2]]), c(10, 10))
})

test_that("compute_confidence_region function works correctly", {
  # Create a simple p-value matrix
  pval_mat <- matrix(runif(100), nrow = 10, ncol = 10)
  rownames(pval_mat) <- paste("mu =", seq(-0.5, 0.5, length.out = 10))
  colnames(pval_mat) <- paste("tau =", seq(0.01, 0.5, length.out = 10))
  
  result <- compute_confidence_region(pval_mat, 0.95)
  
  expect_true(is.list(result))
  expect_true("mu" %in% names(result))
  expect_true("tau" %in% names(result))
  expect_true(is.numeric(result$mu))
  expect_true(is.numeric(result$tau))
})

test_that("metabiv produces consistent results", {
  # Run the same analysis twice
  biv_result1 <- metabiv(event.e = test_data$event.e, n.e = test_data$n.e, 
                         event.c = test_data$event.c, n.c = test_data$n.c, 
                         studlab = test_data$studlab, sm = "RR", verbose = FALSE)
  
  biv_result2 <- metabiv(event.e = test_data$event.e, n.e = test_data$n.e, 
                         event.c = test_data$event.c, n.c = test_data$n.c, 
                         studlab = test_data$studlab, sm = "RR", verbose = FALSE)
  
  expect_equal(biv_result1$mu, biv_result2$mu)
  expect_equal(biv_result1$tau, biv_result2$tau)
  expect_equal(biv_result1$y.k, biv_result2$y.k)
  expect_equal(biv_result1$sigma.2.k, biv_result2$sigma.2.k)
})

test_that("metabiv confidence intervals are reasonable", {
  biv_result <- metabiv(event.e = test_data$event.e, n.e = test_data$n.e, 
                        event.c = test_data$event.c, n.c = test_data$n.c, 
                        studlab = test_data$studlab, sm = "RR", verbose = FALSE)
  
  # Check that confidence intervals contain the point estimate
  expect_true(biv_result$lower <= biv_result$mu)
  expect_true(biv_result$upper >= biv_result$mu)
  
  # Check that individual study CIs are reasonable
  expect_true(all(biv_result$lower.k <= biv_result$y.k))
  expect_true(all(biv_result$upper.k >= biv_result$y.k))
})

test_that("log_odds and inv_log_odds work correctly", {
  p <- 0.3
  log_odds_val <- log_odds(p)
  expect_equal(inv_log_odds(log_odds_val), p, tolerance = 1e-10)
  
  # Test edge cases
  expect_equal(log_odds(0.5), 0)
  expect_equal(inv_log_odds(0), 0.5)
})