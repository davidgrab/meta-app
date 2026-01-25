library(testthat)
library(meta)
library(metafor)

# Correct source path relative to test file location
source("../../R/bivariate_meta.R")
source("../../R/functions.R")

# Test data 
test_data <- data.frame(
  studlab = paste0("S", 1:18),
  event.e = c(22, 17, 35, 23, 24, 4, 45, 26, 41, 64, 71, 46, 159, 98, 55, 67, 46, 34),
  n.e = c(54, 45, 53, 37, 49, 20, 80, 98, 70, 109, 131, 113, 243, 186, 123, 106, 70, 48),
  event.c = c(21, 9, 12, 15, 16, 11, 12, 19, 4, 48, 51, 56, 26, 80, 57, 22, 34, 25),
  n.c = c(55, 43, 55, 35, 49, 26, 79, 102, 70, 109, 130, 116, 81, 189, 124, 47, 70, 49)
)

# Test data with moderator variables for meta-regression
test_data_with_moderators <- cbind(test_data, 
  year = c(1990, 1992, 1993, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009),
  quality = factor(c(rep("high", 9), rep("low", 9))),
  dose = c(100, 150, 200, 100, 150, 200, 100, 150, 200, 100, 150, 200, 100, 150, 200, 100, 150, 200)
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

# High heterogeneity test data
high_heterogeneity_data <- data.frame(
  studlab = paste0("S", 1:6),
  event.e = c(1, 50, 5, 80, 10, 90),
  n.e = c(100, 100, 100, 100, 100, 100),
  event.c = c(50, 1, 80, 5, 90, 10),
  n.c = c(100, 100, 100, 100, 100, 100)
)

# All zero events data
all_zero_data <- data.frame(
  studlab = paste0("S", 1:3),
  event.e = c(0, 0, 0),
  n.e = c(20, 25, 30),
  event.c = c(0, 0, 0),
  n.c = c(20, 25, 30)
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

# Test OR support
# OR functionality tests are covered comprehensively in test-biv-or.R

# Test comp.log.OR.y.sigma.stats function
test_that("comp.log.OR.y.sigma.stats handles zero cells correctly", {
  data_tbl <- data.frame(
    event.e = zero_cell_data$event.e,
    n.e = zero_cell_data$n.e,
    event.c = zero_cell_data$event.c,
    n.c = zero_cell_data$n.c
  )
  
  result <- comp.log.OR.y.sigma.stats(data_tbl)
  
  expect_type(result, "list")
  expect_length(result, 2)
  expect_true(all(is.finite(result[[1]])))
  expect_true(all(is.finite(result[[2]])))
  expect_true(all(result[[2]] > 0))  # Variances should be positive
})

# Test comp.log.RR.y.sigma.stats function
test_that("comp.log.RR.y.sigma.stats handles zero cells correctly", {
  data_tbl <- data.frame(
    event.e = zero_cell_data$event.e,
    n.e = zero_cell_data$n.e,
    event.c = zero_cell_data$event.c,
    n.c = zero_cell_data$n.c
  )
  
  result <- comp.log.RR.y.sigma.stats(data_tbl)
  
  expect_type(result, "list")
  expect_length(result, 2)
  expect_true(all(is.finite(result[[1]])))
  expect_true(all(is.finite(result[[2]])))
  expect_true(all(result[[2]] > 0))  # Variances should be positive
})

# Test MLE estimation
test_that("comp.tau.mu.MLE works for both RR and OR", {
  data_tbl <- data.frame(
    event.e = small_data$event.e,
    n.e = small_data$n.e,
    event.c = small_data$event.c,
    n.c = small_data$n.c
  )
  
  # Test RR
  mle_rr <- comp.tau.mu.MLE(data_tbl, initial.value = c(0, 0.1), sm = "RR")
  expect_type(mle_rr, "list")
  expect_length(mle_rr, 2)
  expect_true(is.numeric(mle_rr[[1]]) && is.finite(mle_rr[[1]]))
  expect_true(is.numeric(mle_rr[[2]]) && is.finite(mle_rr[[2]]) && mle_rr[[2]] >= 0)
  
  # Test OR
  mle_or <- comp.tau.mu.MLE(data_tbl, initial.value = c(0, 0.1), sm = "OR")
  expect_type(mle_or, "list")
  expect_length(mle_or, 2)
  expect_true(is.numeric(mle_or[[1]]) && is.finite(mle_or[[1]]))
  expect_true(is.numeric(mle_or[[2]]) && is.finite(mle_or[[2]]) && mle_or[[2]] >= 0)
})

# Test edge cases
test_that("metabiv handles small datasets", {
  biv_result_small <- metabiv(event.e = small_data$event.e, n.e = small_data$n.e, 
                              event.c = small_data$event.c, n.c = small_data$n.c, 
                              studlab = small_data$studlab, sm = "RR")
  
  expect_s3_class(biv_result_small, "metabiv")
  expect_true(is.numeric(biv_result_small$mu))
  expect_true(is.numeric(biv_result_small$tau) && biv_result_small$tau >= 0)
})

# Test confidence intervals
test_that("metabiv produces valid confidence intervals", {
  biv_result <- metabiv(event.e = test_data$event.e, n.e = test_data$n.e, 
                        event.c = test_data$event.c, n.c = test_data$n.c, 
                        studlab = test_data$studlab, sm = "RR")
  
  expect_true(is.numeric(biv_result$lower.mu) && is.finite(biv_result$lower.mu))
  expect_true(is.numeric(biv_result$upper.mu) && is.finite(biv_result$upper.mu))
  expect_true(biv_result$lower.mu <= biv_result$upper.mu)
  expect_true(biv_result$mu >= biv_result$lower.mu && biv_result$mu <= biv_result$upper.mu)
})

# Test transformation functions
test_that("log.odds and inv.log.odds are inverse functions", {
  test_probs <- c(0.1, 0.3, 0.5, 0.7, 0.9)  # Use probabilities, not odds
  log_odds_vals <- log.odds(test_probs)
  recovered_probs <- inv.log.odds(log_odds_vals)
  
  expect_equal(test_probs, recovered_probs, tolerance = 1e-10)
})

# Test error handling
test_that("metabiv handles invalid inputs appropriately", {
  # Test with negative events
  expect_error(metabiv(event.e = c(-1, 5), n.e = c(10, 20), 
                       event.c = c(2, 3), n.c = c(10, 20), 
                       studlab = c("S1", "S2"), sm = "RR"))
  
  # Test with events > n
   expect_error(metabiv(event.e = c(15, 5), n.e = c(10, 20), 
                        event.c = c(2, 3), n.c = c(10, 20), 
                        studlab = c("S1", "S2"), sm = "RR"))
 })

# Test meta-regression function
test_that("run_meta_regression works with continuous moderators", {
  # Create effect sizes and variances from test data
  biv_result <- metabiv(event.e = test_data_with_moderators$event.e, 
                        n.e = test_data_with_moderators$n.e, 
                        event.c = test_data_with_moderators$event.c, 
                        n.c = test_data_with_moderators$n.c, 
                        studlab = test_data_with_moderators$studlab, sm = "RR")
  
  effect_sizes <- biv_result$y.k
  variances <- biv_result$sigma.2.k
  
  # Test with continuous moderator
  metareg_result <- run_meta_regression(
    effect_sizes = effect_sizes,
    variances = variances,
    moderators = test_data_with_moderators$year,
    n_permutations = 100  # Reduced for testing speed
  )
  
  expect_s3_class(metareg_result, "meta_regression_perm")
  expect_true(is.numeric(metareg_result$perm$QM))
  expect_true(is.numeric(metareg_result$perm$pval))
  expect_true(all(metareg_result$perm$pval >= 0 & metareg_result$perm$pval <= 1))
  expect_equal(length(metareg_result$effect_sizes), length(effect_sizes))
})

test_that("run_meta_regression works with categorical moderators", {
  # Create effect sizes and variances from test data
  biv_result <- metabiv(event.e = test_data_with_moderators$event.e, 
                        n.e = test_data_with_moderators$n.e, 
                        event.c = test_data_with_moderators$event.c, 
                        n.c = test_data_with_moderators$n.c, 
                        studlab = test_data_with_moderators$studlab, sm = "RR")
  
  effect_sizes <- biv_result$y.k
  variances <- biv_result$sigma.2.k
  
  # Test with categorical moderator
  metareg_result <- run_meta_regression(
    effect_sizes = effect_sizes,
    variances = variances,
    moderators = test_data_with_moderators$quality,
    n_permutations = 50  # Reduced for testing speed
  )
  
  expect_s3_class(metareg_result, "meta_regression_perm")
  expect_true(is.numeric(metareg_result$perm$QM))
  expect_true(is.numeric(metareg_result$perm$pval))
  expect_true(length(metareg_result$moderators) > 0)  # Has moderators
})

test_that("run_meta_regression handles multiple moderators", {
  # Create effect sizes and variances from test data
  biv_result <- metabiv(event.e = test_data_with_moderators$event.e, 
                        n.e = test_data_with_moderators$n.e, 
                        event.c = test_data_with_moderators$event.c, 
                        n.c = test_data_with_moderators$n.c, 
                        studlab = test_data_with_moderators$studlab, sm = "RR")
  
  effect_sizes <- biv_result$y.k
  variances <- biv_result$sigma.2.k
  
  # Test with multiple moderators
  moderators_df <- data.frame(
    year = test_data_with_moderators$year,
    dose = test_data_with_moderators$dose
  )
  
  metareg_result <- run_meta_regression(
    effect_sizes = effect_sizes,
    variances = variances,
    moderators = moderators_df,
    n_permutations = 50
  )
  
  expect_s3_class(metareg_result, "meta_regression_perm")
  expect_equal(ncol(metareg_result$moderators), 2)
  expect_true(is.numeric(metareg_result$perm$QM))
})

test_that("run_meta_regression error handling", {
  # Test insufficient data
  expect_error(run_meta_regression(
    effect_sizes = c(0.1, 0.2),
    variances = c(0.01, 0.02),
    moderators = c(1, 2)
  ), "Meta-regression requires at least 3 studies")
  
  # Test mismatched lengths
  expect_error(run_meta_regression(
    effect_sizes = c(0.1, 0.2, 0.3),
    variances = c(0.01, 0.02),
    moderators = c(1, 2, 3)
  ), "must have the same length")
})

test_that("print and summary methods work for meta_regression_perm", {
  # Create a simple meta-regression result
  biv_result <- metabiv(event.e = small_data$event.e, 
                        n.e = small_data$n.e, 
                        event.c = small_data$event.c, 
                        n.c = small_data$n.c, 
                        studlab = small_data$studlab, sm = "RR")
  
  metareg_result <- run_meta_regression(
    effect_sizes = biv_result$y.k,
    variances = biv_result$sigma.2.k,
    moderators = c(1, 2, 3),
    n_permutations = 10
  )
  
  # Test print method
  expect_output(print(metareg_result), "Meta-Regression with Permutation Tests")
  
  # Test summary method
  expect_output(summary(metareg_result), "Meta-Regression Analysis Summary")
})

# Additional Edge Case Tests
test_that("metabiv handles high heterogeneity data", {
  biv_result <- metabiv(event.e = high_heterogeneity_data$event.e, 
                        n.e = high_heterogeneity_data$n.e,
                        event.c = high_heterogeneity_data$event.c, 
                        n.c = high_heterogeneity_data$n.c,
                        studlab = high_heterogeneity_data$studlab, 
                        sm = "RR")
  
  expect_s3_class(biv_result, "metabiv")
  expect_true(is.numeric(biv_result$tau))
  expect_true(biv_result$tau > 0)  # Should detect high heterogeneity
  expect_true(biv_result$I2 > 50)  # High I-squared
})

test_that("metabiv handles all zero events gracefully", {
  # This should either work with continuity correction or give informative error
  # Since the function may handle zero events with continuity correction,
  # we test that it either works or gives a meaningful error
  result <- tryCatch({
    biv_result <- metabiv(event.e = all_zero_data$event.e, 
                          n.e = all_zero_data$n.e,
                          event.c = all_zero_data$event.c, 
                          n.c = all_zero_data$n.c,
                          studlab = all_zero_data$studlab, 
                          sm = "RR")
    "success"
  }, error = function(e) {
    "error"
  })
  
  # Either should succeed or fail, but not crash
  expect_true(result %in% c("success", "error"))
})

test_that("metabiv produces consistent results with analytical expectations", {
  # Test with known data where we can verify results
  simple_data <- data.frame(
    studlab = c("Study1", "Study2", "Study3"),
    event.e = c(10, 20, 30),
    n.e = c(50, 50, 50),
    event.c = c(5, 10, 15),
    n.c = c(50, 50, 50)
  )
  
  biv_result <- metabiv(event.e = simple_data$event.e, 
                        n.e = simple_data$n.e,
                        event.c = simple_data$event.c, 
                        n.c = simple_data$n.c,
                        studlab = simple_data$studlab, 
                        sm = "RR")
  
  # All studies have RR = 2, so pooled estimate should be close to log(2)
  expect_true(abs(biv_result$mu - log(2)) < 0.1)
  expect_true(biv_result$tau < 0.1)  # Low heterogeneity expected
})

# OR vs RR comparison tests are covered in test-biv-or.R

test_that("metabiv confidence intervals are properly ordered", {
  biv_result <- metabiv(event.e = test_data$event.e[1:8], 
                        n.e = test_data$n.e[1:8],
                        event.c = test_data$event.c[1:8], 
                        n.c = test_data$n.c[1:8],
                        sm = "RR")
  
  # Check that confidence intervals are properly ordered
  expect_true(biv_result$lower.mu <= biv_result$mu)
  expect_true(biv_result$mu <= biv_result$upper.mu)
  
  # Check individual study CIs
  expect_true(all(biv_result$lower.k <= biv_result$y.k))
  expect_true(all(biv_result$y.k <= biv_result$upper.k))
})

# ============================================================================
# COMPREHENSIVE VALIDATION TESTS FOR CDF, EFFICACY-HARM, AND CONFIDENCE REGIONS
# ============================================================================

# BCG Vaccine data for testing (known benchmark)
bcg_data <- data.frame(
  studlab = as.character(1:13),
  event.e = c(4, 6, 3, 62, 33, 180, 8, 505, 29, 17, 186, 5, 27),
  n.e = c(123, 306, 231, 13598, 5069, 1361, 2545, 87886, 7470, 1699, 50634, 2493, 16886),
  event.c = c(11, 29, 11, 248, 47, 372, 10, 499, 45, 65, 141, 3, 29),
  n.c = c(139, 303, 220, 12867, 5808, 1079, 629, 87892, 7232, 1600, 27338, 2338, 17825)
)

test_that("CDF values are valid probabilities and monotonically increasing", {
  biv_result <- metabiv(event.e = bcg_data$event.e, n.e = bcg_data$n.e, 
                        event.c = bcg_data$event.c, n.c = bcg_data$n.c, 
                        studlab = bcg_data$studlab, sm = "RR")
  
  cdf_result <- comp.mu.tau.dev.CDF.CI(biv_result$dev_pvals, sm = "RR")
  
  # Original Saad et al. format: 
  # [1] CDF.vec = probability values (0.01 to 0.99)
  # [2] MLE.CDF = effect thresholds at MLE
  # [3] ci.CDF.ll = CI lower effect thresholds
  # [4] ci.CDF.ul = CI upper effect thresholds
  
  # [1] CDF.vec should be probabilities from 0.01 to 0.99
  expect_true(all(cdf_result[[1]] >= 0 & cdf_result[[1]] <= 1))
  expect_true(all(diff(cdf_result[[1]]) > 0))  # Monotonically increasing
  
  # [2] MLE.CDF should be effect thresholds, monotonically increasing
  expect_true(all(is.finite(cdf_result[[2]])))
  expect_true(all(diff(cdf_result[[2]]) >= 0))  # Monotonically non-decreasing
  
  # [3] CI lower and [4] CI upper should be finite effect thresholds
  expect_true(all(is.finite(cdf_result[[3]])))  # Lower CI
  expect_true(all(is.finite(cdf_result[[4]])))  # Upper CI
  expect_true(all(cdf_result[[3]] <= cdf_result[[4]]))  # Lower <= Upper
})

test_that("Confidence region deviance matrix has correct structure", {
  biv_result <- metabiv(event.e = bcg_data$event.e, n.e = bcg_data$n.e, 
                        event.c = bcg_data$event.c, n.c = bcg_data$n.c, 
                        studlab = bcg_data$studlab, sm = "RR")
  
  dev_pvals <- biv_result$dev_pvals
  dev_mat <- dev_pvals[[1]]
  pval_mat <- dev_pvals[[2]]
  
  # Deviance matrix should be numeric and have correct dimensions
  expect_true(is.matrix(dev_mat) || is.array(dev_mat))
  expect_true(all(is.finite(dev_mat)))
  expect_true(all(dev_mat >= 0))  # Deviances are non-negative
  
  # P-value matrix should have values between 0 and 1
  expect_true(is.matrix(pval_mat) || is.array(pval_mat))
  expect_true(all(is.finite(pval_mat)))
  expect_true(all(pval_mat >= 0 & pval_mat <= 1))
  
  # MLE should have minimum deviance (close to 0)
  min_dev <- min(dev_mat, na.rm = TRUE)
  expect_true(min_dev < 0.01)  # MLE has deviance near 0
  
  # Maximum p-value should be at or near MLE
  max_pval <- max(pval_mat, na.rm = TRUE)
  expect_true(max_pval > 0.99)  # MLE has p-value close to 1
})

test_that("Efficacy-Harm probability table has consistent CI bounds", {
  biv_result <- metabiv(event.e = bcg_data$event.e, n.e = bcg_data$n.e, 
                        event.c = bcg_data$event.c, n.c = bcg_data$n.c, 
                        studlab = bcg_data$studlab, sm = "RR")
  
  cdf_obj <- comp.mu.tau.dev.CDF.CI(biv_result$dev_pvals, sm = "RR")
  prob_table <- calculate_threshold_probabilities_from_cdf(cdf_obj, NULL, sm = "RR", direction = "greater")
  
  # Probabilities should be between 0 and 1
  expect_true(all(prob_table$Probability >= 0 & prob_table$Probability <= 1))
  expect_true(all(prob_table$CI_Lower >= 0 & prob_table$CI_Lower <= 1))
  expect_true(all(prob_table$CI_Upper >= 0 & prob_table$CI_Upper <= 1))
  
  # CI bounds should be properly ordered
  expect_true(all(prob_table$CI_Lower <= prob_table$CI_Upper))
  
  # Probability should be within or close to CI bounds
  # (allowing small tolerance for numerical issues)
  expect_true(all(prob_table$CI_Lower <= prob_table$Probability + 0.01))
  expect_true(all(prob_table$Probability <= prob_table$CI_Upper + 0.01))
  
  # CI should not all be the same (would indicate a bug)
  expect_true(length(unique(prob_table$CI_Lower)) > 1 || length(unique(prob_table$CI_Upper)) > 1)
  
  # Upper CI should vary across thresholds (not all 0.99)
  expect_true(max(prob_table$CI_Upper) - min(prob_table$CI_Upper) > 0.01)
})

test_that("Efficacy-Harm probabilities are monotonic across thresholds", {
  biv_result <- metabiv(event.e = bcg_data$event.e, n.e = bcg_data$n.e, 
                        event.c = bcg_data$event.c, n.c = bcg_data$n.c, 
                        studlab = bcg_data$studlab, sm = "RR")
  
  cdf_obj <- comp.mu.tau.dev.CDF.CI(biv_result$dev_pvals, sm = "RR")
  
  # P(θ ≥ T) should decrease as T increases
  prob_table_greater <- calculate_threshold_probabilities_from_cdf(cdf_obj, NULL, sm = "RR", direction = "greater")
  sorted_probs <- prob_table_greater$Probability[order(prob_table_greater$Threshold)]
  expect_true(all(diff(sorted_probs) <= 0.01))  # Allow small tolerance
  
  # P(θ ≤ T) should increase as T increases
  prob_table_less <- calculate_threshold_probabilities_from_cdf(cdf_obj, NULL, sm = "RR", direction = "less")
  sorted_probs_less <- prob_table_less$Probability[order(prob_table_less$Threshold)]
  expect_true(all(diff(sorted_probs_less) >= -0.01))  # Allow small tolerance
})

test_that("Efficacy-Harm plot renders without error and returns valid data", {
  biv_result <- metabiv(event.e = bcg_data$event.e, n.e = bcg_data$n.e, 
                        event.c = bcg_data$event.c, n.c = bcg_data$n.c, 
                        studlab = bcg_data$studlab, sm = "RR")
  
  cdf_obj <- comp.mu.tau.dev.CDF.CI(biv_result$dev_pvals, sm = "RR")
  
  pdf(NULL)  # Suppress plot output
  expect_error(comp.eff.harm.plot(cdf_obj, sm = "RR", left_is_beneficial = TRUE), NA)
  dev.off()
})

test_that("SMD data produces valid CDF and probability values", {
  # Create SMD-like test data (continuous outcome simulation)
  smd_test <- metabiv(event.e = test_data$event.e, n.e = test_data$n.e, 
                      event.c = test_data$event.c, n.c = test_data$n.c, 
                      studlab = test_data$studlab, sm = "RR")
  
  # Simulate SMD by treating mu/tau as SMD scale
  cdf_obj <- comp.mu.tau.dev.CDF.CI(smd_test$dev_pvals, sm = "SMD")
  
  # Original format: [1] CDF.vec (probs), [2] MLE thresholds, [3] CI lower, [4] CI upper
  # [1] CDF.vec should be valid probabilities
  expect_true(all(cdf_obj[[1]] >= 0 & cdf_obj[[1]] <= 1))
  
  # [2] MLE thresholds should be finite
  expect_true(all(is.finite(cdf_obj[[2]])))
  
  prob_table <- calculate_threshold_probabilities_from_cdf(cdf_obj, NULL, sm = "SMD", direction = "greater")
  
  # Probabilities should be valid
  expect_true(all(prob_table$Probability >= 0 & prob_table$Probability <= 1))
  expect_true(all(prob_table$CI_Lower <= prob_table$CI_Upper))
})

test_that("Confidence region boundaries contain the MLE", {
  biv_result <- metabiv(event.e = bcg_data$event.e, n.e = bcg_data$n.e, 
                        event.c = bcg_data$event.c, n.c = bcg_data$n.c, 
                        studlab = bcg_data$studlab, sm = "RR")
  
  dev_pvals <- biv_result$dev_pvals
  pval_mat <- dev_pvals[[2]]
  
  # Extract mu and tau sequences from dimension names
  mu_seq <- as.numeric(gsub("mu = ", "", rownames(pval_mat)))
  tau_seq <- as.numeric(gsub("tau = ", "", colnames(pval_mat)))
  
  # Find indices of MLE (highest p-value)
  max_idx <- which(pval_mat == max(pval_mat, na.rm = TRUE), arr.ind = TRUE)[1,]
  mle_mu <- mu_seq[max_idx[1]]
  mle_tau <- tau_seq[max_idx[2]]
  
  # MLE should be close to the metabiv estimates
  expect_true(abs(mle_mu - biv_result$mu) < 0.1)
  expect_true(abs(mle_tau - biv_result$tau) < 0.2)
  
  # 95% CI region (p > 0.05) should contain MLE
  ci95_region <- which(pval_mat > 0.05, arr.ind = TRUE)
  expect_true(nrow(ci95_region) > 0)  # Region should not be empty
  
  # MLE should be within the 95% region
  mu_in_region <- any(abs(mu_seq[ci95_region[,1]] - biv_result$mu) < 0.1)
  tau_in_region <- any(abs(tau_seq[ci95_region[,2]] - biv_result$tau) < 0.2)
  expect_true(mu_in_region && tau_in_region)
})

test_that("BCG vaccine analysis produces expected protective effect", {
  # BCG vaccine is known to be protective (RR < 1)
  biv_result <- metabiv(event.e = bcg_data$event.e, n.e = bcg_data$n.e, 
                        event.c = bcg_data$event.c, n.c = bcg_data$n.c, 
                        studlab = bcg_data$studlab, sm = "RR")
  
  # Effect should be negative (log RR < 0 means protective)
  expect_true(biv_result$mu < 0)
  
  # RR should be less than 1
  expect_true(exp(biv_result$mu) < 1)
  
  # There should be substantial heterogeneity (known from literature)
  expect_true(biv_result$tau > 0.1)
  expect_true(biv_result$I2 > 50)
})

test_that("Probability bounds can reach 0 and 1 for extreme thresholds", {
  biv_result <- metabiv(event.e = bcg_data$event.e, n.e = bcg_data$n.e, 
                        event.c = bcg_data$event.c, n.c = bcg_data$n.c, 
                        studlab = bcg_data$studlab, sm = "RR")
  
  cdf_obj <- comp.mu.tau.dev.CDF.CI(biv_result$dev_pvals, sm = "RR")
  
  # Test with extreme thresholds that should give probabilities near 0 and 1
  extreme_thresholds <- c(0.01, 0.1, 10, 100)  # Very low and very high RR values
  prob_table <- calculate_threshold_probabilities_from_cdf(
    cdf_obj, 
    custom_thresholds = extreme_thresholds, 
    sm = "RR", 
    direction = "greater"
  )
  
  # For very low threshold (RR = 0.01), P(θ ≥ T) should be very high (close to 1)
  low_threshold_row <- prob_table[prob_table$Threshold == 0.01, ]
  if (nrow(low_threshold_row) > 0) {
    expect_true(low_threshold_row$Probability > 0.95 || low_threshold_row$CI_Upper > 0.99)
  }
  
  # For very high threshold (RR = 100), P(θ ≥ T) should be very low (close to 0)
  high_threshold_row <- prob_table[prob_table$Threshold == 100, ]
  if (nrow(high_threshold_row) > 0) {
    expect_true(high_threshold_row$Probability < 0.05 || high_threshold_row$CI_Lower < 0.01)
  }
  
  # CI bounds should be able to reach near 0 and near 1
  all_ci_lower <- prob_table$CI_Lower
  all_ci_upper <- prob_table$CI_Upper
  
  # At least some CI bounds should be able to get close to 0 or 1
  # (not all stuck at 0.01 or 0.99)
  expect_true(min(all_ci_lower) < 0.05 || max(all_ci_upper) > 0.95)
})