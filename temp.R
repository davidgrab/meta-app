# This is a temporary file for testing purposes.
# It can be used for running ad-hoc R code.

library(testthat)
library(meta)
library(metafor)
library(ggplot2)
library(plotly)
library(BiasedUrn)

source("R/bivariate_meta.R")
source("R/functions.R")



# --- Validation Script ---
# This script validates the output of the metabiv function against
# the results from the 'analysis for BMJ paper.R' script.

# 1. Load the Hypericum (St. John's Wort) dataset
# This is the same as 'tbl.1' in the original analysis script.
hypericum_data <- read.csv("data/hypericum_depression_default.csv")

# Correct the column names to match what the function expects
names(hypericum_data) <- c("study", "ie", "it", "pe", "pt")

# 2. Run the bivariate meta-analysis using the metabiv function
# We use sm = "RR" (Relative Risk) as done in the paper's analysis.
validation_result <- metabiv(
  event.e = hypericum_data$ie,
  n.e = hypericum_data$it,
  event.c = hypericum_data$pe,
  n.c = hypericum_data$pt,
  studlab = hypericum_data$study,
  sm = "RR"
)

# 3. Print the key results (MLE for mu and tau)
cat("--- Validation Results for Hypericum Dataset ---\n\n")
cat("Estimated mu (Overall Effect):", validation_result$mu, "\n")
cat("Estimated tau (Between-Study Heterogeneity):", validation_result$tau, "\n\n")

# 4. Compare with the benchmark from 'analysis for BMJ paper.R'
# The benchmark results are approximately mu = 0.53 and tau = 0.23.
cat("--- Benchmark Values from Original Script ---\n\n")
cat("Expected mu: ~0.53\n")
cat("Expected tau: ~0.23\n\n")

cat("==> Please compare the 'Estimated' values above with the 'Benchmark' values.\n")
cat("==> Minor differences in the third or fourth decimal place are acceptable.\n")


