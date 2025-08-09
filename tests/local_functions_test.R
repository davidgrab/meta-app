# This is a temporary file for testing purposes.
# It can be used for running ad-hoc R code.

# --- Comparison Script ---
# This script validates the output of the metabiv function against
# the results from the 'analysis for BMJ paper.R' script.

# --- Library and Source ---
library(testthat)
library(meta)
library(metafor)
library(ggplot2)
library(plotly)
library(BiasedUrn)

# Source our functions
source("R/bivariate_meta.R")
source("R/functions.R")
source("articales_and_code_this_application_is_based_upon/bmj_paper_functions.R")

# Source the original analysis functions
# original_functions_path <- "articales_and_code_this_application_is_based_upon/R_functions_for_logNormal_OR-TO-RR_analysis 141217.R"
# if (file.exists(original_functions_path)) {
#   source(original_functions_path)
# } else {
#   stop("Original analysis functions not found at: ", original_functions_path)
# }


run_comparison <- function(dataset_name, file_path, mle_start_values) {
  cat(paste("--- Comparing Results for Dataset:", dataset_name, "---\n\n"))
  
  # 1. Load data used in original script
  data_tbl <- read.csv(file_path, blank.lines.skip = TRUE)
  
  # Correct column names for metabiv if they are not standard
  # The original script uses column indices, which is brittle.
  # We will assign robust names.
  # Note: Original CSVs have different header names and formats.
  # We assume the order is: study, event.e, n.e, event.c, n.c
  names(data_tbl) <- c("study_id", "event.e", "n.e", "event.c", "n.c")
  
  # 2. Benchmark from original script's method
  cat("--- Benchmark Values from Original Script ---\n")
  benchmark_mle <- benchmark_comp.tau.mu.log.RR.MLE(data_tbl, mle_start_values)
  cat("Estimated mu (Overall Effect):", benchmark_mle[[1]], "\n")
  cat("Estimated tau (Heterogeneity):", benchmark_mle[[2]], "\n\n")
  
  # 3. Run bivariate meta-analysis using the metabiv function
  cat("--- Validation Results from metabiv ---\n")
  validation_result <- metabiv(
    event.e = data_tbl$event.e,
    n.e = data_tbl$n.e,
    event.c = data_tbl$event.c,
    n.c = data_tbl$n.c,
    studlab = data_tbl$study_id,
    sm = "RR"
  )
  cat("Estimated mu (Overall Effect):", validation_result$mu, "\n")
  cat("Estimated tau (Heterogeneity):", validation_result$tau, "\n\n")
  
  cat("---------------------------------------------------\n\n")
}

# --- Dataset 1: Hypericum (St. John's Wort) vs. Placebo ---
run_comparison(
  "Hypericum vs. Placebo",
  "articales_and_code_this_application_is_based_upon/St. John Warts for MD- A1.1 St John Vs. Placebo-Responders.csv",
  c(0.4, 0.4)
)

# --- Dataset 2: Amytriptyline vs. Placebo ---
run_comparison(
  "Amytriptyline vs. Placebo",
  "articales_and_code_this_application_is_based_upon/Amytriptyline vs placebo for MD- A1.1 response.csv",
  c(0.4, 0.4)
)

# --- Dataset 3: Hypericum vs. other AD ---
run_comparison(
  "Hypericum vs. other AD",
  "articales_and_code_this_application_is_based_upon/St. John Warts for MD- A4.2 St John Vs. other AD-Responders.csv",
  c(0.4, 0.4)
)

# --- Dataset 4.1: Ribavirin (Naive) ---
run_comparison(
  "Ribavirin (Naive)",
  "articales_and_code_this_application_is_based_upon/Ribavirin + interferon vs interferon- failure to sustained virological response- Naive 1.1-1.csv",
  c(-0.4, 0.2)
)

# --- Dataset 4.2: Ribavirin (Relapsers) ---
run_comparison(
  "Ribavirin (Relapsers)",
  "articales_and_code_this_application_is_based_upon/Ribavirin + interferon vs interferon- failure to sustained virological response_relapsers 1.1.csv",
  c(-0.4, 0.01)
)

# --- Dataset 4.3: Ribavirin (Non-responders - Note: same data as 4.2 but different analysis in original) ---
# The original script uses the same data file as 4.2 but with different starting values for MLE
run_comparison(
  "Ribavirin (Non-responders)",
  "articales_and_code_this_application_is_based_upon/Ribavirin + interferon vs interferon- failure to sustained virological response_relapsers 1.1.csv",
  c(-0.4, 0.4)
)

# --- Comparison for Odds Ratio ---
# This section validates the output of the metabiv function for Odds Ratio (OR)
# against the results from the 'analysis for BMJ paper.R' script's functions.

run_or_comparison <- function(dataset_name, file_path) {
  cat(paste("--- Comparing OR Results for Dataset:", dataset_name, "---\n\n"))

  # 1. Load data
  data_tbl <- read.csv(file_path, blank.lines.skip = TRUE)
  names(data_tbl) <- c("study_id", "event.e", "n.e", "event.c", "n.c")

  # 2. Benchmark from original script's method for OR
  cat("--- Benchmark Values from BMJ Paper Analysis (sm='OR') ---\n")
  # This uses `benchmark_comp.tau.mu.log.OR.MLE` from our clean benchmark functions file.
  # This function calculates its own initial values.
  benchmark_mle_or <- benchmark_comp.tau.mu.log.OR.MLE(data_tbl, initial.value = NULL)
  cat("Estimated mu (log OR):", benchmark_mle_or$mu, "\n")
  cat("Estimated tau:", benchmark_mle_or$tau, "\n\n")

  # 3. Validation using metabiv
  cat("--- Validation Results from metabiv (sm='OR') ---\n")
  validation_result <- metabiv(
    event.e = data_tbl$event.e,
    n.e = data_tbl$n.e,
    event.c = data_tbl$event.c,
    n.c = data_tbl$n.c,
    studlab = data_tbl$study_id,
    sm = "OR"
  )
  cat("Estimated mu (log OR):", validation_result$mu, "\n")
  cat("Estimated tau:", validation_result$tau, "\n\n")

  cat("---------------------------------------------------\n\n")
}

# --- Run OR Comparisons for all datasets ---

# Dataset 1
run_or_comparison(
  "Hypericum vs. Placebo",
  "articales_and_code_this_application_is_based_upon/St. John Warts for MD- A1.1 St John Vs. Placebo-Responders.csv"
)

# Dataset 2
run_or_comparison(
  "Amytriptyline vs. Placebo",
  "articales_and_code_this_application_is_based_upon/Amytriptyline vs placebo for MD- A1.1 response.csv"
)

# Dataset 3
run_or_comparison(
  "Hypericum vs. other AD",
  "articales_and_code_this_application_is_based_upon/St. John Warts for MD- A4.2 St John Vs. other AD-Responders.csv"
)

# Dataset 4.1
run_or_comparison(
  "Ribavirin (Naive)",
  "articales_and_code_this_application_is_based_upon/Ribavirin + interferon vs interferon- failure to sustained virological response- Naive 1.1-1.csv"
)

# Dataset 4.2
run_or_comparison(
  "Ribavirin (Relapsers)",
  "articales_and_code_this_application_is_based_upon/Ribavirin + interferon vs interferon- failure to sustained virological response_relapsers 1.1.csv"
)

# Dataset 4.3 (same data as 4.2, for consistency with RR tests)
run_or_comparison(
  "Ribavirin (Non-responders)",
  "articales_and_code_this_application_is_based_upon/Ribavirin + interferon vs interferon- failure to sustained virological response_relapsers 1.1.csv"
)

# --- Comparison for Deviance P-Values ---
# This section validates that the deviance p-value calculations, a key
# step for generating confidence regions, are consistent between the benchmark
# and the application's functions.

run_pval_comparison <- function(dataset_name, file_path) {
  cat(paste("--- Comparing Deviance P-Value Results for Dataset:", dataset_name, "---\\n\\n"))

  # 1. Load data
  data_tbl <- read.csv(file_path, blank.lines.skip = TRUE)
  names(data_tbl) <- c("study_id", "event.e", "n.e", "event.c", "n.c")

  # Define test vectors for mu and tau
  mu.vec.tst <- seq(-1, 1, length.out = 10)
  tau.vec.tst <- seq(0.01, 1, length.out = 10)

  # --- RR Comparison ---
  # Pre-calculate RR stats
  rr_stats <- comp.log.RR.y.sigma.stats(data_tbl)
  y.k_rr <- rr_stats[[1]]
  sigma.2.k_rr <- rr_stats[[2]]

  # 2. Benchmark from original script's method for RR
  cat("--- Benchmark P-Value Results (sm='RR') ---\n")
  benchmark_pval_rr <- benchmark_comp.tau.mu.log.RR.dev.pvals(data_tbl, mu.vec.tst, tau.vec.tst)
  cat("Sum of P-Value Matrix:", sum(benchmark_pval_rr[[2]]), "\\n\\n")

  # 3. Validation using the application's function for RR
  cat("--- Validation P-Value Results from App Function (sm='RR') ---\n")
  validation_pval_rr <- comp.tau.mu.dev.pvals(data.tbl, mu.vec.tst, tau.vec.tst, sm = "RR", y.k.in = y.k_rr, sigma.2.k.in = sigma.2.k_rr)
  cat("Sum of P-Value Matrix:", sum(validation_pval_rr[[2]]), "\\n\\n")

  # --- OR Comparison ---
  # Pre-calculate OR stats
  or_stats <- comp.log.OR.y.sigma.stats(data_tbl)
  y.k_or <- or_stats[[1]]
  sigma.2.k_or <- or_stats[[2]]
  
  # 4. Benchmark from original script's method for OR
  cat("--- Benchmark P-Value Results (sm='OR') ---\n")
  benchmark_pval_or <- benchmark_comp.tau.mu.log.OR.dev.pvals(data_tbl, mu.vec.tst, tau.vec.tst)
  cat("Sum of P-Value Matrix:", sum(benchmark_pval_or[[2]]), "\\n\\n")

  # 5. Validation using the application's function for OR
  cat("--- Validation P-Value Results from App Function (sm='OR') ---\n")
  validation_pval_or <- comp.tau.mu.dev.pvals(data.tbl, mu.vec.tst, tau.vec.tst, sm = "OR", y.k.in = y.k_or, sigma.2.k.in = sigma.2.k_or)
  cat("Sum of P-Value Matrix:", sum(validation_pval_or[[2]]), "\\n\\n")

  cat("---------------------------------------------------\\n\\n")
}

# --- Run P-Value Comparison for one dataset ---
run_pval_comparison(
  "Hypericum vs. Placebo",
  "articales_and_code_this_application_is_based_upon/St. John Warts for MD- A1.1 St John Vs. Placebo-Responders.csv"
)
