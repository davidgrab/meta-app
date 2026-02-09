# replicability.R
# Helper functions for replicability analysis using metarep package
# Based on Jaljuli et al. (2021): "Quantifying Replicability and Consistency in Systematic Reviews"
#
# Validation vs article critical concepts:
# - r-value: replicability measure (minimum number of studies supporting the effect); we use metarep::metarep() which implements the paper's r-value.
# - u_R, u_L: lower bounds (95% confidence) on number of studies with positive/negative effect direction; we use rval_result$u_R, rval_result$u_L from metarep.
# - Replicability statement: "at least X show increase, at least Y show decrease (95% confidence)" matches the paper's reporting (Figure 8 style).
# - Consistency (Section 2.5): Inconsistent when min(u_L, u_R) >= 1; Supports consistency when (u_R >= 2 and u_L == 0) or (u_L >= 2 and u_R == 0). Implemented in get_consistency_status().
# - No distributional assumptions; supports fixed- and random-effects (common.effect parameter). Both supported in our UI.

library(meta)
library(ggplot2)

# Check if metarep is available
check_metarep_installed <- function() {
  if (!requireNamespace("metarep", quietly = TRUE)) {
    # No installation instructions for Posit Cloud users
    stop("The 'metarep' package is required for replicability analysis. Please ensure it is installed in the environment.")
  }
  TRUE
}

#' Compute R-value and lower bounds for replicability analysis
#'
#' @param meta_object A meta-analysis object from the meta package
#' @param u The threshold parameter for replicability (default 2)
#' @param t The significance threshold (default 0.05)
#' @param common_effect Logical, whether to assume a common effect (Pearson's test)
#' @return A metarep object
compute_rvalue <- function(meta_object, u = 2, t = 0.05, common_effect = FALSE) {
  check_metarep_installed()

  tryCatch(
    {
      # Use metarep package directly on the meta object
      rval_result <- metarep::metarep(
        meta_object,
        u = u,
        t = t,
        report.u.max = TRUE,
        common.effect = common_effect
      )

      return(rval_result)
    },
    error = function(e) {
      warning(paste("Error computing r-value:", e$message))
      return(NULL)
    }
  )
}

#' Implement consistency logic from Jaljuli et al. (Section 2.5)
#'
#' @param rval_result Result from compute_rvalue
#' @return A list with consistency metrics for UI cards
get_consistency_status <- function(rval_result) {
  if (is.null(rval_result)) {
    return(list(status = "Error", label = "Error", color = "danger"))
  }

  u_max_L <- rval_result$u_L
  u_max_R <- rval_result$u_R

  # Rule from paper Section 2.5:
  # Inconsistent: min(u_max_L, u_max_R) >= 1
  # Supports consistency: (u_max_R >= 2 and u_max_L == 0) OR (u_max_L >= 2 and u_max_R == 0)

  status <- "Low power / Inconclusive"
  label <- "Inconclusive"
  color <- "secondary"

  if (min(u_max_L, u_max_R) >= 1) {
    status <- "Inconsistent"
    label <- "Inconsistency Detected"
    color <- "warning"
  } else if ((u_max_R >= 2 && u_max_L == 0) || (u_max_L >= 2 && u_max_R == 0)) {
    status <- "Supports consistency"
    label <- "Supports Consistency"
    color <- "success"
  }

  list(status = status, label = label, color = color, u_max_L = u_max_L, u_max_R = u_max_R)
}

#' Generate Replicability Statement (Figure 8 style)
#'
#' @param rval_result Result from compute_rvalue
#' @param k Total number of studies
#' @return A character string
get_replicability_statement <- function(rval_result, k) {
  if (is.null(rval_result)) {
    return("")
  }

  r_val <- round(rval_result$r.value, 4)
  u_L <- rval_result$u_L
  u_R <- rval_result$u_R

  # "Replicability analysis: r-value = ... . Out of k studies, at least X show increase and at least Y show decrease (95% confidence)."
  sprintf(
    "Replicability analysis: r-value = %s. Out of %d studies, at least %d show increase and at least %d show decrease (95%% confidence).",
    r_val, k, u_R, u_L
  )
}

#' Enhanced forest plot for replicability
#' Using metarep's own forest method to ensure correct annotations
render_replicability_forest <- function(rval_result) {
  if (is.null(rval_result)) {
    return(NULL)
  }

  # Use the generic forest method on the metarep object
  forest(
    rval_result,
    layout = "revman5",
    digits.pval = 4,
    test.overall = TRUE
  )
}

#' Diagnostics: R(u) for u = 2...n
#'
#' @param meta_object A meta-analysis object
#' @param n Total number of studies
#' @return A data frame with r(u) values
get_ru_diagnostics <- function(meta_object, n) {
  u_values <- 2:n
  results <- data.frame(
    u = u_values,
    r_u = NA_real_,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(u_values)) {
    try(
      {
        # Use metarep via the generic to calculate r(u)
        # We don't need u.max for this curve, just the p-value
        res <- metarep::metarep(meta_object, u = u_values[i], report.u.max = FALSE)
        results$r_u[i] <- res$r.value
      },
      silent = TRUE
    )
  }

  return(results)
}

#' Plot r(u) curve
#'
#' @param ru_data Data frame from get_ru_diagnostics
#' @return A ggplot object
plot_ru_curve <- function(ru_data) {
  ggplot(ru_data, aes(x = .data$u, y = .data$r_u)) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", alpha = 0.7) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(color = "steelblue", size = 3) +
    scale_y_continuous(limits = c(0, 1), name = "r(u) p-value") +
    scale_x_continuous(breaks = ru_data$u, name = "u (Minimum Replicating Studies)") +
    theme_minimal() +
    labs(
      title = "Replicability Sensitivity: r(u) Curve",
      subtitle = "Shows r-value significance for different replication requirements",
      caption = "Red dashed line: alpha = 0.05"
    ) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      panel.grid.minor = element_blank()
    )
}
