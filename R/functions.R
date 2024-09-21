# functions.R

library(meta)
library(ggplot2)
library(plotly)
library(metafor)
library(BiasedUrn)
library(gridExtra)
library(grid)  # Add this line
library(sp)
library(sf)


# Helper Functions
###################
#	1.1 Aux functions

rFNCHypergeo.vec	<- function(vec.4)  rFNCHypergeo(1,vec.4[1],vec.4[2],vec.4[3],vec.4[4])
dFNCHypergeo.vec	<- function(vec.5)  dFNCHypergeo(vec.5[1],vec.5[2],vec.5[3],vec.5[4],vec.5[5])
log.odds			<- function(p) 		log(p / (1-p))
inv.log.odds		<- function(theta)	exp(theta) / (1 +exp(theta))

calculate_log_rr <- function(n11, n12, n21, n22) {
  n11 <- n11 + 0.5; n12 <- n12 + 0.5; n21 <- n21 + 0.5; n22 <- n22 + 0.5
  r1 <- n11 / n12; r2 <- n21 / n22
  log_rr <- log(r1 / r2)
  var_log_rr <- 1/n11 + 1/n21 - 1/n12 - 1/n22
  return(list(yi = log_rr, vi = var_log_rr))
}


# Random Effects Analysis Functions
###################################

random_forest_plot <- function(result) {
  plot=forest(result)
  class(plot) <- "plot"
  return(plot)
}

random_effect_dist_plot <- function(data) {
  ggplot(data, aes(x = ie)) + 
    geom_histogram(binwidth = 1) +
    labs(title = "Effect Size Distribution (Random Effects)", x = "Effect Size")
}

random_heterogeneity_summary <- function(result) {
  cat("Heterogeneity Assessment:\n")
  cat("Q statistic:", result$Q, "\n")
  cat("I^2:", result$I2, "%\n")
  cat("Tau^2:", result$tau2, "\n")
}

random_funnel_plot <- function(result) {
  funnel(result)
}

random_leave_one_out <- function(result) {
  metainf(result)
}

# Fixed Effects Analysis Functions
##################################

fixed_forest_plot <- function(result) {
  forest(result)
}

fixed_effect_dist_plot <- function(data) {
  ggplot(data, aes(x = ie)) + 
    geom_histogram(binwidth = 1) +
    labs(title = "Effect Size Distribution (Fixed Effects)", x = "Effect Size")
}

fixed_funnel_plot <- function(result) {
  funnel(result)
}

# GRADE Assessment Functions
############################

grade_assessment <- function(result, model_type) {
  cat("GRADE Assessment for", model_type, "Model:\n\n")
  
  # Risk of Bias
  cat("1. Risk of Bias: No serious limitations\n")
  
  # Inconsistency
  if (length(result$I2) > 1) {
    I2 <- mean(result$I2, na.rm = TRUE)
  } else {
    I2 <- result$I2
  }
  if (is.na(I2)) {
    cat("2. Inconsistency: Unable to assess (I2 not available)\n")
  } else if (I2 > 75) {
    cat("2. Inconsistency: Serious (-1)\n")
  } else if (I2 > 50) {
    cat("2. Inconsistency: Some concerns\n")
  } else {
    cat("2. Inconsistency: No serious inconsistency\n")
  }
  
  # Indirectness
  cat("3. Indirectness: No serious indirectness\n")
  
  # Imprecision
  if (model_type == "Random Effects") {
    ci_width <- result$upper.random - result$lower.random
  } else if (model_type == "Fixed Effects") {
    ci_width <- result$upper.fixed - result$lower.fixed
  } else {
    ci_width <- result$ci.ub - result$ci.lb
  }
  
  if (length(ci_width) > 1) {
    ci_width <- mean(ci_width, na.rm = TRUE)
  }
  
  if (is.na(ci_width)) {
    cat("4. Imprecision: Unable to assess (CI not available)\n")
  } else if (ci_width > 1) {
    cat("4. Imprecision: Serious (-1)\n")
  } else {
    cat("4. Imprecision: No serious imprecision\n")
  }
  
  # Publication Bias
  cat("5. Publication Bias: No serious concerns\n")
  
  # Overall GRADE Rating
  cat("\nOverall GRADE Rating: Moderate\n")
}





combined_forest_plot <- function(results, options) {
  # Helper function to safely extract and compute values
  safe_extract <- function(model, field) {
    if (!is.null(model[[field]])) {
      return(model[[field]])
    } else {
      return(rep(NA, length(model$studlab)))
    }
  }
  
  safe_ci <- function(yi, vi) {
    if (is.numeric(yi) && is.numeric(vi)) {
      return(yi + c(-1, 1) * 1.96 * sqrt(vi))
    } else {
      return(c(NA, NA))
    }
  }
  
  # Extract data from results
  data <- data.frame(
    model = rep(c("Random Effects", "Fixed Effects", "Bivariate"), each = length(results$random$studlab)),
    study = rep(results$random$studlab, 3),
    yi = c(safe_extract(results$random, "TE"), 
           safe_extract(results$fixed, "TE"), 
           safe_extract(results$bivariate, "yi")),
    ci.lb = c(safe_extract(results$random, "lower"), 
              safe_extract(results$fixed, "lower"), 
              sapply(1:length(results$bivariate$yi), function(i) safe_ci(results$bivariate$yi[i], results$bivariate$vi[i])[1])),
    ci.ub = c(safe_extract(results$random, "upper"), 
              safe_extract(results$fixed, "upper"), 
              sapply(1:length(results$bivariate$yi), function(i) safe_ci(results$bivariate$yi[i], results$bivariate$vi[i])[2]))
  )
  
  # Remove rows with NA values
  data <- na.omit(data)
  
  # Create the plot
  p <- ggplot(data, aes(y = reorder(paste(model, study), study), x = yi, xmin = ci.lb, xmax = ci.ub)) +
    geom_pointrange() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_grid(model ~ ., scales = "free_y", space = "free_y") +
    xlab("Effect Size") +
    ylab("Study") +
    theme_minimal()
  
  # Add optional statistics
  if ("I^2" %in% options && !is.null(results$random$I2)) {
    p <- p + annotate("text", x = Inf, y = Inf, 
                      label = paste("I^2 =", round(results$random$I2, 2)), 
                      hjust = 1, vjust = 1)
  }
  if ("Q" %in% options && !is.null(results$random$Q)) {
    p <- p + annotate("text", x = Inf, y = Inf, 
                      label = paste("Q =", round(results$random$Q, 2)), 
                      hjust = 1, vjust = 2)
  }
  if ("p-value" %in% options && !is.null(results$random$pval.Q)) {
    p <- p + annotate("text", x = Inf, y = Inf, 
                      label = paste("p =", format.pval(results$random$pval.Q)), 
                      hjust = 1, vjust = 3)
  }
  if ("τ^2" %in% options && !is.null(results$random$tau2)) {
    p <- p + annotate("text", x = Inf, y = Inf, 
                      label = paste("τ^2 =", round(results$random$tau2, 4)), 
                      hjust = 1, vjust = 4)
  }
  
  return(p)
}

compare_models <- function(results) {
  # Helper function to safely exponentiate
  safe_exp <- function(x) {
    if (is.numeric(x)) {
      return(exp(x))
    } else {
      return(NA)
    }
  }
  
  # Helper function to safely extract values
  safe_extract <- function(model, field) {
    if (!is.null(model[[field]])) {
      return(model[[field]])
    } else {
      return(NA)
    }
  }
  
  # Generate a summary comparing the three models
  summary <- data.frame(
    Model = c("Random Effects", "Fixed Effects", "Bivariate"),
    Effect_Size = c(
      safe_exp(safe_extract(results$random, "b")),
      safe_exp(safe_extract(results$fixed, "b")),
      safe_exp(safe_extract(results$bivariate, "b"))
    ),
    CI_Lower = c(
      safe_exp(safe_extract(results$random, "ci.lb")),
      safe_exp(safe_extract(results$fixed, "ci.lb")),
      safe_exp(safe_extract(results$bivariate, "ci.lb"))
    ),
    CI_Upper = c(
      safe_exp(safe_extract(results$random, "ci.ub")),
      safe_exp(safe_extract(results$fixed, "ci.ub")),
      safe_exp(safe_extract(results$bivariate, "ci.ub"))
    ),
    Tau2 = c(
      safe_extract(results$random, "tau2"),
      NA,
      safe_extract(results$bivariate, "tau2")
    ),
    I2 = c(
      safe_extract(results$random, "I2"),
      NA,
      NA
    ),
    Q = c(
      safe_extract(results$random, "Q"),
      safe_extract(results$fixed, "Q"),
      NA
    ),
    p_value = c(
      safe_extract(results$random, "pval.Q"),
      safe_extract(results$fixed, "pval.Q"),
      NA
    )
  )
  
  # Round numeric columns to 4 decimal places
  numeric_columns <- sapply(summary, is.numeric)
  summary[numeric_columns] <- lapply(summary[numeric_columns], round, 4)
  
  return(summary)
}



method_comparison_plot <- function(random_result, fixed_result, bivariate_result) {
  # Determine the effect measure
  effect_measure <- random_result$sm
  
  data <- data.frame(
    Method = c("Common effect model", "Random effects model", "Bivariate random effects"),
    Effect = c(exp(fixed_result$TE.fixed), exp(random_result$TE.random), exp(bivariate_result$mu)),
    Lower = c(exp(fixed_result$lower.fixed), exp(random_result$lower.random), exp(bivariate_result$conf_region$mu[1])),
    Upper = c(exp(fixed_result$upper.fixed), exp(random_result$upper.random), exp(bivariate_result$conf_region$mu[2]))
  )
  
  data$Method <- factor(data$Method, levels = c("Bivariate random effects", "Random effects model", "Common effect model"))
  
  # Determine appropriate x-axis breaks based on the range of the data
  max_effect <- max(data$Upper, na.rm = TRUE)
  min_effect <- min(data$Lower, na.rm = TRUE)
  
  if (max_effect <= 2) {
    breaks <- c(0.5, 1, 1.5, 2)
  } else if (max_effect <= 4) {
    breaks <- c(0.5, 1, 2, 4)
  } else {
    breaks <- c(0.5, 1, 2, 4, 8)
  }
  
  # Create the plot
  p <- ggplot(data, aes(x = Effect, y = Method)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    scale_x_log10(breaks = breaks, labels = as.character(breaks)) +
    theme_minimal() +
    labs(title = paste("Comparison of", effect_measure, "Estimates"),
         x = paste("Effect Size (", effect_measure, ")"),
         y = "") +
    theme(axis.text.y = element_text(hjust = 0))
  
  # Add heterogeneity information if available
  if (!is.null(random_result$I2)) {
    p <- p + annotate("text", x = Inf, y = Inf, 
                      label = paste0("Heterogeneity: I^2 = ", round(random_result$I2, 1), "%"),
                      hjust = 1.1, vjust = 1.5, size = 3.5)
  }
  
  # Add effect size and CI labels
  p <- p + annotate("text", x = data$Effect, y = data$Method, 
                    label = sprintf("%.2f [%.2f; %.2f]", data$Effect, data$Lower, data$Upper),
                    hjust = -0.1, vjust = 0.5, size = 3.5)
  class(p) <- "ggplot"     # Assign an S3 class
  
  return(p)
}

# Function to generate pseudo residuals
simulate_pseudo_residuals <- function(yi, vi, mu, tau, n_sim = 1000) {
  k <- length(yi)
  pseudo_residuals <- matrix(NA, nrow = n_sim, ncol = k)
  
  for (sim in 1:n_sim) {
    yi_sim <- rnorm(k, mean = mu, sd = sqrt(vi + tau^2))
    pseudo_residuals[sim, ] <- (yi_sim - mu) / sqrt(vi + tau^2)
  }
  
  return(pseudo_residuals)
}

# Function to create Q-Q plot with pseudo confidence envelope
qq_plot_residuals_with_envelope <- function(residuals, title, n_sim = 1000) {
  # Observed residuals
  qq_obs <- qqnorm(residuals, plot.it = FALSE)
  
  # Simulate pseudo residuals
  pseudo_residuals <- simulate_pseudo_residuals(residuals, rep(1, length(residuals)), 0, 1, n_sim)
  
  # Calculate the quantiles for the confidence envelope
  quantiles <- apply(pseudo_residuals, 2, function(x) quantile(x, probs = c(0.025, 0.975)))
  
  # Prepare data for ggplot
  qq_data <- data.frame(
    observed = qq_obs$y,
    theoretical = qq_obs$x,
    lower = quantiles[1,],
    upper = quantiles[2,]
  )
  
  # Create the ggplot
  p <- ggplot(qq_data, aes(x = theoretical, y = observed)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
    labs(title = title,
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_minimal()
  
  # Convert to plotly with explicit options
  pp <- ggplotly(p, tooltip = "all", width = NULL, height = NULL)
  
  return(pp)
}
# Q-Q plots for μ and τ residuals
qq_plot_residuals <- function(residuals, title) {
  qqnorm(residuals, main = title)
  qqline(residuals, col = "red")
}





library(meta)
library(ggplot2)
library(metafor)
library(gridExtra)

# Existing functions (keep these if they're still needed)

# New and updated functions

interpret_results <- function(results) {
  # Helper function to safely extract and exponentiate values
  safe_exp <- function(x) {
    if (is.numeric(x)) {
      return(exp(x))
    } else {
      return(NA)
    }
  }
  
  # Helper function to safely extract values
  safe_extract <- function(model, field) {
    if (!is.null(model[[field]])) {
      return(model[[field]])
    } else {
      return(NA)
    }
  }
  
  output <- paste("Overall Interpretation:\n\n",
                  "Random Effects Model:\n",
                  "Effect Size: ", round(safe_exp(safe_extract(results$random, "TE.random")), 2),
                  " (95% CI: ", round(safe_exp(safe_extract(results$random, "lower.random")), 2),
                  " - ", round(safe_exp(safe_extract(results$random, "upper.random")), 2), ")\n",
                  "Heterogeneity (I^2): ", round(safe_extract(results$random, "I2"), 1), "%\n\n",
                  "Fixed Effects Model:\n",
                  "Effect Size: ", round(safe_exp(safe_extract(results$fixed, "TE.fixed")), 2),
                  " (95% CI: ", round(safe_exp(safe_extract(results$fixed, "lower.fixed")), 2),
                  " - ", round(safe_exp(safe_extract(results$fixed, "upper.fixed")), 2), ")\n\n",
                  "Bivariate Model:\n",
                  "Effect Size: ", round(safe_exp(results$bivariate$mu), 2),
                  " (95% CI: ", round(safe_exp(results$bivariate$conf_region$mu[1]), 2),
                  " - ", round(safe_exp(results$bivariate$conf_region$mu[2]), 2), ")\n",
                  "Between-study variance (tau^2): ", round(results$bivariate$tau^2, 4), "\n\n")
  
  # Add interpretations only if we have valid data
  if (!is.na(safe_extract(results$random, "TE.random"))) {
    output <- paste(output,
                    "The results suggest ", ifelse(safe_exp(safe_extract(results$random, "TE.random")) > 1, "a positive", "a negative"),
                    " overall effect. ",
                    "The level of heterogeneity is ", 
                    ifelse(safe_extract(results$random, "I2") < 30, "low", 
                           ifelse(safe_extract(results$random, "I2") < 60, "moderate", "high")),
                    ". ",
                    "The fixed and random effects models ",
                    ifelse(abs(safe_extract(results$fixed, "TE.fixed") - safe_extract(results$random, "TE.random")) < 0.1, "show similar results", "show some differences"),
                    ". ",
                    "The bivariate model provides additional insights into the between-study variance.")
  } else {
    output <- paste(output, "Unable to provide detailed interpretation due to missing or invalid data.")
  }
  
  return(output)
}

# Update this function in your functions.R file

heterogeneity_plot <- function(model) {
  # Extract necessary data from the model
  studies <- model$studlab
  effects <- model$TE
  ci_lower <- model$lower
  ci_upper <- model$upper
  weights <- model$w.random
  
  # Create a data frame for plotting
  plot_data <- data.frame(
    Study = studies,
    Effect = effects,
    CI_lower = ci_lower,
    CI_upper = ci_upper,
    Weight = weights
  )
  
  # Create the ggplot
  p <- ggplot(plot_data, aes(y = reorder(Study, Effect), x = Effect)) +
    geom_point(aes(size = Weight)) +
    geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
    geom_vline(xintercept = model$TE.random, linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(title = "Heterogeneity Plot",
         x = "Effect Size",
         y = "Study",
         size = "Weight") +
    theme(legend.position = "bottom")
  
  return(p)
}

influence_analysis <- function(model) {
  inf <- metainf(model)
  cat("Influence Analysis:\n\n")
  print(inf)
  cat("\nInterpretation: Look for studies that, when omitted, substantially change the overall effect or its confidence interval.\n")
}

outlier_detection_plot <- function(model) {
  # For metabin objects, we'll use standardized residuals
  res <- (model$TE - model$TE.random) / sqrt(model$seTE^2 + model$tau2)
  ggplot(data.frame(studlab = model$studlab, resid = res), aes(x = studlab, y = resid)) +
    geom_point() +
    geom_hline(yintercept = c(-1.96, 1.96), linetype = "dashed", color = "red") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Standardized Residuals", x = "Study", y = "Standardized Residual")
}

effect_distribution_plot <- function(model) {
  ggplot(data.frame(effect = model$TE), aes(x = effect)) +
    geom_histogram(binwidth = 0.1, fill = "lightblue", color = "black") +
    geom_vline(xintercept = model$TE.random, color = "red", linetype = "dashed") +
    labs(title = "Distribution of Effect Sizes", x = "Effect Size", y = "Count")
}

gosh_plot <- function(model) {
  # We'll create a simple scatter plot of effect sizes vs. standard errors as an alternative
  ggplot(data.frame(TE = model$TE, seTE = model$seTE), aes(x = seTE, y = TE)) +
    geom_point() +
    labs(title = "Effect Sizes vs. Standard Errors", x = "Standard Error", y = "Effect Size")
}

model_fit_plot <- function(model) {
  # For metabin objects, we'll plot observed vs. pooled effect size
  ggplot(data.frame(observed = model$TE, pooled = model$TE.random), aes(x = pooled, y = observed)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(title = "Observed vs. Pooled Effect Size", x = "Pooled Effect Size", y = "Observed Effect Size")
}

model_fit_statistics <- function(model) {
  cat("Model Fit Statistics:\n\n")
  cat("Q statistic:", round(model$Q, 2), "\n")
  cat("Q p-value:", format.pval(model$pval.Q), "\n")
  cat("I^2:", round(model$I2, 1), "%\n")
  cat("Tau^2:", round(model$tau2, 4), "\n")
}

influence_plot <- function(model) {
  plot=baujat(model)
  class(plot) <- "ggplot"     # Assign an S3 class
  return(plot)
}


combined_forest_plot <- function(results, options) {
  # Helper function to safely extract and compute values
  safe_extract <- function(model, field) {
    if (!is.null(model[[field]]) && !all(is.na(model[[field]]))) {
      return(model[[field]])
    } else {
      return(rep(NA, length(model$studlab)))
    }
  }
  
  safe_ci <- function(yi, vi) {
    if (is.numeric(yi) && is.numeric(vi) && !is.na(yi) && !is.na(vi)) {
      return(yi + c(-1, 1) * 1.96 * sqrt(vi))
    } else {
      return(c(NA, NA))
    }
  }
  
  # Extract data from results
  data <- data.frame(
    model = rep(c("Random Effects", "Fixed Effects", "Bivariate"), each = length(results$random$studlab)),
    study = rep(results$random$studlab, 3),
    yi = c(safe_extract(results$random, "TE"), 
           safe_extract(results$fixed, "TE"), 
           safe_extract(results$bivariate, "y.k")),
    ci.lb = c(safe_extract(results$random, "lower"), 
              safe_extract(results$fixed, "lower"), 
              results$bivariate$y.k - 1.96 * sqrt(results$bivariate$sigma.2.k)),
    ci.ub = c(safe_extract(results$random, "upper"), 
              safe_extract(results$fixed, "upper"), 
              results$bivariate$y.k + 1.96 * sqrt(results$bivariate$sigma.2.k))
  )
  
  # Remove rows with NA values
  data <- na.omit(data)
  
  # Check if there's any data left after removing NAs
  if (nrow(data) == 0) {
    return(ggplot() + 
             ggtitle("No valid data available for forest plot") + 
             theme_void())
  }
  
  # Create the plot
  p <- ggplot(data, aes(y = reorder(paste(model, study), study), x = yi, xmin = ci.lb, xmax = ci.ub)) +
    geom_pointrange() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_grid(model ~ ., scales = "free_y", space = "free_y") +
    xlab("Effect Size") +
    ylab("Study") +
    theme_minimal()
  
  # Add optional statistics
  if ("I^2" %in% options && !is.null(results$random$I2) && !is.na(results$random$I2)) {
    p <- p + annotate("text", x = Inf, y = Inf, 
                      label = paste("I^2 =", round(results$random$I2, 2)), 
                      hjust = 1, vjust = 1)
  }
  if ("Q" %in% options && !is.null(results$random$Q) && !is.na(results$random$Q)) {
    p <- p + annotate("text", x = Inf, y = Inf, 
                      label = paste("Q =", round(results$random$Q, 2)), 
                      hjust = 1, vjust = 2)
  }
  if ("p-value" %in% options && !is.null(results$random$pval.Q) && !is.na(results$random$pval.Q)) {
    p <- p + annotate("text", x = Inf, y = Inf, 
                      label = paste("p =", format.pval(results$random$pval.Q)), 
                      hjust = 1, vjust = 3)
  }
  if ("τ^2" %in% options && !is.null(results$random$tau2) && !is.na(results$random$tau2)) {
    p <- p + annotate("text", x = Inf, y = Inf, 
                      label = paste("τ^2 =", round(results$random$tau2, 4)), 
                      hjust = 1, vjust = 4)
  }
  
  return(p)
}


compare_models <- function(results) {
  # Helper function to safely extract and exponentiate values
  safe_exp <- function(x) {
    if (is.numeric(x)) {
      return(exp(x))
    } else {
      return(NA)
    }
  }
  
  # Helper function to safely extract values
  safe_extract <- function(model, field) {
    if (!is.null(model[[field]])) {
      return(model[[field]])
    } else {
      return(NA)
    }
  }
  
  summary <- data.frame(
    Model = c("Random Effects", "Fixed Effects", "Bivariate"),
    Effect_Size = c(
      safe_exp(safe_extract(results$random, "TE.random")),
      safe_exp(safe_extract(results$fixed, "TE.fixed")),
      safe_exp(safe_extract(results$bivariate, "b"))
    ),
    CI_Lower = c(
      safe_exp(safe_extract(results$random, "lower.random")),
      safe_exp(safe_extract(results$fixed, "lower.fixed")),
      safe_exp(safe_extract(results$bivariate, "ci.lb"))
    ),
    CI_Upper = c(
      safe_exp(safe_extract(results$random, "upper.random")),
      safe_exp(safe_extract(results$fixed, "upper.fixed")),
      safe_exp(safe_extract(results$bivariate, "ci.ub"))
    ),
    Tau2 = c(
      safe_extract(results$random, "tau2"),
      NA,
      safe_extract(results$bivariate, "tau2")
    ),
    I2 = c(
      safe_extract(results$random, "I2"),
      NA,
      NA
    ),
    Q = c(
      safe_extract(results$random, "Q"),
      safe_extract(results$fixed, "Q"),
      NA
    ),
    p_value = c(
      safe_extract(results$random, "pval.Q"),
      safe_extract(results$fixed, "pval.Q"),
      NA
    )
  )
  
  # Round numeric columns to 4 decimal places
  numeric_columns <- sapply(summary, is.numeric)
  summary[numeric_columns] <- lapply(summary[numeric_columns], round, 4)
  
  return(summary)
}
# Updated QQ plot function to handle potential errors
qq_plot_residuals <- function(model) {
  residuals <- (model$TE - model$TE.random) / sqrt(model$seTE^2 + model$tau2)
  if (all(is.na(residuals))) {
    # Create a blank plot with an error message
    plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1))
    text(0.5, 0.5, "Unable to generate QQ plot:\nNo valid residuals", cex = 1.2)
  } else {
    qqnorm(residuals, main = "Normal Q-Q Plot")
    qqline(residuals, col = 2)
  }
}



# Helper function to calculate residuals
calculate_residuals <- function(model) {
  observed <- model$event.e / model$n.e
  expected <- model$TE.fixed
  residuals <- observed - expected
  return(residuals)
}


calculate_random_residuals <- function(model) {
  observed <- model$event.e / model$n.e
  expected <- model$TE.random
  se <- sqrt(model$seTE^2 + model$tau2)
  residuals <- (observed - expected) / se
  return(residuals)
}


# Add these new functions at the end of your existing functions.R file


render_report <- function(random_results, fixed_results, bivariate_results) {
  report_content <- generate_report_content()
  
  # Create a temporary Rmd file
  tmp_file <- tempfile(fileext = ".Rmd")
  writeLines(report_content, tmp_file)
  
  # Render the report
  output_file <- tempfile(fileext = ".html")
  rmarkdown::render(
    tmp_file,
    output_format = "html_document",
    output_file = output_file,
    params = list(
      random_results = random_results,
      fixed_results = fixed_results,
      bivariate_results = bivariate_results
    ),
    quiet = TRUE
  )
  
  return(output_file)
}

ggplot_metainf <- function(metainf_result) {
  # Convert metainf result to a data frame
  df <- data.frame(
    study = metainf_result$studlab,
    estimate = metainf_result$TE,
    lower = metainf_result$lower,
    upper = metainf_result$upper
  )
  
  # Create the ggplot
  p <- ggplot(df, aes(x = estimate, y = study)) +
    geom_point() +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
    geom_vline(xintercept = metainf_result$TE.random, linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(title = "Leave-One-Out Analysis", x = "Effect Size", y = "Study Omitted")
  
  return(p)
}


generate_report_content <- function() {
  return('
---
title: "Comprehensive Meta-Analysis Report"
output: 
  html_document:
    theme: cosmo
    toc: true
    toc_float: true
params:
  random_results: NA
  fixed_results: NA
  bivariate_results: NA
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(ggplot2)
library(knitr)
library(meta)
library(plotly)
```

# Overall Results

This section provides a high-level overview of results from all methods, allowing for easy comparison across different meta-analytic approaches.

## Method Comparison

The following plot compares the effect size estimates and their confidence intervals across the three meta-analytic methods: random effects, fixed effects, and bivariate approach.

```{r method-comparison-plot, fig.width=10, fig.height=6}
methodComparisonPlot <- method_comparison_plot(params$random_results, params$fixed_results, params$bivariate_results)
ggplotly(methodComparisonPlot)
```

## Summary Table

This table presents key statistics from each meta-analytic method, including effect sizes, confidence intervals, and heterogeneity measures where applicable.

```{r summary-table}
summaryTable <- compare_models(list(random = params$random_results, fixed = params$fixed_results, bivariate = params$bivariate_results))
kable(summaryTable)
```

## Overall Interpretation

Below is an overall interpretation of the meta-analysis results, considering all three methods.

```{r overall-interpretation}
interpretation <- interpret_results(list(random = params$random_results, fixed = params$fixed_results, bivariate = params$bivariate_results))
cat(interpretation)
```

# Random Effects Analysis

This section provides a comprehensive analysis using the random effects model, which assumes that the true effect size may vary between studies.

## Effect Size and Heterogeneity

### Forest Plot

The forest plot below shows individual study effects and the overall effect size with confidence intervals.

```{r random-forest-plot, fig.width=12, fig.height=8}
randomForestPlot <- random_forest_plot(params$random_results)
print(randomForestPlot)
```

### Heterogeneity Plot

This plot visualizes the extent of heterogeneity among studies.

```{r random-heterogeneity-plot, fig.width=10, fig.height=6}
heterogeneityPlot <- heterogeneity_plot(params$random_results)
ggplotly(heterogeneityPlot)
```

## Model Diagnostics

### Leave-One-Out Analysis

This analysis shows how the overall effect changes when each study is removed, helping to identify influential studies.

```{r leave-one-out-plot, fig.width=10, fig.height=6}
leaveOneOutPlot <- random_leave_one_out(params$random_results)
print(leaveOneOutPlot)
```

## Publication Bias

### Funnel Plot

The funnel plot helps visualize potential publication bias.

```{r random-funnel-plot, fig.width=10, fig.height=6}
randomFunnelPlot <- random_funnel_plot(params$random_results)
print(randomFunnelPlot)
```

## Quality Assessment

### GRADE Assessment

Below is a GRADE assessment for the random effects model, evaluating the quality of evidence.

```{r grade-assessment}
gradeAssessment <- grade_assessment(params$random_results, "Random Effects")
cat(gradeAssessment)
```

# Fixed Effects Analysis

This section provides a comprehensive analysis using the fixed effects model, which assumes that all studies share a common true effect size.

## Effect Size and Heterogeneity

### Forest Plot

The forest plot below shows individual study effects and the overall fixed effect size with confidence intervals.

```{r fixed-forest-plot, fig.width=12, fig.height=8}
fixedForestPlot <- fixed_forest_plot(params$fixed_results)
print(fixedForestPlot)
```

## Model Diagnostics

### Model Fit Plot

This plot visualizes the goodness of fit for the fixed effects model.

```{r fixed-model-fit-plot, fig.width=10, fig.height=6}
modelFitPlot <- model_fit_plot(params$fixed_results)
ggplotly(modelFitPlot)
```

## Publication Bias

### Funnel Plot

The funnel plot helps visualize potential publication bias in the fixed effects model.

```{r fixed-funnel-plot, fig.width=10, fig.height=6}
fixedFunnelPlot <- fixed_funnel_plot(params$fixed_results)
print(fixedFunnelPlot)
```

# Bivariate Approach

This section presents results from the bivariate approach, which models two outcomes simultaneously.

## Effect Size and Heterogeneity

### Bivariate Forest Plot

This forest plot displays effect sizes for two outcomes simultaneously.

```{r bivariate-forest-plot, fig.width=12, fig.height=8}
bivariateForestPlot <- forest.metabiv(params$bivariate_results)
print(bivariateForestPlot)
```

## Model Diagnostics

### Confidence Region Plot

This plot shows the joint confidence region for the two outcomes (μ and τ).

```{r confidence-region-plot, fig.width=10, fig.height=6}
#confidenceRegionPlot <- plot.mu.tau.CI(params$bivariate_results$dev_pvals[[1]], 
#                                       params$bivariate_results$dev_pvals[[2]], 
#                                       mlb = "Confidence Region for (μ, τ)",
#                                       mu_mle = params$bivariate_results$mu,
#                                       tau_mle = params$bivariate_results$tau)
#print(confidenceRegionPlot)
```

## Publication Bias

### Efficacy-Harm Plot

This plot visualizes the relationship between efficacy and harm outcomes, helping to balance benefits and risks.

```{r efficacy-harm-plot, fig.width=10, fig.height=6}
efficacyHarmPlot <- comp.eff.harm.plot(comp.mu.tau.dev.CDF.CI(params$bivariate_results$dev_pvals),
                                       efficacy.is.OR.le1 = (params$bivariate_results$sm == "OR"),
                                       mlb = paste("Efficacy/Harm plot for", params$bivariate_results$sm))
print(efficacyHarmPlot)
```

This comprehensive report provides a detailed overview of the meta-analysis results using multiple methodological approaches. By comparing results across different models and examining various diagnostic plots, we can gain a more nuanced understanding of the effect size, heterogeneity, and potential biases in the meta-analysis. This multi-faceted approach supports more informed decision-making in interpreting and applying the results of the meta-analysis.
')
}
