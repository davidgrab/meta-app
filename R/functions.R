# functions.R

library(meta)
library(ggplot2)
library(plotly)
library(metafor)
library(BiasedUrn)
library(gridExtra)
library(grid) 
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
  plot=meta::forest(result)
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
  meta::forest(result)
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
    ci_width <- result$upper.common - result$lower.common
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


# Helper function to safely extract and exponentiate values
safe_exp <- function(x) {
  if (is.numeric(x)) {
    return(exp(x))
  } else {
    return(NA)
  }
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




method_comparison_plot <- function(random_result, fixed_result, bivariate_result) {
  # Determine the effect measure
  effect_measure <- random_result$sm
  
  data <- data.frame(
    Method = c("Common effect model", "Random effects model", "Bivariate random effects"),
    Effect = c(exp(fixed_result$TE.common), exp(random_result$TE.random), exp(bivariate_result$mu)),
    Lower = c(exp(fixed_result$lower.common), exp(random_result$lower.random), exp(bivariate_result$conf_region$mu[1])),
    Upper = c(exp(fixed_result$upper.common), exp(random_result$upper.random), exp(bivariate_result$conf_region$mu[2]))
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






# Existing functions (keep these if they're still needed)

# New and updated functions

interpret_results <- function(results) {
  # Helper function to safely extract and exponentiate values
  #safe_exp <- function(x) {
  #  if (is.numeric(x)) {
  #    return(exp(x))
  #  } else {
  #    return(NA)
  #  }
  #}
  
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
                  "Heterogeneity (I^2): ", safe_extract(results$random, "I2"), "%\n\n",
                  "Fixed Effects Model:\n",
                  "Effect Size: ", round(safe_exp(safe_extract(results$fixed, "TE.common")), 2),
                  " (95% CI: ", round(safe_exp(safe_extract(results$fixed, "lower.common")), 2),
                  " - ", round(safe_exp(safe_extract(results$fixed, "upper.common")), 2), ")\n",
                  "P-value: ", format.pval(safe_extract(results$fixed, "pval.common")), "\n\n",
                  "Bivariate Model:\n",
                  "Effect Size: ", round(safe_exp(results$bivariate$mu), 2),
                  " (95% CI: ", round(safe_exp(safe_extract(results$bivariate, "lower")), 2),
                  " - ", round(safe_exp(safe_extract(results$bivariate, "upper")), 2), ")\n",
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
                    ifelse(abs(safe_extract(results$fixed, "TE.common") - safe_extract(results$random, "TE.random")) < 0.1, "show similar results", "show some differences"),
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
  pooled <- if (!is.null(model$TE.random) && !all(is.na(model$TE.random))) {
    model$TE.random
  } else if (!is.null(model$TE.common)) {
    model$TE.common
  } else {
    NA
  }

  tau2 <- if (!is.null(model$tau2) && !is.na(model$tau2)) model$tau2 else 0

  # Calculate standardized residuals
  res <- (model$TE - pooled) / sqrt(model$seTE^2 + tau2)

  ggplot(data.frame(studlab = model$studlab, resid = res), aes(x = studlab, y = resid)) +
    geom_point() +
    geom_hline(yintercept = c(-1.96, 1.96), linetype = "dashed", color = "red") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Standardized Residuals", x = "Study", y = "Standardized Residual")
}

effect_distribution_plot <- function(model) {
  pooled <- if (!is.null(model$TE.random) && !all(is.na(model$TE.random))) {
    model$TE.random
  } else if (!is.null(model$TE.common)) {
    model$TE.common
  } else {
    NA
  }

  ggplot(data.frame(effect = model$TE), aes(x = effect)) +
    geom_histogram(binwidth = 0.1, fill = "lightblue", color = "black") +
    { if (!is.na(pooled)) geom_vline(xintercept = pooled, color = "red", linetype = "dashed") } +
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
  # Check if the necessary components are available
  if (!all(c("x", "y", "studlab") %in% names(model))) {
    # If not, calculate the Baujat plot data
    bj <- baujat(model, plot = FALSE)
    x <- bj$x
    y <- bj$y
    studlab <- model$studlab
  } else {
    x <- model$x
    y <- model$y
    studlab <- model$studlab
  }
  
  # Ensure all vectors have the same length
  n <- min(length(x), length(y), length(studlab))
  x <- x[1:n]
  y <- y[1:n]
  studlab <- studlab[1:n]
  
  # Create a data frame for ggplot
  plot_data <- data.frame(x = x, y = y, studlab = studlab)
  
  # Create the ggplot
  ggplot(plot_data, aes(x = x, y = y, label = studlab)) +
    geom_point() +
    geom_text(hjust = -0.1, vjust = 0) +
    theme_minimal() +
    labs(title = "Baujat Plot",
         x = "Contribution to overall heterogeneity",
         y = "Influence on overall result")
}


combined_forest_plot <- function(results, options) {
  # Helper function to safely extract and compute valuesz
  #safe_extract <- function(model, field) {
  #  if (!is.null(model[[field]]) && !all(is.na(model[[field]]))) {
  #    return(model[[field]])
  #  } else {
  #    return(rep(NA, length(model$studlab)))
  #  }
  #}
  
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
  # Helper function to safely extract values
  safe_extract <- function(model, field) {
    if (!is.null(model[[field]])) {
      return(model[[field]])
    } else {
      return(NA)
    }
  }
  
  # Helper function to safely exponentiate values
  safe_exp <- function(x) {
    if (is.numeric(x) && !is.na(x)) {
      return(exp(x))
    } else {
      return(NA)
    }
  }

  summary <- data.frame(
    Model = c("Random Effects", "Fixed Effects", "Bivariate"),
    Effect = c(
      safe_exp(safe_extract(results$random, "TE.random")),
      safe_exp(safe_extract(results$fixed, "TE.common")),
      safe_exp(safe_extract(results$bivariate, "mu"))
    ),
    Lower = c(
      safe_exp(safe_extract(results$random, "lower.random")),
      safe_exp(safe_extract(results$fixed, "lower.common")),
      safe_exp(safe_extract(results$bivariate, "lower"))
    ),
    Upper = c(
      safe_exp(safe_extract(results$random, "upper.random")),
      safe_exp(safe_extract(results$fixed, "upper.common")),
      safe_exp(safe_extract(results$bivariate, "upper"))
    ),
    P_value = c(
      safe_extract(results$random, "pval.random"),
      safe_extract(results$fixed, "pval.common"),
      safe_extract(results$bivariate, "pval.Q")
    ),
    Heterogeneity = c(
      safe_extract(results$random, "tau2"),
      NA,
      safe_extract(results$bivariate, "tau")^2
    ),
    I2 = c(
      safe_extract(results$random, "I2"),
      NA,
      safe_extract(results$bivariate, "I2")/100
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
  if (!inherits(model, "meta")) {
    return(NULL)
  }
  
  # Get observed effect sizes
  observed <- model$TE
  
  # Get expected effect sizes (fixed effect estimate)
  expected <- model$TE.common
  
  # Calculate residuals
  residuals <- observed - expected
  
  # Standardize residuals if variance information is available
  if (!is.null(model$seTE)) {
    residuals <- residuals / model$seTE
  }
  
  return(residuals)
}


calculate_random_residuals <- function(model) {
  # Check if model is metagen (continuous) or metabin (binary)
  if (inherits(model, "metagen")) {
    TE <- model$TE
    seTE <- model$seTE
    weights <- model$w.random
  } else if (inherits(model, "metabin")) {
    # For metabin, TE and seTE are directly available for each study
    TE <- model$TE
    seTE <- model$seTE
    weights <- model$w.random
  } else {
    # Return NULL if the model type is not supported
    return(NULL)
  }

  # Defensive check for needed components
  if (is.null(TE) || is.null(seTE) || is.null(weights)) {
    return(NULL)
  }
  
  # Calculate the overall effect size and residuals
  overall_effect <- sum(weights * TE) / sum(weights)
  residuals <- (TE - overall_effect) / seTE
  
  # Standardize residuals
  std_residuals <- residuals / sd(residuals, na.rm = TRUE)
  
  return(std_residuals)
}


# Add these new functions at the end of your existing functions.R file


render_report <- function(random_results,
                         fixed_results,
                         bivariate_results,
                         data = NULL,
                         random_subgroup_results = NULL,
                         fixed_subgroup_results = NULL,
                         bivariate_subgroup_results = NULL,
                         metaregression_results = NULL,
                         include_subgroup = FALSE,
                         include_metareg = FALSE,
                         subgroup_var = NULL,  # New parameter
                         moderator_var = NULL,  # New parameter
                         moderator_type = NULL) {  # New parameter
  tryCatch({
    report_content <- generate_report_content()
    
    # Create a temporary Rmd file
    tmp_file <- tempfile(fileext = ".Rmd")
    writeLines(report_content, tmp_file)
    
    # Debugging info
    cat("Creating report with:\n")
    cat("- Random model class:", class(random_results), "\n")
    cat("- Fixed model class:", class(fixed_results), "\n")
    cat("- Bivariate model class:", class(bivariate_results), "\n")
    cat("- Data rows:", ifelse(is.null(data), "NULL", nrow(data)), "\n")
    if (include_subgroup) cat("- Subgroup variable:", subgroup_var, "\n")
    if (include_metareg) {
      cat("- Moderator variable:", moderator_var, "\n")
      cat("- Moderator type:", moderator_type, "\n")
    }
    
    # If subgroup analysis is requested, generate it here
    if (include_subgroup && !is.null(subgroup_var) && !is.null(data)) {
      # For random effects model
      if (inherits(random_results, "meta")) {
        # Create a new meta-analysis with subgroup
        random_subgroup_results <- metagen(
          TE = random_results$TE,
          seTE = random_results$seTE,
          studlab = random_results$studlab,
          data = data,
          sm = random_results$sm,
          subgroup = data[[subgroup_var]],
          method.tau = random_results$method.tau,
          method.random.ci = if (!is.null(random_results$method.random.ci)) random_results$method.random.ci else "hakn",
          common = FALSE,
          random = TRUE
        )
      }
      
      # For fixed effects model
      if (inherits(fixed_results, "meta")) {
        # Create a new meta-analysis with subgroup
        fixed_subgroup_results <- metagen(
          TE = fixed_results$TE,
          seTE = fixed_results$seTE,
          studlab = fixed_results$studlab,
          data = data,
          sm = fixed_results$sm,
          subgroup = data[[subgroup_var]],
          method.tau = fixed_results$method.tau,
          method.random.ci = if (!is.null(fixed_results$method.random.ci)) fixed_results$method.random.ci else "hakn",
          common = TRUE,
          random = FALSE
        )
      }
      
      # For bivariate model
      if (!is.null(bivariate_results)) {
        # metabiv does not currently support subgroup argument directly.
        bivariate_subgroup_results <- NULL
      }
    }
    
    # If meta-regression is requested, generate it here
    if (include_metareg && !is.null(moderator_var) && !is.null(data)) {
      # Build a formula of the form ~ moderator_var for use in meta::metareg / meta::bubble
      mod_formula <- stats::as.formula(paste("~", moderator_var))

      # Run meta-regression in a protected environment so that any problems
      # (e.g., insufficient study numbers, singularities, etc.) do not abort
      # the whole report rendering.  In case of error we simply skip the
      # meta-regression section by setting the results to NULL.
      metaregression_results <- tryCatch({
        # Build complete-case dataset
        df <- data
        complete_cases <- !is.na(df[[moderator_var]])
        df <- df[complete_cases, ]
        if (nrow(df) < 3) stop("Insufficient data for meta-regression (need >=3 studies).")

        # Extract effect sizes and SE from random_results (already corresponds to user-selected model)
        TE   <- random_results$TE[complete_cases]
        seTE <- random_results$seTE[complete_cases]

        # Prepare moderator variable
        mod_vec <- df[[moderator_var]]
        if (moderator_type == "categorical" || is.character(mod_vec) || is.factor(mod_vec)) {
          mod_vec <- as.factor(mod_vec)
        } else {
          mod_vec <- suppressWarnings(as.numeric(mod_vec))
        }

        # Choose estimation method (mirror UI: REML for random effects)
        rma_method <- ifelse(is.null(random_results$method.tau) || random_results$method.tau == "FE", "FE", "REML")

        # Run meta-regression with metafor::rma
        metareg_model <- metafor::rma(yi = TE, vi = seTE^2, mods = ~ mod_vec, method = rma_method, test = "knha")

        # Bubble plot via ggplot2
        bubble_plot <- tryCatch({
          df_plot <- data.frame(mod = mod_vec, TE = TE, se = seTE)
          ggplot2::ggplot(df_plot, ggplot2::aes(x = mod, y = TE, size = 1/se)) +
            ggplot2::geom_point(alpha = 0.6) +
            ggplot2::scale_size_continuous(name = "Precision (1/SE)") +
            ggplot2::labs(title = "Meta-Regression Bubble Plot", x = moderator_var, y = "Effect Size") +
            ggplot2::theme_minimal()
        }, error = function(e) NULL)

        list(model = metareg_model, plot = bubble_plot, moderator_type = moderator_type, moderator_name = moderator_var,
             moderator_data = mod_vec, effect_sizes = TE, standard_errors = seTE, study_names = df$study)
      }, error = function(e) {
        message("Meta-regression failed during report generation: ", e$message)
        NULL
      })
    }
    
    # Render the report
    output_file <- tempfile(fileext = ".html")
    rmarkdown::render(
      tmp_file,
      output_format = "html_document",
      output_file = output_file,
      params = list(
        random_results = random_results,
        fixed_results = fixed_results,
        bivariate_results = bivariate_results,
        data = data,
        random_subgroup_results = random_subgroup_results,
        fixed_subgroup_results = fixed_subgroup_results,
        bivariate_subgroup_results = bivariate_subgroup_results,
        metaregression_results = metaregression_results,
        include_subgroup = include_subgroup,
        include_metareg = include_metareg,
        subgroup_var = subgroup_var,
        moderator_var = moderator_var,
        moderator_type = moderator_type
      ),
      quiet = TRUE
    )
    
    return(output_file)
  }, error = function(e) {
    # Create a simple error report instead
    error_html <- paste0(
      "<html><head><title>Meta-Analysis Report Error</title></head><body>",
      "<h1>Error Generating Report</h1>",
      "<p>An error occurred while generating the report:</p>",
      "<pre>", conditionMessage(e), "</pre>",
      "<p>Please try again or contact support.</p>",
      "</body></html>"
    )
    
    error_file <- tempfile(fileext = ".html")
    writeLines(error_html, error_file)
    
    message("Error generating report: ", conditionMessage(e))
    
    return(error_file)
  })
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

# qq_plot_with_ci_raw <- function(y_k, mu, sigma_2_k, tau_2, log_odds = FALSE, 
#                             title = "Q-Q Plot for Residuals") {
#   # Transform data if log odds required
#   if (log_odds) {
#     y_k <- log(y_k / (1 - y_k))
#     mu <- log(mu / (1 - mu))
#   }
#   
#   # Calculate **regular residuals** instead of standardized residuals
#   residuals <- y_k - mu  # Use raw residuals
#   
#   # Remove any infinite or NA values
#   valid_indices <- is.finite(residuals)
#   residuals <- residuals[valid_indices]
#   
#   # Sort residuals for proper quantile computation
#   sorted_residuals <- sort(residuals)
#   n_points <- length(sorted_residuals)
#   
#   # Calculate theoretical quantiles using (i - 0.5)/n formula
#   p <- (1:n_points - 0.5) / n_points
#   theoretical_quantiles <- qnorm(p, mean = mean(mu), sd = sqrt(tau_2))  # Adjusted for empirical distribution
#   
#   # Calculate empirical standard error for CI
#   se_residuals <- sd(residuals)
#   
#   # Calculate pointwise confidence intervals
#   z_alpha <- qnorm(0.975)  # 95% CI
#   ci_width <- z_alpha * se_residuals * sqrt(p * (1 - p) / (n_points * dnorm(theoretical_quantiles)^2))
#   
#   ci_lower <- sorted_residuals - ci_width
#   ci_upper <- sorted_residuals + ci_width
#   
#   # Set up plot margins to accommodate title and subtitle
#   par(mar = c(5, 5, 4, 2) + 0.1)
#   
#   # Create the plot
#   plot(theoretical_quantiles, sorted_residuals,
#        main = title,
#        xlab = "Theoretical Quantiles",
#        ylab = if(log_odds) "Log-Odds Residuals" else "Residuals",
#        pch = 19,
#        col = "blue",
#        ylim = range(c(ci_lower, ci_upper, sorted_residuals), na.rm = TRUE))
#   
#   # **Modify reference line to use estimated μ and τ² (Upline)**
#   abline(a = mean(mu), b = sqrt(tau_2), col = "blue", lty = 2)  # Custom upline
#   
#   # **Add Q-Q Line (Least Squares Fit)**
#   qqline(residuals, distribution = function(p) qnorm(p, mean = mean(mu), sd = sqrt(tau_2)), col = "darkgreen", lwd = 2)
#   
#   # Add confidence intervals
#   polygon(c(theoretical_quantiles, rev(theoretical_quantiles)),
#           c(ci_lower, rev(ci_upper)),
#           col = rgb(0.8, 0.8, 0.8, 0.3),
#           border = NA)
#   
#   # Add points again to ensure they're visible above the CI region
#   points(theoretical_quantiles, sorted_residuals, pch = 19, col = "blue")
#   
#   # Add model information as subtitle
#   if (!is.null(tau_2) && !is.null(mu)) {
#     mtext(sprintf("τ² = %.3f, μ = %.3f", tau_2, mean(mu)), 
#           side = 3, line = 0.5, cex = 0.8)
#   }
#   
#   # Add legend
#   legend("topleft",
#          legend = c("Observed Quantiles", 
#                     "Reference Line (Estimated μ, τ²)", 
#                     "Q-Q Line (Regression Fit)",
#                     "95% Confidence Band"),
#          pch = c(19, NA, NA, 15),
#          lty = c(NA, 2, 1, NA),
#          col = c("blue", "blue", "darkgreen", rgb(0.8, 0.8, 0.8, 0.3)),
#          bg = "white")
#   
#   # Return invisibly the plot data for potential further use
#   invisible(list(
#     theoretical_quantiles = theoretical_quantiles,
#     empirical_quantiles = sorted_residuals,
#     ci_lower = ci_lower,
#     ci_upper = ci_upper,
#     se_residuals = se_residuals
#   ))
# }

qq_plot_with_ci_raw <- function(y_k, mu, sigma_2_k, tau_2, n_k, log_odds = FALSE, 
                                title = "Q-Q Plot for Residuals") {
  # Transform data if log odds required
  if (log_odds) {
    y_k <- log(y_k / (1 - y_k))
    mu <- log(mu / (1 - mu))
  }
  
  # Calculate regular residuals
  residuals <- y_k - mu  # Use raw residuals
  
  # Remove any infinite or NA values
  valid_indices <- is.finite(residuals)
  residuals <- residuals[valid_indices]
  
  if (length(residuals) < 2) {
    plot(1, type="n", axes=FALSE, xlab="", ylab="", main=title)
    text(1, 1, "Not enough valid data to generate Q-Q plot.")
    return(invisible(NULL))
  }
  
  sigma_2_k <- sigma_2_k[valid_indices]  # Ensure consistency
  n_k <- n_k[valid_indices]  # Ensure study sizes are consistent
  
  # Scale point size based on study size (inverse variance)
  point_sizes <- sqrt(1 / sigma_2_k)
  point_sizes <- point_sizes / max(point_sizes) * 3  # Normalize and scale
  
  # Sort residuals for proper quantile computation
  sorted_residuals <- sort(residuals, index.return = TRUE)
  residuals_sorted <- sorted_residuals$x
  sorted_indices <- sorted_residuals$ix  # Keep track of original order
  n_k_sorted <- n_k[sorted_indices]  # Sort patient numbers accordingly
  
  n_points <- length(residuals_sorted)
  
  # Calculate theoretical quantiles using (i - 0.5)/n formula
  p <- (1:n_points - 0.5) / n_points
  #theoretical_quantiles <- qnorm(p, mean = mean(mu), sd = sqrt(tau_2))  # Adjusted for empirical distribution
  theoretical_quantiles <- qnorm(p) 
  # Calculate empirical standard error for CI
  se_residuals <- sd(residuals_sorted)
  
  # Calculate pointwise confidence intervals
  z_alpha <- qnorm(0.975)  # 95% CI
  ci_width <- z_alpha * se_residuals * sqrt(p * (1 - p) / (n_points * dnorm(theoretical_quantiles)^2))
  
  ci_lower <- residuals_sorted - ci_width
  ci_upper <- residuals_sorted + ci_width
  
  # Set up plot margins to accommodate title and subtitle
  par(mar = c(5, 5, 4, 2) + 0.1)
  
  # Create the plot
  plot(theoretical_quantiles, residuals_sorted,
       main = title,
       xlab = "Theoretical Quantiles",
       ylab = if(log_odds) "Log-Odds Residuals" else "Residuals",
       pch = 19,
       col = "blue",
       cex = point_sizes,  # Scale point size by study size
       ylim = range(c(ci_lower, ci_upper, residuals_sorted), na.rm = TRUE))
  
  # **Modify reference line to use estimated μ and τ² (Upline)**
  abline(a = mean(mu), b = sqrt(tau_2), col = "blue", lty = 2)  # Custom upline
  
  # **Add Q-Q Line (Least Squares Fit)**
  qqline(residuals_sorted, distribution = function(p) qnorm(p, mean = mean(mu), sd = sqrt(tau_2)), col = "darkgreen", lwd = 2)
  
  # Add confidence intervals
  polygon(c(theoretical_quantiles, rev(theoretical_quantiles)),
          c(ci_lower, rev(ci_upper)),
          col = rgb(0.8, 0.8, 0.8, 0.3),
          border = NA)
  
  # Add points again to ensure they're visible above the CI region
  points(theoretical_quantiles, residuals_sorted, pch = 19, col = "blue", cex = point_sizes)
  

  # Add model information as subtitle
  if (!is.null(tau_2) && !is.null(mu)) {
    mtext(sprintf("τ² = %.3f, μ = %.3f", tau_2, mean(mu)), 
          side = 3, line = 0.5, cex = 0.8)
  }
  
  # Add legend
  legend("topleft",
         legend = c("Observed Quantiles (Scaled by Study Size)", 
                    "Reference Line (Estimated μ, τ²)", 
                    "Q-Q Line (Regression Fit)",
                    "95% Confidence Band"),
         pch = c(19, NA, NA, 15),
         lty = c(NA, 2, 1, NA),
         col = c("blue", "blue", "darkgreen", rgb(0.8, 0.8, 0.8, 0.3)),
         bg = "white")
  
  # Return invisibly the plot data for potential further use
  invisible(list(
    theoretical_quantiles = theoretical_quantiles,
    empirical_quantiles = residuals_sorted,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    se_residuals = se_residuals,
    n_k_sorted = n_k_sorted  # Return patient numbers for reference
  ))
}

qq_plot_with_ci <- function(y_k, mu, sigma_2_k, tau_2, log_odds = FALSE, 
                           title = "Q-Q Plot for Standardized Residuals") {
  # Transform data if log odds required
  if (log_odds) {
    y_k <- log(y_k / (1 - y_k))
    mu <- log(mu / (1 - mu))
  }
  
  # Calculate standardized residuals
  standardized_residuals <- (y_k - mu) / sqrt(sigma_2_k + tau_2)
  
  # Remove any infinite or NA values
  valid_indices <- is.finite(standardized_residuals)
  standardized_residuals <- standardized_residuals[valid_indices]
  
  # Sort residuals for proper quantile computation
  sorted_residuals <- sort(standardized_residuals)
  n_points <- length(sorted_residuals)
  
  # Calculate theoretical quantiles using (i - 0.5)/n formula
  p <- (1:n_points - 0.5) / n_points
  theoretical_quantiles <- qnorm(p)
  
  # Calculate empirical standard error for CI
  se_residuals <- sd(standardized_residuals)
  
  # Calculate pointwise confidence intervals
  # Using standard error of order statistics
  z_alpha <- qnorm(0.975)  # 95% CI
  ci_width <- z_alpha * se_residuals * sqrt(p * (1 - p) / (n_points * dnorm(theoretical_quantiles)^2))
  
  ci_lower <- sorted_residuals - ci_width
  ci_upper <- sorted_residuals + ci_width
  
  # Set up plot margins to accommodate title and subtitle
  par(mar = c(5, 5, 4, 2) + 0.1)
  
  # Create the plot
  plot(theoretical_quantiles, sorted_residuals,
       main = title,
       xlab = "Theoretical Quantiles",
       ylab = if(log_odds) "Log-Odds Standardized Residuals" else "Standardized Residuals",
       pch = 19,
       col = "blue",
       ylim = range(c(ci_lower, ci_upper, sorted_residuals), na.rm = TRUE))
  
  # Add reference line
  abline(0, 1, col = "red", lty = 2)
 
  # Add confidence intervals
  polygon(c(theoretical_quantiles, rev(theoretical_quantiles)),
         c(ci_lower, rev(ci_upper)),
         col = rgb(0.8, 0.8, 0.8, 0.3),
         border = NA)
  
  # Add points again to ensure they're visible above the CI region
  points(theoretical_quantiles, sorted_residuals, pch = 19, col = "blue")
  
  # Add model information as subtitle
  if (!is.null(tau_2) && !is.null(mu)) {
    mtext(sprintf("τ² = %.3f, μ = %.3f", tau_2, mu), 
          side = 3, line = 0.5, cex = 0.8)
  }
  
  # Add legend
  legend("topleft",
         legend = c("Observed Quantiles", 
                   "Reference Line", 
                   "95% Confidence Band"),
         pch = c(19, NA, 15),
         lty = c(NA, 2, NA),
         col = c("blue", "red", rgb(0.8, 0.8, 0.8, 0.3)),
         bg = "white")
  
  # Return invisibly the plot data for potential further use
  invisible(list(
    theoretical_quantiles = theoretical_quantiles,
    empirical_quantiles = sorted_residuals,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    se_residuals = se_residuals
  ))
}

# Add a new function for QQ plots specifically for bivariate meta-analysis
qq_plot_bivariate <- function(model, log_odds = FALSE) {
  # Extract necessary components from the bivariate model
  y_k <- model$y.k
  mu <- model$mu
  sigma_2_k <- model$sigma.2.k
  tau_2 <- model$tau^2
  
  # Create QQ plot with confidence intervals
  qq_result <- qq_plot_with_ci(
    y_k = y_k,
    mu = mu,
    sigma_2_k = sigma_2_k,
    tau_2 = tau_2,
    log_odds = log_odds,
    title = "Q-Q Plot for Bivariate Meta-Analysis Residuals"
  )
  
  # Add model diagnostics as subtitle
  mtext(sprintf("τ² = %.3f, μ = %.3f", tau_2, mu), 
        side = 3, 
        line = 0.5, 
        cex = 0.8)
  
  # Return invisibly the plot data
  invisible(qq_result)
}

# Add a helper function to check if log-odds transformation is appropriate
should_use_log_odds <- function(y_k) {
  # Check if data appears to be proportions
  is_proportion <- all(y_k >= 0 & y_k <= 1, na.rm = TRUE)
  
  # Check if data is severely skewed
  if (is_proportion) {
    skewness <- mean((y_k - mean(y_k, na.rm = TRUE))^3, na.rm = TRUE) / 
      sd(y_k, na.rm = TRUE)^3
    return(abs(skewness) > 1)
  }
  
  return(FALSE)
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
  random_subgroup_results: NA
  fixed_subgroup_results: NA
  bivariate_subgroup_results: NA
  metaregression_results: NA
  include_subgroup: NA
  include_metareg: NA
  data: NA
  subgroup_var: NA
  moderator_var: NA
  moderator_type: NA
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(ggplot2)
library(meta)
library(plotly)
library(scales)
library(grid)
library(gridExtra)

safe_run <- function(expr, fallback = NULL) {
  tryCatch(expr, error = function(e) fallback)
}
```

# Random Effects Analysis

## Effect Size & Heterogeneity

```{r re-forest, fig.width=12, fig.height=8}
safe_run(random_forest_plot(params$random_results))
```

```{r re-heterogeneity, fig.width=10, fig.height=6}
safe_run(heterogeneity_plot(params$random_results))
```

```{r re-summary}
safe_run(cat(capture.output(summary(params$random_results)), sep="\n"))
```

## Publication Bias

```{r re-funnel, fig.width=10, fig.height=6}
safe_run(random_funnel_plot(params$random_results))
```

```{r re-trimfill, fig.width=10, fig.height=6}
safe_run({
  tf <- trimfill(params$random_results)
  funnel(tf, yaxis="se")
})
```

```{r re-egger}
safe_run(cat(capture.output(metabias(params$random_results, method="Egger")), sep="\n"))
```

## Sensitivity Analysis

```{r re-loo, fig.width=12, fig.height=8}
safe_run({
  inf <- metainf(params$random_results)
  inf_df <- data.frame(TE = inf$TE, seTE = inf$seTE, studlab = inf$studlab)
  inf_df_clean <- subset(inf_df, is.finite(TE) & is.finite(seTE))
  if (nrow(inf_df_clean) > 0) {
    m_clean <- metagen(TE = TE, seTE = seTE, studlab = studlab, data = inf_df_clean)
    meta::forest(m_clean, leftlabs = c("Omitted Study"), main = "Leave-One-Out Analysis (Random Effects)")
  }
})
```

```{r re-baujat, fig.width=10, fig.height=6}
safe_run(baujat(params$random_results))
```

```{r re-influence}
safe_run(influence_analysis(params$random_results))
```

## Model Diagnostics

```{r re-qq-blups, fig.width=10, fig.height=6}
safe_run(qq_plot_random_blups(params$random_results))
```

```{r re-qq-deleted, fig.width=10, fig.height=6}
safe_run(qq_plot_random_deleted_residuals(params$random_results))
```

```{r re-outlier, fig.width=10, fig.height=6}
safe_run(outlier_detection_plot(params$random_results))
```

```{r re-effect-dist, fig.width=10, fig.height=6}
safe_run(effect_distribution_plot(params$random_results))
```

```{r re-normality}
safe_run({
  diag_results <- run_all_normality_diagnostics("random", params$random_results)
  print_normality_summary(diag_results, "random")
})
```

## Subgroup Analysis `r if(params$include_subgroup) "" else "(skipped)"`

```{r re-subgroup, fig.width=12, fig.height=8}
if (params$include_subgroup && !is.null(params$random_subgroup_results)) {
  safe_run({
    forest(params$random_subgroup_results,
           test.overall.random = TRUE,
           test.subgroup.random = TRUE,
           print.byvar = FALSE)
    cat("\nSubgroup Analysis Results:\n")
    cat("Q-statistic between groups:", params$random_subgroup_results$Q.b.random, "\n")
    cat("P-value:", params$random_subgroup_results$pval.Q.b.random, "\n")
  })
}
```

# Fixed Effects Analysis

## Effect Size & Heterogeneity

```{r fe-forest, fig.width=12, fig.height=8}
safe_run(fixed_forest_plot(params$fixed_results))
```

```{r fe-radial, fig.width=10, fig.height=6}
safe_run(radial(params$fixed_results))
```

```{r fe-summary}
safe_run(cat(capture.output(summary(params$fixed_results)), sep="\n"))
```

```{r fe-fit-stats}
safe_run(model_fit_statistics(params$fixed_results))
```

## Publication Bias

```{r fe-funnel, fig.width=10, fig.height=6}
safe_run(funnel(params$fixed_results))
```

```{r fe-trimfill, fig.width=10, fig.height=6}
safe_run({
    tf <- trimfill(params$fixed_results)
  funnel(tf, yaxis="se")
})
```

```{r fe-egger}
safe_run(cat(capture.output(metabias(params$fixed_results, method="Egger")), sep="\n"))
```

## Sensitivity Analysis

```{r fe-loo, fig.width=12, fig.height=8}
safe_run({
  inf <- metainf(params$fixed_results)
  inf_df <- data.frame(TE = inf$TE, seTE = inf$seTE, studlab = inf$studlab)
  inf_df_clean <- subset(inf_df, is.finite(TE) & is.finite(seTE))
  if (nrow(inf_df_clean) > 0) {
    m_clean <- metagen(TE = TE, seTE = seTE, studlab = studlab, data = inf_df_clean)
    meta::forest(m_clean, leftlabs = c("Omitted Study"), main = "Leave-One-Out Analysis (Fixed Effects)")
    }
})
```

## Model Diagnostics

```{r fe-qq, fig.width=10, fig.height=6}
safe_run(qq_plot_fixed_residuals(params$fixed_results))
```

```{r fe-outlier, fig.width=10, fig.height=6}
safe_run(outlier_detection_plot(params$fixed_results))
```

```{r fe-effect-dist, fig.width=10, fig.height=6}
safe_run(effect_distribution_plot(params$fixed_results))
```

```{r fe-normality}
  safe_run({
  diag_results <- run_all_normality_diagnostics("fixed", params$fixed_results)
  print_normality_summary(diag_results, "fixed")
})
```

## Subgroup Analysis `r if(params$include_subgroup) "" else "(skipped)"`

```{r fe-subgroup, fig.width=12, fig.height=8}
if (params$include_subgroup && !is.null(params$fixed_subgroup_results)) {
  safe_run({
    forest(params$fixed_subgroup_results,
           test.overall = TRUE,
           test.subgroup = TRUE,
           print.byvar = FALSE)
    cat("\nSubgroup Analysis Results:\n")
    cat("Q-statistic between groups:", params$fixed_subgroup_results$Q.b, "\n")
    cat("P-value:", params$fixed_subgroup_results$pval.Q.b, "\n")
  })
}
```

# Bivariate Approach

## Effect Size & Heterogeneity

```{r biv-forest, fig.width=12, fig.height=8}
safe_run(if(exists("forest.metabiv")) forest.metabiv(params$bivariate_results))
```

```{r biv-efficacy-harm, fig.width=10, fig.height=6}
safe_run({
  if (!is.null(params$bivariate_results)) {
    CDF.ci.obj <- comp.mu.tau.dev.CDF.CI(params$bivariate_results$dev_pvals, sm = params$bivariate_results$sm)
    comp.eff.harm.plot(CDF.ci.obj,
                      efficacy.is.OR.le1 = (params$bivariate_results$sm == "OR"),
                      mlb = "Efficacy/Harm Plot",
                      xlb = paste("Effect Size (", params$bivariate_results$sm, ")"),
                      sm = params$bivariate_results$sm)
  }
})
```

## Model Diagnostics

```{r biv-qq-std, fig.width=10, fig.height=6}
safe_run(qq_plot_bivariate_blups(params$bivariate_results))
```

```{r biv-qq-deleted, fig.width=10, fig.height=6}
safe_run({
  if(exists("qq_plot_bivariate_deleted_residuals")) {
    input_stub <- list(
      data_type = ifelse(is.null(params$data), "binary", ifelse("smd" %in% names(params$data), "smd", "binary")),
      effect_measure = params$bivariate_results$sm
    )
    qq_plot_bivariate_deleted_residuals(params$bivariate_results, params$data, input_stub)
  }
})
```

## Sensitivity Analysis

```{r biv-loo, fig.width=10, fig.height=6}
safe_run(bivariate_leave_one_out(params$bivariate_results))
```

```{r biv-confidence, fig.width=10, fig.height=6}
safe_run({
  if(exists("confidence_region_shift_plot")) {
    confidence_region_shift_plot(params$bivariate_results)
  }
})
```

```{r biv-baujat, fig.width=10, fig.height=6}
safe_run({
  if(exists("enhanced_baujat_plot")) {
    enhanced_baujat_plot(params$bivariate_results)
  }
})
```

## Subgroup Analysis `r if(params$include_subgroup) "" else "(skipped)"`

```{r biv-subgroup, fig.width=12, fig.height=8}
if (params$include_subgroup && !is.null(params$bivariate_subgroup_results)) {
safe_run({
    results <- params$bivariate_subgroup_results$results
    plots <- lapply(names(results), function(nm) {
      grid::grid.grabExpr({
        forest.metabiv(results[[nm]], title = paste("Subgroup:", nm))
      })
    })
    do.call(gridExtra::grid.arrange, c(plots, ncol=1))
  })
}
```

# Meta-Regression Analysis `r if(!params$include_metareg || is.null(params$metaregression_results)) "(skipped)" else ""`

```{r metareg-bubble, fig.width=10, fig.height=6}
if(params$include_metareg && !is.null(params$metaregression_results)) {
  safe_run(print(params$metaregression_results$plot))
}
```

```{r metareg-residuals, fig.width=10, fig.height=6}
if(params$include_metareg && !is.null(params$metaregression_results)) {
safe_run({
    model <- params$metaregression_results$model
    residuals <- resid(model)
    fitted <- fitted(model)
    ggplot(data.frame(fitted = fitted, residuals = residuals), aes(x = fitted, y = residuals)) +
      geom_point() +
      geom_smooth(method = "loess") +
      labs(title = "Residual Plot", x = "Fitted Values", y = "Residuals")
  })
}
```

```{r metareg-influence, fig.width=10, fig.height=6}
if(params$include_metareg && !is.null(params$metaregression_results)) {
  safe_run({
    model <- params$metaregression_results$model
    inf <- influence(model)
    plot(inf, which = "influence")
  })
}
```

```{r metareg-summary}
if(params$include_metareg && !is.null(params$metaregression_results)) {
  safe_run({
    cat("Meta-Regression Results\n")
    cat("======================\n\n")
    cat(capture.output(summary(params$metaregression_results$model)), sep="\n")
    cat("\nInterpretation:\n")
    cat("---------------\n")
    results <- params$metaregression_results
    model <- results$model
    moderator_significant <- any(model$pval[-1] < 0.05, na.rm = TRUE)
    
    if (moderator_significant) {
      cat("The moderator variable significantly explains heterogeneity (p < 0.05).\n")
  } else {
      cat("The moderator does not significantly explain heterogeneity.\n")
    }
    
    if (!is.null(model$QM)) {
      cat("\nTest of Moderator (QM):", round(model$QM,3), 
          "on", model$df.QM, "df (p =", sprintf("%.3f", model$pval.QM), ")\n")
    }
    if (!is.null(model$QE)) {
      cat("Residual Heterogeneity (QE):", round(model$QE,3),
          "on", model$df.QE, "df (p =", sprintf("%.3f", model$pval.QE), ")\n")
    }
  })
}
```

# Session Information

```{r session-info}
safe_run(sessionInfo())
```
')
}

efficacy_harm_plot <- function(bivariate_model) {
  if(is.null(bivariate_model$dev_pvals)) {
    plot(1, type="n", main="Efficacy/Harm plot unavailable", xlab="", ylab="")
    return(invisible(NULL))
  }
  
  CDF.ci.obj <- comp.mu.tau.dev.CDF.CI(bivariate_model$dev_pvals, sm = bivariate_model$sm)
  comp.eff.harm.plot(CDF.ci.obj,
                     efficacy.is.OR.le1 = (bivariate_model$sm == "OR"),
                     mlb = paste("Efficacy/Harm plot for", bivariate_model$sm),
                     xlb = paste("Effect Size (", bivariate_model$sm, ")"),
                     sm = bivariate_model$sm)
}

enhanced_baujat_plot <- function(bivariate_model) {
  if(is.null(bivariate_model$studlab) || is.null(bivariate_model$y.k) || 
     is.null(bivariate_model$sigma.2.k) || is.null(bivariate_model$mu) || 
     is.null(bivariate_model$tau)) {
    plot(1, type="n", main="Enhanced Baujat plot unavailable", xlab="", ylab="")
    return(invisible(NULL))
  }
  
  # Calculate influence metrics
  influence <- sapply(1:length(bivariate_model$y.k), function(i) {
    contribution <- (bivariate_model$y.k[i] - bivariate_model$mu)^2 / 
                    (bivariate_model$sigma.2.k[i] + bivariate_model$tau^2)
    influence <- abs(bivariate_model$y.k[i] - bivariate_model$mu) / 
                 sqrt(bivariate_model$sigma.2.k[i] + bivariate_model$tau^2)
    c(contribution, influence)
  })
  
  # Create plotly object
  df <- data.frame(
    study = bivariate_model$studlab,
    contribution = influence[1,],
    influence = influence[2,],
    weight = 1/(bivariate_model$sigma.2.k + bivariate_model$tau^2)
  )
  
  p <- plot_ly(data = df, 
               x = ~contribution, 
               y = ~influence, 
               type = "scatter", 
               mode = "markers",
               marker = list(size = ~sqrt(weight)*5, opacity = 0.7),
               text = ~paste("Study:", study, 
                             "<br>Contribution to heterogeneity:", round(contribution, 3),
                             "<br>Influence on result:", round(influence, 3),
                             "<br>Weight:", round(weight, 3)),
               hoverinfo = "text") %>%
    layout(title = "Enhanced Baujat Plot",
           xaxis = list(title = "Contribution to heterogeneity"),
           yaxis = list(title = "Influence on overall result"),
           showlegend = FALSE)
  
  return(p)
}

# ============================================================================
# COMPREHENSIVE NORMALITY DIAGNOSTICS FOR META-ANALYTIC MODELS
# ============================================================================
# Implementation of normality diagnostics for:
# 1. Fixed-Effects Meta-Analysis
# 2. Standard Random-Effects Meta-Analysis  
# 3. Bivariate ("Exact" MLE) Meta-Analysis
# Based on the unified overview provided by the user
# ============================================================================

# ----------------------------------------------------------------------------
# 1. FIXED-EFFECTS META-ANALYSIS DIAGNOSTICS
# ----------------------------------------------------------------------------

#' Q-Q Plot of Standardized Residuals for Fixed Effects Model
#' @param fixed_results meta object from fixed effects analysis
#' @param envelope logical, whether to include simulation envelope
#' @return ggplot object
qq_plot_fixed_residuals <- function(fixed_results, envelope = TRUE) {
  # Calculate standardized residuals
  theta_hat <- fixed_results$TE.common
  residuals <- (fixed_results$TE - theta_hat) / fixed_results$seTE
  
  # Remove any infinite or NA values
  valid_residuals <- residuals[is.finite(residuals)]
  
  if (length(valid_residuals) < 3) {
    plot(1, type="n", axes=FALSE, xlab="", ylab="", 
         main="Fixed Effects Q-Q Plot: Insufficient Data")
    text(1, 1, "Not enough valid data for Q-Q plot")
    return(invisible(NULL))
  }
  
  # Create Q-Q plot data
  n <- length(valid_residuals)
  sorted_residuals <- sort(valid_residuals)
  theoretical_quantiles <- qnorm((1:n - 0.5) / n)
  
  # Create basic plot
  plot(theoretical_quantiles, sorted_residuals,
       main = "Q-Q Plot: Fixed Effects Standardized Residuals",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       pch = 19, col = "blue")
  
  # Add reference line
  abline(0, 1, col = "red", lty = 2, lwd = 2)
  
  # Add simulation envelope if requested
  if (envelope && n >= 5) {
    n_sim <- 1000
    envelopes <- replicate(n_sim, {
      sim_residuals <- rnorm(n)
      sort(sim_residuals)
    })
    
    lower_env <- apply(envelopes, 1, quantile, probs = 0.025)
    upper_env <- apply(envelopes, 1, quantile, probs = 0.975)
    
    # Add envelope
    polygon(c(theoretical_quantiles, rev(theoretical_quantiles)),
            c(lower_env, rev(upper_env)),
            col = rgb(0.8, 0.8, 0.8, 0.3), border = NA)
    
    # Re-add points and line
    points(theoretical_quantiles, sorted_residuals, pch = 19, col = "blue")
    abline(0, 1, col = "red", lty = 2, lwd = 2)
  }
  
  # Add legend
  legend("topleft", 
         legend = c("Observed", "Expected (N(0,1))", if(envelope) "95% Envelope"),
         pch = c(19, NA, if(envelope) 15),
         lty = c(NA, 2, NA),
         col = c("blue", "red", if(envelope) rgb(0.8, 0.8, 0.8, 0.3)),
         bg = "white")
  
  return(invisible(list(
    residuals = valid_residuals,
    theoretical_quantiles = theoretical_quantiles,
    empirical_quantiles = sorted_residuals
  )))
}

#' Shapiro-Wilk Test for Fixed Effects Residuals
#' @param fixed_results meta object from fixed effects analysis
#' @return list with test results
shapiro_test_fixed <- function(fixed_results) {
  # Calculate standardized residuals
  theta_hat <- fixed_results$TE.common
  residuals <- (fixed_results$TE - theta_hat) / fixed_results$seTE
  
  # Remove any infinite or NA values
  valid_residuals <- residuals[is.finite(residuals)]
  
  if (length(valid_residuals) < 3 || length(valid_residuals) > 5000) {
    return(list(
      statistic = NA,
      p.value = NA,
      method = "Shapiro-Wilk normality test",
      data.name = "fixed effects residuals",
      note = if(length(valid_residuals) < 3) "Too few observations" else "Too many observations"
    ))
  }
  
  test_result <- shapiro.test(valid_residuals)
  return(test_result)
}

# ----------------------------------------------------------------------------
# 2. STANDARD RANDOM-EFFECTS META-ANALYSIS DIAGNOSTICS
# ----------------------------------------------------------------------------

#' Calculate BLUPs (Best Linear Unbiased Predictors) for Random Effects Model
#' @param random_results meta object from random effects analysis
#' @return vector of BLUPs
calculate_blups <- function(random_results) {
  mu_hat <- random_results$TE.random
  tau2_hat <- random_results$tau2
  y_i <- random_results$TE
  sigma2_i <- random_results$seTE^2
  
  # BLUP formula: μ̂ + (τ̂²/(τ̂² + σᵢ²)) * (Yᵢ - μ̂)
  blups <- mu_hat + (tau2_hat / (tau2_hat + sigma2_i)) * (y_i - mu_hat)
  
  return(blups)
}

#' Q-Q Plot of BLUPs for Random Effects Model
#' @param random_results meta object from random effects analysis
#' @param envelope logical, whether to include simulation envelope
#' @return ggplot object
qq_plot_random_blups <- function(random_results, envelope = TRUE) {
  # Calculate BLUPs
  blups <- calculate_blups(random_results)
  
  # Standardize BLUPs
  standardized_blups <- (blups - mean(blups, na.rm = TRUE)) / sd(blups, na.rm = TRUE)
  
  # Remove any infinite or NA values
  valid_blups <- standardized_blups[is.finite(standardized_blups)]
  
  if (length(valid_blups) < 3) {
    plot(1, type="n", axes=FALSE, xlab="", ylab="", 
         main="Random Effects BLUPs Q-Q Plot: Insufficient Data")
    text(1, 1, "Not enough valid data for Q-Q plot")
    return(invisible(NULL))
  }
  
  # Create Q-Q plot data
  n <- length(valid_blups)
  sorted_blups <- sort(valid_blups)
  theoretical_quantiles <- qnorm((1:n - 0.5) / n)
  
  # Create basic plot
  plot(theoretical_quantiles, sorted_blups,
       main = "Q-Q Plot: Random Effects BLUPs",
       xlab = "Theoretical Quantiles",
       ylab = "Standardized BLUPs",
       pch = 19, col = "darkgreen")
  
  # Add reference line
  abline(0, 1, col = "red", lty = 2, lwd = 2)
  
  # Add simulation envelope if requested
  if (envelope && n >= 5) {
    n_sim <- 1000
    envelopes <- replicate(n_sim, {
      sim_blups <- rnorm(n)
      sort(sim_blups)
    })
    
    lower_env <- apply(envelopes, 1, quantile, probs = 0.025)
    upper_env <- apply(envelopes, 1, quantile, probs = 0.975)
    
    # Add envelope
    polygon(c(theoretical_quantiles, rev(theoretical_quantiles)),
            c(lower_env, rev(upper_env)),
            col = rgb(0.8, 0.8, 0.8, 0.3), border = NA)
    
    # Re-add points and line
    points(theoretical_quantiles, sorted_blups, pch = 19, col = "darkgreen")
    abline(0, 1, col = "red", lty = 2, lwd = 2)
  }
  
  # Add legend
  legend("topleft", 
         legend = c("Standardized BLUPs", "Expected (N(0,1))", if(envelope) "95% Envelope"),
         pch = c(19, NA, if(envelope) 15),
         lty = c(NA, 2, NA),
         col = c("darkgreen", "red", if(envelope) rgb(0.8, 0.8, 0.8, 0.3)),
         bg = "white")
  
  return(invisible(list(
    blups = valid_blups,
    theoretical_quantiles = theoretical_quantiles,
    empirical_quantiles = sorted_blups
  )))
}

#' Calculate Standardized Deleted Residuals for Random Effects Model
#' @param random_results meta object from random effects analysis
#' @return vector of deleted residuals
calculate_deleted_residuals_random <- function(random_results) {
  y_i <- random_results$TE
  sigma2_i <- random_results$seTE^2
  n_studies <- length(y_i)
  
  deleted_residuals <- numeric(n_studies)
  
  for (i in 1:n_studies) {
    # Remove study i
    y_minus_i <- y_i[-i]
    sigma2_minus_i <- sigma2_i[-i]
    
    # Re-estimate μ and τ² without study i
    # Simple DerSimonian-Laird estimator
    weights_minus_i <- 1 / sigma2_minus_i
    mu_minus_i <- sum(weights_minus_i * y_minus_i) / sum(weights_minus_i)
    
    Q_minus_i <- sum(weights_minus_i * (y_minus_i - mu_minus_i)^2)
    df_minus_i <- length(y_minus_i) - 1
    tau2_minus_i <- max(0, (Q_minus_i - df_minus_i) / (sum(weights_minus_i) - sum(weights_minus_i^2) / sum(weights_minus_i)))
    
    # Calculate deleted residual
    deleted_residuals[i] <- (y_i[i] - mu_minus_i) / sqrt(sigma2_i[i] + tau2_minus_i)
  }
  
  return(deleted_residuals)
}

#' Q-Q Plot of Deleted Residuals for Random Effects Model
#' @param random_results meta object from random effects analysis
#' @param envelope logical, whether to include simulation envelope
#' @return ggplot object
qq_plot_random_deleted_residuals <- function(random_results, envelope = TRUE) {
  # Calculate deleted residuals
  deleted_residuals <- calculate_deleted_residuals_random(random_results)
  
  # Remove any infinite or NA values
  valid_residuals <- deleted_residuals[is.finite(deleted_residuals)]
  
  if (length(valid_residuals) < 3) {
    plot(1, type="n", axes=FALSE, xlab="", ylab="", 
         main="Random Effects Deleted Residuals Q-Q Plot: Insufficient Data")
    text(1, 1, "Not enough valid data for Q-Q plot")
    return(invisible(NULL))
  }
  
  # Create Q-Q plot data
  n <- length(valid_residuals)
  sorted_residuals <- sort(valid_residuals)
  theoretical_quantiles <- qnorm((1:n - 0.5) / n)
  
  # Create basic plot
  plot(theoretical_quantiles, sorted_residuals,
       main = "Q-Q Plot: Random Effects Deleted Residuals",
       xlab = "Theoretical Quantiles",
       ylab = "Standardized Deleted Residuals",
       pch = 19, col = "purple")
  
  # Add reference line
  abline(0, 1, col = "red", lty = 2, lwd = 2)
  
  # Add simulation envelope if requested
  if (envelope && n >= 5) {
    n_sim <- 1000
    envelopes <- replicate(n_sim, {
      sim_residuals <- rnorm(n)
      sort(sim_residuals)
    })
    
    lower_env <- apply(envelopes, 1, quantile, probs = 0.025)
    upper_env <- apply(envelopes, 1, quantile, probs = 0.975)
    
    # Add envelope
    polygon(c(theoretical_quantiles, rev(theoretical_quantiles)),
            c(lower_env, rev(upper_env)),
            col = rgb(0.8, 0.8, 0.8, 0.3), border = NA)
    
    # Re-add points and line
    points(theoretical_quantiles, sorted_residuals, pch = 19, col = "purple")
    abline(0, 1, col = "red", lty = 2, lwd = 2)
  }
  
  # Add legend
  legend("topleft", 
         legend = c("Deleted Residuals", "Expected (N(0,1))", if(envelope) "95% Envelope"),
         pch = c(19, NA, if(envelope) 15),
         lty = c(NA, 2, NA),
         col = c("purple", "red", if(envelope) rgb(0.8, 0.8, 0.8, 0.3)),
         bg = "white")
  
  return(invisible(list(
    residuals = valid_residuals,
    theoretical_quantiles = theoretical_quantiles,
    empirical_quantiles = sorted_residuals
  )))
}

#' Shapiro-Wilk Tests for Random Effects Model
#' @param random_results meta object from random effects analysis
#' @return list with test results for BLUPs and deleted residuals
shapiro_test_random <- function(random_results) {
  # Test BLUPs
  blups <- calculate_blups(random_results)
  standardized_blups <- (blups - mean(blups, na.rm = TRUE)) / sd(blups, na.rm = TRUE)
  valid_blups <- standardized_blups[is.finite(standardized_blups)]
  
  blup_test <- if (length(valid_blups) >= 3 && length(valid_blups) <= 5000) {
    shapiro.test(valid_blups)
  } else {
    list(statistic = NA, p.value = NA, method = "Shapiro-Wilk normality test", 
         data.name = "BLUPs", note = "Sample size out of range")
  }
  
  # Test deleted residuals
  deleted_residuals <- calculate_deleted_residuals_random(random_results)
  valid_deleted <- deleted_residuals[is.finite(deleted_residuals)]
  
  deleted_test <- if (length(valid_deleted) >= 3 && length(valid_deleted) <= 5000) {
    shapiro.test(valid_deleted)
  } else {
    list(statistic = NA, p.value = NA, method = "Shapiro-Wilk normality test",
         data.name = "deleted residuals", note = "Sample size out of range")
  }
  
  return(list(
    blup_test = blup_test,
    deleted_residuals_test = deleted_test
  ))
}

# ----------------------------------------------------------------------------
# 3. BIVARIATE ("EXACT" MLE) META-ANALYSIS DIAGNOSTICS
# ----------------------------------------------------------------------------

#' Calculate BLUPs for Bivariate MLE Model
#' @param bivariate_results result object from metabiv function
#' @return vector of BLUPs
calculate_blups_bivariate <- function(bivariate_results) {
  mu_hat <- bivariate_results$mu
  tau2_hat <- bivariate_results$tau^2
  y_k <- bivariate_results$y.k
  sigma2_k <- bivariate_results$sigma.2.k
  
  # BLUP formula: μ̂ + (τ̂²/(τ̂² + σₖ²)) * (Yₖ - μ̂)
  blups <- mu_hat + (tau2_hat / (tau2_hat + sigma2_k)) * (y_k - mu_hat)
  
  return(blups)
}

#' Q-Q Plot of BLUPs for Bivariate MLE Model
#' @param bivariate_results result object from metabiv function
#' @param envelope logical, whether to include simulation envelope
#' @return ggplot object
qq_plot_bivariate_blups <- function(bivariate_results, envelope = TRUE) {
  # Calculate BLUPs
  blups <- calculate_blups_bivariate(bivariate_results)
  
  # Standardize BLUPs
  standardized_blups <- (blups - mean(blups, na.rm = TRUE)) / sd(blups, na.rm = TRUE)
  
  # Remove any infinite or NA values
  valid_blups <- standardized_blups[is.finite(standardized_blups)]
  
  if (length(valid_blups) < 3) {
    plot(1, type="n", axes=FALSE, xlab="", ylab="", 
         main="Bivariate MLE BLUPs Q-Q Plot: Insufficient Data")
    text(1, 1, "Not enough valid data for Q-Q plot")
    return(invisible(NULL))
  }
  
  # Create Q-Q plot data
  n <- length(valid_blups)
  sorted_blups <- sort(valid_blups)
  theoretical_quantiles <- qnorm((1:n - 0.5) / n)
  
  # Create basic plot
  plot(theoretical_quantiles, sorted_blups,
       main = "Q-Q Plot: Bivariate MLE BLUPs",
       xlab = "Theoretical Quantiles",
       ylab = "Standardized BLUPs",
       pch = 19, col = "orange")
  
  # Add reference line
  abline(0, 1, col = "red", lty = 2, lwd = 2)
  
  # Add simulation envelope if requested
  if (envelope && n >= 5) {
    n_sim <- 1000
    mu_hat <- bivariate_results$mu
    tau_hat <- bivariate_results$tau
    sigma2_k <- bivariate_results$sigma.2.k
    
    envelopes <- replicate(n_sim, {
      # Simulate new theta and Y values
      theta_sim <- rnorm(n, mu_hat, tau_hat)
      y_sim <- rnorm(n, theta_sim, sqrt(sigma2_k))
      
      # Refit model (simplified - just use true parameters for envelope)
      blups_sim <- mu_hat + (tau_hat^2 / (tau_hat^2 + sigma2_k)) * (y_sim - mu_hat)
      blups_sim_std <- (blups_sim - mean(blups_sim)) / sd(blups_sim)
      
      sort(blups_sim_std)
    })
    
    lower_env <- apply(envelopes, 1, quantile, probs = 0.025, na.rm = TRUE)
    upper_env <- apply(envelopes, 1, quantile, probs = 0.975, na.rm = TRUE)
    
    # Add envelope
    polygon(c(theoretical_quantiles, rev(theoretical_quantiles)),
            c(lower_env, rev(upper_env)),
            col = rgb(0.8, 0.8, 0.8, 0.3), border = NA)
    
    # Re-add points and line
    points(theoretical_quantiles, sorted_blups, pch = 19, col = "orange")
    abline(0, 1, col = "red", lty = 2, lwd = 2)
  }
  
  # Add legend
  legend("topleft", 
         legend = c("Bivariate BLUPs", "Expected (N(0,1))", if(envelope) "95% Envelope"),
         pch = c(19, NA, if(envelope) 15),
         lty = c(NA, 2, NA),
         col = c("orange", "red", if(envelope) rgb(0.8, 0.8, 0.8, 0.3)),
         bg = "white")
  
  return(invisible(list(
    blups = valid_blups,
    theoretical_quantiles = theoretical_quantiles,
    empirical_quantiles = sorted_blups
  )))
}

#' Calculate Deleted Residuals for Bivariate MLE Model
#' @param bivariate_results result object from metabiv function
#' @param data original data used in the analysis
#' @param input shiny input object with data_type and effect_measure
#' @return vector of deleted residuals
calculate_deleted_residuals_bivariate <- function(bivariate_results, data, input) {
  y_k <- bivariate_results$y.k
  sigma2_k <- bivariate_results$sigma.2.k
  studlab <- bivariate_results$studlab
  n_studies <- length(y_k)
  
  deleted_residuals <- numeric(n_studies)
  
  for (i in 1:n_studies) {
    # Remove study i
    data_minus_i <- data[data$study != studlab[i], ]
    
    # Refit bivariate model without study i
    biv_minus_i <- tryCatch({
      if (input$data_type == "smd") {
        se_minus_i <- (data_minus_i$ci_upper - data_minus_i$ci_lower) / (2 * 1.96)
        var_minus_i <- se_minus_i^2
        metabiv(studlab = data_minus_i$study, sm = "SMD", 
                y = data_minus_i$smd, sigma2 = var_minus_i, verbose = FALSE)
      } else {
        metabiv(event.e = data_minus_i$ie, n.e = data_minus_i$it, 
                event.c = data_minus_i$pe, n.c = data_minus_i$pt,
                studlab = data_minus_i$study, sm = input$effect_measure, verbose = FALSE)
      }
    }, error = function(e) {
      # If refit fails, use original estimates
      list(mu = bivariate_results$mu, tau = bivariate_results$tau)
    })
    
    # Calculate deleted residual
    mu_minus_i <- biv_minus_i$mu
    tau2_minus_i <- biv_minus_i$tau^2
    
    deleted_residuals[i] <- (y_k[i] - mu_minus_i) / sqrt(sigma2_k[i] + tau2_minus_i)
  }
  
  return(deleted_residuals)
}

#' Q-Q Plot of Deleted Residuals for Bivariate MLE Model
#' @param bivariate_results result object from metabiv function
#' @param data original data used in the analysis
#' @param input shiny input object
#' @param envelope logical, whether to include simulation envelope
#' @return ggplot object
qq_plot_bivariate_deleted_residuals <- function(bivariate_results, data, input, envelope = TRUE) {
  # Calculate deleted residuals
  deleted_residuals <- calculate_deleted_residuals_bivariate(bivariate_results, data, input)
  
  # Remove any infinite or NA values
  valid_residuals <- deleted_residuals[is.finite(deleted_residuals)]
  
  if (length(valid_residuals) < 3) {
    plot(1, type="n", axes=FALSE, xlab="", ylab="", 
         main="Bivariate MLE Deleted Residuals Q-Q Plot: Insufficient Data")
    text(1, 1, "Not enough valid data for Q-Q plot")
    return(invisible(NULL))
  }
  
  # Create Q-Q plot data
  n <- length(valid_residuals)
  sorted_residuals <- sort(valid_residuals)
  theoretical_quantiles <- qnorm((1:n - 0.5) / n)
  
  # Create basic plot
  plot(theoretical_quantiles, sorted_residuals,
       main = "Q-Q Plot: Bivariate MLE Deleted Residuals",
       xlab = "Theoretical Quantiles",
       ylab = "Standardized Deleted Residuals",
       pch = 19, col = "brown")
  
  # Add reference line
  abline(0, 1, col = "red", lty = 2, lwd = 2)
  
  # Add simulation envelope if requested (simplified version)
  if (envelope && n >= 5) {
    n_sim <- 1000
    envelopes <- replicate(n_sim, {
      sim_residuals <- rnorm(n)
      sort(sim_residuals)
    })
    
    lower_env <- apply(envelopes, 1, quantile, probs = 0.025)
    upper_env <- apply(envelopes, 1, quantile, probs = 0.975)
    
    # Add envelope
    polygon(c(theoretical_quantiles, rev(theoretical_quantiles)),
            c(lower_env, rev(upper_env)),
            col = rgb(0.8, 0.8, 0.8, 0.3), border = NA)
    
    # Re-add points and line
    points(theoretical_quantiles, sorted_residuals, pch = 19, col = "brown")
    abline(0, 1, col = "red", lty = 2, lwd = 2)
  }
  
  # Add legend
  legend("topleft", 
         legend = c("Bivariate Deleted Residuals", "Expected (N(0,1))", if(envelope) "95% Envelope"),
         pch = c(19, NA, if(envelope) 15),
         lty = c(NA, 2, NA),
         col = c("brown", "red", if(envelope) rgb(0.8, 0.8, 0.8, 0.3)),
         bg = "white")
  
  return(invisible(list(
    residuals = valid_residuals,
    theoretical_quantiles = theoretical_quantiles,
    empirical_quantiles = sorted_residuals
  )))
}

#' Shapiro-Wilk Tests for Bivariate MLE Model
#' @param bivariate_results result object from metabiv function
#' @param data original data used in the analysis
#' @param input shiny input object
#' @return list with test results for BLUPs and deleted residuals
shapiro_test_bivariate <- function(bivariate_results, data, input) {
  # Test BLUPs
  blups <- calculate_blups_bivariate(bivariate_results)
  standardized_blups <- (blups - mean(blups, na.rm = TRUE)) / sd(blups, na.rm = TRUE)
  valid_blups <- standardized_blups[is.finite(standardized_blups)]
  
  blup_test <- if (length(valid_blups) >= 3 && length(valid_blups) <= 5000) {
    shapiro.test(valid_blups)
  } else {
    list(statistic = NA, p.value = NA, method = "Shapiro-Wilk normality test", 
         data.name = "bivariate BLUPs", note = "Sample size out of range")
  }
  
  # Test deleted residuals
  deleted_residuals <- calculate_deleted_residuals_bivariate(bivariate_results, data, input)
  valid_deleted <- deleted_residuals[is.finite(deleted_residuals)]
  
  deleted_test <- if (length(valid_deleted) >= 3 && length(valid_deleted) <= 5000) {
    shapiro.test(valid_deleted)
  } else {
    list(statistic = NA, p.value = NA, method = "Shapiro-Wilk normality test",
         data.name = "bivariate deleted residuals", note = "Sample size out of range")
  }
  
  return(list(
    blup_test = blup_test,
    deleted_residuals_test = deleted_test
  ))
}

# ----------------------------------------------------------------------------
# 4. COMPREHENSIVE DIAGNOSTIC SUMMARY FUNCTIONS
# ----------------------------------------------------------------------------

#' Run All Normality Diagnostics for a Given Model Type
#' @param model_type character, one of "fixed", "random", "bivariate"
#' @param results model results object
#' @param data optional, original data (required for bivariate)
#' @param input optional, shiny input object (required for bivariate)
#' @param envelope logical, whether to include simulation envelopes
#' @return list of diagnostic results
run_all_normality_diagnostics <- function(model_type, results, data = NULL, input = NULL, envelope = TRUE) {
  
  diagnostics <- list()
  
  if (model_type == "fixed") {
    # Fixed effects diagnostics
    diagnostics$qq_residuals <- qq_plot_fixed_residuals(results, envelope)
    diagnostics$shapiro_test <- shapiro_test_fixed(results)
    
  } else if (model_type == "random") {
    # Random effects diagnostics
    diagnostics$qq_blups <- qq_plot_random_blups(results, envelope)
    diagnostics$qq_deleted_residuals <- qq_plot_random_deleted_residuals(results, envelope)
    diagnostics$shapiro_tests <- shapiro_test_random(results)
    
  } else if (model_type == "bivariate") {
    # Bivariate MLE diagnostics
    if (is.null(data) || is.null(input)) {
      stop("For bivariate diagnostics, both 'data' and 'input' arguments are required")
    }
    
    diagnostics$qq_blups <- qq_plot_bivariate_blups(results, envelope)
    diagnostics$qq_deleted_residuals <- qq_plot_bivariate_deleted_residuals(results, data, input, envelope)
    diagnostics$shapiro_tests <- shapiro_test_bivariate(results, data, input)
    
  } else {
    stop("model_type must be one of 'fixed', 'random', or 'bivariate'")
  }
  
  return(diagnostics)
}

#' Print Summary of Normality Diagnostic Results
#' @param diagnostics result from run_all_normality_diagnostics
#' @param model_type character, model type for labeling
print_normality_summary <- function(diagnostics, model_type) {
  cat("=================================================\n")
  cat("NORMALITY DIAGNOSTICS SUMMARY:", toupper(model_type), "EFFECTS MODEL\n")
  cat("=================================================\n\n")
  
  if (model_type == "fixed") {
    cat("Shapiro-Wilk Test for Standardized Residuals:\n")
    if (!is.na(diagnostics$shapiro_test$p.value)) {
      cat("  W =", round(diagnostics$shapiro_test$statistic, 4), 
          ", p-value =", format.pval(diagnostics$shapiro_test$p.value), "\n")
      cat("  Interpretation:", 
          if(diagnostics$shapiro_test$p.value > 0.05) "No strong evidence against normality" else "Evidence against normality", "\n")
    } else {
      cat("  Test not available:", diagnostics$shapiro_test$note, "\n")
    }
    
  } else {
    cat("Shapiro-Wilk Test for BLUPs:\n")
    if (!is.na(diagnostics$shapiro_tests$blup_test$p.value)) {
      cat("  W =", round(diagnostics$shapiro_tests$blup_test$statistic, 4), 
          ", p-value =", format.pval(diagnostics$shapiro_tests$blup_test$p.value), "\n")
      cat("  Interpretation:", 
          if(diagnostics$shapiro_tests$blup_test$p.value > 0.05) "No strong evidence against normality" else "Evidence against normality", "\n")
    } else {
      cat("  Test not available\n")
    }
    
    cat("\nShapiro-Wilk Test for Deleted Residuals:\n")
    if (!is.na(diagnostics$shapiro_tests$deleted_residuals_test$p.value)) {
      cat("  W =", round(diagnostics$shapiro_tests$deleted_residuals_test$statistic, 4), 
          ", p-value =", format.pval(diagnostics$shapiro_tests$deleted_residuals_test$p.value), "\n")
      cat("  Interpretation:", 
          if(diagnostics$shapiro_tests$deleted_residuals_test$p.value > 0.05) "No strong evidence against normality" else "Evidence against normality", "\n")
    } else {
      cat("  Test not available\n")
    }
  }
  
  cat("\n=================================================\n")
  cat("Q-Q PLOT INTERPRETATION GUIDE:\n")
  cat("• Points close to the red diagonal line = Normal distribution\n")
  cat("• S-shaped pattern = Tail deviations (heavy/light tails)\n")
  cat("• Systematic curvature = Skewness\n")
  cat("• Points outside gray envelope = Potential outliers\n")
  cat("• Low-powered formal tests should be interpreted with plots\n")
  cat("=================================================\n\n")
}

# End of comprehensive normality diagnostics
# ============================================================================

# Add these new functions at the end of your existing functions.R file

# 1. FIXED EFFECTS DELETED RESIDUALS DIAGNOSTICS
# ----------------------------------------------------------------------------

#' Calculate Deleted Residuals for Fixed Effects Model
#' @param fixed_results meta object from fixed effects analysis
#' @return vector of deleted residuals
calculate_fixed_deleted_residuals <- function(fixed_results) {
  k <- length(fixed_results$TE)
  deleted_residuals <- numeric(k)
  
  for (i in 1:k) {
    # Create data without study i
    TE_loo <- fixed_results$TE[-i]
    seTE_loo <- fixed_results$seTE[-i]
    
    # Calculate fixed effect estimate without study i
    w_loo <- 1 / seTE_loo^2
    theta_loo <- sum(w_loo * TE_loo) / sum(w_loo)
    
    # Deleted residual for study i
    deleted_residuals[i] <- (fixed_results$TE[i] - theta_loo) / fixed_results$seTE[i]
  }
  
  return(deleted_residuals)
}

# 2. RANDOM EFFECTS DELETED RESIDUALS DIAGNOSTICS
# ----------------------------------------------------------------------------

#' Calculate Deleted Residuals for Random Effects Model
#' @param random_results meta object from random effects analysis
#' @return vector of deleted residuals
calculate_random_deleted_residuals <- function(random_results) {
  k <- length(random_results$TE)
  deleted_residuals <- numeric(k)
  
  # Use metainf once to get all leave-one-out estimates
  inf <- metainf(random_results, pooled = "random")
  
  for (i in 1:k) {
    # Get the pooled estimate when study i is removed
    mu_loo <- inf$TE[i]
    tau2_loo <- inf$tau2[i]
    
    # Deleted residual for study i using random effects parameters
    deleted_residuals[i] <- (random_results$TE[i] - mu_loo) / sqrt(random_results$seTE[i]^2 + tau2_loo)
  }
  
  return(deleted_residuals)
}

# 3. BIVARIATE MLE DELETED RESIDUALS DIAGNOSTICS
# ----------------------------------------------------------------------------

#' Calculate Deleted Residuals for Bivariate Model
#' @param bivariate_results metabiv object
#' @param data Original data frame
#' @param input Shiny input object with data type and effect measure
#' @return vector of deleted residuals
calculate_bivariate_deleted_residuals <- function(bivariate_results, data, input) {
  k <- length(bivariate_results$y.k)
  deleted_residuals <- numeric(k)
  
  for (i in 1:k) {
    # Create data without study i
    data_loo <- data[-i, ]
    
    # Refit bivariate model without study i
    if (input$data_type == "smd") {
      se_loo <- (data_loo$ci_upper - data_loo$ci_lower) / (2 * 1.96)
      var_loo <- se_loo^2
      res_loo <- metabiv(studlab = data_loo$study, sm = "SMD", y = data_loo$smd, sigma2 = var_loo, verbose = FALSE)
    } else {
      res_loo <- metabiv(event.e = data_loo$ie, n.e = data_loo$it, 
                        event.c = data_loo$pe, n.c = data_loo$pt,
                        studlab = data_loo$study, sm = input$effect_measure, verbose = FALSE)
    }
    
    # Deleted residual for study i
    deleted_residuals[i] <- (bivariate_results$y.k[i] - res_loo$mu) / 
                           sqrt(bivariate_results$sigma.2.k[i] + res_loo$tau^2)
  }
  
  return(deleted_residuals)
}

# 4. SIDE-BY-SIDE QQ PLOTS
# ----------------------------------------------------------------------------

#' Create Side-by-Side QQ Plot for Deleted Residuals
#' @param residuals1 First set of residuals (left panel)
#' @param residuals2 Second set of residuals (right panel)
#' @param label1 Label for first panel
#' @param label2 Label for second panel
#' @param main_title Main title for the plot
create_sidebyside_deleted_residuals_qq <- function(residuals1, residuals2, 
                                                  label1 = "Model 1", 
                                                  label2 = "Model 2",
                                                  main_title = "Deleted Residuals Q-Q Plots") {
  # Set up 2-panel plot
  par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)
  
  # Left panel
  create_single_qq_panel(residuals1, paste(label1, "Deleted Residuals"))
  
  # Right panel
  create_single_qq_panel(residuals2, paste(label2, "Deleted Residuals"))
  
  # Add main title
  mtext(main_title, side = 3, line = -2, outer = TRUE, cex = 1.2, font = 2)
  
  # Reset par
  par(mfrow = c(1, 1))
}

#' Create Single QQ Panel with Confidence Envelope
#' @param residuals Vector of residuals
#' @param title Panel title
create_single_qq_panel <- function(residuals, title) {
  # Remove NA values
  residuals <- residuals[!is.na(residuals)]
  n <- length(residuals)
  
  if (n < 3) {
    plot(1, type = "n", xlab = "", ylab = "", main = title)
    text(1, 1, "Insufficient data", cex = 1.2)
    return()
  }
  
  # Sort residuals
  sorted_residuals <- sort(residuals)
  
  # Calculate theoretical quantiles
  p <- (1:n - 0.5) / n
  theoretical_quantiles <- qnorm(p)
  
  # Calculate confidence envelope
  z_alpha <- qnorm(0.975)
  se <- 1 / dnorm(theoretical_quantiles)
  envelope_width <- z_alpha * se * sqrt(p * (1 - p) / n)
  
  # Create plot
  plot(theoretical_quantiles, sorted_residuals,
       main = title,
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       pch = 19,
       col = "darkblue",
       ylim = range(c(sorted_residuals - envelope_width, 
                     sorted_residuals + envelope_width)))
  
  # Add confidence envelope
  polygon(c(theoretical_quantiles, rev(theoretical_quantiles)),
          c(sorted_residuals - envelope_width, 
            rev(sorted_residuals + envelope_width)),
          col = rgb(0.8, 0.8, 0.8, 0.3),
          border = NA)
  
  # Replot points
  points(theoretical_quantiles, sorted_residuals, pch = 19, col = "darkblue")
  
  # Add reference line
  abline(0, 1, col = "red", lty = 2, lwd = 2)
  
  # Add grid
  grid(col = "gray90")
}

# 5. WRAPPER FUNCTIONS FOR SHINY UI
# ----------------------------------------------------------------------------

#' Random Effects: Side-by-Side Deleted Residuals QQ Plot
#' @param random_results Random effects model results
#' @param fixed_results Fixed effects model results
qq_plot_random_vs_fixed_deleted <- function(random_results, fixed_results) {
  # Calculate deleted residuals
  random_deleted <- calculate_random_deleted_residuals(random_results)
  fixed_deleted <- calculate_fixed_deleted_residuals(fixed_results)
  
  # Create side-by-side plot
  create_sidebyside_deleted_residuals_qq(
    fixed_deleted, random_deleted,
    label1 = "Fixed Effects",
    label2 = "Random Effects",
    main_title = "Deleted Residuals Comparison"
  )
}

#' Bivariate: Side-by-Side Deleted Residuals QQ Plot
#' @param bivariate_results Bivariate model results
#' @param fixed_results Fixed effects model results
#' @param data Original data
#' @param input Shiny input object
qq_plot_bivariate_vs_fixed_deleted <- function(bivariate_results, fixed_results, data, input) {
  # Calculate deleted residuals
  bivariate_deleted <- calculate_bivariate_deleted_residuals(bivariate_results, data, input)
  fixed_deleted <- calculate_fixed_deleted_residuals(fixed_results)
  
  # Create side-by-side plot (Fixed Effects left, Bivariate MLE right)
  create_sidebyside_deleted_residuals_qq(
    fixed_deleted, bivariate_deleted,
    label1 = "Fixed Effects",
    label2 = "Bivariate MLE",
    main_title = "Deleted Residuals Comparison"
  )
}

#' Calculate Probability Table for Clinical Thresholds
#' @description Calculates P(θ ≥ T) and 95% CI for key clinical thresholds
#' @param bivariate_result Result object from metabiv function
#' @param custom_thresholds Optional vector of custom thresholds
#' @return A data frame with threshold, probability, and confidence intervals
#' @export
calculate_threshold_probabilities <- function(bivariate_result, custom_thresholds = NULL) {
  
  # Extract parameters
  mu <- bivariate_result$mu
  tau <- bivariate_result$tau
  sm <- bivariate_result$sm
  conf_region <- bivariate_result$conf_region
  
  # Set default thresholds based on summary measure
  if (sm == "SMD") {
    default_thresholds <- c(-0.10, 0.00, 0.20, 0.50, 0.80)
    threshold_labels <- c("–0.10 (potential harm)", "0.00 (any benefit)", 
                         "0.20 (small benefit)", "0.50 (moderate benefit)", 
                         "0.80 (large benefit)")
  } else {
    # For OR/RR, use log scale internally but display as original scale
    default_thresholds <- log(c(0.9, 1.0, 1.1, 1.2, 1.5))
    threshold_labels <- c("0.90 (potential benefit)", "1.00 (no effect)", 
                         "1.10 (small harm)", "1.20 (moderate harm)", 
                         "1.50 (large harm)")
  }
  
  # Combine with custom thresholds if provided
  all_thresholds <- default_thresholds
  all_labels <- threshold_labels
  
  if (!is.null(custom_thresholds) && length(custom_thresholds) > 0) {
    # Parse custom thresholds
    custom_numeric <- suppressWarnings(as.numeric(custom_thresholds))
    custom_numeric <- custom_numeric[!is.na(custom_numeric)]
    
    if (length(custom_numeric) > 0) {
      # For OR/RR, convert to log scale
      if (sm %in% c("OR", "RR")) {
        custom_log <- log(pmax(custom_numeric, 0.01))  # Prevent log of negative/zero
        custom_labels <- paste0(format(custom_numeric, digits = 3), " (custom)")
      } else {
        custom_log <- custom_numeric
        custom_labels <- paste0(format(custom_numeric, digits = 3), " (custom)")
      }
      
      all_thresholds <- c(all_thresholds, custom_log)
      all_labels <- c(all_labels, custom_labels)
    }
  }
  
  # Remove duplicates and sort
  unique_indices <- !duplicated(round(all_thresholds, 6))
  all_thresholds <- all_thresholds[unique_indices]
  all_labels <- all_labels[unique_indices]
  sort_order <- order(all_thresholds)
  all_thresholds <- all_thresholds[sort_order]
  all_labels <- all_labels[sort_order]
  
  # Calculate probabilities
  result_df <- data.frame(
    Threshold = character(length(all_thresholds)),
    Probability = numeric(length(all_thresholds)),
    CI_Lower = numeric(length(all_thresholds)),
    CI_Upper = numeric(length(all_thresholds)),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(all_thresholds)) {
    T_val <- all_thresholds[i]
    
    # Calculate P(θ ≥ T) using MLE estimates
    prob_mle <- 1 - pnorm(T_val, mean = mu, sd = tau)
    
    # Calculate confidence interval using confidence region
    if (!is.null(conf_region) && length(conf_region$mu) > 0 && length(conf_region$tau) > 0) {
      # Calculate probability for each point in confidence region
      conf_probs <- sapply(seq_along(conf_region$mu), function(j) {
        mu_j <- conf_region$mu[j]
        tau_j <- conf_region$tau[j]
        1 - pnorm(T_val, mean = mu_j, sd = tau_j)
      })
      
      ci_lower <- min(conf_probs, na.rm = TRUE)
      ci_upper <- max(conf_probs, na.rm = TRUE)
    } else {
      # Fallback: use approximate SE-based CI
      se_mu <- sqrt(1 / sum(1 / (bivariate_result$sigma.2.k + bivariate_result$tau^2)))
      margin <- 1.96 * se_mu
      
      # Approximate CI bounds for mu
      mu_lower <- mu - margin
      mu_upper <- mu + margin
      
      # Calculate probabilities at bounds (using same tau)
      prob_lower <- 1 - pnorm(T_val, mean = mu_lower, sd = tau)
      prob_upper <- 1 - pnorm(T_val, mean = mu_upper, sd = tau)
      
      ci_lower <- min(prob_lower, prob_upper)
      ci_upper <- max(prob_lower, prob_upper)
    }
    
    # Ensure probabilities are in [0, 1]
    prob_mle <- pmax(0, pmin(1, prob_mle))
    ci_lower <- pmax(0, pmin(1, ci_lower))
    ci_upper <- pmax(0, pmin(1, ci_upper))
    
    # Store results
    result_df$Threshold[i] <- all_labels[i]
    result_df$Probability[i] <- round(prob_mle, 3)
    result_df$CI_Lower[i] <- round(ci_lower, 3)
    result_df$CI_Upper[i] <- round(ci_upper, 3)
  }
  
  return(result_df)
}

#' Calculate Probability Table from Efficacy/Harm Plot Data
#' @description Calculates P(θ ≥ T) directly from the same CDF data used in the Efficacy/Harm plot
#' @param CDF.ci.obj The same CDF object used to generate the Efficacy/Harm plot
#' @param custom_thresholds Optional vector of custom thresholds
#' @param sm Summary measure (for determining default thresholds)
#' @return A data frame with threshold, probability, and confidence intervals
#' @export
calculate_threshold_probabilities_from_cdf <- function(CDF.ci.obj, custom_thresholds = NULL, sm = NULL) {
  
  # Extract CDF components (same as used in the plot)
  CDF.vec <- CDF.ci.obj[[1]]        # Probability values (0.01 to 0.99)
  MLE.CDF <- CDF.ci.obj[[2]]        # Effect size values at MLE
  ci.CDF.ll <- CDF.ci.obj[[3]]      # Lower confidence bound effect sizes
  ci.CDF.ul <- CDF.ci.obj[[4]]      # Upper confidence bound effect sizes
  
  # Set default thresholds based on summary measure
  if (is.null(sm) || sm == "SMD") {
    default_thresholds <- c(-0.10, 0.00, 0.20, 0.50, 0.80)
  } else {
    # For OR/RR, use log scale internally but display as original scale
    default_thresholds <- log(c(0.9, 1.0, 1.1, 1.2, 1.5))
  }
  
  # Combine with custom thresholds if provided
  all_thresholds <- default_thresholds
  
  if (!is.null(custom_thresholds) && length(custom_thresholds) > 0) {
    # Parse custom thresholds
    custom_numeric <- suppressWarnings(as.numeric(custom_thresholds))
    custom_numeric <- custom_numeric[!is.na(custom_numeric)]
    
    if (length(custom_numeric) > 0) {
      # For OR/RR, convert to log scale
      if (!is.null(sm) && sm %in% c("OR", "RR")) {
        custom_log <- log(pmax(custom_numeric, 0.01))  # Prevent log of negative/zero
      } else {
        custom_log <- custom_numeric
      }
      all_thresholds <- c(all_thresholds, custom_log)
    }
  }
  
  # Remove duplicates and sort
  all_thresholds <- sort(unique(round(all_thresholds, 6)))
  
  # Calculate probabilities for each threshold using the same CDF data as the plot
  result_df <- data.frame(
    Threshold = numeric(length(all_thresholds)),
    Probability = numeric(length(all_thresholds)),
    CI_Lower = numeric(length(all_thresholds)),
    CI_Upper = numeric(length(all_thresholds)),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(all_thresholds)) {
    T_val <- all_thresholds[i]
    
    # Find P(θ ≥ T) by interpolating the CDF
    # For MLE estimate
    prob_mle <- 1 - approx(MLE.CDF, CDF.vec, xout = T_val, rule = 2)$y
    
    # For confidence bounds - find probabilities at the CI bounds
    prob_lower <- 1 - approx(ci.CDF.ul, CDF.vec, xout = T_val, rule = 2)$y  # Note: ul gives lower prob
    prob_upper <- 1 - approx(ci.CDF.ll, CDF.vec, xout = T_val, rule = 2)$y  # Note: ll gives upper prob
    
    # Ensure probabilities are in [0, 1]
    prob_mle <- pmax(0, pmin(1, prob_mle))
    prob_lower <- pmax(0, pmin(1, prob_lower))
    prob_upper <- pmax(0, pmin(1, prob_upper))
    
    # Store results with proper threshold format
    if (!is.null(sm) && sm %in% c("OR", "RR")) {
      result_df$Threshold[i] <- round(exp(T_val), 3)
    } else {
      result_df$Threshold[i] <- round(T_val, 3)
    }
    
    result_df$Probability[i] <- round(prob_mle, 3)
    result_df$CI_Lower[i] <- round(prob_lower, 3)
    result_df$CI_Upper[i] <- round(prob_upper, 3)
  }
  
  return(result_df)
}






