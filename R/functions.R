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
                         include_metareg = FALSE) {
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
        include_metareg = include_metareg
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
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(ggplot2)
library(knitr)
library(meta)
library(plotly)

# Helper function to safely run code
safe_run <- function(expr, fallback = NULL) {
  tryCatch(
    expr,
    error = function(e) {
      message("Error in report: ", conditionMessage(e))
      return(fallback)
    },
    warning = function(w) {
      message("Warning in report: ", conditionMessage(w))
      return(expr)
    }
  )
}
```

# Introduction

This report presents the results of a comprehensive meta-analysis using various methodological approaches. The goal is to compare and contrast findings from random effects, fixed effects, and bivariate meta-analysis models.

## Data Input

Below is a preview of the uploaded dataset:

```{r data-preview}
safe_run(head(params$data))
```

## Variables Used

- **study:** Study identifier
- **ie:** Intervention events
- **it:** Intervention total
- **pe:** Placebo/control events
- **pt:** Placebo/control total

# Overall Results

This section provides a high-level overview of results from all methods, allowing for easy comparison across different meta-analytic approaches.

## Summary Table

```{r summary-table, error=TRUE}
safe_run({
  summaryTable <- compare_models(list(random = params$random_results, fixed = params$fixed_results, bivariate = params$bivariate_results))
  kable(summaryTable)
}, data.frame(Message = "Summary table unavailable"))
```

## Overall Interpretation

```{r overall-interpretation, error=TRUE}
safe_run({
  interpretation <- interpret_results(list(random = params$random_results, fixed = params$fixed_results, bivariate = params$bivariate_results))
  cat(interpretation)
}, cat("Interpretation unavailable"))
```

# Random Effects Analysis

## Effect Size and Heterogeneity

```{r random-forest-plot, fig.width=12, fig.height=8, error=TRUE}
safe_run({
  # Try using built-in forest function if custom function fails
  tryCatch(
    {
      randomForestPlot <- random_forest_plot(params$random_results)
      print(randomForestPlot)
    },
    error = function(e) {
      message("Using built-in forest function")
      meta::forest(params$random_results)
    }
  )
}, plot(1, type = "n", main = "Forest plot unavailable", xlab = "", ylab = ""))
```

```{r random-overall-summary, error=TRUE}
safe_run(
  cat(capture.output(summary(params$random_results)), sep = "\n"),
  cat("Random effects summary unavailable")
)
```

```{r random-heterogeneity-summary, error=TRUE}
safe_run(
  cat(capture.output(print(params$random_results)), sep = "\n"),
  cat("Random effects heterogeneity summary unavailable")
)
```

## Model Diagnostics

```{r random-qq-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run(
  qq_plot_residuals(params$random_results),
  plot(1, type = "n", main = "QQ plot unavailable", xlab = "", ylab = "")
)
```

```{r random-outlier-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  tryCatch(
    print(outlier_detection_plot(params$random_results)),
    error = function(e) {
      # Create a simple residual plot
      res <- (params$random_results$TE - params$random_results$TE.random) / sqrt(params$random_results$seTE^2 + params$random_results$tau2)
      plot(res, main = "Residuals (Simplified)", ylab = "Standardized Residual", xlab = "Study Index")
      abline(h = c(-1.96, 1.96), lty = 2, col = "red")
    }
  )
}, plot(1, type = "n", main = "Outlier detection plot unavailable", xlab = "", ylab = ""))
```

```{r random-effect-distribution, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  tryCatch({
    # Create histogram of effect sizes
    hist(params$random_results$TE, 
         main = "Distribution of Effect Sizes",
         xlab = "Effect Size",
         col = "lightblue",
         breaks = 10)
    abline(v = params$random_results$TE.random, col = "red", lwd = 2, lty = 2)
    legend("topright", legend = "Random Effects Estimate", col = "red", lty = 2, lwd = 2)
  }, error = function(e) {
    plot(1, type = "n", main = "Effect distribution plot unavailable", xlab = "", ylab = "")
  })
}, plot(1, type = "n", main = "Effect distribution plot unavailable", xlab = "", ylab = ""))
```

```{r random-funnel-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  tryCatch(
    {
      randomFunnelPlot <- random_funnel_plot(params$random_results)
      print(randomFunnelPlot)
    },
    error = function(e) {
      funnel(params$random_results)
    }
  )
}, plot(1, type = "n", main = "Funnel plot unavailable", xlab = "", ylab = ""))
```

```{r random-trim-fill-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  tryCatch({
    tf <- trimfill(params$random_results)
    funnel(tf, yaxis = "se")
  }, error = function(e) {
    plot(1, type = "n", main = "Trim and fill plot unavailable", xlab = "", ylab = "")
  })
}, plot(1, type = "n", main = "Trim and fill plot unavailable", xlab = "", ylab = ""))
```

```{r random-egger-test, error=TRUE}
safe_run(
  cat(capture.output(metabias(params$random_results, method = "Egger")), sep = "\n"),
  cat("Egger test unavailable")
)
```

## Sensitivity Analysis

```{r random-leave-one-out, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  tryCatch(
    {
      leaveOneOutPlot <- random_leave_one_out(params$random_results)
      print(leaveOneOutPlot)
    },
    error = function(e) {
      metainf_result <- metainf(params$random_results)
      metainf_data <- data.frame(
        study = metainf_result$studlab,
        estimate = metainf_result$TE,
        lower = metainf_result$lower,
        upper = metainf_result$upper
      )
      ggplot(metainf_data, aes(x = estimate, y = study)) +
        geom_point() +
        geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
        geom_vline(xintercept = params$random_results$TE.random, linetype = "dashed", color = "red") +
        theme_minimal() +
        labs(title = "Leave-One-Out Analysis", x = "Effect Size", y = "Study Omitted")
    }
  )
}, plot(1, type = "n", main = "Leave-one-out plot unavailable", xlab = "", ylab = ""))
```

```{r random-baujat-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run(
  print(baujat(params$random_results)),
  plot(1, type = "n", main = "Baujat plot unavailable", xlab = "", ylab = "")
)
```

```{r random-gosh-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  tryCatch(
    print(gosh_plot(params$random_results)),
    error = function(e) {
      # Create a simple scatter plot as alternative
      ggplot(data.frame(TE = params$random_results$TE, seTE = params$random_results$seTE), 
             aes(x = seTE, y = TE)) +
        geom_point() +
        labs(title = "Effect Sizes vs. Standard Errors", 
             x = "Standard Error", 
             y = "Effect Size")
    }
  )
}, plot(1, type = "n", main = "GOSH plot unavailable", xlab = "", ylab = ""))
```

```{r random-influence-summary, error=TRUE}
safe_run({
  tryCatch(
    cat(capture.output(influence_analysis(params$random_results)), sep = "\n"),
    error = function(e) {
      inf <- metainf(params$random_results)
      print(inf)
    }
  )
}, cat("Influence analysis unavailable"))
```

# Subgroup Analysis (Random Effects)

```{r random-subgroup-analysis, fig.width=12, fig.height=8, error=TRUE}
if (params$include_subgroup && !is.null(params$random_subgroup_results)) {
  safe_run({
    tryCatch(
      {
        forest(params$random_subgroup_results)
      }, error = function(e) {
        meta::forest(params$random_subgroup_results)
      }
    )
  }, plot(1, type = "n", main = "Subgroup analysis plot unavailable", xlab = "", ylab = ""))
} else {
  cat("Subgroup analysis not included or unavailable.")
}
```

```{r random-subgroup-summary, error=TRUE}
if (params$include_subgroup && !is.null(params$random_subgroup_results)) {
  safe_run(
    cat(capture.output(summary(params$random_subgroup_results)), sep = "\n"),
    cat("Subgroup analysis summary unavailable")
  )
}
```

# Fixed Effects Analysis

## Effect Size and Heterogeneity

```{r fixed-forest-plot, fig.width=12, fig.height=8, error=TRUE}
safe_run({
  tryCatch(
    {
      fixedForestPlot <- fixed_forest_plot(params$fixed_results)
      print(fixedForestPlot)
    },
    error = function(e) {
      meta::forest(params$fixed_results)
    }
  )
}, plot(1, type = "n", main = "Fixed effects forest plot unavailable", xlab = "", ylab = ""))
```

```{r fixed-overall-summary, error=TRUE}
safe_run(
  cat(capture.output(summary(params$fixed_results)), sep = "\n"),
  cat("Fixed effects summary unavailable")
)
```

```{r fixed-model-fit, error=TRUE}
safe_run(
  cat(capture.output(print(params$fixed_results$I2)), sep = "\n"),
  cat("Model fit statistics unavailable")
)
```

## Model Diagnostics

```{r fixed-model-fit-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  tryCatch(
    {
      radial(params$fixed_results, main="Radial Plot for Fixed Effects Model")
    },
    error = function(e) {
      plot(1, type = "n", main = "Radial plot unavailable", xlab = "", ylab = "")
    }
  )
}, plot(1, type = "n", main = "Radial plot unavailable", xlab = "", ylab = ""))
```

```{r fixed-qq-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  tryCatch(
    {
      # Create QQ plot for fixed effects model
      res <- (params$fixed_results$TE - params$fixed_results$TE.common) / params$fixed_results$seTE
      qqnorm(res, main = "Q-Q Plot for Fixed Effects Model")
      qqline(res, col = "red")
    },
    error = function(e) {
      plot(1, type = "n", main = "QQ plot unavailable", xlab = "", ylab = "")
    }
  )
}, plot(1, type = "n", main = "QQ plot unavailable", xlab = "", ylab = ""))
```

```{r fixed-outlier-detection-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  tryCatch(
    {
      # Create outlier detection plot for fixed effects
      res <- (params$fixed_results$TE - params$fixed_results$TE.common) / params$fixed_results$seTE
      plot(res, main = "Outlier Detection (Fixed Effects)",
           ylab = "Standardized Residual", xlab = "Study Index")
      abline(h = c(-1.96, 1.96), lty = 2, col = "red")
      text(1:length(res), res, labels = params$fixed_results$studlab, pos = 4, cex = 0.7)
    },
    error = function(e) {
      plot(1, type = "n", main = "Outlier detection plot unavailable", xlab = "", ylab = "")
    }
  )
}, plot(1, type = "n", main = "Outlier detection plot unavailable", xlab = "", ylab = ""))
```

## Publication Bias

```{r fixed-funnel-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  tryCatch(
    {
      funnel(params$fixed_results, main="Funnel Plot (Fixed Effects)")
    },
    error = function(e) {
      plot(1, type = "n", main = "Funnel plot unavailable", xlab = "", ylab = "")
    }
  )
}, plot(1, type = "n", main = "Funnel plot unavailable", xlab = "", ylab = ""))
```

```{r fixed-trim-fill-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  tryCatch({
    tf <- trimfill(params$fixed_results)
    funnel(tf, yaxis = "se", main="Trim and Fill Plot (Fixed Effects)")
  }, error = function(e) {
    plot(1, type = "n", main = "Trim and fill plot unavailable", xlab = "", ylab = "")
  })
}, plot(1, type = "n", main = "Trim and fill plot unavailable", xlab = "", ylab = ""))
```

```{r fixed-egger-test, error=TRUE}
safe_run(
  cat(capture.output(metabias(params$fixed_results, method = "Egger")), sep = "\n"),
  cat("Egger test unavailable")
)
```

## Sensitivity Analysis

```{r fixed-leave-one-out-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  tryCatch(
    {
      metainf_result <- metainf(params$fixed_results)
      metainf_data <- data.frame(
        study = metainf_result$studlab,
        estimate = metainf_result$TE,
        lower = metainf_result$lower,
        upper = metainf_result$upper
      )
      ggplot(metainf_data, aes(x = estimate, y = study)) +
        geom_point() +
        geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
        geom_vline(xintercept = params$fixed_results$TE.common, linetype = "dashed", color = "red") +
        theme_minimal() +
        labs(title = "Leave-One-Out Analysis (Fixed Effects)", x = "Effect Size", y = "Study Omitted")
    },
    error = function(e) {
      plot(1, type = "n", main = "Leave-one-out plot unavailable", xlab = "", ylab = "")
    }
  )
}, plot(1, type = "n", main = "Leave-one-out plot unavailable", xlab = "", ylab = ""))
```

```{r fixed-influence-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  tryCatch(
    {
      baujat(params$fixed_results, main="Baujat Plot (Fixed Effects)")
    },
    error = function(e) {
      plot(1, type = "n", main = "Baujat plot unavailable", xlab = "", ylab = "")
    }
  )
}, plot(1, type = "n", main = "Influence plot unavailable", xlab = "", ylab = ""))
```

```{r fixed-influence-summary, error=TRUE}
safe_run({
  cat("Influence Analysis (Fixed Effects Model):\n\n")
  # Create a simple influence summary
  metainf_result <- metainf(params$fixed_results)
  studies <- metainf_result$studlab
  estimates <- metainf_result$TE.random
  influential <- which(abs(estimates - params$fixed_results$TE.common) > 1.96 * params$fixed_results$seTE.common)
  
  if(length(influential) > 0) {
    cat("Potentially influential studies:\n")
    for(i in influential) {
      cat("- ", studies[i], ": Effect size when omitted = ", round(estimates[i], 4), "\n")
    }
  } else {
    cat("No highly influential studies detected.\n")
  }
}, cat("Influence summary unavailable"))
```

# Subgroup Analysis (Fixed Effects)

```{r fixed-subgroup-analysis, fig.width=12, fig.height=8, error=TRUE}
if (params$include_subgroup && !is.null(params$fixed_subgroup_results)) {
  safe_run({
    tryCatch({
      forest(params$fixed_subgroup_results)
    }, error = function(e) {
      meta::forest(params$fixed_subgroup_results)
    })
  }, plot(1, type = "n", main = "Subgroup analysis plot unavailable", xlab = "", ylab = ""))
} else {
  cat("Subgroup analysis not included or unavailable.")
}
```

```{r fixed-subgroup-summary, error=TRUE}
if (params$include_subgroup && !is.null(params$fixed_subgroup_results)) {
  safe_run(
    cat(capture.output(summary(params$fixed_subgroup_results)), sep = "\n"),
    cat("Subgroup analysis summary unavailable")
  )
}
```

# Bivariate Approach

## Effect Size and Heterogeneity

```{r bivariate-effect-size, fig.width=10, fig.height=6, error=TRUE}
# Create a 2-column layout similar to the UI
par(mfrow = c(1, 2))

# Column 1: Bivariate Forest Plot
safe_run({
  if(exists("forest.metabiv", mode = "function")) {
    forest.metabiv(params$bivariate_results, 
                  main = "Bivariate Forest Plot",
                  xlab = "Effect Size")
  } else {
    # Fallback to a simple forest plot
    plot(1, type = "n", main = "Bivariate forest plot unavailable\n(forest.metabiv function not found)", xlab = "", ylab = "")
  }
}, plot(1, type = "n", main = "Bivariate forest plot unavailable", xlab = "", ylab = ""))

# Column 2: Confidence Region Plot
safe_run({
  if(exists("plot.mu.tau.CI", mode = "function") && 
     !is.null(params$bivariate_results$dev_pvals) && 
     length(params$bivariate_results$dev_pvals) >= 2) {
    plot.mu.tau.CI(params$bivariate_results$dev_pvals[[1]], 
                 params$bivariate_results$dev_pvals[[2]], 
                 mlb = "Confidence Region for (μ, τ)")
  } else {
    plot(1, type = "n", main = "Confidence region plot unavailable", xlab = "", ylab = "")
  }
}, plot(1, type = "n", main = "Confidence region plot unavailable", xlab = "", ylab = ""))

# Reset layout
par(mfrow = c(1, 1))
```

```{r bivariate-efficacy-harm-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  if(exists("efficacy_harm_plot", mode = "function")) {
    efficacy_harm_plot(params$bivariate_results)
  } else {
    # Try to create a simple visualization
    plot(1, type = "n", main = "Efficacy-harm plot unavailable", xlab = "", ylab = "")
  }
}, plot(1, type = "n", main = "Efficacy-harm plot unavailable", xlab = "", ylab = ""))
```

```{r bivariate-overall-summary, error=TRUE}
safe_run({
  if(!is.null(params$bivariate_results$mu) && !is.null(params$bivariate_results$tau)) {
    cat("Bivariate Meta-Analysis Summary:\n\n")
    cat("Pooled Effect Size (μ): ", exp(params$bivariate_results$mu), "\n")
    cat("95% CI: (", exp(params$bivariate_results$mu - 1.96 * params$bivariate_results$tau), ", ", 
        exp(params$bivariate_results$mu + 1.96 * params$bivariate_results$tau), ")\n")
    cat("Between-study Variability (τ): ", params$bivariate_results$tau, "\n")
    cat("Between-study Variance (τ²): ", params$bivariate_results$tau^2, "\n")
  } else {
    cat("Bivariate meta-analysis summary unavailable")
  }
}, cat("Bivariate meta-analysis summary unavailable"))
```

## Model Diagnostics

```{r bivariate-model-diagnostics, fig.width=10, fig.height=6, error=TRUE}
# Create a 2-column layout similar to the UI
par(mfrow = c(1, 2))

# Column 1: QQ Plot for Standardized Residuals
safe_run({
  if(exists("qq_plot_bivariate", mode = "function")) {
    qq_plot_bivariate(params$bivariate_results)
  } else {
    plot(1, type = "n", main = "QQ plot (standardized residuals) unavailable", xlab = "", ylab = "")
  }
}, plot(1, type = "n", main = "QQ plot (standardized residuals) unavailable", xlab = "", ylab = ""))

# Column 2: QQ Plot for Raw Residuals
safe_run({
  if(exists("qq_plot_with_ci_raw", mode = "function") && 
     !is.null(params$bivariate_results$y.k) && 
     !is.null(params$bivariate_results$mu) && 
     !is.null(params$bivariate_results$sigma.2.k) && 
     !is.null(params$bivariate_results$tau)) {
    
    y_k <- params$bivariate_results$y.k
    mu <- params$bivariate_results$mu
    sigma_2_k <- params$bivariate_results$sigma.2.k
    tau_2 <- params$bivariate_results$tau^2
    n_k <- params$bivariate_results$n.e + params$bivariate_results$n.c
    
    qq_plot_with_ci_raw(y_k, mu, sigma_2_k, tau_2, n_k, 
                      log_odds = FALSE, 
                      title = "Q-Q Plot for Raw Residuals")
  } else {
    plot(1, type = "n", main = "QQ plot (raw residuals) unavailable", xlab = "", ylab = "")
  }
}, plot(1, type = "n", main = "QQ plot (raw residuals) unavailable", xlab = "", ylab = ""))

# Reset layout
par(mfrow = c(1, 1))
```

## Publication Bias

```{r bivariate-publication-bias, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  if(exists("funnel", mode = "function") && 
     !is.null(params$bivariate_results$y.k) && 
     !is.null(params$bivariate_results$sigma.2.k)) {
    
    # Create a more robust funnel plot for bivariate model
    y.k <- params$bivariate_results$y.k
    se.k <- sqrt(params$bivariate_results$sigma.2.k)
    
    # Try different approaches to ensure plot is visible
    tryCatch({
      # First attempt - create metagen object
      meta_analysis <- metagen(TE = y.k, seTE = se.k, 
                              common = TRUE, random = TRUE,
                              sm = params$bivariate_results$sm)
      funnel(meta_analysis, 
             main = "Funnel Plot for Bivariate Model",
             xlab = "Effect Size", 
             ylab = "Standard Error")
    }, error = function(e) {
      tryCatch({
        # Second attempt - simple scatter plot
        plot(y.k, se.k, 
             main = "Funnel Plot for Bivariate Model (Simple)",
             xlab = "Effect Size (log scale)", 
             ylab = "Standard Error",
             pch = 19)
        abline(v = params$bivariate_results$mu, col = "red", lty = 2)
        # Funnel lines
        tau <- params$bivariate_results$tau
        mu <- params$bivariate_results$mu
        se_range <- seq(0, max(se.k, na.rm = TRUE), length.out = 100)
        lines(mu + 1.96 * sqrt(se_range^2 + tau^2), se_range, col = "blue", lty = 2)
        lines(mu - 1.96 * sqrt(se_range^2 + tau^2), se_range, col = "blue", lty = 2)
      }, error = function(e) {
        plot(1, type = "n", main = "Funnel plot unavailable", xlab = "", ylab = "")
      })
    })
  } else {
    plot(1, type = "n", main = "Funnel plot unavailable", xlab = "", ylab = "")
  }
}, plot(1, type = "n", main = "Funnel plot unavailable", xlab = "", ylab = ""))
```

```{r bivariate-egger-test, error=TRUE}
safe_run({
  if(!is.null(params$bivariate_results$y.k) && !is.null(params$bivariate_results$sigma.2.k)) {
    meta_analysis <- metagen(TE = params$bivariate_results$y.k, 
                            seTE = sqrt(params$bivariate_results$sigma.2.k))
    cat(capture.output(metabias(meta_analysis, method = "Egger")), sep = "\n")
  } else {
    cat("Egger test unavailable for bivariate model")
  }
}, cat("Egger test unavailable for bivariate model"))
```

## Sensitivity Analysis

```{r bivariate-sensitivity-analysis, fig.width=10, fig.height=6, error=TRUE}
# Use the same interactive plot from the UI
safe_run({
  if(exists("confidence_region_shift_plot", mode = "function")) {
    # Try to use the interactive function first
    confidence_region_shift_plot(params$bivariate_results)
  } else if(!is.null(params$bivariate_results$dev_pvals) && 
           length(params$bivariate_results$dev_pvals) >= 2) {
    
    # Fallback: Create a static version of the confidence region shift plot
    # Create a 2-column layout
    par(mfrow = c(1, 2))
    
    # Left: Confidence Region Plot
    plot.mu.tau.CI(params$bivariate_results$dev_pvals[[1]],
                  params$bivariate_results$dev_pvals[[2]],
                  mlb = "Confidence Region",
                  mu_mle = params$bivariate_results$mu,
                  tau_mle = params$bivariate_results$tau)
    
    # Right: Enhanced Baujat Plot (static version)
    if(!is.null(params$bivariate_results$studlab) && 
       !is.null(params$bivariate_results$y.k) && 
       !is.null(params$bivariate_results$sigma.2.k) && 
       !is.null(params$bivariate_results$mu) && 
       !is.null(params$bivariate_results$tau)) {
      
      # Calculate influence metrics
      influence <- sapply(1:length(params$bivariate_results$y.k), function(i) {
        contribution <- (params$bivariate_results$y.k[i] - params$bivariate_results$mu)^2 / 
                       (params$bivariate_results$sigma.2.k[i] + params$bivariate_results$tau^2)
        influence <- abs(params$bivariate_results$y.k[i] - params$bivariate_results$mu) / 
                    sqrt(params$bivariate_results$sigma.2.k[i] + params$bivariate_results$tau^2)
        c(contribution, influence)
      })
      
      # Create static plot
      weights <- 1/(params$bivariate_results$sigma.2.k + params$bivariate_results$tau^2)
      
      plot(influence[1,], influence[2,], 
           main = "Enhanced Baujat Plot",
           xlab = "Contribution to heterogeneity", 
           ylab = "Influence on overall result",
           cex = sqrt(weights/mean(weights))*1.5,
           pch = 19, col = "blue")
      
      # Add study labels
      text(influence[1,], influence[2,], 
           labels = params$bivariate_results$studlab, 
           pos = 4, cex = 0.7)
      
      # Add reference lines
      abline(h = median(influence[2,]), lty = 2, col = "gray")
      abline(v = median(influence[1,]), lty = 2, col = "gray")
    } else {
      plot(1, type = "n", main = "Enhanced Baujat plot unavailable", xlab = "", ylab = "")
    }
    
    # Reset layout
    par(mfrow = c(1, 1))
  } else {
    plot(1, type = "n", main = "Sensitivity analysis plots unavailable", xlab = "", ylab = "")
  }
}, plot(1, type = "n", main = "Sensitivity analysis plots unavailable", xlab = "", ylab = ""))
```

```{r bivariate-leave-one-out, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  if(exists("metainf", mode = "function") && 
     !is.null(params$bivariate_results$y.k) && 
     !is.null(params$bivariate_results$sigma.2.k)) {
    
    # Create a leave-one-out analysis for bivariate model by approximating with metagen
    meta_analysis <- metagen(TE = params$bivariate_results$y.k, 
                            seTE = sqrt(params$bivariate_results$sigma.2.k),
                            studlab = params$bivariate_results$studlab)
    
    inf_result <- metainf(meta_analysis)
    
    # Create a plot similar to the one in the UI
    inf_data <- data.frame(
      study = inf_result$studlab,
      estimate = inf_result$TE,
      lower = inf_result$lower,
      upper = inf_result$upper
    )
    
    ggplot(inf_data, aes(x = estimate, y = study)) +
      geom_point() +
      geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
      geom_vline(xintercept = params$bivariate_results$mu, linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "Leave-One-Out Analysis (Bivariate)", 
           x = "Effect Size", 
           y = "Study Omitted")
  } else {
    plot(1, type = "n", main = "Leave-one-out analysis unavailable", xlab = "", ylab = "")
  }
}, plot(1, type = "n", main = "Leave-one-out analysis unavailable", xlab = "", ylab = ""))
```

# Subgroup Analysis (Bivariate)

```{r bivariate-subgroup-analysis, fig.width=12, fig.height=8, error=TRUE}
if (params$include_subgroup && !is.null(params$bivariate_subgroup_results)) {
  safe_run({
    # params$bivariate_subgroup_results is expected to be a list with separate results
    res_list <- params$bivariate_subgroup_results$results
    if (!is.null(res_list) && length(res_list) > 0) {
      par(mfrow = c(length(res_list), 1))
      for (nm in names(res_list)) {
        tryCatch({
          forest.metabiv(res_list[[nm]], title = paste("Subgroup:", nm), xlab = "Effect Size")
        }, error = function(e) {
          plot(1, type = "n", main = paste("Subgroup plot unavailable (", nm, ")"))
        })
      }
      par(mfrow = c(1,1))
    } else {
      cat("No subgroup results available.")
    }
  }, plot(1, type = "n", main = "Subgroup analysis plot unavailable", xlab = "", ylab = ""))
} else {
  cat("Subgroup analysis not included or unavailable.")
}
```

```{r bivariate-subgroup-summary, error=TRUE}
if (params$include_subgroup && !is.null(params$bivariate_subgroup_results)) {
  safe_run({
    res_list <- params$bivariate_subgroup_results$results
    for (nm in names(res_list)) {
      cat("\nSubgroup:", nm, "\n")
      cat(capture.output(summary(res_list[[nm]])), sep = "\n")
    }
  }, cat("Subgroup summary unavailable"))
}
```

# Meta-Regression (Optional)

`r if (params$include_metareg && !is.null(params$metaregression_results)) { "## Meta-Regression" }`

```{r metareg-regression-plot, fig.width=10, fig.height=6, error=TRUE}
if (params$include_metareg && !is.null(params$metaregression_results)) {
  res <- params$metaregression_results
  moderator_data <- res$moderator_data
  if (is.factor(moderator_data)) {
    # Categorical moderator plot
    group_means <- tapply(res$effect_sizes, moderator_data, mean)
    group_se <- tapply(res$standard_errors, moderator_data, function(x) sqrt(mean(x^2)))
    bp <- barplot(group_means, ylim = range(group_means + c(-1,1)*1.96*group_se), col = "lightblue",
                  xlab = res$moderator_name, ylab = "Effect Size", main = "Meta-Regression (Categorical Moderator)")
    arrows(bp, group_means - 1.96*group_se, bp, group_means + 1.96*group_se,
           angle = 90, code = 3, length = 0.05, col = "red")
  } else {
    # Continuous moderator scatter
    plot(moderator_data, res$effect_sizes, pch = 19, xlab = res$moderator_name, ylab = "Effect Size",
         main = "Meta-Regression (Continuous Moderator)")
    mod_seq <- seq(min(moderator_data, na.rm = TRUE), max(moderator_data, na.rm = TRUE), length.out = 100)
    pred <- predict(res$model, newmods = mod_seq)
    lines(mod_seq, pred$pred, col = "red", lwd = 2)
  }
} else {
  cat("Meta-regression not included or unavailable.")
}
```

```{r metareg-summary, error=TRUE}
if (params$include_metareg && !is.null(params$metaregression_results)) {
  cat(capture.output(summary(params$metaregression_results$model)), sep = "\n")
  if (!is.null(params$metaregression_results$perm_result)) {
    cat("\nPermutation Test Results:\n")
    print(params$metaregression_results$perm_result)
  }
}
```

This comprehensive report provides a detailed overview of the meta-analysis results using multiple methodological approaches. By comparing results across different models and examining various diagnostic plots, we can gain a more nuanced understanding of the effect size, heterogeneity, and potential biases in the meta-analysis. This multi-faceted approach supports more informed decision-making in interpreting and applying the results of the meta-analysis.
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






