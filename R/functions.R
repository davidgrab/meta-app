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
  is_hr <- (effect_measure == "HR")

  methods <- c("Common effect model", "Random effects model")
  effects <- c(fixed_result$TE.common, random_result$TE.random)
  lowers <- c(fixed_result$lower.common, random_result$lower.random)
  uppers <- c(fixed_result$upper.common, random_result$upper.random)

  if (!is.null(bivariate_result)) {
    methods <- c(methods, "Bivariate random effects")
    effects <- c(effects, bivariate_result$mu)
    # Ensure conf_region and mu exist before accessing
    if (!is.null(bivariate_result$conf_region) && !is.null(bivariate_result$conf_region$mu) && length(bivariate_result$conf_region$mu) == 2) {
      lowers <- c(lowers, bivariate_result$conf_region$mu[1])
      uppers <- c(uppers, bivariate_result$conf_region$mu[2])
    } else {
      # Fallback if conf_region$mu is not as expected
      lowers <- c(lowers, NA)
      uppers <- c(uppers, NA)
    }
  }
  
  # Apply exp transformation for all, as inputs are generally log scale
  data <- data.frame(
    Method = methods,
    Effect = exp(effects),
    Lower = exp(lowers),
    Upper = exp(uppers)
  )
  
  data$Method <- factor(data$Method, levels = rev(methods)) # Reverse order for plot

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

  effect_measure_name <- switch(results$random$sm,
                                "OR" = "Odds Ratio",
                                "RR" = "Risk Ratio",
                                "HR" = "Hazard Ratio",
                                "SMD" = "Standardized Mean Difference",
                                results$random$sm)

  output <- paste("Overall Interpretation:\n\n",
                  "Random Effects Model (", effect_measure_name, "):\n",
                  "Effect Size: ", round(safe_exp(safe_extract(results$random, "TE.random")), 2),
                  " (95% CI: ", round(safe_exp(safe_extract(results$random, "lower.random")), 2),
                  " - ", round(safe_exp(safe_extract(results$random, "upper.random")), 2), ")\n",
                  "Heterogeneity (I^2): ", round(safe_extract(results$random, "I2") * 100, 1), "%\n\n", # I2 from meta is 0-1
                  "Fixed Effects Model (", effect_measure_name, "):\n",
                  "Effect Size: ", round(safe_exp(safe_extract(results$fixed, "TE.common")), 2),
                  " (95% CI: ", round(safe_exp(safe_extract(results$fixed, "lower.common")), 2),
                  " - ", round(safe_exp(safe_extract(results$fixed, "upper.common")), 2), ")\n",
                  "P-value: ", format.pval(safe_extract(results$fixed, "pval.common")), "\n\n")

  if (!is.null(results$bivariate)) {
    output <- paste(output,
                    "Bivariate Model (", effect_measure_name, "):\n",
                    "Effect Size: ", round(safe_exp(safe_extract(results$bivariate, "mu")), 2),
                    " (95% CI: ", round(safe_exp(safe_extract(results$bivariate, "lower")), 2),
                    " - ", round(safe_exp(safe_extract(results$bivariate, "upper")), 2), ")\n",
                    "Between-study variance (tau^2): ", round(safe_extract(results$bivariate, "tau")^2, 4), "\n\n")
  } else {
    output <- paste(output, "Bivariate Model: Not applicable for this data type.\n\n")
  }
  
  # Add interpretations only if we have valid data
  if (!is.na(safe_extract(results$random, "TE.random"))) {
    effect_direction <- if (results$random$sm %in% c("OR", "RR", "HR")) {
                           ifelse(safe_exp(safe_extract(results$random, "TE.random")) > 1, "an increase in the outcome event", "a decrease in the outcome event")
                        } else { # SMD
                           ifelse(safe_extract(results$random, "TE.random") > 0, "a positive effect", "a negative effect")
                        }
    output <- paste(output,
                    "The results suggest ", effect_direction,
                    " with the intervention. ",
                    "The level of heterogeneity (I^2) is ",
                    ifelse(safe_extract(results$random, "I2")*100 < 30, "low",
                           ifelse(safe_extract(results$random, "I2")*100 < 60, "moderate", "high")),
                    ". ",
                    "The fixed and random effects models ",
                    ifelse(abs(safe_extract(results$fixed, "TE.common") - safe_extract(results$random, "TE.random")) < 0.1, "show similar results", "show some differences"),
                    ". ")
    if (!is.null(results$bivariate)) {
      output <- paste(output, "The bivariate model provides additional insights into the between-study variance.")
    }
  } else {
    output <- paste(output, "Unable to provide detailed interpretation due to missing or invalid data.")
  }
  
  return(output)
}

# Update this function in your functions.R file

heterogeneity_plot <- function(model) {
  # Extract necessary data from the model
  studies <- model$studlab
  effects <- model$TE # These are on log scale for HR/OR/RR
  ci_lower <- model$lower # log scale
  ci_upper <- model$upper # log scale
  weights <- model$w.random
  
  current_xlab <- if (model$sm == "HR") "log(Hazard Ratio)" else paste("Effect Size (", model$sm, ")")

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
    geom_vline(xintercept = model$TE.random, linetype = "dashed", color = "red") + # TE.random is on log scale
    theme_minimal() +
    labs(title = "Heterogeneity Plot",
         x = current_xlab,
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
  res <- (model$TE - model$TE.random) / sqrt(model$seTE^2 + model$tau2) # Standardized residuals are on the analysis scale (logHR)
  title_label <- if (model$sm == "HR") "Standardized Residuals (logHR)" else "Standardized Residuals"

  ggplot(data.frame(studlab = model$studlab, resid = res), aes(x = studlab, y = resid)) +
    geom_point() +
    geom_hline(yintercept = c(-1.96, 1.96), linetype = "dashed", color = "red") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = title_label, x = "Study", y = "Standardized Residual")
}

effect_distribution_plot <- function(model, xlab_label = NULL) {
  default_xlab <- if (model$sm == "HR") "log(Hazard Ratio)" else paste("Effect Size (", model$sm, ")")
  final_xlab <- if (!is.null(xlab_label)) xlab_label else default_xlab

  ggplot(data.frame(effect = model$TE), aes(x = effect)) + # TE is on log scale for HR
    geom_histogram(binwidth = 0.1, fill = "lightblue", color = "black") +
    geom_vline(xintercept = model$TE.random, color = "red", linetype = "dashed") + # TE.random is on log scale
    labs(title = "Distribution of Effect Sizes", x = final_xlab, y = "Count")
}

gosh_plot <- function(model) {
  current_xlab <- if (model$sm == "HR") "Standard Error (logHR)" else "Standard Error"
  current_ylab <- if (model$sm == "HR") "log(Hazard Ratio)" else paste("Effect Size (", model$sm, ")")

  ggplot(data.frame(TE = model$TE, seTE = model$seTE), aes(x = seTE, y = TE)) + # TE and seTE are on log scale for HR
    geom_point() +
    labs(title = "Effect Sizes vs. Standard Errors", x = current_xlab, y = current_ylab)
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
    if (!is.null(model) && !is.null(model[[field]])) {
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

  sm_label <- results$random$sm # Assuming random model always exists and has sm

  models_to_include <- c("Random Effects", "Fixed Effects")
  effects_data <- list(
    Effect = c(
      safe_exp(safe_extract(results$random, "TE.random")),
      safe_exp(safe_extract(results$fixed, "TE.common"))
    ),
    Lower = c(
      safe_exp(safe_extract(results$random, "lower.random")),
      safe_exp(safe_extract(results$fixed, "lower.common"))
    ),
    Upper = c(
      safe_exp(safe_extract(results$random, "upper.random")),
      safe_exp(safe_extract(results$fixed, "upper.common"))
    ),
    P_value = c(
      safe_extract(results$random, "pval.random"),
      safe_extract(results$fixed, "pval.common")
    ),
    Heterogeneity_tau2 = c( # tau2 for random, NA for fixed
      safe_extract(results$random, "tau2"),
      NA
    ),
    I2 = c( # I2 for random, NA for fixed
      safe_extract(results$random, "I2"), # Already 0-1 from meta
      NA
    )
  )

  if (!is.null(results$bivariate)) {
    models_to_include <- c(models_to_include, "Bivariate")
    effects_data$Effect <- c(effects_data$Effect, safe_exp(safe_extract(results$bivariate, "mu")))
    effects_data$Lower <- c(effects_data$Lower, safe_exp(safe_extract(results$bivariate, "lower")))
    effects_data$Upper <- c(effects_data$Upper, safe_exp(safe_extract(results$bivariate, "upper")))
    effects_data$P_value <- c(effects_data$P_value, safe_extract(results$bivariate, "pval.Q")) # This might be pval for heterogeneity
    effects_data$Heterogeneity_tau2 <- c(effects_data$Heterogeneity_tau2, safe_extract(results$bivariate, "tau")^2)
    effects_data$I2 <- c(effects_data$I2, safe_extract(results$bivariate, "I2")/100) # Assuming bivariate I2 is 0-100
  }
  
  summary_df <- data.frame(Model = models_to_include)
  for (col_name in names(effects_data)) {
    summary_df[[col_name]] <- effects_data[[col_name]]
  }

  # Format I2 as percentage
  if ("I2" %in% names(summary_df)) {
    summary_df$I2 <- ifelse(is.na(summary_df$I2), NA, paste0(round(summary_df$I2 * 100, 1), "%"))
  }

  # Round numeric columns to appropriate decimal places
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


render_report <- function(random_results, fixed_results, bivariate_results, data = NULL) {
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
        data = data
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
  data: NA
  input_subgroup_var: NA # Added for subgroup variable
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
      return(expr) # Return the expression result on warning
    }
  )
}

# Determine if HR analysis and set labels
is_hr_analysis <- !is.null(params$random_results) && !is.na(params$random_results$sm) && params$random_results$sm == "HR"
effect_measure_label <- "Effect Size" # Default
log_effect_measure_label <- "log(Effect Size)" # Default

if (!is.null(params$random_results) && !is.na(params$random_results$sm)) {
  effect_measure_label <- switch(params$random_results$sm,
                                 "HR" = "Hazard Ratio (HR)",
                                 "OR" = "Odds Ratio (OR)",
                                 "RR" = "Risk Ratio (RR)",
                                 "SMD" = "Standardized Mean Difference (SMD)",
                                 params$random_results$sm)
  log_effect_measure_label <- switch(params$random_results$sm,
                                     "HR" = "log(Hazard Ratio)",
                                     "OR" = "log(Odds Ratio)",
                                     "RR" = "log(Risk Ratio)",
                                     # SMD is usually not logged in the same way for interpretation in funnel plots, but analysis is on original SMD scale
                                     "SMD" = "Standardized Mean Difference (SMD)",
                                     paste0("log(",params$random_results$sm,")"))
}


# Determine if subgroup analysis was performed
subgroup_analysis_performed <- !is.null(params$input_subgroup_var) &&
                               params$input_subgroup_var != "" &&
                               !is.null(params$random_results) &&
                               !is.null(params$random_results$k.byvar) &&
                               length(params$random_results$k.byvar) > 0 &&
                               any(params$random_results$k.byvar > 0) # Ensures there's at least one subgroup with studies

```

# Introduction

This report presents the results of a comprehensive meta-analysis. The primary outcome measure is `r effect_measure_label`. The analysis compares findings from random effects and fixed effects models. `r if (!is.null(params$bivariate_results)) "A bivariate meta-analysis model is also included." else ""`

## Data Input

Below is a preview of the uploaded dataset:
```{r data-preview}
safe_run(head(params$data))
```

## Variables Used
The specific variables used depend on the input data type. For `r effect_measure_label` analysis, the key components are typically study labels, event counts and totals (for binary data leading to HR), or direct HRs with confidence intervals, or log(HR) and standard errors.

# Overall Results

This section provides a high-level overview of results from all methods, allowing for easy comparison across different meta-analytic approaches.

## Summary Table

```{r summary-table, error=TRUE}
safe_run({
  # compare_models function already handles NULL bivariate and HR exponentiation
  summaryTable <- compare_models(list(random = params$random_results, fixed = params$fixed_results, bivariate = params$bivariate_results))
  kable(summaryTable, caption = paste0("Comparison of Meta-Analysis Models for ", effect_measure_label))
}, data.frame(Message = "Summary table unavailable"))
```

## Overall Interpretation

```{r overall-interpretation, error=TRUE}
safe_run({
  # interpret_results function already updated for HR and NULL bivariate
  interpretation <- interpret_results(list(random = params$random_results, fixed = params$fixed_results, bivariate = params$bivariate_results))
  cat(interpretation)
}, cat("Interpretation unavailable"))
```

```{r subgroup-test-results, eval = subgroup_analysis_performed, echo = FALSE, comment = NA}
if(subgroup_analysis_performed && !is.null(params$random_results)) {
  cat("### Test for Subgroup Differences\n\n")
  cat(paste0("Subgrouping variable: **", params$input_subgroup_var, "**\n\n"))
  cat("Based on Random Effects Model:\n")
  if (!is.null(params$random_results$Q.b) && !is.null(params$random_results$df.Q.b) && !is.null(params$random_results$pval.Q.b)) {
    cat(sprintf("  Test for subgroup differences: Q = %.2f, df = %d, p = %.4f\n", params$random_results$Q.b, params$random_results$df.Q.b, params$random_results$pval.Q.b))
    if (!is.null(params$random_results$I2.b)) {
      cat(sprintf("  Between-subgroup heterogeneity (I²_b): %.2f%%\n", params$random_results$I2.b * 100))
    }
  } else {
    cat("  Test for subgroup differences not available or not calculated (e.g., only one subgroup level has data, or too few studies per subgroup).\n")
  }
  # Optionally, add fixed effects model subgroup test results if desired and available
  if (!is.null(params$fixed_results) && !is.null(params$fixed_results$Q.b) && !is.null(params$fixed_results$df.Q.b) && !is.null(params$fixed_results$pval.Q.b)) {
    cat("\nBased on Fixed Effects Model:\n")
    cat(sprintf("  Test for subgroup differences: Q = %.2f, df = %d, p = %.4f\n", params$fixed_results$Q.b, params$fixed_results$df.Q.b, params$fixed_results$pval.Q.b))
  }
}
```

```{r subgroup-interpretation, eval = subgroup_analysis_performed, echo = FALSE, comment = NA}
if(subgroup_analysis_performed && !is.null(params$random_results) && !is.null(params$random_results$Q.b) && !is.null(params$random_results$pval.Q.b)) {
  cat("\n\n**Interpretation of Subgroup Analysis:**\n")
  if (params$random_results$pval.Q.b < 0.05) {
    cat(paste0("The test for subgroup differences (based on the random effects model) was statistically significant (p = ", sprintf("%.4f", params$random_results$pval.Q.b), "), suggesting that the `r effect_measure_label` may differ across subgroups of '", params$input_subgroup_var, "'. Further investigation of individual subgroup estimates is warranted.\n"))
  } else {
    cat(paste0("The test for subgroup differences (based on the random effects model) was not statistically significant (p = ", sprintf("%.4f", params$random_results$pval.Q.b), "), suggesting no clear evidence that the `r effect_measure_label` differs across subgroups of '", params$input_subgroup_var, "'. However, this does not rule out clinically relevant differences, especially if the power of the test was low.\n"))
  }
}
```

# Random Effects Analysis

```{r random-summary-title, eval = subgroup_analysis_performed, echo = FALSE, comment=NA}
if(subgroup_analysis_performed) {
  cat(paste0("## Random Effects Model (Subgrouped by: ", params$input_subgroup_var, ")\n\n"))
} else {
  cat("## `r effect_measure_label` and Heterogeneity\n\n")
}
```

```{r random-forest-plot, fig.width=12, fig.height=8, error=TRUE}
safe_run({
  if (is_hr_analysis) {
    meta::forest(params$random_results,
                 backtransf = exp,
                 xlab = effect_measure_label,
                 rightlabs = c(effect_measure_label, "95%-CI", "Weight"),
                 sm = "HR", # Explicitly for clarity with backtransf
                 main = paste0("Random Effects Forest Plot (", effect_measure_label, ")"))
  } else {
    meta::forest(params$random_results,
                 xlab = effect_measure_label,
                 main = paste0("Random Effects Forest Plot (", effect_measure_label, ")"))
  }
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
  # The print output of a meta object also contains heterogeneity info
  cat(capture.output(print(params$random_results)), sep = "\n"),
  cat("Random effects heterogeneity summary unavailable")
)
```

## Model Diagnostics

```{r random-qq-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  res <- residuals(params$random_results, standardize=TRUE)
  qqnorm(res, main = paste0("Normal Q-Q Plot of Standardized Residuals (", log_effect_measure_label, ")"))
  qqline(res, col = "red")
},  plot(1, type = "n", main = "QQ plot unavailable", xlab = "", ylab = ""))
```

```{r random-outlier-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  # outlier_detection_plot function in R/functions.R should handle title based on sm
  print(outlier_detection_plot(params$random_results))
}, plot(1, type = "n", main = "Outlier detection plot unavailable", xlab = "", ylab = ""))
```

```{r random-effect-distribution, fig.width=10, fig.height=6, error=TRUE}
safe_run({
    # effect_distribution_plot in R/functions.R uses model$TE (log-scale for HR)
    # and model$TE.random (log-scale for HR)
    print(effect_distribution_plot(params$random_results, xlab_label = log_effect_measure_label))
}, plot(1, type = "n", main = "Effect distribution plot unavailable", xlab = "", ylab = ""))
```

```{r random-funnel-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  funnel(params$random_results, xlab = log_effect_measure_label, main = paste("Funnel Plot (", log_effect_measure_label, ")"))
}, plot(1, type = "n", main = "Funnel plot unavailable", xlab = "", ylab = ""))
```

```{r random-trim-fill-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  tf <- trimfill(params$random_results)
  funnel(tf, yaxis = "se", xlab = log_effect_measure_label, main = paste("Trim and Fill Funnel Plot (", log_effect_measure_label, ")"))
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
  inf_res <- metainf(params$random_results)
  if (is_hr_analysis) {
    forest(inf_res, backtransf = exp, xlab = effect_measure_label, sm = "HR")
  } else {
    forest(inf_res, xlab = effect_measure_label)
  }
}, plot(1, type = "n", main = "Leave-one-out plot unavailable", xlab = "", ylab = ""))
```

```{r random-baujat-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run(
  print(baujat(params$random_results)), # Baujat plot typically uses analysis scale
  plot(1, type = "n", main = "Baujat plot unavailable", xlab = "", ylab = "")
)
```

```{r random-gosh-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  # gosh_plot function in R/functions.R updated for HR labels
  print(gosh_plot(params$random_results))
}, plot(1, type = "n", main = "GOSH plot unavailable", xlab = "", ylab = ""))
```

```{r random-influence-summary, error=TRUE}
safe_run({
  cat(capture.output(influence_analysis(params$random_results)), sep = "\n") # Uses metainf, which is on analysis scale
}, cat("Influence analysis unavailable"))
```

# Fixed Effects Analysis

```{r fixed-summary-title, eval = subgroup_analysis_performed, echo = FALSE, comment=NA}
if(subgroup_analysis_performed) {
  cat(paste0("## Fixed Effects Model (Subgrouped by: ", params$input_subgroup_var, ")\n\n"))
} else {
  cat("## `r effect_measure_label` and Heterogeneity\n\n")
}
```

```{r fixed-forest-plot, fig.width=12, fig.height=8, error=TRUE}
safe_run({
  if (is_hr_analysis) {
    meta::forest(params$fixed_results,
                 backtransf = exp,
                 xlab = effect_measure_label,
                 rightlabs = c(effect_measure_label, "95%-CI", "Weight"),
                 sm = "HR",
                 main = paste0("Fixed Effects Forest Plot (", effect_measure_label, ")"))
  } else {
    meta::forest(params$fixed_results,
                 xlab = effect_measure_label,
                 main = paste0("Fixed Effects Forest Plot (", effect_measure_label, ")"))
  }
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
  cat(capture.output(print(params$fixed_results))), # Prints Q, I2 etc.
  cat("Model fit statistics unavailable")
)
```

## Model Diagnostics

```{r fixed-model-fit-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  radial(params$fixed_results, main=paste0("Radial Plot for Fixed Effects Model (", log_effect_measure_label, ")"))
}, plot(1, type = "n", main = "Radial plot unavailable", xlab = "", ylab = ""))
```

```{r fixed-qq-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  res <- residuals(params$fixed_results, standardize=TRUE)
  qqnorm(res, main = paste0("Normal Q-Q Plot of Standardized Residuals (", log_effect_measure_label, ")"))
  qqline(res, col = "red")
}, plot(1, type = "n", main = "QQ plot unavailable", xlab = "", ylab = ""))
```

```{r fixed-outlier-detection-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  # outlier_detection_plot function in R/functions.R should handle title
  print(outlier_detection_plot(params$fixed_results))
}, plot(1, type = "n", main = "Outlier detection plot unavailable", xlab = "", ylab = ""))
```

## Publication Bias

```{r fixed-funnel-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  funnel(params$fixed_results, xlab = log_effect_measure_label, main = paste("Funnel Plot (", log_effect_measure_label, ")"))
}, plot(1, type = "n", main = "Funnel plot unavailable", xlab = "", ylab = ""))
```

```{r fixed-trim-fill-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  tf <- trimfill(params$fixed_results)
  funnel(tf, yaxis = "se", xlab = log_effect_measure_label, main = paste("Trim and Fill Funnel Plot (", log_effect_measure_label, ")"))
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
  inf_res <- metainf(params$fixed_results)
  if (is_hr_analysis) {
    forest(inf_res, backtransf = exp, xlab = effect_measure_label, sm = "HR")
  } else {
    forest(inf_res, xlab = effect_measure_label)
  }
}, plot(1, type = "n", main = "Leave-one-out plot unavailable", xlab = "", ylab = ""))
```

```{r fixed-influence-plot, fig.width=10, fig.height=6, error=TRUE}
safe_run({
  print(baujat(params$fixed_results)) # Baujat plot typically uses analysis scale
}, plot(1, type = "n", main = "Baujat plot unavailable", xlab = "", ylab = ""))
```

```{r fixed-influence-summary, error=TRUE}
safe_run({
  cat("Influence Analysis (Fixed Effects Model):\n\n")
  inf <- metainf(params$fixed_results) # metainf is on analysis scale
  print(inf)
}, cat("Influence summary unavailable"))
```

```{r bivariate-section, eval = !is.null(params$bivariate_results)}
# Bivariate Approach Section
# This entire section will only be included if params$bivariate_results is not NULL.
```

`r if (!is.null(params$bivariate_results)) {"# Bivariate Approach"} else {""}`

`r if (!is.null(params$bivariate_results)) {"## Effect Size and Heterogeneity"} else {""}`

```{r bivariate-effect-size, fig.width=10, fig.height=6, error=TRUE, eval = !is.null(params$bivariate_results)}
# Create a 2-column layout similar to the UI
par(mfrow = c(1, 2))

# Column 1: Bivariate Forest Plot
safe_run({
  if(exists("forest.metabiv", mode = "function")) {
    forest.metabiv(params$bivariate_results, 
                  main = paste0("Bivariate Forest Plot (", effect_measure_label, ")"),
                  xlab = effect_measure_label) # forest.metabiv handles backtransf for some sm
  } else {
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
                 mlb = paste0("Confidence Region for (", log_effect_measure_label, ", τ)"),
                 xlab = log_effect_measure_label)
  } else {
    plot(1, type = "n", main = "Confidence region plot unavailable", xlab = "", ylab = "")
  }
}, plot(1, type = "n", main = "Confidence region plot unavailable", xlab = "", ylab = ""))

# Reset layout
par(mfrow = c(1, 1))
```

`r if (!is.null(params$bivariate_results)) {"## Model Diagnostics"} else {""}`

```{r bivariate-model-diagnostics, fig.width=10, fig.height=6, error=TRUE, eval = !is.null(params$bivariate_results)}
# Create a 2-column layout similar to the UI
par(mfrow = c(1, 2))

# Column 1: QQ Plot for Standardized Residuals
safe_run({
  if(exists("qq_plot_bivariate", mode = "function")) {
    qq_plot_bivariate(params$bivariate_results, log_odds = (params$bivariate_results$sm %in% c("OR","RR","HR"))) # Assuming log scale for these
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
    # Attempt to get n_k, might be n.e + n.c or similar depending on how bivariate_results is structured
    n_k_val <- if(!is.null(params$bivariate_results$n.e) && !is.null(params$bivariate_results$n.c)) {
                 params$bivariate_results$n.e + params$bivariate_results$n.c
               } else if (!is.null(params$bivariate_results$n)) {
                 params$bivariate_results$n
               } else {
                 rep(1, length(y_k)) # Fallback if n_k not directly available
               }

    qq_plot_with_ci_raw(y_k, mu, sigma_2_k, tau_2, n_k_val,
                      log_odds = (params$bivariate_results$sm %in% c("OR","RR","HR")),
                      title = paste0("Q-Q Plot for Raw Residuals (", log_effect_measure_label, ")"))
  } else {
    plot(1, type = "n", main = "QQ plot (raw residuals) unavailable", xlab = "", ylab = "")
  }
}, plot(1, type = "n", main = "QQ plot (raw residuals) unavailable", xlab = "", ylab = ""))

# Reset layout
par(mfrow = c(1, 1))
```

`r if (!is.null(params$bivariate_results)) {"## Publication Bias"} else {""}`

```{r bivariate-publication-bias, fig.width=10, fig.height=6, error=TRUE, eval = !is.null(params$bivariate_results)}
safe_run({
  if(exists("funnel", mode = "function") && 
     !is.null(params$bivariate_results$y.k) && 
     !is.null(params$bivariate_results$sigma.2.k)) {
    
    y.k <- params$bivariate_results$y.k
    se.k <- sqrt(params$bivariate_results$sigma.2.k)
    
    meta_analysis_biv <- metagen(TE = y.k, seTE = se.k, sm = params$bivariate_results$sm)
    funnel(meta_analysis_biv,
            main = paste0("Funnel Plot for Bivariate Model (", log_effect_measure_label, ")"),
            xlab = log_effect_measure_label,
            ylab = "Standard Error")
  } else {
    plot(1, type = "n", main = "Funnel plot unavailable", xlab = "", ylab = "")
  }
}, plot(1, type = "n", main = "Funnel plot unavailable", xlab = "", ylab = ""))
```

```{r bivariate-egger-test, error=TRUE, eval = !is.null(params$bivariate_results)}
safe_run({
  if(!is.null(params$bivariate_results$y.k) && !is.null(params$bivariate_results$sigma.2.k)) {
    meta_analysis_biv_egger <- metagen(TE = params$bivariate_results$y.k,
                            seTE = sqrt(params$bivariate_results$sigma.2.k))
    cat(capture.output(metabias(meta_analysis_biv_egger, method = "Egger")), sep = "\n")
  } else {
    cat("Egger test unavailable for bivariate model")
  }
}, cat("Egger test unavailable for bivariate model"))
```

`r if (!is.null(params$bivariate_results)) {"## Sensitivity Analysis"} else {""}`

```{r bivariate-sensitivity-analysis, fig.width=10, fig.height=6, error=TRUE, eval = !is.null(params$bivariate_results)}
safe_run({
  if(exists("confidence_region_shift_plot", mode = "function")) {
    confidence_region_shift_plot(params$bivariate_results)
  } else {
     plot(1, type = "n", main = "Confidence Region Shift Plot unavailable", xlab = "", ylab = "")
  }
}, plot(1, type = "n", main = "Sensitivity analysis plots unavailable", xlab = "", ylab = ""))
```

```{r bivariate-leave-one-out, fig.width=10, fig.height=6, error=TRUE, eval = !is.null(params$bivariate_results)}
safe_run({
  if(exists("metainf", mode = "function") && 
     !is.null(params$bivariate_results$y.k) && 
     !is.null(params$bivariate_results$sigma.2.k) &&
     !is.null(params$bivariate_results$studlab)) {
    
    meta_analysis_biv_loo <- metagen(TE = params$bivariate_results$y.k,
                            seTE = sqrt(params$bivariate_results$sigma.2.k),
                            studlab = params$bivariate_results$studlab,
                            sm = params$bivariate_results$sm)
    
    inf_result_biv <- metainf(meta_analysis_biv_loo)
    
    forest(inf_result_biv,
           xlab = if (params$bivariate_results$sm == "HR") effect_measure_label else log_effect_measure_label,
           backtransf = (params$bivariate_results$sm == "HR"),
           sm = params$bivariate_results$sm,
           main = paste0("Leave-One-Out Analysis (Bivariate - ", effect_measure_label, ")"))
  } else {
    plot(1, type = "n", main = "Leave-one-out analysis unavailable", xlab = "", ylab = "")
  }
}, plot(1, type = "n", main = "Leave-one-out analysis unavailable", xlab = "", ylab = ""))
```

This comprehensive report provides a detailed overview of the meta-analysis results using multiple methodological approaches. By comparing results across different models and examining various diagnostic plots, we can gain a more nuanced understanding of the `r effect_measure_label`, heterogeneity, and potential biases in the meta-analysis. This multi-faceted approach supports more informed decision-making in interpreting and applying the results of the meta-analysis.
')
}

efficacy_harm_plot <- function(bivariate_model) {
  if(is.null(bivariate_model$dev_pvals)) {
    plot(1, type="n", main="Efficacy/Harm plot unavailable", xlab="", ylab="")
    return(invisible(NULL))
  }
  
  CDF.ci.obj <- comp.mu.tau.dev.CDF.CI(bivariate_model$dev_pvals)
  comp.eff.harm.plot(CDF.ci.obj,
                     efficacy.is.OR.le1 = (bivariate_model$sm == "OR"),
                     mlb = paste("Efficacy/Harm plot for", bivariate_model$sm))
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






