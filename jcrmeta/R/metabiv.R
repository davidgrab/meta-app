#' Bivariate Meta-Analysis for Binary Outcomes
#'
#' This package implements the methodology described in Saad et al. (2019)
#' for conducting bivariate meta-analysis in the presence of unexplained heterogeneity.
#'
#' @title Bivariate Meta-Analysis
#' @description Performs a bivariate meta-analysis for binary outcomes
#' @param event.e A numeric vector of event counts in the experimental group
#' @param n.e A numeric vector of sample sizes in the experimental group
#' @param event.c A numeric vector of event counts in the control group
#' @param n.c A numeric vector of sample sizes in the control group
#' @param studlab An optional character vector of study labels
#' @param data An optional data frame containing the variables
#' @param sm A character string specifying the summary measure ("RR", "OR", or "SMD")
#' @param y An optional numeric vector of pre-calculated effect sizes
#' @param sigma2 An optional numeric vector of pre-calculated variances
#' @param level Confidence level for individual studies
#' @param level.ma Confidence level for meta-analysis
#' @param verbose Logical, whether to print verbose output
#' @return A list with class "metabiv" containing the results of the bivariate meta-analysis
#' @examples
#' # Example with binary data
#' event.e <- c(10, 15, 20)
#' n.e <- c(100, 120, 150)
#' event.c <- c(5, 8, 12)
#' n.c <- c(100, 110, 140)
#' result <- metabiv(event.e, n.e, event.c, n.c, sm = "OR")
#' @export
metabiv <- function(event.e = NULL, n.e = NULL, event.c = NULL, n.c = NULL, studlab = NULL,
                    data = NULL, sm = "RR", y = NULL, sigma2 = NULL,
                    level = 0.95, level.ma = 0.95, verbose = TRUE) {
  
  # Helper function for conditional logging
  log_if_verbose <- function(...) {
    if (verbose) {
      message(paste(..., collapse = ""))
    }
  }
  
  if (is.null(y) || is.null(sigma2)) {
    # Data preparation for binary data
    if (!is.null(data)) {
      event.e <- data[[deparse(substitute(event.e))]]
      n.e <- data[[deparse(substitute(n.e))]]
      event.c <- data[[deparse(substitute(event.c))]]
      n.c <- data[[deparse(substitute(n.c))]]
      if (!is.null(studlab))
        studlab <- data[[deparse(substitute(studlab))]]
    }
    
    k.All <- length(event.e)
    if (is.null(studlab))
      studlab <- paste("Study", seq_len(k.All))
    
    # Input validation for binary data
    if (any(event.e < 0) || any(event.c < 0)) {
      stop("Event counts cannot be negative")
    }
    if (any(n.e <= 0) || any(n.c <= 0)) {
      stop("Sample sizes must be positive")
    }
    if (any(event.e > n.e) || any(event.c > n.c)) {
      stop("Event counts cannot exceed sample sizes")
    }
    
    data.tbl <- data.frame(studlab, event.e, n.e, event.c, n.c)
    
    # Calculate effect sizes and variances for binary data
    res <- switch(sm,
                  "RR" = log_rr(data.tbl),
                  "OR" = log_or(data.tbl),
                  stop("Unsupported summary measure for binary data"))
    y.k <- res[[1]]
    sigma.2.k <- res[[2]]
    
  } else {
    # Use pre-calculated effect sizes and variances for SMD data
    y.k <- y
    sigma.2.k <- sigma2
    k.All <- length(y.k)
    if (is.null(studlab))
      studlab <- paste("Study", seq_len(k.All))
    
    data.tbl <- data.frame(studlab, y.k, sigma.2.k)
  }
  
  # Print individual study effects for comparison with supplementary material
  log_if_verbose("\n=== MLE-BASED META-ANALYSIS ESTIMATES ===")
  log_if_verbose("Summary Measure: ", sm)
  log_if_verbose("Number of studies: ", k.All, "\n")
  
  if (sm %in% c("OR", "RR")) {
    log_if_verbose("INDIVIDUAL STUDY EFFECTS:")
    log_if_verbose("========================")
    log_if_verbose("LOG-SCALE (for literature comparison):")
    log_if_verbose("--------------------------------------")
    for(i in 1:length(y.k)) {
      log_if_verbose(sprintf("Study %2d (%s): log-%s = %8.4f, SE = %6.4f, Var = %8.6f", 
                  i, studlab[i], sm, y.k[i], sqrt(sigma.2.k[i]), sigma.2.k[i]))
    }
    log_if_verbose("\nORIGINAL SCALE (for clinical interpretation):")
    log_if_verbose("---------------------------------------------")
    for(i in 1:length(y.k)) {
      log_if_verbose(sprintf("Study %2d (%s): %s = %8.4f, 95%% CI: [%6.4f, %6.4f]", 
                  i, studlab[i], sm, exp(y.k[i]), 
                  exp(y.k[i] - 1.96 * sqrt(sigma.2.k[i])), 
                  exp(y.k[i] + 1.96 * sqrt(sigma.2.k[i]))))
    }
  } else {
    log_if_verbose("INDIVIDUAL STUDY SMD EFFECTS:")
    log_if_verbose("============================")
    for(i in 1:length(y.k)) {
      log_if_verbose(sprintf("Study %2d (%s): SMD = %8.4f, SE = %6.4f, 95%% CI: [%6.4f, %6.4f]", 
                  i, studlab[i], y.k[i], sqrt(sigma.2.k[i]),
                  y.k[i] - 1.96 * sqrt(sigma.2.k[i]),
                  y.k[i] + 1.96 * sqrt(sigma.2.k[i])))
    }
  }
  
  # Estimate tau and mu using MLE
  # Improved initial value calculation using DerSimonian-Laird
  w.k <- 1 / sigma.2.k
  mu.init <- sum(w.k * y.k) / sum(w.k)
  Q.init <- sum(w.k * (y.k - mu.init)^2)
  df.init <- k.All - 1
  tau2.init <- max(0, (Q.init - df.init) / (sum(w.k) - sum(w.k^2)/sum(w.k)))
  initial.value <- c(mu.init, sqrt(tau2.init))
  
  log_if_verbose("\nINITIAL VALUES (DerSimonian-Laird):")
  log_if_verbose("===================================")
  if (sm %in% c("OR", "RR")) {
    log_if_verbose(sprintf("mu_initial (log-%s) = %8.6f  ->  %s_initial = %8.4f", 
                sm, mu.init, sm, exp(mu.init)))
    log_if_verbose(sprintf("tau_initial (log-scale) = %8.6f", sqrt(tau2.init)))
  } else {
    log_if_verbose(sprintf("mu_initial (SMD) = %8.6f", mu.init))
    log_if_verbose(sprintf("tau_initial = %8.6f", sqrt(tau2.init)))
  }

  mle_result <- comp_tau_mu_mle(data.tbl, initial.value, sm, y.k, sigma.2.k)
  mu <- mle_result$mu
  tau <- mle_result$tau
  tau2 <- tau^2
  
  # Calculate confidence intervals
  ci_level <- level
  z_score <- qnorm((1 + ci_level) / 2)
  
  # Individual study CIs (on original scale for both SMD and log OR/RR)
  lower.k <- y.k - z_score * sqrt(pmax(sigma.2.k, 0))
  upper.k <- y.k + z_score * sqrt(pmax(sigma.2.k, 0))
  
  # Overall effect CI (on original scale for both SMD and log OR/RR)
  se.mu <- sqrt(1 / sum(1 / (sigma.2.k + tau2)))
  lower <- mu - z_score * se.mu
  upper <- mu + z_score * se.mu
  
  # Print comprehensive MLE estimates
  log_if_verbose("\nMLE ESTIMATES:")
  log_if_verbose("==============")
  
  if (sm %in% c("OR", "RR")) {
    log_if_verbose("RAW LOG-SCALE (for literature comparison):")
    log_if_verbose("------------------------------------------")
    log_if_verbose(sprintf("mu (log-%s) = %10.6f", sm, mu))
    log_if_verbose(sprintf("tau (log-scale) = %10.6f", tau))
    log_if_verbose(sprintf("tau^2 (log-scale) = %10.6f", tau2))
    log_if_verbose(sprintf("SE(mu) (log-scale) = %10.6f", se.mu))
    log_if_verbose(sprintf("95%% CI for mu (log-%s): [%8.6f, %8.6f]", sm, lower, upper))
    
    log_if_verbose("\nTRANSFORMED ORIGINAL SCALE (for clinical interpretation):")
    log_if_verbose("--------------------------------------------------------")
    log_if_verbose(sprintf("mu -> %s estimate = exp(%8.6f) = %8.4f", sm, mu, exp(mu)))
    log_if_verbose(sprintf("95%% CI for %s: [%6.4f, %6.4f]", sm, exp(lower), exp(upper)))
    
    # Calculate approximate CI for tau on original scale
    # Note: This is an approximation since tau represents log-scale heterogeneity
    log_if_verbose(sprintf("tau (between-study SD on log-scale) = %8.6f", tau))
    log_if_verbose(sprintf("tau^2 (between-study variance on log-scale) = %8.6f", tau2))
    
    # For interpretation: what does tau mean in terms of RR/OR variability?
    log_if_verbose(sprintf("Interpretation: 95%% of true study %ss lie approximately between:", sm))
    log_if_verbose(sprintf("  Lower bound ~= %6.4f", exp(mu - 1.96 * tau)))
    log_if_verbose(sprintf("  Upper bound ~= %6.4f", exp(mu + 1.96 * tau)))
    
  } else {
    log_if_verbose("SMD ESTIMATES:")
    log_if_verbose("--------------")
    log_if_verbose(sprintf("mu (SMD) = %10.6f", mu))
    log_if_verbose(sprintf("tau = %10.6f", tau))
    log_if_verbose(sprintf("tau^2 = %10.6f", tau2))
    log_if_verbose(sprintf("SE(mu) = %10.6f", se.mu))
    log_if_verbose(sprintf("95%% CI for mu (SMD): [%8.6f, %8.6f]", lower, upper))
    log_if_verbose(sprintf("95%% of true study SMDs lie approximately between: [%6.4f, %6.4f]", 
                mu - 1.96 * tau, mu + 1.96 * tau))
  }
  
  # Calculate Q statistic
  w <- 1 / pmax(sigma.2.k, 1e-10)
  Q <- sum(w * (y.k - mu)^2)
  df <- k.All - 1
  
  # Calculate p-value for Q
  p_value <- 1 - pchisq(Q, df)
  
  # Calculate I2
  I2 <- max(0, 100 * (Q - df) / Q)
  
  # Calculate H2
  H2 <- Q / df
  
  log_if_verbose("\nHETEROGENEITY STATISTICS:")
  log_if_verbose("========================")
  log_if_verbose(sprintf("Q = %8.4f (df = %d, p = %6.4f)", Q, df, p_value))
  log_if_verbose(sprintf("I^2 = %6.2f%%", I2))
  log_if_verbose(sprintf("H^2 = %8.4f", H2))
  
  # Calculate deviance and p-values
  # Set appropriate range for mu based on summary measure
  if (sm == "SMD") {
    # For SMD, use wider range around the MLE estimate with higher resolution
    mu_range <- max(3, 1.5 * max(abs(c(y.k, mu))))
    mu.vec <- seq(-mu_range, mu_range, length.out = 150)  # Higher resolution for smoother contours
    tau.vec <- seq(0.01, 1, length.out = 150)  # Higher resolution for smoother contours
  } else {
    # For OR/RR, use traditional log scale range
  mu.vec <- seq(-1, 1, length.out = 100)
  tau.vec <- seq(0.01, 1, length.out = 100)
  }
  
  # For all summary measures, use the standard chi-squared approximation.
  # The exact test for OR is deferred as a future improvement.
  dev_pvals <- comp_tau_mu_dev_pvals(data.tbl, mu.vec, tau.vec, sm, y.k.in = y.k, sigma.2.k.in = sigma.2.k)
  
  # Compute confidence region
  conf_region <- compute_confidence_region(dev_pvals[[2]], level.ma)
  
  # Print confidence region bounds
  log_if_verbose("\nCONFIDENCE REGION BOUNDS:")
  log_if_verbose("========================")
  
  if (sm %in% c("OR", "RR")) {
    log_if_verbose("RAW LOG-SCALE:")
    log_if_verbose("--------------")
    log_if_verbose(sprintf("mu (log-%s) range: [%8.6f, %8.6f]", sm, min(conf_region$mu), max(conf_region$mu)))
    log_if_verbose(sprintf("tau range: [%8.6f, %8.6f]", min(conf_region$tau), max(conf_region$tau)))
    
    log_if_verbose("\nTRANSFORMED ORIGINAL SCALE:")
    log_if_verbose("---------------------------")
    log_if_verbose(sprintf("%s range: [%6.4f, %6.4f]", sm, exp(min(conf_region$mu)), exp(max(conf_region$mu))))
    log_if_verbose(sprintf("tau range (log-scale): [%6.4f, %6.4f]", min(conf_region$tau), max(conf_region$tau)))
  } else {
    log_if_verbose("SMD SCALE:")
    log_if_verbose("----------")
    log_if_verbose(sprintf("mu (SMD) range: [%8.6f, %8.6f]", min(conf_region$mu), max(conf_region$mu)))
    log_if_verbose(sprintf("tau range: [%8.6f, %8.6f]", min(conf_region$tau), max(conf_region$tau)))
  }
  
  # Summary comparison table
  log_if_verbose("\nSUMMARY FOR COMPARISON WITH OTHER META-ANALYSES:")
  log_if_verbose("================================================")
  
  if (sm %in% c("OR", "RR")) {
    log_if_verbose(sprintf("Pooled %s estimate: %6.4f (95%% CI: %6.4f to %6.4f)", 
                sm, exp(mu), exp(lower), exp(upper)))
    log_if_verbose(sprintf("Between-study heterogeneity (tau): %6.4f (on log-scale)", tau))
    log_if_verbose(sprintf("I^2 = %5.1f%%, indicating %s heterogeneity", I2, 
                ifelse(I2 < 25, "low", ifelse(I2 < 50, "moderate", ifelse(I2 < 75, "substantial", "considerable")))))
  } else {
    log_if_verbose(sprintf("Pooled SMD estimate: %6.4f (95%% CI: %6.4f to %6.4f)", 
                mu, lower, upper))
    log_if_verbose(sprintf("Between-study heterogeneity (tau): %6.4f", tau))
    log_if_verbose(sprintf("I^2 = %5.1f%%, indicating %s heterogeneity", I2, 
                ifelse(I2 < 25, "low", ifelse(I2 < 50, "moderate", ifelse(I2 < 75, "substantial", "considerable")))))
  }
  
  log_if_verbose("\n=== END MLE ESTIMATES ===\n")
  
  # Prepare results
  res <- list(
    studlab = studlab,
    event.e = if(!is.null(y)) NULL else event.e, 
    n.e = if(!is.null(y)) NULL else n.e,
    event.c = if(!is.null(y)) NULL else event.c, 
    n.c = if(!is.null(y)) NULL else n.c,
    y.k = y.k,
    sigma.2.k = sigma.2.k,
    sm = sm,
    level = level,
    level.ma = level.ma,
    mu = mu,
    tau = tau,
    tau2 = tau2,
    lower = lower,
    upper = upper,
    lower.k = lower.k,
    upper.k = upper.k,
    Q = Q,
    df = df,
    pval.Q = p_value,
    I2 = I2,
    H2 = H2,
    w = w,
    dev_pvals = dev_pvals,
    conf_region = conf_region,
    call = match.call(),
    tbl = data.tbl,
    n_k = if(!is.null(y)) rep(NA, k.All) else n.c+n.e
    
  )
  
  class(res) <- "metabiv"
  res
}