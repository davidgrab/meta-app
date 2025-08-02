# metabiv: Bivariate Meta-Analysis for Binary Outcomes
# This package implements the methodology described in Saad et al. (2019)
# for conducting bivariate meta-analysis in the presence of unexplained heterogeneity.

# Required libraries
library(meta)
library(ggplot2)
library(plotly)
library(metafor)
library(BiasedUrn)
library(sf)

# Replace all cat() statements with console_log() function
console_log <- function(...) {
  # This sends output directly to the R console/server logs, not to Shiny UI
  message(paste(..., collapse = ""))
}

#' @title Log Odds Function
#' @description Calculates the log odds from a probability
#' @param p A probability value
#' @return The log odds of the input probability
#' @export
log.odds <- function(p) log(p / (1-p))

#' @title Inverse Log Odds Function
#' @description Calculates the probability from log odds
#' @param theta A log odds value
#' @return The probability corresponding to the input log odds
#' @export
inv.log.odds <- function(theta) exp(theta) / (1 + exp(theta))

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
    
    # Remove rows with missing values
    complete_cases <- complete.cases(event.e, n.e, event.c, n.c)
    if (sum(complete_cases) == 0) {
      stop("No complete cases available for analysis")
    }
    
    event.e <- event.e[complete_cases]
    n.e <- n.e[complete_cases]
    event.c <- event.c[complete_cases]
    n.c <- n.c[complete_cases]
    studlab <- studlab[complete_cases]
    k.All <- length(event.e)  # Update k.All after filtering
    
    # Input validation for binary data
    if (any(event.e < 0, na.rm = TRUE) || any(event.c < 0, na.rm = TRUE)) {
      stop("Event counts cannot be negative")
    }
    if (any(n.e <= 0, na.rm = TRUE) || any(n.c <= 0, na.rm = TRUE)) {
      stop("Sample sizes must be positive")
    }
    if (any(event.e > n.e, na.rm = TRUE) || any(event.c > n.c, na.rm = TRUE)) {
      stop("Event counts cannot exceed sample sizes")
    }
    
    data.tbl <- data.frame(studlab, event.e, n.e, event.c, n.c)
    
    # Calculate effect sizes and variances for binary data
    res <- switch(sm,
                  "RR" = comp.log.RR.y.sigma.stats(data.tbl),
                  "OR" = comp.log.OR.y.sigma.stats(data.tbl),
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
    log_if_verbose(sprintf("μ_initial (log-%s) = %8.6f  →  %s_initial = %8.4f", 
                sm, mu.init, sm, exp(mu.init)))
    log_if_verbose(sprintf("τ_initial (log-scale) = %8.6f", sqrt(tau2.init)))
  } else {
    log_if_verbose(sprintf("μ_initial (SMD) = %8.6f", mu.init))
    log_if_verbose(sprintf("τ_initial = %8.6f", sqrt(tau2.init)))
  }

  mle_result <- comp.tau.mu.MLE(data.tbl, initial.value, sm, y.k, sigma.2.k)
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
    log_if_verbose(sprintf("μ (log-%s) = %10.6f", sm, mu))
    log_if_verbose(sprintf("τ (log-scale) = %10.6f", tau))
    log_if_verbose(sprintf("τ² (log-scale) = %10.6f", tau2))
    log_if_verbose(sprintf("SE(μ) (log-scale) = %10.6f", se.mu))
    log_if_verbose(sprintf("95%% CI for μ (log-%s): [%8.6f, %8.6f]", sm, lower, upper))
    
    log_if_verbose("\nTRANSFORMED ORIGINAL SCALE (for clinical interpretation):")
    log_if_verbose("--------------------------------------------------------")
    log_if_verbose(sprintf("μ → %s estimate = exp(%8.6f) = %8.4f", sm, mu, exp(mu)))
    log_if_verbose(sprintf("95%% CI for %s: [%6.4f, %6.4f]", sm, exp(lower), exp(upper)))
    
    # Calculate approximate CI for tau on original scale
    # Note: This is an approximation since tau represents log-scale heterogeneity
    log_if_verbose(sprintf("τ (between-study SD on log-scale) = %8.6f", tau))
    log_if_verbose(sprintf("τ² (between-study variance on log-scale) = %8.6f", tau2))
    
    # For interpretation: what does tau mean in terms of RR/OR variability?
    log_if_verbose(sprintf("Interpretation: 95%% of true study %ss lie approximately between:", sm))
    log_if_verbose(sprintf("  Lower bound ≈ %6.4f", exp(mu - 1.96 * tau)))
    log_if_verbose(sprintf("  Upper bound ≈ %6.4f", exp(mu + 1.96 * tau)))
    
  } else {
    log_if_verbose("SMD ESTIMATES:")
    log_if_verbose("--------------")
    log_if_verbose(sprintf("μ (SMD) = %10.6f", mu))
    log_if_verbose(sprintf("τ = %10.6f", tau))
    log_if_verbose(sprintf("τ² = %10.6f", tau2))
    log_if_verbose(sprintf("SE(μ) = %10.6f", se.mu))
    log_if_verbose(sprintf("95%% CI for μ (SMD): [%8.6f, %8.6f]", lower, upper))
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
  log_if_verbose(sprintf("I² = %6.2f%%", I2))
  log_if_verbose(sprintf("H² = %8.4f", H2))
  
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
  dev_pvals <- comp.tau.mu.dev.pvals(data.tbl, mu.vec, tau.vec, sm, y.k.in = y.k, sigma.2.k.in = sigma.2.k)
  
  # Compute confidence region
  conf_region <- compute_confidence_region(dev_pvals[[2]], level.ma)
  
  # Print confidence region bounds
  log_if_verbose("\nCONFIDENCE REGION BOUNDS:")
  log_if_verbose("========================")
  
  if (sm %in% c("OR", "RR")) {
    log_if_verbose("RAW LOG-SCALE:")
    log_if_verbose("--------------")
    log_if_verbose(sprintf("μ (log-%s) range: [%8.6f, %8.6f]", sm, min(conf_region$mu), max(conf_region$mu)))
    log_if_verbose(sprintf("τ range: [%8.6f, %8.6f]", min(conf_region$tau), max(conf_region$tau)))
    
    log_if_verbose("\nTRANSFORMED ORIGINAL SCALE:")
    log_if_verbose("---------------------------")
    log_if_verbose(sprintf("%s range: [%6.4f, %6.4f]", sm, exp(min(conf_region$mu)), exp(max(conf_region$mu))))
    log_if_verbose(sprintf("τ range (log-scale): [%6.4f, %6.4f]", min(conf_region$tau), max(conf_region$tau)))
  } else {
    log_if_verbose("SMD SCALE:")
    log_if_verbose("----------")
    log_if_verbose(sprintf("μ (SMD) range: [%8.6f, %8.6f]", min(conf_region$mu), max(conf_region$mu)))
    log_if_verbose(sprintf("τ range: [%8.6f, %8.6f]", min(conf_region$tau), max(conf_region$tau)))
  }
  
  # Summary comparison table
  log_if_verbose("\nSUMMARY FOR COMPARISON WITH OTHER META-ANALYSES:")
  log_if_verbose("================================================")
  
  if (sm %in% c("OR", "RR")) {
    log_if_verbose(sprintf("Pooled %s estimate: %6.4f (95%% CI: %6.4f to %6.4f)", 
                sm, exp(mu), exp(lower), exp(upper)))
    log_if_verbose(sprintf("Between-study heterogeneity (τ): %6.4f (on log-scale)", tau))
    log_if_verbose(sprintf("I² = %5.1f%%, indicating %s heterogeneity", I2, 
                ifelse(I2 < 25, "low", ifelse(I2 < 50, "moderate", ifelse(I2 < 75, "substantial", "considerable")))))
  } else {
    log_if_verbose(sprintf("Pooled SMD estimate: %6.4f (95%% CI: %6.4f to %6.4f)", 
                mu, lower, upper))
    log_if_verbose(sprintf("Between-study heterogeneity (τ): %6.4f", tau))
    log_if_verbose(sprintf("I² = %5.1f%%, indicating %s heterogeneity", I2, 
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
    var.k = sigma.2.k,
    sm = sm,
    level = level,
    level.ma = level.ma,
    mu = mu,
    tau = tau,
    tau2 = tau2,
    lower.mu = lower,
    upper.mu = upper,
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

#' @title Compute Log Risk Ratio Statistics
#' @description Calculates the log risk ratio and its variance
#' @param data.tbl A data frame containing the study data
#' @return A list containing the log risk ratios and their variances
#' @export
comp.log.RR.y.sigma.stats <- function(data.tbl) {
  n.11 <- data.tbl$event.e
  n.21 <- data.tbl$event.c
  n.12 <- data.tbl$n.e - data.tbl$event.e
  n.22 <- data.tbl$n.c - data.tbl$event.c
  pls.5 <- 0.5 * (n.11 == 0 | n.12 == 0 | n.21 == 0 | n.22 == 0)
  pi.1g1 <- (n.11 + pls.5) / (n.11 + n.12 + 2 * pls.5)
  pi.1g2 <- (n.21 + pls.5) / (n.21 + n.22 + 2 * pls.5)
  sigma.2.k <- (1 - pi.1g1) / (pi.1g1 * (n.11 + n.12 + 2 * pls.5)) +
    (1 - pi.1g2) / (pi.1g2 * (n.21 + n.22 + 2 * pls.5))
  y.k <- log(pi.1g1 / pi.1g2)
  
  return(list(y.k, sigma.2.k))
}

#' @title Compute Log Odds Ratio Statistics
#' @description Calculates the log odds ratio and its variance
#' @param data.tbl A data frame containing the study data
#' @return A list containing the log odds ratios and their variances
#' @export
comp.log.OR.y.sigma.stats <- function(data.tbl) {
  n.11 <- data.tbl$event.e
  n.21 <- data.tbl$event.c
  n.12 <- data.tbl$n.e - data.tbl$event.e
  n.22 <- data.tbl$n.c - data.tbl$event.c
  pls.5 <- 0.5 * (n.11 == 0 | n.12 == 0 | n.21 == 0 | n.22 == 0)
  y.k <- log((n.11 + pls.5) * (n.22 + pls.5) / ((n.12 + pls.5) * (n.21 + pls.5)))
  sigma.2.k <- 1 / (n.11 + pls.5) + 1 / (n.12 + pls.5) + 1 / (n.21 + pls.5) + 1 / (n.22 + pls.5)
  
  return(list(y.k, sigma.2.k))
}

#' @title Maximum Likelihood Estimation
#' @description Performs maximum likelihood estimation for the bivariate model
#' @param data.tbl A data frame containing the study data
#' @param initial.value Initial values for the optimization
#' @param sm Summary measure ("RR" or "OR")
#' @param y.k.in Optional pre-calculated effect sizes
#' @param sigma.2.k.in Optional pre-calculated variances
#' @return A list containing the MLE estimates for mu and tau
#' @export
comp.tau.mu.MLE <- function(data.tbl, initial.value, sm, y.k.in = NULL, sigma.2.k.in = NULL) {
  if (is.null(y.k.in) || is.null(sigma.2.k.in)) {
    a <- switch(sm,
                "RR" = comp.log.RR.y.sigma.stats(data.tbl),
                "OR" = comp.log.OR.y.sigma.stats(data.tbl))
    y.k <- a[[1]]
    sigma.2.k <- a[[2]]
  } else {
    y.k <- y.k.in
    sigma.2.k <- sigma.2.k.in
  }
  
  minus.loglik <- function(par.vec) {
    # Add small epsilon for stability
    sd_val <- sqrt(par.vec[2]^2 + sigma.2.k)
    if (any(is.nan(sd_val)) || any(!is.finite(sd_val))) return(Inf)
    -sum(dnorm(y.k, mean = par.vec[1], sd = sd_val, log = TRUE))
  }
  
  a <- nlminb(initial.value, minus.loglik, lower = c(-Inf, 0))
  
  return(list(mu = a$par[1], tau = a$par[2]))
}

#' @title Compute Deviance and P-values
#' @description Calculates the deviance and p-values for the bivariate model
#' @param data.tbl A data frame containing the study data
#' @param mu.vec.tst Vector of mu values to test
#' @param tau.vec.tst Vector of tau values to test
#' @param sm Summary measure ("RR" or "OR")
#' @param y.k.in Optional pre-calculated effect sizes
#' @param sigma.2.k.in Optional pre-calculated variances
#' @return A list containing the deviance matrix and p-value matrix
#' @export
comp.tau.mu.dev.pvals <- function(data.tbl, mu.vec.tst, tau.vec.tst, sm, y.k.in = NULL, sigma.2.k.in = NULL) {
  n.mu <- length(mu.vec.tst)
  n.tau <- length(tau.vec.tst)

  if (is.null(y.k.in) || is.null(sigma.2.k.in)) {
    a <- switch(sm,
                "RR" = comp.log.RR.y.sigma.stats(data.tbl),
                "OR" = comp.log.OR.y.sigma.stats(data.tbl))
    y.k <- a[[1]]
    sigma.2.k <- a[[2]]
  } else {
    y.k <- y.k.in
    sigma.2.k <- sigma.2.k.in
  }
  
  K <- length(y.k)
  y.mat.k.i <- rep(c(y.k), each = n.mu * n.tau)
  sigma.2.k.i <- rep(c(sigma.2.k), each = n.mu * n.tau)
  tau.k.i <- rep(rep(tau.vec.tst, each = n.mu), K)
  mu.k.i <- rep(mu.vec.tst, n.tau * K)
  
  loglik.vec <- dnorm(y.mat.k.i, mean = mu.k.i, sd = sqrt(tau.k.i^2 + sigma.2.k.i), log = TRUE)
  loglik.mu.tau <- c(array(loglik.vec, dim = c(n.mu * n.tau, K)) %*% cbind(rep(1, K)))
  dev.mat <- array(-2*(loglik.mu.tau - max(loglik.mu.tau)), dim=c(n.mu, n.tau))
  dimnames(dev.mat) <- list(paste("mu = ", round(mu.vec.tst, 2)), paste("tau = ", round(tau.vec.tst, 3)))
  
  pval.mat <- array(1 - pchisq(dev.mat, 2), dim = c(n.mu, n.tau))
  dimnames(pval.mat) <- dimnames(dev.mat)
  
  return(list(dev.mat, pval.mat))
}

#' @title Compute Confidence Region
#' @description Calculates the confidence region for mu and tau
#' @param pval.mat Matrix of p-values
#' @param level.ma Confidence level for meta-analysis
#' @return A list containing the mu and tau values in the confidence region
#' @export
compute_confidence_region <- function(pval.mat, level.ma) {
  conf_region <- which(pval.mat >= (1 - level.ma), arr.ind = TRUE)
  mu.vec <- as.numeric(gsub("mu = ", "", rownames(pval.mat)))
  tau.vec <- as.numeric(gsub("tau = ", "", colnames(pval.mat)))
  
  return(list(mu = mu.vec[conf_region[, 1]], tau = tau.vec[conf_region[, 2]]))
}

#' @title Compute CDF and Confidence Intervals
#' @description Calculates the CDF and its confidence intervals with clamped ranges
#' @param dev.lst A list containing the deviance and p-value matrices
#' @param N.sig Number of simulations for smoothing
#' @param alpha Significance level
#' @param min_ci Minimum value for confidence intervals (default: log(0.2))
#' @param max_ci Maximum value for confidence intervals (default: log(10))
#' @return A list containing the CDF vector, MLE CDF, and lower and upper CIs
#' @export
comp.mu.tau.dev.CDF.CI <- function(dev.lst, N.sig = 100, alpha = 0.05, 
                                  min_ci = log(0.2), max_ci = log(10), sm = NULL) {
  # Extract matrices and dimensions
  dev.mat <- dev.lst[[1]]
  pval.mat <- dev.lst[[2]]
  n.mu <- dim(pval.mat)[1]
  n.tau <- dim(pval.mat)[2]
  
  # Set appropriate ranges based on summary measure
  if (!is.null(sm) && sm == "SMD") {
    min_ci <- -3
    max_ci <- 3
  }
  
  # Extract sequences with error handling
  seq.mu <- tryCatch({
    sapply(strsplit(dimnames(pval.mat)[[1]], "mu ="), as.numeric)[2, ]
  }, error = function(e) {
    seq(min_ci, max_ci, length.out = n.mu)
  })
  
  seq.tau <- tryCatch({
    sapply(strsplit(dimnames(pval.mat)[[2]], "tau ="), as.numeric)[2, ]
  }, error = function(e) {
    seq(0.01, 1, length.out = n.tau)
  })
  
  # Clamp sequences to reasonable ranges
  seq.mu <- pmax(pmin(seq.mu, max_ci), min_ci)
  seq.tau <- pmax(pmin(seq.tau, 1), 0.01)
  
  x.mu <- rep(seq.mu, n.tau)
  x.tau <- rep(seq.tau, each = n.mu)
  
  # Find MLE with safety checks
  zero_indices <- which(dev.mat == min(dev.mat, na.rm = TRUE))
  if (length(zero_indices) > 0) {
    MLE.mu <- mean(x.mu[zero_indices], na.rm = TRUE)
    MLE.tau <- mean(x.tau[zero_indices], na.rm = TRUE)
  } else {
    MLE.mu <- mean(seq.mu, na.rm = TRUE)
    MLE.tau <- mean(seq.tau, na.rm = TRUE)
  }
  
  # Clamp MLE values
  MLE.mu <- pmax(pmin(MLE.mu, max_ci), min_ci)
  MLE.tau <- pmax(pmin(MLE.tau, 1), 0.01)
  
  # Calculate logit probabilities with bounds
  logit.p <- log.odds(pmax(pmin(c(pval.mat), 1 - 1/N.sig), 1/N.sig))
  
  # Fit loess model with error handling
  logit.p.loess <- tryCatch({
    loess(logit.p ~ x.mu + x.tau, span = 0.1, degree = 2)
  }, error = function(e) {
    # Fallback to simpler model if loess fails
    lm(logit.p ~ x.mu + x.tau)
  })
  
  # Create prediction grid with clamped ranges
  tau.pred.vec <- rep(seq(min(seq.tau), max(seq.tau), length = 200), 200)
  mu.pred.vec <- rep(seq(min(seq.mu), max(seq.mu), length = 200), each = 200)
  
  # Get smoothed probabilities
  smth.pval.mat <- tryCatch({
    inv.log.odds(predict(logit.p.loess, data.frame(x.mu = mu.pred.vec, x.tau = tau.pred.vec)))
  }, error = function(e) {
    # Fallback to simple interpolation if prediction fails
    rep(mean(inv.log.odds(logit.p), na.rm = TRUE), length(mu.pred.vec))
  })
  
  # Calculate confidence intervals
  mu.ci.vec <- mu.pred.vec[alpha < smth.pval.mat]
  tau.ci.vec <- tau.pred.vec[alpha < smth.pval.mat]
  
  # Ensure we have some values for CI
  if (length(mu.ci.vec) == 0) {
    mu.ci.vec <- c(MLE.mu - MLE.tau, MLE.mu + MLE.tau)
    tau.ci.vec <- c(MLE.tau, MLE.tau)
  }
  
  n.ci <- length(mu.ci.vec)
  
  # Calculate CDF with clamped ranges
  CDF.vec <- seq(0.01, 0.99, length = 99)
  MLE.CDF <- pmax(pmin(qnorm(CDF.vec, mean = MLE.mu, sd = MLE.tau), max_ci), min_ci)
  
  # Use the statistically correct method for both SMD and OR/RR
  # This properly accounts for uncertainty in both mu and tau parameters
  ci.CDF.mat <- array(
    pmax(pmin(
      qnorm(rep(CDF.vec, each = n.ci), 
            mean = rep(mu.ci.vec, 99), 
            sd = rep(tau.ci.vec, 99)),
      max_ci), min_ci),
    dim = c(n.ci, 99)
  )
  
  ci.CDF.ll <- apply(ci.CDF.mat, 2, min)
  ci.CDF.ul <- apply(ci.CDF.mat, 2, max)
  
  # Final safety check on bounds
  ci.CDF.ll <- pmax(ci.CDF.ll, min_ci)
  ci.CDF.ul <- pmin(ci.CDF.ul, max_ci)
  
  return(list(CDF.vec, MLE.CDF, ci.CDF.ll, ci.CDF.ul))
}

#' @title Efficacy/Harm Plot
#' @description Creates an efficacy/harm plot based on the CDF confidence intervals
#' @param CDF.ci.obj A list containing CDF and CI information
#' @param efficacy.is.OR.le1 Logical, whether efficacy is defined as OR <= 1
#' @param mlb Main label for the plot
#' @param xlb X-axis label
#' @param min.OR Minimum OR to display
#' @param max.OR Maximum OR to display
#' @return A ggplot object representing the efficacy/harm plot
#' @export
comp.eff.harm.plot <- function(CDF.ci.obj, efficacy.is.OR.le1 = TRUE, mlb = "Efficacy/Harm Plot", 
                               xlb = "Efficacy/Harm", min.OR = 0.3, max.OR = 3, sm = NULL) {
  
  # Check if we're dealing with SMD
  is_smd <- !is.null(sm) && sm == "SMD"
  
  if (is_smd) {
    # For SMD: Use linear scale centered around 0
    x.seq <- seq(-3, 3, length = 1000)
    
    # Use the CDF values directly (no exponential transformation)
    x.est.taper <- c(
      seq(-3, min(CDF.ci.obj[[2]]), length = 50),
      CDF.ci.obj[[2]],
      seq(max(CDF.ci.obj[[2]]), 3, length = 50)
    )
    
    x.ll.taper <- c(
      seq(-3, min(CDF.ci.obj[[3]]), length = 50),
      CDF.ci.obj[[3]],
      seq(max(CDF.ci.obj[[3]]), 3, length = 50)
    )
    
    x.ul.taper <- c(
      seq(-3, min(CDF.ci.obj[[4]]), length = 50),
      CDF.ci.obj[[4]],
      seq(max(CDF.ci.obj[[4]]), 3, length = 50)
    )
    
    # Safe approximation with error handling
    safe_approx <- function(x, y, xout) {
      tryCatch({
        # Remove duplicate x values before interpolation
        unique_indices <- !duplicated(x)
        x_unique <- x[unique_indices]
        y_unique <- y[unique_indices]
        
        # Ensure we have at least 2 unique points for interpolation
        if (length(x_unique) < 2) {
          return(rep(mean(y, na.rm = TRUE), length(xout)))
        }
        
        approx(x_unique, y_unique, xout = xout)$y
      }, error = function(e) {
        rep(mean(y, na.rm = TRUE), length(xout))
      })
    }
    
    # Calculate CDFs with error handling
    cdf.est <- safe_approx(x.est.taper, c(rep(0, 50), CDF.ci.obj[[1]], rep(1, 50)), x.seq)
    cdf.ll <- safe_approx(x.ll.taper, c(rep(0, 50), CDF.ci.obj[[1]], rep(1, 50)), x.seq)
    cdf.ul <- safe_approx(x.ul.taper, c(rep(0, 50), CDF.ci.obj[[1]], rep(1, 50)), x.seq)
    
    # Set colors (beneficial vs harmful for SMD)
    le0.col <- 3  # Green for beneficial (SMD < 0 if lower is better, or SMD > 0 if higher is better)
    gt0.col <- 2  # Red for harmful
    
    # Calculate values for plotting with SMD ranges
    le0.vec <- seq(-3, 0, length = 200)
    le0.est <- safe_approx(x.seq, cdf.est, xout = le0.vec)
    le0.ll <- safe_approx(x.seq, cdf.ll, xout = le0.vec)
    le0.ul <- safe_approx(x.seq, cdf.ul, xout = le0.vec)
    
    gt0.vec <- seq(0, 3, length = 200)
    gt0.est <- safe_approx(x.seq, 1-cdf.est, xout = gt0.vec)
    gt0.ll <- safe_approx(x.seq, 1-cdf.ll, xout = gt0.vec)
    gt0.ul <- safe_approx(x.seq, 1-cdf.ul, xout = gt0.vec)
    
    # Create data frame for plotting
    plot_data <- rbind(
      data.frame(x = le0.vec, y = le0.est, lower = le0.ll, upper = le0.ul, group = "le0"),
      data.frame(x = gt0.vec, y = gt0.est, lower = gt0.ll, upper = gt0.ul, group = "gt0")
    )
    
    # Remove any non-finite values
    plot_data <- plot_data[is.finite(plot_data$x) & 
                          is.finite(plot_data$y) & 
                          is.finite(plot_data$lower) & 
                          is.finite(plot_data$upper), ]
    
    # --- NEW: Focus x-axis on region where probability > threshold ---
    prob_thresh <- 0.005
    nonzero_idx <- which(plot_data$y > prob_thresh)
    if (length(nonzero_idx) > 0) {
      x_focus <- range(plot_data$x[nonzero_idx], na.rm = TRUE)
      x_margin <- diff(x_focus) * 0.05
      x_lims <- c(x_focus[1] - x_margin, x_focus[2] + x_margin)
      plot_data <- plot_data[plot_data$x >= x_lims[1] & plot_data$x <= x_lims[2], ]
    } else {
      x_lims <- range(plot_data$x, na.rm = TRUE)
    }
    # ---------------------------------------------------------------

    # Create plot with ggplot2 - linear scale for SMD with auto-scaled x-axis
    x_range <- range(plot_data$x, na.rm = TRUE)
    x_margin <- diff(x_range) * 0.1
    
    p <- ggplot(plot_data, aes(x = x, y = y, color = group, fill = group)) +
      geom_line(aes(y = y), linewidth = 1) +  # Changed from size = 1
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
      scale_x_continuous(breaks = pretty(x_lims, n = 8),
                        limits = x_lims) +
      scale_color_manual(values = c("le0" = le0.col, "gt0" = gt0.col)) +
      scale_fill_manual(values = c("le0" = le0.col, "gt0" = gt0.col)) +
      labs(title = mlb, x = xlb, y = "Probability") +
      theme_minimal() +
      theme(legend.position = "none") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray50")
    
  } else {
    # Original code for OR/RR with log scale
  x.seq <- exp(seq(-5, log(10), length = 1000))
  
  # Safely get CDF values with finite bounds
  safe_exp <- function(x) {
    ex <- exp(x)
    ex[!is.finite(ex)] <- ifelse(x[!is.finite(ex)] < 0, min.OR, max.OR)
    return(ex)
  }
  
  # Apply safe exponential transformation
  x.est.taper <- c(
    seq(min.OR, min(safe_exp(CDF.ci.obj[[2]])), length = 50),
    safe_exp(CDF.ci.obj[[2]]),
    seq(max(safe_exp(CDF.ci.obj[[2]])), max.OR, length = 50)
  )
  
  x.ll.taper <- c(
    seq(min.OR, min(safe_exp(CDF.ci.obj[[3]])), length = 50),
    safe_exp(CDF.ci.obj[[3]]),
    seq(max(safe_exp(CDF.ci.obj[[3]])), max.OR, length = 50)
  )
  
  x.ul.taper <- c(
    seq(min.OR, min(safe_exp(CDF.ci.obj[[4]])), length = 50),
    safe_exp(CDF.ci.obj[[4]]),
    seq(max(safe_exp(CDF.ci.obj[[4]])), max.OR, length = 50)
  )
  
  # Safe approximation with error handling
  safe_approx <- function(x, y, xout) {
    tryCatch({
      # Remove duplicate x values before interpolation
      unique_indices <- !duplicated(x)
      x_unique <- x[unique_indices]
      y_unique <- y[unique_indices]
      
      # Ensure we have at least 2 unique points for interpolation
      if (length(x_unique) < 2) {
        return(rep(mean(y, na.rm = TRUE), length(xout)))
      }
      
      approx(x_unique, y_unique, xout = xout)$y
    }, error = function(e) {
      rep(mean(y, na.rm = TRUE), length(xout))
    })
  }
  
  # Calculate CDFs with error handling
  cdf.est <- safe_approx(x.est.taper, c(rep(0, 50), CDF.ci.obj[[1]], rep(1, 50)), x.seq)
  cdf.ll <- safe_approx(x.ll.taper, c(rep(0, 50), CDF.ci.obj[[1]], rep(1, 50)), x.seq)
  cdf.ul <- safe_approx(x.ul.taper, c(rep(0, 50), CDF.ci.obj[[1]], rep(1, 50)), x.seq)
  
  # Set colors based on efficacy definition
  if(efficacy.is.OR.le1) {
    le1.col <- 3 
    mt1.col <- 2 
  } else {
    le1.col <- 2 
    mt1.col <- 3 
  }
  
  # Calculate values for plotting with fixed ranges
  le1.vec <- exp(seq(log(min.OR), 0, length = 200))
  le1.est <- safe_approx(x.seq, cdf.est, xout = le1.vec)
  le1.ll <- safe_approx(x.seq, cdf.ll, xout = le1.vec)
  le1.ul <- safe_approx(x.seq, cdf.ul, xout = le1.vec)
  
  mt1.vec <- exp(seq(0, log(max.OR), length = 200))
  mt1.est <- safe_approx(x.seq, 1-cdf.est, xout = mt1.vec)
  mt1.ll <- safe_approx(x.seq, 1-cdf.ll, xout = mt1.vec)
  mt1.ul <- safe_approx(x.seq, 1-cdf.ul, xout = mt1.vec)
  
  # Create data frame for plotting
  plot_data <- rbind(
    data.frame(x = le1.vec, y = le1.est, lower = le1.ll, upper = le1.ul, group = "le1"),
    data.frame(x = mt1.vec, y = mt1.est, lower = mt1.ll, upper = mt1.ul, group = "mt1")
  )
  
  # Remove any non-finite values
  plot_data <- plot_data[is.finite(plot_data$x) & 
                        is.finite(plot_data$y) & 
                        is.finite(plot_data$lower) & 
                        is.finite(plot_data$upper), ]

  # --- NEW: Focus x-axis on region where probability > threshold ---
  prob_thresh <- 0.005
  nonzero_idx <- which(plot_data$y > prob_thresh)
  if (length(nonzero_idx) > 0) {
    x_focus <- range(plot_data$x[nonzero_idx], na.rm = TRUE)
    x_factor <- 1.05
    x_lims <- c(x_focus[1] / x_factor, x_focus[2] * x_factor)
    plot_data <- plot_data[plot_data$x >= x_lims[1] & plot_data$x <= x_lims[2], ]
  } else {
    x_lims <- range(plot_data$x, na.rm = TRUE)
  }
  # ---------------------------------------------------------------

  # Calculate nice breaks for log scale
  log_range <- log10(x_lims)
  log_breaks <- pretty(log_range, n = 6)
  x_breaks <- 10^log_breaks
  x_breaks <- x_breaks[x_breaks >= x_lims[1] & x_breaks <= x_lims[2]]

  # Create plot with ggplot2 - log scale for OR/RR with auto-scaled x-axis
  x_range <- range(plot_data$x, na.rm = TRUE)
  x_factor <- 1.2  # multiplicative margin for log scale
  
  p <- ggplot(plot_data, aes(x = x, y = y, color = group, fill = group)) +
    geom_line(aes(y = y), linewidth = 1) +  # Changed from size = 1
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    scale_x_log10(breaks = x_breaks,
                  labels = format(x_breaks, digits = 2),
                  limits = x_lims) +
    scale_color_manual(values = c("le1" = le1.col, "mt1" = mt1.col)) +
    scale_fill_manual(values = c("le1" = le1.col, "mt1" = mt1.col)) +
    labs(title = mlb, x = xlb, y = "Probability") +
    theme_minimal() +
    theme(legend.position = "none") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50")
  }
  
  return(p)
}


#' @title Get Contours for Confidence Regions
#' @description Helper function to compute contour lines from p-value matrix
#' @param pval_mat Matrix of p-values
#' @param level Confidence level
#' @return List of contour lines
get_contours <- function(pval_mat, level) {
  # Get dimensions
  n <- dim(pval_mat)[1]
  
  # Get sequences from matrix dimnames
  seq.mu <- sapply(strsplit(dimnames(pval_mat)[[1]], "mu ="), as.numeric)[2,]
  seq.tau <- sapply(strsplit(dimnames(pval_mat)[[2]], "tau ="), as.numeric)[2,]
  
  # Compute contour lines
  contourLines(
    x = seq.mu,
    y = seq.tau,
    z = matrix(pval_mat, nrow = n),
    levels = level
  )
}

#' @title Confidence Region Shift Plot
#' @description Creates a plot showing how the confidence region shifts when individual studies are removed
#' @param x A metabiv object
#' @param alpha Significance level
#' @return A plotly object representing the confidence region shift plot
#' @export
confidence_region_shift_plot <- function(x, alpha = 0.05) {
  # Get data dimensions and setup
  k <- nrow(x$tbl)
  sm <- x$sm
  
  # Get sequences from x$dev_pvals (matching plot.mu.tau.CI approach)
  dev.mat <- x$dev_pvals[[1]]
  pval.mat <- x$dev_pvals[[2]]
  seq.mu <- sapply(strsplit(dimnames(pval.mat)[[1]], "mu ="), as.numeric)[2,]
  seq.tau <- sapply(strsplit(dimnames(pval.mat)[[2]], "tau ="), as.numeric)[2,]
  
  # Transform appropriately based on summary measure
  if (sm == "SMD") {
    mu.pred.vec <- seq(min(seq.mu), max(seq.mu), length.out = 100)
  } else {
  mu.pred.vec <- exp(seq(min(seq.mu), max(seq.mu), length.out = 100))
  }
  tau.pred.vec <- seq(min(seq.tau), max(seq.tau), length.out = 100)
  
  # Get full model contours
  full_contour_50 <- get_contours(pval.mat, 0.50)
  full_contour_95 <- get_contours(pval.mat, 0.05)
  
  # Compute leave-one-out estimates and metrics
  loo_results <- lapply(1:k, function(i) {
    tbl_mod <- x$tbl[-i, ]
    
    # Calculate leave-one-out confidence region
    if (sm == "SMD") {
      res_i <- metabiv(studlab = tbl_mod$studlab,
                       sm = sm,
                       y = tbl_mod$y.k,
                       sigma2 = tbl_mod$sigma.2.k)
    } else {
      res_i <- metabiv(event.e = tbl_mod$event.e,
                       n.e = tbl_mod$n.e,
                       event.c = tbl_mod$event.c,
                       n.c = tbl_mod$n.c,
                       studlab = tbl_mod$studlab,
                       sm = sm)
    }
    
    # Get contours
    contour_50 <- get_contours(res_i$dev_pvals[[2]], 0.50)
    contour_95 <- get_contours(res_i$dev_pvals[[2]], 0.05)
    
    # Calculate metrics
    iou <- calculate_iou(contour_95[[1]], full_contour_95[[1]])
    shift <- calculate_shift(x$mu, res_i$mu)
    width_diff <- calculate_width_diff(full_contour_95[[1]], contour_95[[1]])
    hellinger <- calculate_hellinger(full_contour_95[[1]]$x, contour_95[[1]]$x)
    kld <- calculate_kld(full_contour_95[[1]]$x, contour_95[[1]]$x)
    rmse <- calculate_rmse_bounds(full_contour_95[[1]], contour_95[[1]])
    coverage_prob <- calculate_coverage_prob(full_contour_95[[1]], contour_95[[1]])
    combined_score <- calculate_combined_score(iou, hellinger, kld, rmse, coverage_prob)
    
    list(mu = res_i$mu, tau = res_i$tau,
         contour_50 = contour_50, contour_95 = contour_95,
         iou = iou, shift = shift, width_diff = width_diff,
         hellinger = hellinger, kld = kld, rmse = rmse,
         coverage_prob = coverage_prob, combined_score = combined_score)
  })
  
  # Create plot
  p <- plot_ly()
  
  # Add full model contours
  if (sm == "SMD") {
    p <- add_trace(p, 
                   x = full_contour_95[[1]]$x, 
                   y = full_contour_95[[1]]$y,
                   type = "scatter", mode = "lines",
                   line = list(color = "red", width = 1),
                   name = "Full Model 95% CI")
    
    p <- add_trace(p, 
                   x = full_contour_50[[1]]$x, 
                   y = full_contour_50[[1]]$y,
                   type = "scatter", mode = "lines",
                   line = list(color = "blue", width = 1),
                   name = "Full Model 50% CI")
  } else {
  p <- add_trace(p, 
                 x = exp(full_contour_95[[1]]$x), 
                 y = full_contour_95[[1]]$y,
                 type = "scatter", mode = "lines",
                 line = list(color = "red", width = 1),
                 name = "Full Model 95% CI")
  
  p <- add_trace(p, 
                 x = exp(full_contour_50[[1]]$x), 
                 y = full_contour_50[[1]]$y,
                 type = "scatter", mode = "lines",
                 line = list(color = "blue", width = 1),
                 name = "Full Model 50% CI")
  }
  
  # Add leave-one-out contours with transparency
  for (i in 1:k) {
    contour_95 <- loo_results[[i]]$contour_95
    contour_50 <- loo_results[[i]]$contour_50
    
    # Create appropriate hover text based on summary measure
    if (sm == "SMD") {
      hover_text <- sprintf(
        "Study: %d (omitted)<br>Effect Size (SMD): %.3f<br>τ: %.3f<br>
         Relative Shift: %.3f<br>IoU: %.3f<br>Hellinger: %.3f<br>KLD: %.3f<br>
         RMSE: %.3f<br>Coverage Prob: %.3f<br>Combined Score: %.3f<br>
         50%% CI: (%.3f, %.3f) to (%.3f, %.3f)<br>95%% CI: (%.3f, %.3f) to (%.3f, %.3f)",
        i, loo_results[[i]]$mu, loo_results[[i]]$tau,
        loo_results[[i]]$shift, loo_results[[i]]$iou, loo_results[[i]]$hellinger,
        loo_results[[i]]$kld, loo_results[[i]]$rmse, loo_results[[i]]$coverage_prob,
        loo_results[[i]]$combined_score,
        min(sapply(contour_50, function(x) min(x$x))), min(sapply(contour_50, function(x) min(x$y))),
        max(sapply(contour_50, function(x) max(x$x))), max(sapply(contour_50, function(x) max(x$y))),
        min(sapply(contour_95, function(x) min(x$x))), min(sapply(contour_95, function(x) min(x$y))),
        max(sapply(contour_95, function(x) max(x$x))), max(sapply(contour_95, function(x) max(x$y)))
      )
    } else {
    hover_text <- sprintf(
      "Study: %d (omitted)<br>Effect Size: %.3f (log: %.3f)<br>τ: %.3f<br>
       Relative Shift: %.3f<br>IoU: %.3f<br>Hellinger: %.3f<br>KLD: %.3f<br>
       RMSE: %.3f<br>Coverage Prob: %.3f<br>Combined Score: %.3f<br>
       50%% CI: (%.3f, %.3f) to (%.3f, %.3f)<br>95%% CI: (%.3f, %.3f) to (%.3f, %.3f)",
      i, exp(loo_results[[i]]$mu), loo_results[[i]]$mu, loo_results[[i]]$tau,
      loo_results[[i]]$shift, loo_results[[i]]$iou, loo_results[[i]]$hellinger,
      loo_results[[i]]$kld, loo_results[[i]]$rmse, loo_results[[i]]$coverage_prob,
      loo_results[[i]]$combined_score,
      exp(min(sapply(contour_50, function(x) min(x$x)))), min(sapply(contour_50, function(x) min(x$y))),
      exp(max(sapply(contour_50, function(x) max(x$x)))), max(sapply(contour_50, function(x) max(x$y))),
      exp(min(sapply(contour_95, function(x) min(x$x)))), min(sapply(contour_95, function(x) min(x$y))),
      exp(max(sapply(contour_95, function(x) max(x$x)))), max(sapply(contour_95, function(x) max(x$y)))
    )
    }
    # Add points for studies
    mu_val <- if (sm == "SMD") loo_results[[i]]$mu else exp(loo_results[[i]]$mu)
    p <- add_trace(p, 
                   x = mu_val, 
                   y = loo_results[[i]]$tau,
                   type = "scatter", mode = "markers",
                   marker = list(size = 8,
                                 color = loo_results[[i]]$shift,
                                 colorscale = "Viridis",
                                 showscale = FALSE),
                   text = hover_text,
                   hoverinfo = "text",
                   showlegend = FALSE,
                   customdata = i)
    # Add contours
    contour_x_vals <- if (sm == "SMD") contour_95[[1]]$x else exp(contour_95[[1]]$x)
    p <- add_trace(p, 
                   x = contour_x_vals, 
                   y = contour_95[[1]]$y,
                   type = "scatter", mode = "lines",
                   line = list(color = "blue", width = 2),
                   opacity = 0.05, showlegend = FALSE,
                   hoverinfo = "none",
                   customdata = i,
                   name = paste0("LOO Contour 95 - Study ", i))
  }
  

  # Add full model point
  main_mu_val <- if (sm == "SMD") x$mu else exp(x$mu)
  p <- add_trace(p, 
                 x = main_mu_val, 
                 y = x$tau,
                 type = "scatter", mode = "markers",
                 marker = list(size = 12,
                             symbol = "cross",
                             color = "green"),
                 name = "Full Model MLE")
  
  # Set layout
  if (sm == "SMD") {
    p <- p %>% layout(
      title = paste("Confidence Region Shift Plot for", sm),
      xaxis = list(
        title = "Effect Size (SMD)",
        range = c(-3, 3)
      ),
      yaxis = list(
        title = "tau",
        range = c(0, 1)
      ),
      showlegend = TRUE,
      hovermode = "closest"
    )
  } else {
  p <- p %>% layout(
    title = paste("Confidence Region Shift Plot for", sm),
    xaxis = list(
      title = "Effect Size (OR/RR)",
      type = "log",
      range = log10(c(0.5, 2.5)),
      ticktext = c("0.5", "1.0", "1.5", "2.0", "2.5"),
      tickvals = c(0.5, 1.0, 1.5, 2.0, 2.5)
    ),
    yaxis = list(
      title = "tau",
      range = c(0, 1)
    ),
    showlegend = TRUE,
    hovermode = "closest"
  )
  }
  
  # Add reference line
  ref_val <- if (sm == "SMD") 0 else 1
  p <- add_trace(p,
                 x = c(ref_val, ref_val),
                 y = c(0, 1),
                 type = "scatter", mode = "lines",
                 line = list(color = "gray", dash = "dash"),
                 showlegend = FALSE)
  
  # Add interactive highlighting
  p <- p %>% htmlwidgets::onRender("
  function(el, x) {
    el.on('plotly_hover', function(d) {
      if (!d.points || d.points.length === 0) return;

      var point = d.points[0]; 
      var studyNumber = point.customdata; // Get study number from customdata
      var data = el.data;
      
      console.log('Hovered study number:', studyNumber);

      if (point.curveNumber !== undefined) {
        var traceName = data[point.curveNumber].name;

        // If hovering over a study point
        if (data[point.curveNumber].mode === 'markers') {
          for (var i = 0; i < data.length; i++) {
            if (data[i].name && data[i].name.startsWith('LOO Contour 95 - Study ')) {
              if (data[i].name === 'LOO Contour 95 - Study ' + studyNumber) {
                data[i].opacity = 1.0; // Highlight only the correct contour
              } else {
                data[i].opacity = 0.02; // Keep others dimmed
              }
            }
          }
        }

        // If hovering over a contour
        else if (traceName && traceName.startsWith('LOO Contour 95 - Study ')) {
          for (var i = 0; i < data.length; i++) {
            if (data[i].name && data[i].name.startsWith('LOO Contour 95 - Study ')) {
              if (data[i].name === traceName) {
                data[i].opacity = 1.0; // Highlight only the specific contour hovered
              } else {
                data[i].opacity = 0.02; // Keep others dimmed
              }
            }
          }
        }
        Plotly.redraw(el);
      }
    });

    el.on('plotly_unhover', function(d) {
      var data = el.data;
      for (var i = 0; i < data.length; i++) {
        if (data[i].name && data[i].name.startsWith('LOO Contour 95 - Study ')) {
          data[i].opacity = 0.02; // Reset all contours to default dimmed opacity
        }
      }
      Plotly.redraw(el);
    });
  }
")
  return(p)
}

#' @title Bivariate GOSH Plot
#' @description Creates a Graphical Display of Study Heterogeneity (GOSH) plot for bivariate meta-analysis
#' @param bivariate_result A metabiv object
#' @param n_subsets Number of subsets to generate
#' @param subset_size Size of each subset (if NULL, uses half of total studies)
#' @return A plotly object representing the GOSH plot
#' @export
# bivariate_gosh_plot <- function(bivariate_result, n_subsets = 1000, subset_size = NULL) {
#   data <- bivariate_result$tbl
#   k <- nrow(data)
#   
#   if (is.null(subset_size)) subset_size <- max(2, floor(k/2))
#   
#   subsets <- replicate(n_subsets, sample(1:k, size = subset_size, replace = FALSE))
#   
#   gosh_results <- apply(subsets, 2, function(subset) {
#     res <- metabiv(event.e = data$event.e[subset], 
#                    n.e = data$n.e[subset], 
#                    event.c = data$event.c[subset], 
#                    n.c = data$n.c[subset],
#                    studlab = data$studlab[subset],
#                    sm = bivariate_result$sm)
#     c(mu = res$mu, tau = res$tau)
#   })
#   
#   gosh_df <- as.data.frame(t(gosh_results))
#   
#   p <- plot_ly(data = gosh_df, x = ~mu, y = ~tau, type = "scatter", mode = "markers",
#                marker = list(size = 3, opacity = 0.5)) %>%
#     layout(title = paste("GOSH Plot for", bivariate_result$sm),
#            xaxis = list(title = "μ"),
#            yaxis = list(title = "τ"))
#   
#   return(p)
# }

#' @title Bivariate GRADE Assessment
#' @description Performs a GRADE assessment for bivariate meta-analysis results
#' @param bivariate_result A metabiv object
#' @param risk_of_bias Risk of bias assessment (Low, Unclear, or High)
#' @param indirectness Indirectness assessment (Low, Unclear, or High)
#' @return A character string containing the GRADE assessment
#' @export
# bivariate_grade_assessment <- function(bivariate_result, risk_of_bias, indirectness) {
#   cat("Bivariate GRADE Assessment\n\n")
#   cat("Risk of Bias:", risk_of_bias, "\n")
#   cat("Indirectness:", indirectness, "\n")
#   
#   # Inconsistency (based on tau)
#   inconsistency <- if(bivariate_result$tau > 0.5) "High" else if(bivariate_result$tau > 0.3) "Moderate" else "Low"
#   cat("Inconsistency:", inconsistency, "\n")
#   
#   # Imprecision (based on the ratio of mu to tau)
#   imprecision <- if(abs(bivariate_result$mu) / bivariate_result$tau < 2) "High" else "Low"
#   cat("Imprecision:", imprecision, "\n")
#   
#   # Publication Bias (simplified test)
#   pub_bias_test <- lm(bivariate_result$y.k ~ sqrt(bivariate_result$sigma.2.k))
#   pub_bias <- if(abs(coef(pub_bias_test)[1]) > 0.1) "Suspected" else "Not suspected"
#   cat("Publication Bias:", pub_bias, "\n\n")
#   
#   # Overall GRADE score
#   grade_score <- 4  # Start with high quality
#   if (risk_of_bias == "High") grade_score <- grade_score - 1
#   if (indirectness == "High") grade_score <- grade_score - 1
#   if (inconsistency == "High") grade_score <- grade_score - 2
#   else if (inconsistency == "Moderate") grade_score <- grade_score - 1
#   if (imprecision == "High") grade_score <- grade_score - 1
#   if (pub_bias == "Suspected") grade_score <- grade_score - 1
#   
#   overall_grade <- switch(as.character(max(0, grade_score)),
#                           "4" = "High",
#                           "3" = "Moderate",
#                           "2" = "Low",
#                           "1" = "Very Low",
#                           "0" = "Very Low")
#   
#   cat("Overall GRADE:", overall_grade, "\n")
#   
#   return(overall_grade)
# }

#' @title Forest Plot for Bivariate Meta-Analysis
#' @description Creates a forest plot for bivariate meta-analysis results
#' @param x A metabiv object
#' @param xlab Label for the x-axis
#' @param refline Reference line (default: 1 for OR/RR)
#' @param leftcols Vector of column names to include on the left side of the plot
#' @param rightcols Vector of column names to include on the right side of the plot
#' @param digits Number of digits for rounding
#' @param study_weights Whether to show study weights
#' @param text_size Base text size for the plot
#' @param title Optional title for the plot
#' @param weight_column Name of weight column to display
#' @param ... Additional arguments passed to the forest function
#' @return A forest plot
#' @export
forest.metabiv <- function(x, xlab = "Effect Size", title = "Forest Plot") {
  # Ensure x is a metabiv object
  if (!inherits(x, "metabiv")) {
    stop("Input must be a 'metabiv' object.")
  }

  # 1. Create a base meta-analysis object. This holds the individual study data.
  m <- metagen(
    TE = x$y.k,
    seTE = sqrt(x$sigma.2.k),
    studlab = x$studlab,
    sm = x$sm,
    level = x$level,
    common = FALSE,
    random = TRUE # Set to TRUE to allow summary, then overwrite below
  )

  # 2. Manually overwrite the default random-effects results with our bivariate model's results.
  # This ensures the summary diamond and stats reflect our MLE calculation.
  m$TE.random <- x$mu
  
  # Handle missing confidence intervals
  if (is.numeric(x$lower.mu) && is.numeric(x$upper.mu) && 
      is.finite(x$lower.mu) && is.finite(x$upper.mu)) {
    m$lower.random <- x$lower.mu
    m$upper.random <- x$upper.mu
    m$seTE.random <- (x$upper.mu - x$lower.mu) / (2 * qnorm((1 + x$level.ma) / 2))
  } else {
    # Use fallback values if confidence intervals are not available
    m$lower.random <- x$mu - 1.96 * sqrt(x$tau2)
    m$upper.random <- x$mu + 1.96 * sqrt(x$tau2)
    m$seTE.random <- sqrt(x$tau2)
  }
  
  m$tau2 <- x$tau2
  m$I2 <- x$I2 / 100 # meta expects I2 as a proportion (0-1)
  m$H2 <- x$H2
  m$Q <- x$Q
  m$df.Q <- x$df
  m$k.random <- m$k # Set flag that random-effects summary is available

  # 3. Create the forest plot using the modified meta object.
  # 'overall = TRUE' now displays our custom bivariate summary.
  # Layout parameters are adjusted to prevent text overlap.
  forest(
    m,
    xlab = xlab,
    smlab = title,
    overall = TRUE,
    hetstat = TRUE,
    print.I2 = TRUE,
    print.tau2 = TRUE,
    col.diamond = "blue",
    col.diamond.lines = "blue",
    plotwidth = "8cm",
    colgap = "3mm",
    spacing.factor = 1.2, # Increase vertical spacing
    fs.hetstat = 8      # Set font size for heterogeneity stats
  )
}

# Helper function to validate metabiv object
validate_metabiv <- function(x) {
  required_elements <- c("y.k", "sigma.2.k", "sm", "mu", "tau", 
                         "lower", "upper", "lower.k", "upper.k")
  
  missing_elements <- required_elements[!required_elements %in% names(x)]
  
  if (length(missing_elements) > 0) {
    stop("Invalid metabiv object: missing elements: ", 
         paste(missing_elements, collapse = ", "))
  }
  
  if (!all(sapply(x$sigma.2.k, is.numeric)) || 
      !all(sapply(x$y.k, is.numeric))) {
    stop("Invalid metabiv object: non-numeric effect sizes or variances")
  }
  
  invisible(TRUE)
}
#' @title Print Method for Metabiv Objects
#' @description Prints a summary of the bivariate meta-analysis results
#' @param x A metabiv object
#' @param ... Additional arguments (not used)
#' @return None (prints to console)
#' @export
print.metabiv <- function(x, ...) {
  cat("Bivariate Meta-Analysis\n\n")
  cat("Summary Measure:", x$sm, "\n")
  cat("Number of studies:", length(x$y.k), "\n\n")
  cat("Random effects model:\n")
  cat("  μ estimate:", round(x$mu, 4), "\n")
  cat("  τ estimate:", round(x$tau, 4), "\n")
  cat("  95% Confidence Interval for μ: [", round(x$lower, 4), ", ", round(x$upper, 4), "]\n")
  cat("  95% Confidence Region:\n")
  cat("    μ range: [", round(min(x$conf_region$mu), 4), ", ", round(max(x$conf_region$mu), 4), "]\n")
  cat("    τ range: [", round(min(x$conf_region$tau), 4), ", ", round(max(x$conf_region$tau), 4), "]\n\n")
  cat("Heterogeneity:\n")
  cat("  Q statistic:", round(x$Q, 2), "\n")
  cat("  I^2:", round(x$I2, 1), "%\n")
  cat("  H^2:", round(x$H2, 2), "\n")
}

#' @title Summary Method for Metabiv Objects
#' @description Provides a summary of the bivariate meta-analysis results
#' @param object A metabiv object
#' @param ... Additional arguments (not used)
#' @return A list containing summary statistics
#' @export
summary.metabiv <- function(object, ...) {
  res <- list(
    sm = object$sm,
    k = length(object$y.k),
    mu = object$mu,
    tau = object$tau,
    ci_mu = c(object$lower, object$upper),
    conf_region = object$conf_region,
    Q = object$Q,
    I2 = object$I2,
    H2 = object$H2
  )
  class(res) <- "summary.metabiv"
  return(res)
}

#' @title Print Method for Summary of Metabiv Objects
#' @description Prints the summary of bivariate meta-analysis results
#' @param x A summary.metabiv object
#' @param ... Additional arguments (not used)
#' @return None (prints to console)
#' @export
print.summary.metabiv <- function(x, ...) {
  cat("Summary of Bivariate Meta-Analysis\n\n")
  cat("Summary Measure:", x$sm, "\n")
  cat("Number of studies:", x$k, "\n\n")
  cat("Random effects model:\n")
  cat("  μ estimate:", round(x$mu, 4), "\n")
  cat("  τ estimate:", round(x$tau, 4), "\n")
  if (is.numeric(x$lower.mu) && is.numeric(x$upper.mu)) {
    cat("  95% Confidence Interval for μ: [", round(x$lower.mu, 4), ", ", round(x$upper.mu, 4), "]\n")
  } else {
    cat("  95% Confidence Interval for μ: [Not available]\n")
  }
  cat("  95% Confidence Region:\n")
  cat("    μ range: [", round(min(x$conf_region$mu), 4), ", ", round(max(x$conf_region$mu), 4), "]\n")
  cat("    τ range: [", round(min(x$conf_region$tau), 4), ", ", round(max(x$conf_region$tau), 4), "]\n\n")
  cat("Heterogeneity:\n")
  cat("  Q statistic:", round(x$Q, 2), "\n")
  cat("  I^2:", round(x$I2, 1), "%\n")
  cat("  H^2:", round(x$H2, 2), "\n")
}



# Bivariate Approach Functions
##############################

comp.tau.mu.log.RR.MLE <- function(data.tbl, initial.value) {
  a <- comp.log.RR.y.sigma.stats(data.tbl)
  y.k <- a[[1]]
  sigma.2.k <- a[[2]]
  
  minus.loglik <- function(par.vec) {
    sd_val <- sqrt(par.vec[2]^2 + sigma.2.k)
    if (any(is.nan(sd_val)) || any(!is.finite(sd_val))) return(Inf)
    -sum(dnorm(y.k, mean = par.vec[1], sd = sd_val, log = TRUE))
  }
  
  # Calculate DerSimonian-Laird if no initial value is provided
  if (is.null(initial.value)) {
  w <- 1 / sigma.2.k
  mu.init <- sum(w * y.k) / sum(w)
  Q <- sum(w * (y.k - mu.init)^2)
  df <- length(y.k) - 1
  tau2.init <- max(0, (Q - df) / (sum(w) - sum(w^2) / sum(w)))
    initial.value <- c(mu.init, sqrt(tau2.init))
  }

  a <- nlminb(initial.value, minus.loglik, lower = c(-Inf, 0))
  return(list(mu = a$par[1], tau = a$par[2]))
}

comp.tau.mu.log.OR.MLE <- function(data.tbl, initial.value) {
  # Step 1: Calculate effect sizes
  a <- comp.log.OR.y.sigma.stats(data.tbl)
  y.k <- a[[1]]
  sigma.2.k <- a[[2]]
  
  # Step 2: Define the negative log-likelihood function
  neg.loglik <- function(par) {
    mu <- par[1]
    tau <- par[2]
    sd_val <- sqrt(tau^2 + sigma.2.k)
    # Return Inf for non-finite values to guide optimizer
    if (any(!is.finite(sd_val))) return(Inf)
    -sum(dnorm(y.k, mean = mu, sd = sd_val, log = TRUE))
  }
  
  # Use provided initial values, or calculate DerSimonian-Laird estimates
  if (is.null(initial.value)) {
      w <- 1 / sigma.2.k
      mu.init <- sum(w * y.k) / sum(w)
      Q <- sum(w * (y.k - mu.init)^2)
      df <- length(y.k) - 1
      tau2.init <- max(0, (Q - df) / (sum(w) - sum(w^2) / sum(w)))
      par.init <- c(mu.init, sqrt(tau2.init))
  } else {
      par.init <- initial.value
  }
  
  # Step 3: Run optimizer with lower bound for tau
  opt_result <- nlminb(start = par.init, objective = neg.loglik, lower = c(-Inf, 0))
  
  # Return results
  list(mu = opt_result$par[1], tau = opt_result$par[2])
}

comp.tau.mu.log.OR.dev.pvals <- function(data.tbl, mu.vec.tst, tau.vec.tst) {
  n.mu <- length(mu.vec.tst)
  n.tau <- length(tau.vec.tst) 
  a <- comp.log.OR.y.sigma.stats(data.tbl)
  y.k <- a[[1]]
  sigma.2.k <- a[[2]]
  K <- length(y.k) 
  y.mat.k.i <- rep(c(y.k), each = n.mu*n.tau)
  sigma.2.k.i <- rep(c(sigma.2.k), each = n.mu*n.tau)
  tau.k.i <- rep(rep(tau.vec.tst, each = n.mu), K)
  mu.k.i <- rep(mu.vec.tst, n.tau*K)
  
  loglik.vec <- dnorm(y.mat.k.i, mean = mu.k.i, sd = sqrt(tau.k.i^2 + sigma.2.k.i), log = TRUE)
  loglik.mu.tau <- c(array(loglik.vec, dim=c(n.mu*n.tau, K)) %*% cbind(rep(1,K)))
  dev.mat <- array(-2*(loglik.mu.tau - max(loglik.mu.tau)), dim=c(n.mu, n.tau))
  dimnames(dev.mat) <- list(paste("mu = ", round(mu.vec.tst, 2)), paste("tau = ", round(tau.vec.tst, 3)))
  
  pval.mat <- array(1 - pchisq(dev.mat, 2), dim=c(n.mu, n.tau))
  dimnames(pval.mat) <- dimnames(dev.mat)
  
  return(list(dev.mat, pval.mat))
}

comp.tau.mu.log.RR.dev.pvals <- function(data.tbl, mu.vec.tst, tau.vec.tst) {
  n.mu <- length(mu.vec.tst)
  n.tau <- length(tau.vec.tst) 
  a <- comp.log.RR.y.sigma.stats(data.tbl)
  y.k <- a[[1]]
  sigma.2.k <- a[[2]]
  K <- length(y.k) 
  y.mat.k.i <- rep(c(y.k), each = n.mu*n.tau)
  sigma.2.k.i <- rep(c(sigma.2.k), each = n.mu*n.tau)
  tau.k.i <- rep(rep(tau.vec.tst, each = n.mu), K)
  mu.k.i <- rep(mu.vec.tst, n.tau*K)
  
  loglik.vec <- dnorm(y.mat.k.i, mean = mu.k.i, sd = sqrt(tau.k.i^2 + sigma.2.k.i), log = TRUE)
  loglik.mu.tau <- c(array(loglik.vec, dim=c(n.mu*n.tau, K)) %*% cbind(rep(1,K)))
  dev.mat <- array(-2*(loglik.mu.tau - max(loglik.mu.tau)), dim=c(n.mu, n.tau))
  dimnames(dev.mat) <- list(paste("mu = ", round(mu.vec.tst, 2)), paste("tau = ", round(tau.vec.tst, 3)))
  
  pval.mat <- array(1 - pchisq(dev.mat, 2), dim=c(n.mu, n.tau))
  dimnames(pval.mat) <- dimnames(dev.mat)
  
  return(list(dev.mat, pval.mat))
}


# plot.mu.tau.CI <- function(dev.mat, pval.mat, p.cntr.vec = c(0.05, 0.50), N.sig = 100, mlb = "", mu_mle = NULL, tau_mle = NULL) {
#   # Extract dimensions and sequences
#   n.mu <- dim(pval.mat)[1]
#   n.tau <- dim(pval.mat)[2]
#   seq.mu <- sapply(strsplit(dimnames(pval.mat)[[1]], "mu ="), as.numeric)[2,]
#   seq.tau <- sapply(strsplit(dimnames(pval.mat)[[2]], "tau ="), as.numeric)[2,]
# 
#     # Create prediction grid in log scale
#   mu.pred.vec <- seq(min(seq.mu), max(seq.mu), length.out = 100)
#   tau.pred.vec <- seq(min(seq.tau), max(seq.tau), length.out = 100)
#   
#   # Calculate p-values
#   logit.p <- log.odds(c(pval.mat))
#   logit.p[c(pval.mat) < 1/N.sig] <- log.odds(1/N.sig) - (log.odds(2/N.sig) - log.odds(1/N.sig))
#   logit.p[c(pval.mat) > 1 - 1/N.sig] <- log.odds((N.sig-1)/N.sig) + (log.odds((N.sig-1)/N.sig) - log.odds((N.sig-2)/N.sig))
#   
#   # Fit loess model
#   logit.p.loess <- loess(logit.p ~ rep(seq.mu, n.tau) + rep(seq.tau, each = n.mu), span = 0.1)
#   
#   # Create prediction grid
#   pred.grid <- expand.grid(
#     x = mu.pred.vec,
#     y = tau.pred.vec
#   )
#   
#   # Get smoothed p-values
#   smth.pval.mat <- matrix(
#     inv.log.odds(predict(logit.p.loess, pred.grid)),
#     nrow = 100, ncol = 100
#   )
#   
#   # Convert mu values to exp scale for plotting
#   plot_mu <- exp(mu.pred.vec)
#   
#   #browser()
#   
#   # Set up plot
#   par(mar = c(5, 5, 4, 2) + 0.1)
#   plot(1, type = "n", log = "x",
#        xlim = c(0.3, 3.2),  # Fixed range that works well for both OR and RR
#        ylim = range(tau.pred.vec),
#        xlab = "Effect Size (OR/RR)", 
#        ylab = "tau",
#        main = mlb,
#        cex.lab = 1.2, 
#        cex.axis = 0.8, 
#        cex.main = 1.2,
#        xaxt = "n")
#   
#   # Add contours
#   contour(plot_mu, tau.pred.vec, smth.pval.mat, 
#           levels = p.cntr.vec,
#           col = c("red", "blue"), 
#           add = TRUE, 
#           drawlabels = FALSE)
#   
#   # Add reference line at 1
#   abline(v = 1, lty = 2, col = "gray")
#   
#   # Add MLE point
#   if(is.null(mu_mle) || is.null(tau_mle)) {
#     mle_index <- which(dev.mat == min(dev.mat), arr.ind = TRUE)[1,]
#     mu_mle <- seq.mu[mle_index[1]]
#     tau_mle <- seq.tau[mle_index[2]]
#   }
#   points(exp(mu_mle), tau_mle, pch = 3, col = "green", cex = 1.5, lwd = 2)
#   
#   # Add reference lines with proper transformation
#   #abline(0, 1/qnorm(0.75), col = "blue", lty = 3)
#   #abline(0.5/qnorm(0.75), -1/qnorm(0.75), col = "blue", lty = 3)
#   
#   # Add vertical reference lines
#   abline(v = exp(0), col = "blue", lty = 3)
#   # abline(v = exp(0.5), col = "blue", lty = 3)
#   
#   # Add custom x-axis
#   axis(1, at = c(0.3, 0.5, 1.0, 2.0, 3.2))
#   
#   # Add legend
#   legend("topright", 
#          legend = c("95% CI", "50% CI", "MLE"),
#          col = c("red", "blue", "green"), 
#          lty = c(1, 1, NA), 
#          pch = c(NA, NA, 3),
#          cex = 0.8, 
#          bg = "white", 
#          box.lwd = 0)
#   
#   invisible(list(mu_mle = mu_mle, tau_mle = tau_mle))
# }

plot.mu.tau.CI <- function(dev.mat, pval.mat, p.cntr.vec = c(0.05, 0.50), mlb = "", xlab = "Effect Size (OR/RR)", mu_mle = NULL, tau_mle = NULL, sm = NULL) {
  # Extract sequences for µ and τ
  seq.mu <- sapply(strsplit(dimnames(pval.mat)[[1]], "mu ="), as.numeric)[2,]
  seq.tau <- sapply(strsplit(dimnames(pval.mat)[[2]], "tau ="), as.numeric)[2,]
  
  # Check if we're dealing with SMD
  is_smd <- !is.null(sm) && sm == "SMD"
  
  if (is_smd) {
    # For SMD: Use linear scale centered around 0
    mu.pred.vec <- seq(min(seq.mu), max(seq.mu), length.out = 100)
    tau.pred.vec <- seq(min(seq.tau), max(seq.tau), length.out = 100)
    
    # Extract 95% and 50% confidence contours using the correct method
    contour_50 <- get_contours(pval.mat, 0.50)
    contour_95 <- get_contours(pval.mat, 0.05)
    
    # Auto-scale x-axis to data range with small margin
    mu_range <- range(c(contour_95[[1]]$x, mu_mle))
    x_margin <- diff(mu_range) * 0.1
    
    # Set up plot with linear scale and auto-scaled x-axis
    plot(1, type = "n",
         xlim = c(mu_range[1] - x_margin, mu_range[2] + x_margin),
         ylim = range(tau.pred.vec),
         xlab = xlab, 
         ylab = "τ",
         main = mlb,
         cex.lab = 1.2, 
         cex.axis = 0.8, 
         cex.main = 1.2)
    
    # Draw confidence contours (no exp transformation for SMD)
    lines(contour_95[[1]]$x, contour_95[[1]]$y, col = "red", lwd = 2, lty = 1)  # 95% CI
    lines(contour_50[[1]]$x, contour_50[[1]]$y, col = "blue", lwd = 2, lty = 1) # 50% CI
    
    # Add MLE point
    if (is.null(mu_mle) || is.null(tau_mle)) {
      mle_index <- which(dev.mat == min(dev.mat), arr.ind = TRUE)[1,]
      mu_mle <- seq.mu[mle_index[1]]
      tau_mle <- seq.tau[mle_index[2]]
    }
    points(mu_mle, tau_mle, pch = 3, col = "green", cex = 1.5, lwd = 2)
    
    # Add reference line at 0 for SMD
    abline(v = 0, lty = 2, col = "gray")
    
  } else {
    # Original code for OR/RR with log scale
  # Convert to exponentiated scale for effect sizes
  mu.pred.vec <- exp(seq(min(seq.mu), max(seq.mu), length.out = 100))
  tau.pred.vec <- seq(min(seq.tau), max(seq.tau), length.out = 100)
  
  # Extract 95% and 50% confidence contours using the correct method
  contour_50 <- get_contours(pval.mat, 0.50)
  contour_95 <- get_contours(pval.mat, 0.05)
  
  # Auto-scale x-axis to data range with small margin
  mu_range_exp <- range(c(exp(contour_95[[1]]$x), exp(mu_mle)))
  x_factor <- 1.2  # multiplicative margin for log scale
  
    # Set up plot with log scale and auto-scaled x-axis
  plot(1, type = "n", log = "x",
       xlim = c(mu_range_exp[1] / x_factor, mu_range_exp[2] * x_factor),
       ylim = range(tau.pred.vec),
       xlab = xlab, 
       ylab = "τ",
       main = mlb,
       cex.lab = 1.2, 
       cex.axis = 0.8, 
       cex.main = 1.2,
       xaxt = "n")
  
  # Draw confidence contours
  lines(exp(contour_95[[1]]$x), contour_95[[1]]$y, col = "red", lwd = 2, lty = 1)  # 95% CI
  lines(exp(contour_50[[1]]$x), contour_50[[1]]$y, col = "blue", lwd = 2, lty = 1) # 50% CI
  
  # Add MLE point
  if (is.null(mu_mle) || is.null(tau_mle)) {
    mle_index <- which(dev.mat == min(dev.mat), arr.ind = TRUE)[1,]
    mu_mle <- seq.mu[mle_index[1]]
    tau_mle <- seq.tau[mle_index[2]]
  }
  points(exp(mu_mle), tau_mle, pch = 3, col = "green", cex = 1.5, lwd = 2)
  
  # Add reference lines
  abline(v = 1, lty = 2, col = "gray")
  abline(v = exp(0), col = "blue", lty = 3)
  
  # Custom x-axis with auto-scaled nice ticks
  x_ticks <- pretty(mu_range_exp, n = 5)
  x_ticks <- x_ticks[x_ticks > 0]  # ensure positive for log scale
  axis(1, at = x_ticks, labels = format(x_ticks, digits = 2))
  }
  
  # Add legend
  legend("topright", 
         legend = c("95% CI", "50% CI", "MLE"),
         col = c("red", "blue", "green"), 
         lty = c(1, 1, NA), 
         pch = c(NA, NA, 3),
         cex = 0.8, 
         bg = "white", 
         box.lwd = 0)
  
  return(invisible(list(mu_mle = mu_mle, tau_mle = tau_mle)))
}

# Helper function to create a valid polygon for sf operations
create_valid_polygon <- function(contour) {
  # Ensure the contour is a closed loop
  if (contour$x[1] != contour$x[length(contour$x)] || contour$y[1] != contour$y[length(contour$y)]) {
    contour$x <- c(contour$x, contour$x[1])
    contour$y <- c(contour$y, contour$y[1])
  }
  
  # Create a polygon and make it valid
  p <- st_polygon(list(cbind(contour$x, contour$y)))
  
  # Check validity and attempt to fix if needed
  if (!st_is_valid(p)) {
    p <- st_make_valid(p)
  }
  return(p)
}

calculate_iou <- function(contour1, contour2) {
  tryCatch({
    # Ensure contours have the same length for comparison by interpolating
    len1 <- length(contour1$x)
    len2 <- length(contour2$x)
    if (len1 != len2) {
      if (len1 > len2) {
        # Remove duplicate x values before interpolation
        unique_indices2 <- !duplicated(contour2$x)
        contour2$x <- contour2$x[unique_indices2]
        contour2$y <- contour2$y[unique_indices2]
        
        # Check if we have enough unique points
        if (length(contour2$x) < 2) {
          return(0)
        }
        
        contour2$x <- approx(1:length(contour2$x), contour2$x, n = len1)$y
        contour2$y <- approx(1:length(contour2$y), contour2$y, n = len1)$y
      } else {
        # Remove duplicate x values before interpolation
        unique_indices1 <- !duplicated(contour1$x)
        contour1$x <- contour1$x[unique_indices1]
        contour1$y <- contour1$y[unique_indices1]
        
        # Check if we have enough unique points
        if (length(contour1$x) < 2) {
          return(0)
        }
        
        contour1$x <- approx(1:length(contour1$x), contour1$x, n = len2)$y
        contour1$y <- approx(1:length(contour1$y), contour1$y, n = len2)$y
      }
    }
    
    poly1 <- create_valid_polygon(contour1)
    poly2 <- create_valid_polygon(contour2)
    
    # Calculate intersection and union
    intersection <- st_intersection(st_sfc(poly1), st_sfc(poly2))
    union <- st_union(st_sfc(poly1), st_sfc(poly2))
    
    if (st_area(intersection) == 0) {
      return(0)
    } else {
      return(as.numeric(st_area(intersection) / st_area(union)))
    }
  }, error = function(e) {
    warning("Error in calculate_iou: ", e$message)
    return(0)  # Return 0 if there's an error
  })
}

calculate_shift <- function(mu_main, mu_secondary) {
  return(abs(mu_main - mu_secondary))
}

calculate_width_diff <- function(contour1, contour2) {
  width1 <- max(contour1$x) - min(contour1$x)
  width2 <- max(contour2$x) - min(contour2$x)
  return(abs(width1 - width2))
}

calculate_hellinger <- function(x1, x2) {
  if (!is.numeric(x1)) x1 <- as.numeric(unlist(x1))
  if (!is.numeric(x2)) x2 <- as.numeric(unlist(x2))
  dist1 <- table(cut(x1, breaks = 10)) / length(x1)
  dist2 <- table(cut(x2, breaks = 10)) / length(x2)
  return(sqrt(1 - sum(sqrt(dist1 * dist2))))
}

calculate_kld <- function(x1, x2) {
  if (!is.numeric(x1)) x1 <- as.numeric(unlist(x1))
  if (!is.numeric(x2)) x2 <- as.numeric(unlist(x2))
  dist1 <- table(cut(x1, breaks = 10)) / length(x1)
  dist2 <- table(cut(x2, breaks = 10)) / length(x2)
  return(sum(dist1 * log(dist1 / dist2)))
}

calculate_rmse_bounds <- function(contour1, contour2) {
  rmse <- sqrt(mean((contour1$y - contour2$y)^2))
  return(rmse)
}

calculate_coverage_prob <- function(main_contour, secondary_contour) {
  coverage <- mean((secondary_contour$y >= main_contour$y))
  return(coverage)
}

calculate_combined_score <- function(iou, hellinger, kld, rmse, coverage_prob) {
  weights <- c(0.25, 0.2, 0.2, 0.2, 0.15)
  score <- sum(weights * c(iou, hellinger, kld, rmse, coverage_prob))
  return(score)
}

#' @title Compute Exact Deviance and P-values for Log-OR
#' @description Calculates deviance and p-values using an exact simulation test for Odds Ratios.
#' @param data.tbl A data frame containing the study data
#' @param mu.vec.tst Vector of mu values to test
#' @param tau.vec.tst Vector of tau values to test
#' @param N.sig Number of simulations for the exact test
#' @return A list containing the deviance matrix and the exact p-value matrix
#' @export
comp.tau.mu.log.OR.dev.pvals.exact <- function(data.tbl, mu.vec.tst, tau.vec.tst, N.sig = 200) {
  n.mu <- length(mu.vec.tst)
  n.tau <- length(tau.vec.tst)
  
  a <- comp.log.OR.y.sigma.stats(data.tbl)
  y.k <- a[[1]]
  sigma.2.k <- a[[2]]
  K <- length(y.k)
  
  # Compute the observed deviance matrix first
  y.mat.k.i <- rep(c(y.k), each = n.mu * n.tau)
  sigma.2.k.i <- rep(c(sigma.2.k), each = n.mu * n.tau)
  tau.k.i <- rep(rep(tau.vec.tst, each = n.mu), K)
  mu.k.i <- rep(mu.vec.tst, n.tau * K)
  
  loglik.vec <- dnorm(y.mat.k.i, mean = mu.k.i, sd = sqrt(tau.k.i^2 + sigma.2.k.i), log = TRUE)
  loglik.mu.tau <- c(array(loglik.vec, dim = c(n.mu * n.tau, K)) %*% cbind(rep(1, K)))
  dev.mat <- array(-2 * (loglik.mu.tau - max(loglik.mu.tau)), dim = c(n.mu, n.tau))
  dimnames(dev.mat) <- list(paste("mu = ", round(mu.vec.tst, 2)), paste("tau = ", round(tau.vec.tst, 3)))
  
  # Compute exact p-values via simulation
  pval.mat <- array(0, dim = dim(dev.mat))
  dimnames(pval.mat) <- dimnames(dev.mat)
  
  for (j1 in 1:n.mu) {
    for (j2 in 1:n.tau) {
      # Only run simulation if the chi-squared p-value is not already tiny, to save time
      if (dev.mat[j1, j2] < qchisq(1 - 1e-5, 2)) {
        sim_devs <- replicate(N.sig, {
          # Step 1: Simulate new tables based on the noncentral hypergeometric distribution
          theta.k.sim <- exp(rnorm(K, mean = mu.vec.tst[j1], sd = tau.vec.tst[j2]))
          data.mat.sim <- cbind(data.tbl$n.e, data.tbl$n.c, data.tbl$event.e + data.tbl$event.c, theta.k.sim)
          
          # Helper for rFNCHypergeo which expects a vector
          rFNCHypergeo.vec <- function(vec.4) rFNCHypergeo(1, vec.4[1], vec.4[2], vec.4[3], vec.4[4])
          
          x.vec.sim <- apply(data.mat.sim, 1, rFNCHypergeo.vec)
          
          smp.tbl <- data.tbl
          smp.tbl$event.e <- x.vec.sim
          smp.tbl$event.c <- (data.tbl$event.e + data.tbl$event.c) - x.vec.sim
          
          # Step 2: Recalculate effect sizes for the simulated table
          a_sim <- comp.log.OR.y.sigma.stats(smp.tbl)
          y.k_sim <- a_sim[[1]]
          sigma.2.k_sim <- a_sim[[2]]
          
          # Step 3: Recalculate the deviance for the simulated data
          y.mat.k.i_sim <- rep(c(y.k_sim), each = n.mu * n.tau)
          sigma.2.k.i_sim <- rep(c(sigma.2.k_sim), each = n.mu * n.tau)
          
          loglik.vec_sim <- dnorm(y.mat.k.i_sim, mean = mu.k.i, sd = sqrt(tau.k.i^2 + sigma.2.k.i_sim), log = TRUE)
          loglik.mu.tau_sim <- c(array(loglik.vec_sim, dim = c(n.mu * n.tau, K)) %*% cbind(rep(1, K)))
          
          # The deviance of the simulated data, evaluated at the same (j1, j2) point
          dev.mu.tau_sim <- -2 * (loglik.mu.tau_sim[j1 + (j2 - 1) * n.mu] - max(loglik.mu.tau_sim))
          dev.mu.tau_sim
        })
        
        # The p-value is the fraction of simulated deviances greater than the observed deviance
        pval.mat[j1, j2] <- sum(sim_devs >= dev.mat[j1, j2]) / N.sig
      }
    }
  }
  
  return(list(dev.mat, pval.mat))
}

