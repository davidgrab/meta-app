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
#' @return A list with class "metabiv" containing the results of the bivariate meta-analysis
#' @export
metabiv <- function(event.e = NULL, n.e = NULL, event.c = NULL, n.c = NULL, studlab = NULL,
                    data = NULL, sm = "RR", y = NULL, sigma2 = NULL,
                    level = 0.95, level.ma = 0.95) {
  
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
  
  # Estimate tau and mu using MLE
  initial.value <- c(0.4, 0.4)
  mle_result <- comp.tau.mu.MLE(data.tbl, initial.value, sm, y.k, sigma.2.k)
  mu <- mle_result$mu
  tau <- mle_result$tau
  tau2 <- tau^2
  
  # Calculate confidence intervals
  ci_level <- level
  z_score <- qnorm((1 + ci_level) / 2)
  
  # Individual study CIs (on log scale)
  lower.k <- y.k - z_score * sqrt(pmax(sigma.2.k, 0))
  upper.k <- y.k + z_score * sqrt(pmax(sigma.2.k, 0))
  
  # Overall effect CI (on log scale)
  se.mu <- sqrt(1 / sum(1 / (sigma.2.k + tau2)))
  lower <- mu - z_score * se.mu
  upper <- mu + z_score * se.mu
  
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
  
  # Calculate deviance and p-values
  mu.vec <- seq(-1, 1, length.out = 100)
  tau.vec <- seq(0.01, 1, length.out = 100)
  dev_pvals <- comp.tau.mu.dev.pvals(data.tbl, mu.vec, tau.vec, sm, y.k, sigma.2.k)
  
  # Compute confidence region
  conf_region <- compute_confidence_region(dev_pvals[[2]], level.ma)
  
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
  
  minus.loglik <- function(par.vec) -sum(dnorm(y.k, mean = par.vec[1], sd = sqrt(par.vec[2]^2 + sigma.2.k), log = TRUE))
  a <- nlminb(initial.value, minus.loglik)
  
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
  dev.mat <- array(-2 * (loglik.mu.tau - max(loglik.mu.tau)), dim = c(n.mu, n.tau))
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
                                  min_ci = log(0.2), max_ci = log(10)) {
  # Extract matrices and dimensions
  dev.mat <- dev.lst[[1]]
  pval.mat <- dev.lst[[2]]
  n.mu <- dim(pval.mat)[1]
  n.tau <- dim(pval.mat)[2]
  
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
    loess(logit.p ~ x.mu + x.tau, span = 0.1)
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
  
  # Calculate CI bounds with clamping
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
                               xlb = "Efficacy/Harm", min.OR = 0.3, max.OR = 3) {
  # Create sequence for x-axis with safeguards
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
      approx(x, y, xout = xout)$y
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
  
  # Create plot with ggplot2
  p <- ggplot(plot_data, aes(x = x, y = y, color = group, fill = group)) +
    geom_line(aes(y = y), size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10),
                  labels = c("0.1", "0.2", "0.5", "1", "2", "5", "10"),
                  limits = c(min.OR, max.OR)) +
    scale_color_manual(values = c("le1" = le1.col, "mt1" = mt1.col)) +
    scale_fill_manual(values = c("le1" = le1.col, "mt1" = mt1.col)) +
    labs(title = mlb, x = xlb, y = "Probability") +
    theme_minimal() +
    theme(legend.position = "none")
  
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
  
  # Transform to exp scale for plotting
  mu.pred.vec <- exp(seq(min(seq.mu), max(seq.mu), length.out = 100))
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
  
  # Add leave-one-out contours with transparency
  for (i in 1:k) {
    contour_95 <- loo_results[[i]]$contour_95
    contour_50 <- loo_results[[i]]$contour_50
    
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
    # Add points for studies
    p <- add_trace(p, 
                   x = exp(loo_results[[i]]$mu), 
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
    p <- add_trace(p, 
                   x = exp(contour_95[[1]]$x), 
                   y = contour_95[[1]]$y,
                   type = "scatter", mode = "lines",
                   line = list(color = "blue", width = 2),
                   opacity = 0.05, showlegend = FALSE,
                   hoverinfo = "none",
                   customdata = i,
                   name = paste0("LOO Contour 95 - Study ", i))
  }
  

  # Add full model point
  p <- add_trace(p, 
                 x = exp(x$mu), 
                 y = x$tau,
                 type = "scatter", mode = "markers",
                 marker = list(size = 12,
                             symbol = "cross",
                             color = "green"),
                 name = "Full Model MLE")
  
  # Set layout
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
  
  # Add reference line
  p <- add_trace(p,
                 x = c(1, 1),
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
forest.metabiv <- function(x, xlab = "Effect Size", refline = 1, 
                           leftcols = c("studlab"), 
                           rightcols = c("effect", "ci", "w.random"), 
                           digits = 2,
                           study_weights = TRUE,
                           text_size = 10,
                           title = NULL,
                           weight_column = "w.random",
                           ...) {
  
  # Input validation
  if (!inherits(x, "metabiv")) {
    stop("Input must be a metabiv object")
  }
  
  # Calculate standard errors with safeguards
  seTE <- try({
    sqrt(pmax(x$sigma.2.k, .Machine$double.eps))
  }, silent = TRUE)
  
  if (inherits(seTE, "try-error")) {
    stop("Error calculating standard errors")
  }
  
  # Calculate weights
  w.random <- 1/(seTE^2 + pmax(x$tau^2, .Machine$double.eps))
  w.random.pct <- 100 * w.random/sum(w.random)
  
  # Create study-level effect sizes and CIs
  study_effects <- data.frame(
    studlab = x$studlab,
    TE = x$y.k,
    seTE = seTE,
    lower = x$lower.k,
    upper = x$upper.k,
    effect = sprintf("%.*f", digits, exp(x$y.k)),
    ci = sprintf("[%.*f, %.*f]", 
                 digits, exp(x$lower.k), 
                 digits, exp(x$upper.k)),
    w.random = w.random.pct,
    stringsAsFactors = FALSE
  )
  
  # Add overall effect if available
  if (!is.null(x$mu) && !is.null(x$tau)) {
    overall_effect <- data.frame(
      studlab = "Overall",
      TE = x$mu,
      seTE = max(x$tau, sqrt(.Machine$double.eps)),
      lower = x$lower,
      upper = x$upper,
      effect = sprintf("%.*f", digits, exp(x$mu)),
      ci = sprintf("[%.*f, %.*f]", 
                   digits, exp(x$lower), 
                   digits, exp(x$upper)),
      w.random = NA,
      stringsAsFactors = FALSE
    )
    
    forest_data <- rbind(study_effects, overall_effect)
  } else {
    forest_data <- study_effects
  }
  
  # Create meta object with proper settings
  meta_obj <- try({
    metagen(TE = TE,
            seTE = seTE,
            studlab = studlab,
            data = forest_data,
            sm = x$sm,
            common = FALSE,
            random = TRUE,
            method.tau = "DL",
            hakn = FALSE)
  }, silent = TRUE)
  
  if (inherits(meta_obj, "try-error")) {
    stop("Error creating meta object: ", meta_obj)
  }
  
  # Override meta object with bivariate results
  meta_obj$TE.random <- x$mu
  meta_obj$seTE.random <- x$tau
  meta_obj$lower.random <- x$lower
  meta_obj$upper.random <- x$upper
  meta_obj$tau <- x$tau
  meta_obj$tau2 <- x$tau^2
  meta_obj$sm <- x$sm
  meta_obj$level <- x$level
  meta_obj$level.ma <- x$level.ma
  
  # Prepare heterogeneity measures
  het_measures <- sprintf(
    "Heterogeneity: τ² = %.*f; I² = %.*f%%",
    digits, x$tau2,
    1, x$I2
  )
  
  # Set up column specifications
  if (study_weights) {
    if (!weight_column %in% names(forest_data)) {
      warning("Specified weight column not found, using w.random")
      weight_column <- "w.random"
    }
    rightcols <- unique(c(rightcols, weight_column))
  }
  
  # Create the forest plot with enhanced settings
  meta::forest(meta_obj,
         leftcols = leftcols,
         rightcols = rightcols,
         leftlabs = c("Study"),
         rightlabs = c("Effect", "95% CI", "Weight"),
         xlab = xlab,
         refline = refline,
         print.tau2 = TRUE,
         col.diamond = "blue",
         col.diamond.lines = "blue",
         col.predict = "red",
         addpred = TRUE,
         digits = digits,
         fontsize = text_size,
         spacing = 1,
         squaresize = 0.8,
         hetstat = TRUE,
         print.I2 = TRUE,
         print.Q = TRUE,
         overall = TRUE,
         overall.hetstat = TRUE,
         test.overall = TRUE,
         label.left = "Favors Control",
         label.right = "Favors Treatment",
         lty.ref = 2,
         col.ref = "gray",
         smlab = if (is.null(title)) paste("Random Effects Model for", x$sm) else title,
         ...)
  
  # Return invisibly
  invisible(meta_obj)
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
  cat("  95% Confidence Interval for μ: [", round(x$ci_mu[1], 4), ", ", round(x$ci_mu[2], 4), "]\n")
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
  minus.loglik <- function(par.vec) -sum(dnorm(y.k, mean = par.vec[1], sd = sqrt(par.vec[2]^2 + sigma.2.k), log = TRUE))
  a <- nlminb(initial.value, minus.loglik)
  return(list(mu = a$par[1], tau = a$par[2]))
}

comp.tau.mu.log.OR.MLE <- function(data.tbl, initial.value) {
  # Step 1: Calculate initial estimates using DerSimonian and Laird method
  a <- comp.log.OR.y.sigma.stats(data.tbl)
  y.k <- a[[1]]
  sigma.2.k <- a[[2]]
  
  # DerSimonian and Laird estimate for tau^2
  w <- 1 / sigma.2.k
  mu.init <- sum(w * y.k) / sum(w)
  Q <- sum(w * (y.k - mu.init)^2)
  df <- length(y.k) - 1
  tau2.init <- max(0, (Q - df) / (sum(w) - sum(w^2) / sum(w)))
  
  # Initial parameter vector
  par.init <- c(mu.init, sqrt(tau2.init))
  
  # Step 2: Define the negative log-likelihood function
  neg.loglik <- function(par) {
    mu <- par[1]
    tau <- par[2]
    -sum(dnorm(y.k, mean = mu, sd = sqrt(tau^2 + sigma.2.k), log = TRUE))
  }
  
  # Step 3: Define gradient and Hessian functions
  gradient <- function(par) {
    mu <- par[1]
    tau <- par[2]
    v <- tau^2 + sigma.2.k
    d_mu <- sum((y.k - mu) / v)
    d_tau <- sum(tau * ((y.k - mu)^2 / v^2 - 1 / v))
    c(d_mu, d_tau)
  }
  
  hessian <- function(par) {
    mu <- par[1]
    tau <- par[2]
    v <- tau^2 + sigma.2.k
    h11 <- -sum(1 / v)
    h12 <- h21 <- -2 * sum(tau * (y.k - mu) / v^2)
    h22 <- sum((y.k - mu)^2 / v^2 - 1 / v - 2 * tau^2 * (y.k - mu)^2 / v^3)
    matrix(c(h11, h12, h21, h22), nrow = 2)
  }
  
  # Step 4: Implement Newton-Raphson method
  newton_raphson <- function(par, max_iter = 100, tol = 1e-6) {
    for (i in 1:max_iter) {
      g <- gradient(par)
      H <- hessian(par)
      delta <- solve(H, g)
      par_new <- par - delta
      if (sqrt(sum(delta^2)) < tol) {
        return(par_new)
      }
      par <- par_new
    }
    warning("Newton-Raphson did not converge")
    par
  }
  
  # Step 5: Run Newton-Raphson
  result <- newton_raphson(par.init)
  
  # Return results
  list(mu = result[1], tau = result[2])
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

plot.mu.tau.CI <- function(dev.mat, pval.mat, p.cntr.vec = c(0.05, 0.50), mlb = "", mu_mle = NULL, tau_mle = NULL) {
  # Extract sequences for µ and τ
  seq.mu <- sapply(strsplit(dimnames(pval.mat)[[1]], "mu ="), as.numeric)[2,]
  seq.tau <- sapply(strsplit(dimnames(pval.mat)[[2]], "tau ="), as.numeric)[2,]
  
  # Convert to exponentiated scale for effect sizes
  mu.pred.vec <- exp(seq(min(seq.mu), max(seq.mu), length.out = 100))
  tau.pred.vec <- seq(min(seq.tau), max(seq.tau), length.out = 100)
  
  # Extract 95% and 50% confidence contours using the correct method
  contour_50 <- get_contours(pval.mat, 0.50)
  contour_95 <- get_contours(pval.mat, 0.05)
  
  # Set up plot
  plot(1, type = "n", log = "x",
       xlim = c(min(mu.pred.vec), max(mu.pred.vec)),
       ylim = range(tau.pred.vec),
       xlab = "Effect Size (OR/RR)", 
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
  
  # Custom x-axis
  axis(1, at = c(0.5, 1.0, 2.0, 3.2))
  
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
calculate_iou <- function(contour1, contour2) {
  tryCatch({
    poly1 <- create_valid_polygon(contour1)
    poly2 <- create_valid_polygon(contour2)
    
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

