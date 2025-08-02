#' Helper Functions for Bivariate Meta-Analysis
#'
#' This file contains helper functions for calculating log risk ratios,
#' log odds ratios, and their variances.

#' @title Log Odds Function
#' @description Calculates the log odds from a probability
#' @param p A probability value
#' @return The log odds of the input probability
#' @export
log_odds <- function(p) log(p / (1-p))

#' @title Inverse Log Odds Function
#' @description Calculates the probability from log odds
#' @param theta A log odds value
#' @return The probability corresponding to the input log odds
#' @export
inv_log_odds <- function(theta) exp(theta) / (1 + exp(theta))

#' @title Compute Log Risk Ratio Statistics
#' @description Calculates the log risk ratio and its variance
#' @param data.tbl A data frame containing the study data with columns:
#'   event.e, n.e, event.c, n.c
#' @return A list containing the log risk ratios and their variances
#' @examples
#' data <- data.frame(event.e = c(10, 15), n.e = c(100, 120), 
#'                    event.c = c(5, 8), n.c = c(100, 110))
#' result <- log_rr(data)
#' @export
log_rr <- function(data.tbl) {
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
#' @param data.tbl A data frame containing the study data with columns:
#'   event.e, n.e, event.c, n.c
#' @return A list containing the log odds ratios and their variances
#' @examples
#' data <- data.frame(event.e = c(10, 15), n.e = c(100, 120), 
#'                    event.c = c(5, 8), n.c = c(100, 110))
#' result <- log_or(data)
#' @export
log_or <- function(data.tbl) {
  n.11 <- data.tbl$event.e
  n.21 <- data.tbl$event.c
  n.12 <- data.tbl$n.e - data.tbl$event.e
  n.22 <- data.tbl$n.c - data.tbl$event.c
  pls.5 <- 0.5 * (n.11 == 0 | n.12 == 0 | n.21 == 0 | n.22 == 0)
  y.k <- log((n.11 + pls.5) * (n.22 + pls.5) / ((n.12 + pls.5) * (n.21 + pls.5)))
  sigma.2.k <- 1 / (n.11 + pls.5) + 1 / (n.12 + pls.5) + 1 / (n.21 + pls.5) + 1 / (n.22 + pls.5)
  
  return(list(y.k, sigma.2.k))
}

#' @title Calculate Variance of Log Risk Ratio
#' @description Calculates the variance of the log risk ratio
#' @param event.e Event count in experimental group
#' @param n.e Sample size in experimental group
#' @param event.c Event count in control group
#' @param n.c Sample size in control group
#' @return The variance of the log risk ratio
#' @examples
#' variance_log_rr(10, 100, 5, 100)
#' @export
variance_log_rr <- function(event.e, n.e, event.c, n.c) {
  pls.5 <- 0.5 * (event.e == 0 | (n.e - event.e) == 0 | event.c == 0 | (n.c - event.c) == 0)
  pi.1g1 <- (event.e + pls.5) / (n.e + 2 * pls.5)
  pi.1g2 <- (event.c + pls.5) / (n.c + 2 * pls.5)
  
  variance <- (1 - pi.1g1) / (pi.1g1 * (n.e + 2 * pls.5)) +
              (1 - pi.1g2) / (pi.1g2 * (n.c + 2 * pls.5))
  
  return(variance)
}

#' @title Calculate Variance of Log Odds Ratio
#' @description Calculates the variance of the log odds ratio
#' @param event.e Event count in experimental group
#' @param n.e Sample size in experimental group
#' @param event.c Event count in control group
#' @param n.c Sample size in control group
#' @return The variance of the log odds ratio
#' @examples
#' variance_log_or(10, 100, 5, 100)
#' @export
variance_log_or <- function(event.e, n.e, event.c, n.c) {
  n.11 <- event.e
  n.21 <- event.c
  n.12 <- n.e - event.e
  n.22 <- n.c - event.c
  pls.5 <- 0.5 * (n.11 == 0 | n.12 == 0 | n.21 == 0 | n.22 == 0)
  
  variance <- 1 / (n.11 + pls.5) + 1 / (n.12 + pls.5) + 1 / (n.21 + pls.5) + 1 / (n.22 + pls.5)
  
  return(variance)
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
comp_tau_mu_mle <- function(data.tbl, initial.value, sm, y.k.in = NULL, sigma.2.k.in = NULL) {
  if (is.null(y.k.in) || is.null(sigma.2.k.in)) {
    a <- switch(sm,
                "RR" = log_rr(data.tbl),
                "OR" = log_or(data.tbl))
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
comp_tau_mu_dev_pvals <- function(data.tbl, mu.vec.tst, tau.vec.tst, sm, y.k.in = NULL, sigma.2.k.in = NULL) {
  n.mu <- length(mu.vec.tst)
  n.tau <- length(tau.vec.tst)

  if (is.null(y.k.in) || is.null(sigma.2.k.in)) {
    a <- switch(sm,
                "RR" = log_rr(data.tbl),
                "OR" = log_or(data.tbl))
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