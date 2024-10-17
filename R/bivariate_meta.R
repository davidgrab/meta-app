# metabiv: Bivariate Meta-Analysis for Binary Outcomes
# This package implements the methodology described in Saad et al. (2019)
# for conducting bivariate meta-analysis in the presence of unexplained heterogeneity.

# Required libraries
library(meta)
library(ggplot2)
library(plotly)

#' Bivariate Meta-Analysis for Binary Outcomes
#'
#' @param event.e A numeric vector of event counts in the experimental group.
#' @param n.e A numeric vector of sample sizes in the experimental group.
#' @param event.c A numeric vector of event counts in the control group.
#' @param n.c A numeric vector of sample sizes in the control group.
#' @param studlab A character vector of study labels (optional).
#' @param sm A character string specifying the summary measure ("RR", "OR").
#' @param level A numeric value specifying the confidence level for individual study estimates (default is 0.95).
#' @param level.ma A numeric value specifying the confidence level for the meta-analysis estimate (default is 0.95).
#' @return A list with class "metabiv" containing the results of the bivariate meta-analysis.
#' @export
metabiv <- function(event.e, n.e, event.c, n.c, studlab = NULL,
                    data = NULL, sm = "RR",
                    level = 0.95, level.ma = 0.95) {
  
  # Data preparation
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
  
  # Create data table
  data.tbl <- data.frame(studlab, event.e, n.e, event.c, n.c)
  
  # Calculate effect sizes and variances based on summary measure
  res <- switch(sm,
                "RR" = comp.log.RR.y.sigma.stats(data.tbl),
                "OR" = comp.log.OR.y.sigma.stats(data.tbl),
                stop("Unsupported summary measure"))
  y.k <- res[[1]]
  sigma.2.k <- res[[2]]
  
  # Estimate tau and mu using MLE
  initial.value <- c(0.4, 0.4)
  mle_result <- comp.tau.mu.MLE(data.tbl, initial.value, sm)
  
  # Calculate deviance and p-values
  mu.vec <- seq(-1, 1, length.out = 100)
  tau.vec <- seq(0.01, 1, length.out = 100)
  dev_pvals <- comp.tau.mu.dev.pvals(data.tbl, mu.vec, tau.vec, sm)
  
  # Compute confidence region
  conf_region <- compute_confidence_region(dev_pvals[[2]], level.ma)
  
  # Diagnostic Plot
  draw.diagnostic.plot(data.tbl, mlb = "Diagnostic plot")
  
  # Exact tests or normal approximation for rr
  exact_test_results <- if (any(n.e + n.c < 50)) {
    if (sm == "OR") {
      exact_tests(data.tbl, sm)
    } else {
      normal_approximation(data.tbl, sm)
    }
  } else {
    NULL
  }
  
  # Prepare results
  res <- list(
    studlab = studlab,
    event.e = event.e, n.e = n.e,
    event.c = event.c, n.c = n.c,
    y.k = y.k,
    sigma.2.k = sigma.2.k,
    sm = sm,
    level = level,
    level.ma = level.ma,
    mu = mle_result$mu,
    tau = mle_result$tau,
    dev_pvals = dev_pvals,
    conf_region = conf_region,
    exact_tests = exact_test_results,
    call = match.call(),
    tbl = data.tbl
  )
  
  class(res) <- c("metabiv", "meta")
  res
}

# Function for exact tests (OR only)
exact_tests <- function(data.tbl, sm) {
  if (sm == "OR") {
    samp_data <- samp.log.OR.data.tbl(data.tbl)
    exact_test_result <- comp.tau.mu.log.OR.dev.pvals(samp_data, mu.vec.tst = seq(-1, 1, length = 100), tau.vec.tst = seq(0.01, 1, length = 100))
    return(exact_test_result)
  } else {
    stop("Exact tests currently only supported for OR")
  }
}

# Function for normal approximation (RR)
normal_approximation <- function(data.tbl, sm) {
  if (sm == "RR") {
    approx_result <- comp.tau.mu.log.RR.dev.pvals(data.tbl, mu.vec.tst = seq(-1, 1, length = 100), tau.vec.tst = seq(0.01, 1, length = 100))
    return(approx_result)
  } else {
    stop("Normal approximation is only available for RR.")
  }
}

# Function for generating diagnostic plots
draw.diagnostic.plot <- function(tbl, mlb = "Diagnostic plot for Meta-analysis") {
  ggplot(tbl, aes(x = event.e/n.e, y = event.c/n.c)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    labs(x = "Treatment Group Success Rate", y = "Control Group Success Rate", title = mlb) +
    theme_minimal()
}

# Deviance statistic calculation
comp.tau.mu.dev.pvals <- function(data.tbl, mu.vec.tst, tau.vec.tst, sm) {
  n.mu <- length(mu.vec.tst)
  n.tau <- length(tau.vec.tst)
  a <- switch(sm,
              "RR" = comp.log.RR.y.sigma.stats(data.tbl),
              "OR" = comp.log.OR.y.sigma.stats(data.tbl))
  y.k <- a[[1]]
  sigma.2.k <- a[[2]]
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

# Helper functions for effect size calculations

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

#comp.log.RR.y.sigma.stats <- function(data.tbl) {
#  n.11 <- data.tbl[,2]
# n.21 <- data.tbl[,4]
# n.12 <- data.tbl[,3] - data.tbl[,2]
#  n.22 <- data.tbl[,5] - data.tbl[,4]
#  pls.5 <- 0.5 * (n.11 == 0 | n.12 == 0 | n.21 == 0 | n.22 == 0)
#  pi.1g1 <- (n.11+pls.5) / (n.11 + n.12 + 2*pls.5)
#  pi.1g2 <- (n.21+pls.5) / (n.21 + n.22 + 2*pls.5)
#  sigma.2.k <- (1 - pi.1g1) / ( pi.1g1 * (n.11 + n.12 + 2*pls.5) ) + (1 - pi.1g2) / ( pi.1g2 * (n.21 + n.22 + 2*pls.5) )   
#  y.k <- log(pi.1g1 / pi.1g2)
#  return(list(y.k, sigma.2.k))
#}


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



# Maximum Likelihood Estimation
comp.tau.mu.MLE <- function(data.tbl, initial.value, sm) {
  a <- switch(sm,
              "RR" = comp.log.RR.y.sigma.stats(data.tbl),
              "OR" = comp.log.OR.y.sigma.stats(data.tbl))
  y.k <- a[[1]]
  sigma.2.k <- a[[2]]
  
  minus.loglik <- function(par.vec) -sum(dnorm(y.k, mean = par.vec[1], sd = sqrt(par.vec[2]^2 + sigma.2.k), log = TRUE))
  a <- nlminb(initial.value, minus.loglik)
  
  return(list(mu = a$par[1], tau = a$par[2]))
}


# Compute confidence region
compute_confidence_region <- function(pval.mat, level.ma) {
  conf_region <- which(pval.mat >= (1 - level.ma), arr.ind = TRUE)
  mu.vec <- as.numeric(gsub("mu = ", "", rownames(pval.mat)))
  tau.vec <- as.numeric(gsub("tau = ", "", colnames(pval.mat)))
  
  return(list(mu = mu.vec[conf_region[, 1]], tau = tau.vec[conf_region[, 2]]))
}

#' Plot method for metabiv objects
#'
#' @param x A metabiv object
#' @param type The type of plot to create: "region", "cdf", or "superiority"
#' @param ... Additional arguments passed to the plotting function
#' @export
plot.metabiv <- function(x, type = "region", ...) {
  if (type == "region") {
    plot_confidence_region(x)
  } else if (type == "cdf") {
    plot_cdf(x)
  } else if (type == "superiority") {
    plot_superiority(x)
  }
}

plot.mu.tau.CI <- function(dev.mat, pval.mat, p.cntr.vec = c(0.05, 0.50), N.sig = 100, mlb = "", new.plot = TRUE, ccol = 3, mu_mle = NULL, tau_mle = NULL) {
  n.mu <- dim(pval.mat)[1]
  n.tau <- dim(pval.mat)[2]
  seq.mu <- sapply((strsplit(dimnames(pval.mat)[[1]], "mu =")), as.numeric)[2,]
  seq.tau <- sapply((strsplit(dimnames(pval.mat)[[2]], "tau =")), as.numeric)[2,]
  x.mu <- rep(seq.mu, n.tau)
  x.tau <- rep(seq.tau, each = n.mu)
  
  mu.pred.vec <- seq(min(seq.mu), max(seq.mu), length = 100)
  tau.pred.vec <- seq(min(seq.tau), max(seq.tau), length = 100)
  
  logit.p <- log.odds(c(pval.mat))
  logit.p[c(pval.mat) < 1/N.sig] <- log.odds(1/N.sig) - (log.odds(2/N.sig) - log.odds(1/N.sig))
  logit.p[c(pval.mat) > 1 - 1/N.sig] <- log.odds((N.sig-1)/N.sig) + (log.odds((N.sig-1)/N.sig) - log.odds((N.sig-2)/N.sig))
  
  logit.p.loess <- loess(logit.p ~ x.mu + x.tau, span = .1)
  smth.pval.mat <- array(inv.log.odds(predict(logit.p.loess, data.frame(x.mu = rep(mu.pred.vec, 100), x.tau = rep(tau.pred.vec, each = 100)))), dim=c(100, 100))
  dimnames(smth.pval.mat) <- list(paste("mu = ", round(mu.pred.vec, 2)), paste("tau = ", round(tau.pred.vec, 3)))
  
  if(new.plot) {
    # Create a new plot
    contour(mu.pred.vec, tau.pred.vec, smth.pval.mat, levels = p.cntr.vec, col = ccol, lwd = 2, 
            drawlabels = FALSE, xlab = "mu", ylab = "tau", main = mlb)
  } else {
    # Add to existing plot
    contour(mu.pred.vec, tau.pred.vec, smth.pval.mat, levels = p.cntr.vec, col = ccol, lwd = 2, 
            drawlabels = FALSE, add = TRUE)
  }
  
  # Plot the dev.mat contour with dashed lines
  contour(seq.mu, seq.tau, dev.mat, col = ccol, lty = 2, 
          levels = round(qchisq(1 - p.cntr.vec, 2), 1), add = TRUE)
  
  # Add MLE point
  if(is.null(mu_mle) || is.null(tau_mle)) {
    # If MLE not provided, estimate from dev.mat
    mle_index <- which(dev.mat == min(dev.mat), arr.ind = TRUE)[1,]
    mu_mle <- seq.mu[mle_index[1]]
    tau_mle <- seq.tau[mle_index[2]]
  }
  
  # Add MLE point as a smaller green cross
  points(mu_mle, tau_mle, pch = 3, col = "green", cex = 1.2, lwd = 2)
  
  # Return MLE values
  return(list(mu_mle = mu_mle, tau_mle = tau_mle))
}


plot_confidence_region <- function(x) {
  plot.mu.tau.CI(x$dev_pvals[[1]], x$dev_pvals[[2]], 
                 mlb = "Confidence Region for (μ, τ)",
                 mu_mle = x$mu, tau_mle = x$tau)
}


plot_cdf <- function(x) {
  CDF.ci.obj <- comp.mu.tau.dev.CDF.CI(x$dev_pvals)
  plot(exp(CDF.ci.obj[[2]]), CDF.ci.obj[[1]], type = "l",
       xlab = paste("Effect Size (", x$sm, ")", sep=""), ylab = "Cumulative Probability",
       main = paste("CDF of", x$sm, "Distribution"))
  lines(exp(CDF.ci.obj[[3]]), CDF.ci.obj[[1]], lty = 2)
  lines(exp(CDF.ci.obj[[4]]), CDF.ci.obj[[1]], lty = 2)
}

plot_superiority <- function(x) {
  comp.eff.harm.plot(comp.mu.tau.dev.CDF.CI(x$dev_pvals),
                     efficacy.is.OR.le1 = (x$sm == "OR"),
                     mlb = paste("Superiority/Inferiority Plot for", x$sm))
}

# Helper functions for plotting

compute_cdf <- function(x) {
  r_vec <- exp(seq(-2, 2, length.out = 100))
  cdf <- pnorm((transform_effect_size(r_vec, x$sm) - x$mu) / x$tau)
  cdf_lower <- pnorm((transform_effect_size(r_vec, x$sm) - min(x$conf_region$mu)) / max(x$conf_region$tau))
  cdf_upper <- pnorm((transform_effect_size(r_vec, x$sm) - max(x$conf_region$mu)) / min(x$conf_region$tau))
  
  data.frame(r = r_vec, cdf = cdf, cdf_lower = cdf_lower, cdf_upper = cdf_upper)
}

# Continuing from the previous code...

compute_superiority <- function(x) {

  r_vec <- exp(seq(-2, 2, length.out = 100))
  
  superiority <- 1 - pnorm((transform_effect_size(r_vec, x$sm) - x$mu) / x$tau)
  superiority_lower <- 1 - pnorm((transform_effect_size(r_vec, x$sm) - max(x$conf_region$mu)) / min(x$conf_region$tau))
  superiority_upper <- 1 - pnorm((transform_effect_size(r_vec, x$sm) - min(x$conf_region$mu)) / max(x$conf_region$tau))
  
  inferiority <- pnorm((transform_effect_size(r_vec, x$sm) - x$mu) / x$tau)
  inferiority_lower <- pnorm((transform_effect_size(r_vec, x$sm) - max(x$conf_region$mu)) / min(x$conf_region$tau))
  inferiority_upper <- pnorm((transform_effect_size(r_vec, x$sm) - min(x$conf_region$mu)) / max(x$conf_region$tau))
  
  data.frame(r = r_vec, 
             superiority = superiority, 
             superiority_lower = superiority_lower, 
             superiority_upper = superiority_upper,
             inferiority = inferiority,
             inferiority_lower = inferiority_lower,
             inferiority_upper = inferiority_upper)
}

# Helper function to transform effect sizes
transform_effect_size <- function(r, sm) {
  switch(sm,
         "RR" = log(r),
         "OR" = log(r),
         stop("Unsupported summary measure"))
}

#' Summary method for metabiv objects
#'
#' @param object A metabiv object
#' @param ... Additional arguments (not used)
#' @export
summary.metabiv <- function(object, ...) {
  cat("Bivariate Meta-Analysis\n\n")
  cat("Summary Measure:", object$sm, "\n")
  cat("Number of studies:", length(object$y.k), "\n\n")
  cat("Random effects model:\n")
  cat("  μ estimate:", round(object$mu, 4), "\n")
  cat("  τ estimate:", round(object$tau, 4), "\n")
  cat("  95% Confidence Region:\n")
  cat("    μ range: [", round(min(object$conf_region$mu), 4), ", ", round(max(object$conf_region$mu), 4), "]\n")
  cat("    τ range: [", round(min(object$conf_region$tau), 4), ", ", round(max(object$conf_region$tau), 4), "]\n")
}

#' Forest plot method for metabiv objects
#'
#' @param x A metabiv object
#' @param xlab Label for the x-axis (default: "Effect Size")
#' @param refline Reference line (default: 0)
#' @param leftcols Vector of column names to include on the left side of the plot
#' @param rightcols Vector of column names to include on the right side of the plot
#' @param digits Number of digits for rounding (default: 2)
#' @param ... Additional arguments passed to the forest function
#'
#' @importFrom meta forest metagen
#' @importFrom graphics par
#'
#' @export
forest.metabiv <- function(x, xlab = "Effect Size", refline = 0, 
                           leftcols = c("studlab"), 
                           rightcols = c("effect", "ci"), 
                           digits = 2, ...) {
  
  # Input validation
  if (!inherits(x, "metabiv")) {
    stop("Input must be a metabiv object")
  }
  
  # Print diagnostic information
  cat("Input data summary:\n")
  print(summary(x$y.k))
  print(summary(x$sigma.2.k))
  print(summary(x$mu))
  print(summary(x$tau))
  
  # Create a data frame for the forest plot
  forest_data <- data.frame(
    studlab = x$studlab,
    TE = x$y.k,
    seTE = sqrt(pmax(x$sigma.2.k, 0)),  # Ensure non-negative values
    lower = x$y.k - qnorm(0.975) * sqrt(pmax(x$sigma.2.k, 0)),
    upper = x$y.k + qnorm(0.975) * sqrt(pmax(x$sigma.2.k, 0)),
    effect = sprintf("%.*f", digits, x$y.k),
    ci = sprintf("[%.*f, %.*f]", digits, 
                 x$y.k - qnorm(0.975) * sqrt(pmax(x$sigma.2.k, 0)),
                 digits, 
                 x$y.k + qnorm(0.975) * sqrt(pmax(x$sigma.2.k, 0)))
  )
  
  # Add the overall effect
  forest_data <- rbind(forest_data,
                       data.frame(
                         studlab = "Overall",
                         TE = x$mu,
                         seTE = max(x$tau, 0),  # Ensure non-negative value
                         lower = x$mu - qnorm(0.975) * max(x$tau, 0),
                         upper = x$mu + qnorm(0.975) * max(x$tau, 0),
                         effect = sprintf("%.*f", digits, x$mu),
                         ci = sprintf("[%.*f, %.*f]", digits, 
                                      x$mu - qnorm(0.975) * max(x$tau, 0),
                                      digits, 
                                      x$mu + qnorm(0.975) * max(x$tau, 0))
                       ))
  
  # Remove any rows with NA or infinite values
  forest_data <- forest_data[complete.cases(forest_data) & 
                               is.finite(rowSums(forest_data[,c("TE","seTE","lower","upper")])), ]
  
  # Check if we have any valid data left
  if(nrow(forest_data) == 0) {
    stop("No valid data available for forest plot")
  }
  
  # Create the forest plot
  tryCatch({
    # Set up the plotting area
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mar = c(4, 4, 2, 2) + 0.1)
    
    # Create meta object
    m <- metagen(TE = TE, seTE = seTE, studlab = studlab, data = forest_data,
                 sm = x$sm, fixed = FALSE, random = TRUE)
    
    # Generate forest plot
    forest(m,
           leftcols = leftcols,
           rightcols = rightcols,
           leftlabs = c("Study", "Effect Size", "95% CI"),
           xlab = xlab,
           refline = refline,
           print.tau2 = FALSE,
           col.diamond = "blue",
           col.diamond.lines = "blue",
           col.predict = "red",
           addpred = TRUE,
           smlab = paste("Random Effects Model for", x$sm),
           hetstat = FALSE,
           overall = TRUE,
           ...)
    
  }, error = function(e) {
    cat("Error in creating forest plot:", conditionMessage(e), "\n")
    cat("Data used for forest plot:\n")
    print(forest_data)
    stop("Forest plot creation failed")
  })
}

#' Efficacy/Harm Plot
#'
#' @param x A metabiv object
#' @param efficacy.is.le1 Logical, whether efficacy is defined as effect <= 1 (for RR/OR) 
#' @param title Plot title
#' @param xlab X-axis label
#' @param min.effect Minimum effect size to display
#' @param max.effect Maximum effect size to display
#' @export
comp.eff.harm.plot <- function(CDF.ci.obj, efficacy.is.OR.le1 = TRUE, mlb = "Efficacy/Harm Plot", 
                               xlb = "Efficacy/Harm", min.OR = 0.3, max.OR = 3) {
  x.seq <- exp(seq(-5, log(10), length = 1000))
  x.est.taper <- c(seq(exp(-5), min(exp(CDF.ci.obj[[2]])), length = 50), exp(CDF.ci.obj[[2]]),
                   seq(max(exp(CDF.ci.obj[[2]])), 10, length = 50))
  x.ll.taper <- c(seq(exp(-5), min(exp(CDF.ci.obj[[3]])), length = 50), exp(CDF.ci.obj[[3]]),
                  seq(max(exp(CDF.ci.obj[[3]])), 10, length = 50))
  x.ul.taper <- c(seq(exp(-5), min(exp(CDF.ci.obj[[4]])), length = 50), exp(CDF.ci.obj[[4]]),
                  seq(max(exp(CDF.ci.obj[[4]])), 10, length = 50))
  
  cdf.est <- approx(x.est.taper, c(rep(0, 50), CDF.ci.obj[[1]], rep(1, 50)), xout = x.seq)$y
  cdf.ll <- approx(x.ll.taper, c(rep(0, 50), CDF.ci.obj[[1]], rep(1, 50)), xout = x.seq)$y
  cdf.ul <- approx(x.ul.taper, c(rep(0, 50), CDF.ci.obj[[1]], rep(1, 50)), xout = x.seq)$y
  
  if (efficacy.is.OR.le1) {
    le1.col <- 3
    mt1.col <- 2
  } else {
    le1.col <- 2
    mt1.col <- 3
  }
  le1.vec <- exp(seq(log(min.OR), 0, length = 200))
  le1.est <- approx(x.seq, cdf.est, xout = le1.vec)$y
  le1.ll <- approx(x.seq, cdf.ll, xout = le1.vec)$y
  le1.ul <- approx(x.seq, cdf.ul, xout = le1.vec)$y
  
  plot(le1.vec, le1.est, type = "l", log = "x", xlim = c(min.OR, max.OR), ylim = c(0, 1), col = le1.col, lty = 2, lwd = 3,
       main = mlb, ylab = "Probability", xlab = xlb)
  lines(le1.vec, le1.ll, col = le1.col, lty = 1, lwd = 2)
  lines(le1.vec, le1.ul, col = le1.col, lty = 1, lwd = 2)
  
  mt1.vec <- exp(seq(0, log(max.OR), length = 200))
  mt1.est <- approx(x.seq, 1 - cdf.est, xout = mt1.vec)$y
  mt1.ll <- approx(x.seq, 1 - cdf.ll, xout = mt1.vec)$y
  mt1.ul <- approx(x.seq, 1 - cdf.ul, xout = mt1.vec)$y
  
  lines(mt1.vec, mt1.est, col = mt1.col, lty = 2, lwd = 3)
  lines(mt1.vec, mt1.ll, col = mt1.col, lty = 1, lwd = 2)
  lines(mt1.vec, mt1.ul, col = mt1.col, lty = 1, lwd = 2)
  
  abline(v = 1, lwd = 0.5)
}

#' Compute I-squared statistic
#'
#' @param x A metabiv object
#' @return The I-squared statistic
#' @export
compute_i_squared <- function(x) {
  Q <- sum((x$y.k - x$mu)^2 / x$sigma.2.k)
  df <- length(x$y.k) - 1
  I2 <- max(0, 100 * (Q - df) / Q)
  return(I2)
}

#' Leave-one-out analysis
#'
#' @param x A metabiv object
#' @return A list of metabiv objects, each with one study removed
#' @export
leave_one_out <- function(x) {
  k <- length(x$y.k)
  loo_results <- lapply(1:k, function(i) {
    metabiv(event.e = x$event.e[-i], n.e = x$n.e[-i],
            event.c = x$event.c[-i], n.c = x$n.c[-i],
            studlab = x$studlab[-i], sm = x$sm,
            level = x$level, level.ma = x$level.ma)
  })
  names(loo_results) <- paste("Omit", x$studlab)
  return(loo_results)
}


compute_i_squared <- function(x) {
  Q <- sum((x$y.k - x$mu)^2 / x$sigma.2.k)
  df <- length(x$y.k) - 1
  I2 <- max(0, 100 * (Q - df) / Q)
  return(I2)
}

leave_one_out <- function(x) {
  k <- length(x$y.k)
  loo_results <- lapply(1:k, function(i) {
    metabiv(event.e = x$event.e[-i], n.e = x$n.e[-i],
            event.c = x$event.c[-i], n.c = x$n.c[-i],
            studlab = x$studlab[-i], sm = x$sm,
            level = x$level, level.ma = x$level.ma)
  })
  names(loo_results) <- paste("Omit", x$studlab)
  return(loo_results)
}


bivariate_grade_assessment <- function(bivariate_result, risk_of_bias, indirectness) {
  cat("Bivariate GRADE Assessment\n\n")
  cat("Risk of Bias:", risk_of_bias, "\n")
  cat("Indirectness:", indirectness, "\n")
  
  # Inconsistency (based on tau)
  inconsistency <- if(bivariate_result$tau > 0.5) "High" else if(bivariate_result$tau > 0.3) "Moderate" else "Low"
  cat("Inconsistency:", inconsistency, "\n")
  
  # Imprecision (based on the ratio of mu to tau)
  imprecision <- if(abs(bivariate_result$mu) / bivariate_result$tau < 2) "High" else "Low"
  cat("Imprecision:", imprecision, "\n")
  
  # Publication Bias (simplified test)
  pub_bias_test <- lm(bivariate_result$y.k ~ sqrt(bivariate_result$sigma.2.k))
  pub_bias <- if(abs(coef(pub_bias_test)[1]) > 0.1) "Suspected" else "Not suspected"
  cat("Publication Bias:", pub_bias, "\n\n")
  
  # Overall GRADE score
  grade_score <- 4  # Start with high quality
  if (risk_of_bias == "High") grade_score <- grade_score - 1
  if (indirectness == "High") grade_score <- grade_score - 1
  if (inconsistency == "High") grade_score <- grade_score - 2
  else if (inconsistency == "Moderate") grade_score <- grade_score - 1
  if (imprecision == "High") grade_score <- grade_score - 1
  if (pub_bias == "Suspected") grade_score <- grade_score - 1
  
  overall_grade <- switch(as.character(max(0, grade_score)),
                          "4" = "High",
                          "3" = "Moderate",
                          "2" = "Low",
                          "1" = "Very Low",
                          "0" = "Very Low")
  
  cat("Overall GRADE:", overall_grade, "\n")
}


bivariate_gosh_plot <- function(bivariate_result, n_subsets = 1000, subset_size = NULL) {
  data <- bivariate_result$tbl
  k <- nrow(data)
  
  if (is.null(subset_size)) subset_size <- max(2, floor(k/2))
  
  subsets <- replicate(n_subsets, sample(1:k, size = subset_size, replace = FALSE))
  
  gosh_results <- apply(subsets, 2, function(subset) {
    res <- metabiv(event.e = data$event.e[subset], 
                   n.e = data$n.e[subset], 
                   event.c = data$event.c[subset], 
                   n.c = data$n.c[subset],
                   studlab = data$studlab[subset],
                   sm = bivariate_result$sm)
    c(mu = res$mu, tau = res$tau)
  })
  
  gosh_df <- as.data.frame(t(gosh_results))
  
  p <- plot_ly(data = gosh_df, x = ~mu, y = ~tau, type = "scatter", mode = "markers",
               marker = list(size = 3, opacity = 0.5)) %>%
    layout(title = paste("GOSH Plot for", bivariate_result$sm),
           xaxis = list(title = "μ"),
           yaxis = list(title = "τ"))
  
  return(p)
}


confidence_region_shift_plot <- function(x, alpha = 0.05) {
  k <- nrow(x$tbl)
  sm <- x$sm  # Get the summary measure (OR or RR)
  
  if (sm == "OR") {
    # OR calculations (as before)
    comp_func <- comp.log.OR.y.sigma.stats
    dev_func <- comp.tau.mu.log.OR.dev.pvals
    mle_func <- comp.tau.mu.log.OR.MLE
    
    # Compute full model CI
    aa1 <- dev_func(x$tbl, 
                    mu.vec.tst = seq(0, 1, length = 100), 
                    tau.vec.tst = seq(0.01, 1, length = 100))
    aa2 <- mle_func(x$tbl, initial.value = c(0.4, 0.4))
    
    # Function to compute CI contours
    get_contours <- function(pval_mat, level) {
      contourLines(seq(0, 1, length = 100), seq(0.01, 1, length = 100), 
                   pval_mat, levels = level)
    }
    
    mu_range <- c(0, 1)
    tau_range <- c(0.01, 1)
    
  } else if (sm == "RR") {
    # RR calculations (new method)
    comp_func <- comp.log.RR.y.sigma.stats
    dev_func <- comp.tau.mu.log.RR.dev.pvals
    mle_func <- comp.tau.mu.log.RR.MLE
    
    # Compute full model CI
    mu_range <- range(x$y.k) + c(-1, 1) * 2 * sqrt(max(x$sigma.2.k))
    tau_range <- c(0.01, max(2, 2 * sqrt(max(x$sigma.2.k))))
    
    # Compute full model CI
    aa1 <- dev_func(x$tbl, 
                    mu.vec.tst = seq(0, 1, length = 100), 
                    tau.vec.tst = seq(0.01, 1, length = 100))
    aa2 <- mle_func(x$tbl, initial.value = c(0.4, 0.4))
    
    # Function to compute CI contours
    # Function to compute CI contours
    get_contours <- function(pval_mat, level) {
      contourLines(seq(0, 1, length = 100), seq(0.01, 1, length = 100), 
                   pval_mat, levels = level)
    }
     
  } else {
    stop("Unsupported summary measure. Use 'OR' or 'RR'.")
  }
  
  full_contour_50 <- get_contours(aa1[[2]], 0.50)
  full_contour_95 <- get_contours(aa1[[2]], 0.05)
  
  # Compute leave-one-out estimates and CIs
  loo_results <- lapply(1:k, function(i) {
    tbl_mod <- x$tbl[-i, ]
    aa2_i <- mle_func(tbl_mod, initial.value = c(0.4, 0.4))
    aa_i <- dev_func(tbl_mod, 
                     mu.vec.tst = seq(0, 1, length = 100), 
                     tau.vec.tst = seq(0.01, 1, length = 100))
    contour_50 <- get_contours(aa_i[[2]], 0.50)
    contour_95 <- get_contours(aa_i[[2]], 0.05)
    
    # Calculate metrics
    iou <- calculate_iou(contour_95[[1]], full_contour_95[[1]])
    shift <- calculate_shift(aa2$mu, aa2_i$mu)
    width_diff <- calculate_width_diff(full_contour_95[[1]], contour_95[[1]])
    hellinger <- calculate_hellinger(full_contour_95[[1]]$x, contour_95[[1]]$x)
    kld <- calculate_kld(full_contour_95[[1]]$x, contour_95[[1]]$x)
    rmse <- calculate_rmse_bounds(full_contour_95[[1]], contour_95[[1]])
    coverage_prob <- calculate_coverage_prob(full_contour_95[[1]], contour_95[[1]])
    combined_score <- calculate_combined_score(iou, hellinger, kld, rmse, coverage_prob)
    
    list(mu = aa2_i$mu, tau = aa2_i$tau, 
         contour_50 = contour_50, contour_95 = contour_95, 
         iou = iou, shift = shift, width_diff = width_diff, 
         hellinger = hellinger, kld = kld, rmse = rmse, 
         coverage_prob = coverage_prob, combined_score = combined_score)
  })
  
  # Prepare data for plotting
  plot_data <- data.frame(
    study = 1:k,
    mu = sapply(loo_results, function(x) x$mu),
    tau = sapply(loo_results, function(x) x$tau),
    iou = sapply(loo_results, function(x) x$iou),
    shift = sapply(loo_results, function(x) x$shift),
    width_diff = sapply(loo_results, function(x) x$width_diff),
    hellinger = sapply(loo_results, function(x) x$hellinger),
    kld = sapply(loo_results, function(x) x$kld),
    rmse = sapply(loo_results, function(x) x$rmse),
    coverage_prob = sapply(loo_results, function(x) x$coverage_prob),
    combined_score = sapply(loo_results, function(x) x$combined_score)
  )
  
  # Compute relative shifts
  plot_data$rel_shift <- sqrt((plot_data$mu - aa2$mu)^2 + 
                                (plot_data$tau - aa2$tau)^2)
  
  # Create the plotly object
  p <- plot_ly()
  
  # Add full model contours
  p <- add_trace(p, x = full_contour_95[[1]]$x, y = full_contour_95[[1]]$y,
                 type = "scatter", mode = "lines", line = list(color = "green", width = 1),
                 name = "Full Model 95% CI")
  p <- add_trace(p, x = full_contour_50[[1]]$x, y = full_contour_50[[1]]$y,
                 type = "scatter", mode = "lines", line = list(color = "green", width = 1),
                 name = "Full Model 50% CI")
  
  # Add leave-one-out contours with high transparency
  for (i in 1:k) {
    contour_50 <- loo_results[[i]]$contour_50
    contour_95 <- loo_results[[i]]$contour_95
    
    p <- add_trace(p, x = contour_95[[1]]$x, y = contour_95[[1]]$y,
                   type = "scatter", mode = "lines",
                   line = list(color = "blue", width = 2),
                   opacity = 0.05, showlegend = FALSE, hoverinfo = "none",
                   name = paste0("LOO Contour 95 - Study ", i))
    p <- add_trace(p, x = contour_50[[1]]$x, y = contour_50[[1]]$y,
                   type = "scatter", mode = "lines",
                   line = list(color = "blue", width = 2),
                   opacity = 0.05, showlegend = FALSE, hoverinfo = "none",
                   name = paste0("LOO Contour 50 - Study ", i))
  }
  
  # Add non-transparent hover points with full hover information
  for (i in 1:k) {
    hover_text <- sprintf(
      "Study: %d (omitted)<br>μ: %.3f<br>τ: %.3f<br>Relative Shift: %.3f<br>IoU: %.3f<br>hellinger: %.3f<br>kld: %.3f<br>rmse: %.3f<br>coverage_prob: %.3f<br>combined_score: %.3f<br>50%% CI: (%.3f, %.3f) to (%.3f, %.3f)<br>95%% CI: (%.3f, %.3f) to (%.3f, %.3f)",
      i, plot_data$mu[i], plot_data$tau[i], plot_data$rel_shift[i], plot_data$iou[i],
      plot_data$hellinger[i], plot_data$kld[i], plot_data$rmse[i], plot_data$coverage_prob[i], plot_data$combined_score[i],
      min(sapply(loo_results[[i]]$contour_50, function(x) min(x$x))), min(sapply(loo_results[[i]]$contour_50, function(x) min(x$y))),
      max(sapply(loo_results[[i]]$contour_50, function(x) max(x$x))), max(sapply(loo_results[[i]]$contour_50, function(x) max(x$y))),
      min(sapply(loo_results[[i]]$contour_95, function(x) min(x$x))), min(sapply(loo_results[[i]]$contour_95, function(x) min(x$y))),
      max(sapply(loo_results[[i]]$contour_95, function(x) max(x$x))), max(sapply(loo_results[[i]]$contour_95, function(x) max(x$y)))
    )
    
    p <- add_trace(p, x = plot_data$mu[i], y = plot_data$tau[i],
                   type = "scatter", mode = "markers+text",
                   marker = list(size = 8, 
                                 color = plot_data$rel_shift[i], 
                                 colorscale = "Viridis",
                                 showscale = TRUE),
                   text = i,
                   textposition = "middle center",
                   textfont = list(color = "white", size = 5),
                   hoverinfo = "text", 
                   hovertext = hover_text,
                   showlegend = FALSE,
                   customdata = i,
                   name = paste0("Point - Study ", i))
  }
  
  # Add hover information for full model CI
  hover_text_full <- sprintf("Full Model<br>μ: %.3f<br>τ: %.3f<br>50%% CI: (%.3f, %.3f) to (%.3f, %.3f)<br>95%% CI: (%.3f, %.3f) to (%.3f, %.3f)",
                             aa2$mu, aa2$tau,
                             min(sapply(full_contour_50, function(x) min(x$x))), min(sapply(full_contour_50, function(x) min(x$y))),
                             max(sapply(full_contour_50, function(x) max(x$x))), max(sapply(full_contour_50, function(x) max(x$y))),
                             min(sapply(full_contour_95, function(x) min(x$x))), min(sapply(full_contour_95, function(x) min(x$y))),
                             max(sapply(full_contour_95, function(x) max(x$x))), max(sapply(full_contour_95, function(x) max(x$y))))
  
  p <- add_trace(p, x = aa2$mu, y = aa2$tau,
                 type = "scatter", mode = "markers",
                 marker = list(size = 10, color = "red"),
                 hoverinfo = "text", text = hover_text_full,
                 showlegend = FALSE,
                 name = "Full Model MLE")
  
  # Set layout
  p <- p %>% layout(title = paste("Confidence Region Shift Plot for", sm),
                    xaxis = list(title = "μ", range = mu_range),
                    yaxis = list(title = "τ", range = tau_range),
                    hovermode = "closest")
  
  # Add JavaScript for interactive hover
  p <- p %>% htmlwidgets::onRender("
    function(el, x) {
      el.on('plotly_hover', function(d) {
        var pointNumber = d.points[0].pointNumber;
        var studyNumber = d.points[0].customdata;
        var data = el.data;

        for (var i = 0; i < data.length; i++) {
          if (data[i].name && data[i].name.includes('LOO Contour') && data[i].name.includes('Study ' + studyNumber)) {
            data[i].opacity = 1.0;
          } else if (data[i].name && data[i].name.includes('LOO Contour')) {
            data[i].opacity = 0.01;
          }
        }
        Plotly.redraw(el);
      });

      el.on('plotly_unhover', function(d) {
        var data = el.data;
        for (var i = 0; i < data.length; i++) {
          if (data[i].name && data[i].name.includes('LOO Contour')) {
            data[i].opacity = 0.01;
          }
        }
        Plotly.redraw(el);
      });
    }
  ")
  
  return(p)
}
# Helper functions for calculating metrics
library(sf)

# Helper function to close a polygon if it's not already closed
close_polygon <- function(x, y) {
  if (!identical(x[1], x[length(x)]) || !identical(y[1], y[length(y)])) {
    x <- c(x, x[1])
    y <- c(y, y[1])
  }
  return(list(x = x, y = y))
}

# Helper function to create a valid polygon
create_valid_polygon <- function(contour) {
  closed <- close_polygon(contour$x, contour$y)
  poly <- st_polygon(list(cbind(closed$x, closed$y)))
  if (!st_is_valid(poly)) {
    poly <- st_make_valid(poly)
  }
  return(poly)
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


# 4.2 Function that uses deviance pvalue matrix to compute marginal confidence intervals and MLE for the CDF
comp.mu.tau.dev.CDF.CI <- function(dev.lst, N.sig = 100, alpha = 0.05) {
  dev.mat <- dev.lst[[1]]
  pval.mat <- dev.lst[[2]]
  n.mu <- dim(pval.mat)[1]
  n.tau <- dim(pval.mat)[2]
  seq.mu <- sapply(strsplit(dimnames(pval.mat)[[1]], "mu ="), as.numeric)[2, ]
  seq.tau <- sapply(strsplit(dimnames(pval.mat)[[2]], "tau ="), as.numeric)[2, ]
  x.mu <- rep(seq.mu, n.tau)
  x.tau <- rep(seq.tau, each = n.mu)
  MLE.mu <- x.mu[which(dev.mat == 0)]
  MLE.tau <- x.tau[which(dev.mat == 0)]
  
  logit.p <- log.odds(c(pval.mat))
  logit.p[c(pval.mat) < 1 / N.sig] <- log.odds(1 / N.sig) - (log.odds(2 / N.sig) - log.odds(1 / N.sig))
  logit.p[c(pval.mat) > 1 - 1 / N.sig] <- log.odds((N.sig - 1) / N.sig) + (log.odds((N.sig - 1) / N.sig) - log.odds((N.sig - 2) / N.sig))
  logit.p.loess <- loess(logit.p ~ x.mu + x.tau, span = .1)
  
  tau.pred.vec <- rep(seq(min(seq.tau), max(seq.tau), length = 200), 200)
  mu.pred.vec <- rep(seq(min(seq.mu), max(seq.mu), length = 200), each = 200)
  smth.pval.mat <- inv.log.odds(predict(logit.p.loess, data.frame(x.mu = mu.pred.vec, x.tau = tau.pred.vec)))
  mu.ci.vec <- mu.pred.vec[alpha < smth.pval.mat]
  tau.ci.vec <- tau.pred.vec[alpha < smth.pval.mat]
  n.ci <- sum(alpha < smth.pval.mat)
  
  CDF.vec <- seq(0.01, 0.99, length = 99)
  MLE.CDF <- qnorm(CDF.vec, mean = MLE.mu, sd = MLE.tau)
  ci.CDF.mat <- array(qnorm(rep(CDF.vec, each = n.ci), mean = rep(mu.ci.vec, 99), sd = rep(tau.ci.vec, 99)), dim = c(n.ci, 99))
  ci.CDF.ll <- apply(ci.CDF.mat, 2, min)
  ci.CDF.ul <- apply(ci.CDF.mat, 2, max)
  
  return(list(CDF.vec, MLE.CDF, ci.CDF.ll, ci.CDF.ul))
}



#	3.1 Function that computes log-OR deviance  p-value for each mu and tau value

# Function for OR MLE
#comp.tau.mu.log.OR.MLE <- function(data.tbl, initial.value) {
#  a <- comp.log.OR.y.sigma.stats(data.tbl)
#  y.k <- a[[1]]
#  sigma.2.k <- a[[2]]
#  
#  minus.loglik <- function(par.vec) -sum(dnorm(y.k, mean = par.vec[1], sd = sqrt(par.vec[2]^2 + sigma.2.k), log = TRUE))
#  a <- nlminb(initial.value, minus.loglik)
#  
#  return(list(mu = a$par[1], tau = a$par[2]))
#}

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
# Function for OR deviance p-values
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

# Function to generate sample data (not used in confidence_region_shift_plot, but included for completeness)
samp.log.OR.data.tbl <- function(data.tbl, mu = 0, tau = 1) {
  theta.k <- exp(rnorm(nrow(data.tbl), mu, tau))
  data.mat <- cbind(data.tbl[,3], data.tbl[,5], data.tbl[,2] + data.tbl[,4], theta.k)
  x.vec <- apply(data.mat, 1, function(vec) {
    rbinom(1, vec[1] + vec[2], (vec[4] * vec[2]) / (vec[4] * vec[2] + vec[1]))
  })
  samp.data <- data.tbl
  samp.data[,2] <- x.vec
  samp.data[,4] <- data.tbl[,2] + data.tbl[,4] - x.vec
  return(samp.data)
}

# Helper function for both OR and RR
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




