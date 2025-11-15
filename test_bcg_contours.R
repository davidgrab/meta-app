# Test script to debug BCG vaccine ellipse issue
source("R/bivariate_meta.R")

# Load BCG vaccine data
colditz_data <- read.csv("data/colditz_1994_bcg_vaccine.csv", stringsAsFactors = FALSE)

# Rename columns to match expected format
colnames(colditz_data) <- c("study", "ie", "it", "pe", "pt")

# Run JCR analysis
cat("Running JCR analysis on BCG vaccine data...\n")
result <- metabiv(
  event.e = colditz_data$ie,
  n.e = colditz_data$it,
  event.c = colditz_data$pe,
  n.c = colditz_data$pt,
  studlab = colditz_data$study,
  sm = "RR",
  verbose = TRUE
)

# Extract contour data
contour_50 <- get_contours(result$dev_pvals[[2]], 0.50)
contour_95 <- get_contours(result$dev_pvals[[2]], 0.05)

# Check ranges
cat("\n=== CONTOUR RANGES ===\n")
cat("95% CI x-range (log scale):", range(contour_95[[1]]$x), "\n")
cat("95% CI y-range (tau):", range(contour_95[[1]]$y), "\n")
cat("50% CI x-range (log scale):", range(contour_50[[1]]$x), "\n")
cat("50% CI y-range (tau):", range(contour_50[[1]]$y), "\n")

cat("\n95% CI x-range (exp scale):", range(exp(contour_95[[1]]$x)), "\n")
cat("50% CI x-range (exp scale):", range(exp(contour_50[[1]]$x)), "\n")

cat("\nMLE values:\n")
cat("mu (log):", result$mu, "\n")
cat("mu (exp):", exp(result$mu), "\n")
cat("tau:", result$tau, "\n")

# Calculate what the axis limits should be
all_x_log <- c(contour_95[[1]]$x, contour_50[[1]]$x, result$mu)
all_y <- c(contour_95[[1]]$y, contour_50[[1]]$y, result$tau)
all_x_exp <- exp(all_x_log)

x_range_exp <- range(all_x_exp, na.rm = TRUE)
y_range <- range(all_y, na.rm = TRUE)

# Get grid boundaries
seq.mu <- sapply(strsplit(dimnames(result$dev_pvals[[2]])[[1]], "mu ="), as.numeric)[2,]
seq.tau <- sapply(strsplit(dimnames(result$dev_pvals[[2]])[[2]], "tau ="), as.numeric)[2,]
tau_grid_max <- max(seq.tau)
tau_grid_min <- min(seq.tau)
mu_grid_max <- max(seq.mu)
mu_grid_min <- min(seq.mu)

cat("\n=== GRID BOUNDARIES ===\n")
cat("tau_grid_max:", tau_grid_max, "\n")
cat("tau_grid_min:", tau_grid_min, "\n")
cat("mu_grid_max:", mu_grid_max, "\n")
cat("mu_grid_min:", mu_grid_min, "\n")

cat("\n=== CALCULATED AXIS LIMITS ===\n")
cat("x_range_exp:", x_range_exp, "\n")
cat("y_range:", y_range, "\n")

# Check if contour hits boundaries
cat("\n=== BOUNDARY DETECTION ===\n")
cat("y_range[2] hits tau_grid_max?", abs(y_range[2] - tau_grid_max) < 1e-6, "\n")
cat("y_range[1] hits tau_grid_min?", abs(y_range[1] - tau_grid_min) < 1e-6, "\n")
cat("max(all_x_log) hits mu_grid_max?", abs(max(all_x_log) - mu_grid_max) < 1e-6, "\n")
cat("min(all_x_log) hits mu_grid_min?", abs(min(all_x_log) - mu_grid_min) < 1e-6, "\n")

# With boundary detection
x_factor <- 1.01
y_margin <- diff(y_range) * 0.01

# Apply boundary detection logic
if (abs(y_range[2] - tau_grid_max) < 1e-6) {
  y_max <- tau_grid_max + diff(y_range) * 0.05
  cat("Extended y_max beyond boundary to:", y_max, "\n")
} else {
  y_max <- y_range[2] + y_margin
}

if (abs(y_range[1] - tau_grid_min) < 1e-6) {
  y_min <- max(0.0, tau_grid_min - diff(y_range) * 0.05)
  cat("Extended y_min beyond boundary to:", y_min, "\n")
} else {
  y_min <- max(0.0, y_range[1] - y_margin)
}

if (abs(max(all_x_log) - mu_grid_max) < 1e-6 || abs(min(all_x_log) - mu_grid_min) < 1e-6) {
  x_factor <- 1.05
  cat("Extended x_factor to:", x_factor, "\n")
}

x_lim <- c(x_range_exp[1] / x_factor, x_range_exp[2] * x_factor)
y_lim <- c(y_min, y_max)

cat("\n=== FINAL AXIS LIMITS (with boundary detection) ===\n")
cat("x_lim:", x_lim, "\n")
cat("y_lim:", y_lim, "\n")

# Check if contours extend beyond limits
cat("\n=== CHECKING IF CONTOURS EXCEED LIMITS ===\n")
cat("95% CI max x (exp):", max(exp(contour_95[[1]]$x)), "vs x_lim[2]:", x_lim[2], "\n")
cat("95% CI max y:", max(contour_95[[1]]$y), "vs y_lim[2]:", y_lim[2], "\n")
cat("95% CI min x (exp):", min(exp(contour_95[[1]]$x)), "vs x_lim[1]:", x_lim[1], "\n")
cat("95% CI min y:", min(contour_95[[1]]$y), "vs y_lim[1]:", y_lim[1], "\n")

# Generate the plot to see what happens
cat("\n=== GENERATING PLOT ===\n")
png("test_bcg_plot.png", width = 800, height = 600)
plot.mu.tau.CI(
  result$dev_pvals[[1]],
  result$dev_pvals[[2]],
  mlb = "Confidence Region for (Risk Ratio, τ)",
  xlab = "Effect Size (Risk Ratio)",
  mu_mle = result$mu,
  tau_mle = result$tau,
  sm = "RR"
)
dev.off()
cat("Plot saved to test_bcg_plot.png\n")

