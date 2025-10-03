# jcrmeta

A comprehensive R package for Joint Confidence Region (JCR) meta-analysis, implementing the methodology described in Saad et al. (2019). The JCR method uses joint maximum likelihood estimation to simultaneously estimate the overall effect (μ) and between-study heterogeneity (τ).

## Features

- **Joint Confidence Region (JCR) Meta-Analysis**: Perform meta-analysis with joint MLE estimation for more precise results
- **Multiple Summary Measures**: Support for Risk Ratio (RR), Odds Ratio (OR), and Standardized Mean Difference (SMD)
- **Maximum Likelihood Estimation**: Advanced MLE-based parameter estimation with DerSimonian-Laird initial values
- **Heterogeneity Assessment**: Calculate Q-statistics, I², and H² for heterogeneity evaluation
- **Confidence Regions**: Compute confidence regions for effect size and heterogeneity parameters
- **Helper Functions**: Comprehensive set of utility functions for effect size calculations

## Installation

```r
# Install from local source
devtools::install("path/to/jcrmeta")
```

## Usage

### Basic Example

```r
library(jcrmeta)

# Example with binary data for odds ratio
event.e <- c(10, 15, 20)
n.e <- c(100, 120, 150)
event.c <- c(5, 8, 12)
n.c <- c(100, 110, 140)

# Perform JCR meta-analysis
result <- metabiv(event.e, n.e, event.c, n.c, sm = "OR")
print(result)
```

### Pre-calculated Effect Sizes

```r
# Using pre-calculated effect sizes and variances
y <- c(0.5, 0.8, 0.3)  # Effect sizes
sigma2 <- c(0.1, 0.15, 0.12)  # Variances

result <- metabiv(y = y, sigma2 = sigma2, sm = "SMD")
```

## Functions

### Main Functions
- `metabiv()`: Main function for JCR meta-analysis with joint MLE estimation

### Helper Functions
- `log_rr()`, `log_or()`: Calculate log risk ratios and log odds ratios
- `variance_log_rr()`, `variance_log_or()`: Calculate variances for log effect sizes
- `log_odds()`, `inv_log_odds()`: Log odds transformations
- `comp_tau_mu_mle()`: Maximum likelihood estimation for tau and mu
- `comp_tau_mu_dev_pvals()`: Compute deviance and p-values
- `compute_confidence_region()`: Calculate confidence regions

## License

MIT License