# Meta-Analysis App

A comprehensive Shiny application for performing meta-analyses with advanced statistical methods, including the Joint Confidence Region (JCR) approach for jointly estimating effect size and heterogeneity.

## Live App

Try the app in your browser:

**[Open the Meta-Analysis App](https://meta-app-jcr-main.share.connect.posit.cloud/)**

## Features

### Core Meta-Analysis Methods
- **Fixed Effects Model**: Common effect estimation using inverse-variance weighting
- **Random Effects Model**: DerSimonian-Laird and other heterogeneity estimators
- **Joint Confidence Region (JCR) Method**: Advanced approach using joint MLE estimation of effect size (μ) and between-study heterogeneity (τ), based on Saad et al. (2019)

### Effect Size Measures
- Risk Ratio (RR)
- Odds Ratio (OR)
- Standardized Mean Difference (SMD)

### Diagnostics & Visualization
- **Forest Plots**: Interactive visualizations with study weights and confidence intervals
- **Confidence Region Plots**: 2D joint confidence regions for μ and τ (JCR method)
- **Efficacy-Harm Probability Plots**: Probability tables for treatment decisions
- **Q-Q Plots**: Normality diagnostics with simulation envelopes
- **Funnel Plots**: Publication bias assessment with trim-and-fill
- **Baujat & Influence Plots**: Identify influential studies
- **Leave-One-Out Analysis**: Sensitivity to individual studies

### Additional Features
- **Meta-Regression**: Explore moderator effects on treatment outcomes
- **Subgroup Analysis**: Compare effects across categorical variables
- **Downloadable Reports**: Export comprehensive analysis reports
- **Multiple Data Formats**: Support for CSV and Excel files

## Quick Start

### Run Locally

1. Clone this repository:
   ```bash
   git clone https://github.com/davidgrab/meta-app.git
   cd meta-app
   ```

2. Install required R packages:
   ```r
   install.packages(c(
     "shiny", "meta", "metafor", "ggplot2", "plotly", "DT", 
     "bslib", "shinyjs", "rmarkdown", "knitr", "gridExtra", 
     "testthat", "BiasedUrn", "bsicons", "readxl", "shinycssloaders"
   ))
   ```

3. Run the app:
   ```r
   shiny::runApp()
   ```

### Data Format

**Binary (2×2) data:**
| Column | Description |
|--------|-------------|
| `study` | Study identifier |
| `ie` | Intervention events |
| `it` | Intervention total |
| `pe` | Placebo/control events |
| `pt` | Placebo/control total |

**Continuous (SMD) data:**
| Column | Description |
|--------|-------------|
| `study` | Study identifier |
| `smd` | Standardized mean difference |
| `ci_lower` | Lower confidence interval |
| `ci_upper` | Upper confidence interval |

## Project Structure

```
meta-app/
├── app.R                 # Main Shiny app entry point
├── R/
│   ├── ui.R              # User interface definition
│   ├── server.R          # Server logic and reactives
│   ├── functions.R       # Helper functions and plotting
│   └── bivariate_meta.R  # JCR method implementation
├── data/                 # Example datasets
├── www/                  # Static assets (CSS, logos)
├── tests/testthat/       # Unit tests
├── jcrmeta/              # R package for JCR method (in development)
├── docs/                 # Documentation
└── .circleci/            # CI/CD configuration
```

## Testing

Run the test suite:

```r
testthat::test_dir("tests/testthat")
```

## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

To report bugs or request features, open an issue on the [GitHub Issues page](https://github.com/davidgrab/meta-app/issues).

## License

This project is licensed under the MIT License - see [LICENSE](LICENSE) for details.

## About

This application was developed by **David Grabois** as part of a Master's thesis at **Tel Aviv University**, Department of Statistics and Operations Research.

**Thesis:** *"Modern Meta-Analysis: Joint Confidence Regions for Effect Size and Heterogeneity"*

### The JCR Method

The Joint Confidence Region method provides a frequentist approach to jointly estimating the overall effect (μ) and between-study heterogeneity (τ) using Maximum Likelihood Estimation. Unlike traditional methods that estimate these parameters separately, JCR acknowledges their interdependence and provides:

- Joint confidence regions via likelihood-ratio tests with χ²(2) distribution
- Efficacy-harm probability plots for clinical decision-making
- Better characterization of uncertainty when heterogeneity is present

**Based on:**
> Saad, A., Yekutieli, D., Lev-Ran, S., Gross, R., & Guyatt, G. H. (2019). Getting more out of meta-analyses: a new approach to meta-analysis in light of unexplained heterogeneity. *Journal of Clinical Epidemiology*, 107, 101-106.

## Acknowledgments

- [Shiny](https://shiny.posit.co/) by Posit
- [meta](https://cran.r-project.org/package=meta) package by Guido Schwarzer
- [metafor](https://www.metafor-project.org/) package by Wolfgang Viechtbauer

## Contact

For questions or feedback, please open an issue or contact [davidgarbois@gmail.com](mailto:davidgarbois@gmail.com).
