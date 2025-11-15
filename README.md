# Meta-Analysis App


## Live App

Try the latest master build in your browser:

- [Open the app on Posit Cloud](https://davidgrab-meta-app.share.connect.posit.cloud/)

A comprehensive Shiny application for performing meta-analyses with advanced statistical methods, bivariate analysis, and interactive visualizations.

## Features

### Core Meta-Analysis Capabilities
- **Multiple Effect Size Measures**: Support for risk ratios (RR), odds ratios (OR), standardized mean differences, and more
- **Advanced Statistical Methods**: Fixed-effects, random-effects, and mixed-effects models
- **Joint Confidence Region (JCR) Method**: Advanced meta-analysis with joint MLE estimation of effect size and heterogeneity (Saad et al., 2019)
- **Meta-Regression**: Advanced meta-regression with permutation tests for robust inference

### Statistical Enhancements
- **Permutation Testing**: Non-parametric significance testing for meta-regression
- **Comprehensive OR Support**: Complete odds ratio calculations with continuity corrections
- **Zero-Cell Handling**: Robust methods for studies with zero events
- **Influence Analysis**: Leave-one-out diagnostics and influence plots

### Visualization & Reporting
- **Interactive Forest Plots**: Dynamic visualizations with confidence intervals
- **Publication Bias Assessment**: Funnel plots, Egger's test, and trim-and-fill analysis
- **Comprehensive Reports**: Downloadable analysis reports with statistical summaries
- **Data Import/Export**: Support for CSV, Excel, and manual data entry

## 🚀 Quick Start

Get up and running in minutes:

1. Clone this repository:
   ```
   git clone https://github.com/davidgrab/meta-app.git
   ```

2. Install the required R packages:
   ```r
   install.packages(c("shiny", "meta", "metafor", "ggplot2", "plotly", "DT", "bslib", "shinyjs", "rmarkdown", "knitr", "gridExtra", "sp", "sf", "testthat","BiasedUrn","bsicons","readxl","shinycssloaders","pandoc"))
   ```

3. Run the app locally:
   ```r
   shiny::runApp("path/to/meta-app")
   ```

**Data Format:**
- For binary (2x2) data: columns should be `study`, `ie`, `it`, `pe`, `pt`
- For continuous data: columns should be `study`, `smd`, `ci_lower`, `ci_upper` (the SMD column may also be labeled `CoNC` or `HeadGrid-G`)

## 📁 Project Structure

- `app.R`: Main Shiny app file
- `R/`: Folder containing R scripts
  - `ui.R`: User interface definition
  - `server.R`: Server logic
  - `functions.R`: Helper functions
  - `bivariate_meta.R`: Bivariate meta-analysis functions
- `data/`: Sample datasets
- `www/`: Static assets (CSS, images)
- `tests/`: Unit tests

## 🧪 Testing

Run the test suite:

```r
testthat::test_dir("tests/testthat")
```

## 🤝 Contributing

We welcome contributions! Please see our [Contributing Guide](CONTRIBUTING.md) for more details.

To report a bug or request a feature, please open an issue on our [GitHub Issues page](https://github.com/davidgrab/meta-app/issues).

## 🗺️ Roadmap

- Migrate the JCR method logic from `R/bivariate_meta.R` into the `jcrmeta/` package for reuse and testability. See `docs/JCR_PACKAGE_MIGRATION.md` for the step-by-step plan.

## 📜 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- [Shiny](https://shiny.rstudio.com/) by RStudio
- [meta](https://cran.r-project.org/web/packages/meta/index.html) package by Guido Schwarzer
- [metafor](https://www.metafor-project.org/) package by Wolfgang Viechtbauer

## 📞 Contact

For questions or feedback, please open an issue or contact [davidgarbois@gmail.com](mailto:davidgarbois@gmail.com).
