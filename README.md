# 🚀 Modern Meta-Analysis Shiny App

<p align="center">
  <a href="https://app.circleci.com/pipelines/github/davidgrab/meta-app">
    <img src="https://img.shields.io/circleci/build/github/davidgrab/meta-app/main" alt="CircleCI">
  </a>
  <a href="https://opensource.org/licenses/MIT">
    <img src="https://img.shields.io/badge/License-MIT-yellow.svg" alt="License: MIT">
  </a>
  <a href="https://cran.r-project.org/">
    <img src="https://img.shields.io/badge/R-≥4.0.0-blue.svg" alt="R Version">
  </a>
</p>

Revolutionize your meta-analysis workflow with our cutting-edge Shiny app! 📊✨ Harness the power of advanced statistical methods and stunning visualizations to elevate your research.

## 🌟 Live Demo

Experience the future of meta-analysis: [Modern Meta-Analysis App](https://davidgrab-meta-app.share.connect.posit.cloud/)

## 🎯 Key Features

- 📤 Seamless data upload and preview
- 🧮 Versatile analysis methods (Random Effects, Fixed Effects, Bivariate)
- 📈 Eye-catching visualizations (forest plots, funnel plots, and more)
- 🔍 In-depth heterogeneity assessment
- 🔬 Robust sensitivity and influence analyses
- 📚 Comprehensive publication bias evaluation
- ⚖️ Rigorous quality assessment (GRADE)
- 📄 One-click comprehensive report generation
- **Supports both binary (2x2) and continuous (SMD) data structures**
- **For continuous data, the SMD column may also appear as CoNC or HeadGrid-G (all interpreted as SMD for now)**

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

## 📜 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- [Shiny](https://shiny.rstudio.com/) by RStudio
- [meta](https://cran.r-project.org/web/packages/meta/index.html) package by Guido Schwarzer
- [metafor](https://www.metafor-project.org/) package by Wolfgang Viechtbauer

## 📞 Contact

For questions or feedback, please open an issue or contact [davidgrabois@mail.tau.ac.il](mailto:davidgrabois@mail.tau.ac.il).
