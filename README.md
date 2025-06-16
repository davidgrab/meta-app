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

Experience the future of meta-analysis: [Modern Meta-Analysis App](https://modern-meta-analysis.shinyapps.io/meta-app/)

## 🎯 Key Features

- 📤 Seamless data upload and preview
- 🧮 Versatile analysis methods (Random Effects, Fixed Effects, Bivariate)
- 📈 Eye-catching visualizations (forest plots, funnel plots, and more)
- 🔍 In-depth heterogeneity assessment
- 🔬 Robust sensitivity and influence analyses
- 📚 Comprehensive publication bias evaluation
- ⚖️ Rigorous quality assessment (GRADE)
- 📄 One-click comprehensive report generation
- **Supports binary (2x2), continuous (Standardized Mean Difference - SMD), and Hazard Ratio (HR) data structures**
- **For continuous data, the SMD column may also appear as CoNC or HeadGrid-G (all interpreted as SMD for now)**
- **For Hazard Ratios, data can be provided as HR and 95% CI, or as log(HR) and its standard error**

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
- For **binary (2x2) data**:
    - Columns: `study, ie, it, pe, pt`
    - `ie`: Intervention group events
    - `it`: Intervention group total
    - `pe`: Placebo/control group events
    - `pt`: Placebo/control group total
- For **continuous (SMD) data**:
    - Columns: `study, smd, ci_lower, ci_upper`
    - `smd`: Standardized Mean Difference (can also be labeled `CoNC` or `HeadGrid-G`)
    - `ci_lower`: Lower bound of the 95% confidence interval for SMD
    - `ci_upper`: Upper bound of the 95% confidence interval for SMD
- For **Hazard Ratio (HR) data**:
    - **Format 1 (HR and Confidence Intervals):**
        - Columns: `study, hr, ci_lower, ci_upper`
        - `hr`: Hazard Ratio
        - `ci_lower`: Lower bound of the 95% confidence interval for HR
        - `ci_upper`: Upper bound of the 95% confidence interval for HR
        - *The app will internally convert these to log(HR) and its standard error for analysis.*
    - **Format 2 (Log Hazard Ratio and Standard Error):**
        - Columns: `study, loghr, se_loghr`
        - `loghr`: Natural logarithm of the Hazard Ratio
        - `se_loghr`: Standard error of the log Hazard Ratio

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
