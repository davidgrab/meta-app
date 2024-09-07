# Modern Meta-Analysis Shiny App

[![CircleCI](https://circleci.com/gh/yourusername/meta-app.svg?style=svg)](https://circleci.com/gh/yourusername/meta-app)

This Shiny app provides a comprehensive tool for conducting and visualizing meta-analyses using various methods. It supports random effects, fixed effects, and bivariate approaches to meta-analysis.

## Live Demo

You can access the live version of this app at: https://modern-meta-analysis.shinyapps.io/meta-app/

## Features

- Data upload and preview
- Multiple meta-analysis methods (Random Effects, Fixed Effects, Bivariate)
- Extensive visualizations (forest plots, funnel plots, etc.)
- Heterogeneity assessment
- Sensitivity and influence analyses
- Publication bias evaluation
- Quality assessment (GRADE)
- Comprehensive report generation

## Installation

To run this app locally:

1. Clone this repository
2. Install the required R packages:

```r
install.packages(c("shiny", "meta", "metafor", "ggplot2", "plotly", "DT", "bslib", "shinyjs", "rmarkdown", "knitr","gridExtra","sp","sf","testthat"))