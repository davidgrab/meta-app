# Introducing **Modern Meta-Analysis**: an Open-Source Shiny App for Effortless Evidence Synthesis

> Built as part of my Tel-Aviv University thesis under the guidance of **Prof. Daniele Gattelli**, now available to everyone under the MIT license.

![Modern Meta-Analysis banner](https://img.shields.io/badge/Meta-Analysis-Made-Modern-blue)

## 🚀 Try it now

👉 **Live demo:** <https://modern-meta-analysis.shinyapps.io/meta-app/>

*(Bookmark it! No installation required—your browser is all you need.)*

---

## Why another meta-analysis tool?

Meta-analysis sits at the heart of evidence-based decision-making. Yet, researchers often juggle R scripts, Excel sheets, and half-baked GUIs to reach publication-ready figures and statistics. I felt that gap firsthand while working on my thesis. So I set out to build a single, intuitive interface that:

* Reduces friction between data upload and insight.
* Makes advanced analyses—**bivariate**, **meta-regression**, **subgroup**—one-click affairs.
* Exports fully reproducible, publication-ready reports.

The result is **Modern Meta-Analysis**: a shiny (literally) web app powered by battle-tested R packages like **meta** and **metafor**, wrapped in a user-friendly UI.

---

## Feature tour 🧑‍🔬

### 1. Data Preview
* Drag-and-drop CSV upload
* Automatic data type detection
* Interactive table filtering & summary stats

### 2. Random & Fixed Effects Analyses
* Forest and funnel plots
* Heterogeneity metrics (\(Q\), \(I^2\), \(\tau^2\))
* Leave-one-out diagnostics
* Trim-and-fill publication-bias assessment

### 3. Bivariate Approach
* Joint modeling of efficacy *and* harm
* Confidence-region plots
* Efficacy-vs-harm scatter (with quadrants)

### 4. Meta-Regression
* Continuous moderators
* Permutation tests
* Bubble plots

### 5. Five handy subtabs everywhere
`Effect Size & Heterogeneity • Subgroup Analysis • Model Diagnostics • Publication Bias • Sensitivity Analysis`

### 6. One-click PDF/HTML report
* Mirrors the exact UI hierarchy
* Embeds every plot you saw on screen
* Includes session info for full reproducibility

### 7. Dark-mode goodness 🌙
Because staring at white screens at 2 AM hurts.

---

## Under the hood 🔧

```r
# Core packages
library(shiny)      # Reactive web framework
library(meta)       # Classical meta-analysis
library(metafor)    # Meta-analysis & meta-regression
library(ggplot2)    # Plots galore
# …and many others (see DESCRIPTION)
```

The entire codebase lives on GitHub, structured as a conventional Shiny project:

* `app.R` – launch point
* `R/` – modularized **ui**, **server**, and helper functions
* `tests/` – automated unit tests via **testthat**

Feel free to browse, fork, or rip it apart. It's all yours under the permissive MIT license.

---

## Contribute, critique, collaborate 🤝

Whether you are a seasoned statistician, a budding R developer, or simply someone who needs meta-analysis without hassle, your input is invaluable.

1. **Star** the repo: <https://github.com/davidgrab/meta-app>
2. **Open an issue** for bugs, feature requests, or questions.
3. **Submit a pull request**—even small typo fixes are welcome.
4. **Reach out** directly: [davidgrabois@mail.tau.ac.il](mailto:davidgrabois@mail.tau.ac.il).

*Don't want to code?* That's fine! You can still:

* Upload your dataset, run analyses, and share feedback.
* Request new plots or tests you'd like to see.
* Tell us how the UI could serve you better.

---

## Roadmap 🗺️

- [ ] **Network meta-analysis**
- [ ] **Bayesian models (via Stan)**
- [ ] **Interactive report builder (drag-and-drop sections)**
- [ ] **Multilingual interface**

Your votes and pull requests will shape what comes first.

---

## Acknowledgements 🙏

This project started as my M.Sc. thesis at **Tel Aviv University**, under the invaluable mentorship of **Prof. Daniele Gattelli**. A big thank-you to the maintainers of the R ecosystem and open-source community—you make science move forward.

---

## TL;DR

* **Modern Meta-Analysis** is free, open-source, and live at <https://modern-meta-analysis.shinyapps.io/meta-app/>.
* It packs advanced statistical muscle behind a friendly UI—and spits out a beautiful, reproducible report.
* Contributions of all kinds are welcome: code, issues, feature ideas, or just a chat.

> Evidence synthesis should be **transparent**, **reproducible**, and **accessible**. Let's make that happen—together.

**— David Grabois**

*MIT License • 2025* 