# Modern Meta-Analysis App: Technical Validation Report

*Prepared for clinicians, statisticians and developers—v1.0*

---

## 1  Overview of Code Architecture

| Layer | Key Files | Responsibilities |
|-------|-----------|------------------|
| **Entry** | `app.R` | Unified launch point sourcing UI & server modules. |
| **Presentation** | <mcfile name="ui.R" path="/Users/davidgrabois/meta-app/R/ui.R"></mcfile> | Defines navigation: Data Preview ➜ Random Effects ➜ Fixed Effects ➜ JCR Method ➜ Meta-Regression; embeds help-text & dark-mode. |
| **Business Logic** | <mcfile name="server.R" path="/Users/davidgrabois/meta-app/R/server.R"></mcfile> | Manages reactives: data upload, analysis buttons, tab gating, report download, subgroup/meta-reg modules. |
| **Statistical Engines** | <mcfile name="functions.R" path="/Users/davidgrabois/meta-app/R/functions.R"></mcfile> | Helper wrappers around **meta/metafor** (fixed, random), diagnostics, forest/funnel/QQ plots, GRADE evaluation, combined plots. |
| **Novel Methods** | <mcfile name="bivariate_meta.R" path="/Users/davidgrabois/meta-app/R/bivariate_meta.R"></mcfile> | Custom implementation of Saad et al. (2019) Joint Confidence Region (JCR) method via joint MLE, confidence region and deviance grid search. |

---

## 2  Mathematical & Literature Foundations

| Feature | Reference | Key Equation | Code Lines |
|---------|-----------|-------------|-----------|
| **Fixed / Random Effects** | *Introduction to Meta-Analysis* (Borenstein 2009) ch. 9 | \(\hat\mu_{FE}=\sum w_i y_i / \sum w_i\); DerSimonian-Laird \(\tau^2_{DL}\) | functions.R `random_effect_dist_plot` (≈ 45–65) & weighting in bivariate_meta.R 120–140 |
| **Heterogeneity Metrics** | Cochrane Handbook ch. 10 | \(Q, I^2, H^2\) definitions | bivariate_meta.R 280–310; plotted everywhere. |
| **JCR Method** | *Saad et al.* 2019 + Supplement | Log-ratio vector \(y\_k\sim N(\mu, \sigma_k^2+\tau^2)\); joint MLE via deviance grid search for \((\mu,\tau)\) | bivariate_meta.R 150–260 (initial values) & 320–420 (MLE & CI region) |
| **Normality Diagnostics** | *Handbook of Meta-Analysis* (2023) ch. 11 | Deleted-residual QQ, BLUP QQ | ui.R tabs ➜ server.R ≈ 650–780 functions. |
| **Publication Bias** | Egger/T-&-Fill (Handbook ch. 11) | Egger regression, Duval & Tweedie trim-fill | wrapped via meta::funnel & metafor::trimfill in functions.R. |

---

## 3  Implementation Checks

| Item | Expected (Literature) | Observed in Code | Verdict |
|------|-----------------------|------------------|---------|
| Continuity correction for 0-cells (binary data) | Add 0.5 to each cell (Saad et al.) | `calculate_log_rr()` applies +0.5 before log transform | ✅ Conforms |
| Effect measure switch (RR / OR) | Both measures recognised in Saad model | UI passes input, but `metabiv()` currently hard-codes RR transformation; OR branch left TODO | ❌ Partial |
| MLE of (μ, τ²) | Profile likelihood or grid search as in Saad | Grid search over (μ, τ) with deviance computation; matches Appendix A equations | ✅ Conforms |
| Confidence region construction | Deviance contour at χ²₂,0.95 ≈ 5.99 | `contour_levels <- c(qchisq(0.95, 2))` inside bivariate_meta.R | ✅ Conforms |
| Model diagnostics | Deleted-residual & BLUP QQ as Handbook ch.11 | Implemented in `random_effect_diagnostics_*` functions | ✅ Conforms |
| Publication bias tools | Funnel + trim-and-fill | `random_effect_funnel_plot()` wraps `metafor::trimfill()` | ✅ Conforms |
| Meta-regression (moderator analysis) | Mixed-effects meta-regression per Borenstein ch.10 | Placeholder `run_meta_regression()` returns "Not yet implemented" | ❌ Missing |
| JCR Efficiency hump plot | Derived from joint CDF of (μ, τ) | Implemented in `efficiency_home_plot()`; naming updated to "hump" | ✅ Conforms |

### Overall Alignment

Out of 8 key checkpoints, **6 fully align** with the cited literature, **1 is partially implemented**, and **1 is missing**.

---

## 4  Required Improvements

1. **Complete OR support in `metabiv()`** – add log-odds transform branch analogous to RR.
2. **Implement `run_meta_regression()`** – leverage `metafor::rma.mv` with permutation tests.
3. **Expose REML vs ML option for τ² estimation** in the bivariate grid search.
4. **Automated unit tests** for bivariate routines (extend `tests/testthat`).
5. **Package-ify core JCR code** to decouple from Shiny, easing CRAN submission.

---

## 5  Verdict

The application is **largely consistent with the mathematical foundations** referenced, but **not yet 100 % complete**. Addressing the two outstanding items above will bring full compliance and strengthen reproducibility.