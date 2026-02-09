# JCR Method: Implementation Review

## How the app implements JCR

### Runtime source of truth

- **The Shiny app uses `R/bivariate_meta.R` only.** It is loaded via `source("R/bivariate_meta.R")` in `R/server.R` (and in `R/functions.R` for report generation). The app does **not** load the `jcrmeta` package at runtime.
- All JCR calculations in the app therefore come from the single file **`R/bivariate_meta.R`**.

### Entry points and flow

1. **Main entry:** `metabiv(...)`  
   Called from:
   - `R/server.R` (binary and SMD analyses, leave-one-out, subgroups)
   - `R/functions.R` (report generation, subgroup reports, LOO)

2. **Inputs:**
   - **Binary (RR/OR):** `event.e`, `n.e`, `event.c`, `n.c`, optional `studlab`, `data`, `sm` ("RR" or "OR").
   - **Pre-computed (SMD):** `y`, `sigma2`, `studlab`, `sm = "SMD"`.

3. **Core pipeline inside `metabiv()`:**
   - **Effect sizes & variances:**  
     - RR/OR: `comp.log.RR.y.sigma.stats(data.tbl)` or `comp.log.OR.y.sigma.stats(data.tbl)` (with 0.5 continuity correction where needed).  
     - SMD: use supplied `y`, `sigma2`.
   - **Initial values:** DerSimonian–Laird (e.g. via `meta::metagen` or equivalent) for μ and τ.
   - **MLE:** `comp.tau.mu.MLE(data.tbl, initial.value, sm, y.k, sigma.2.k)` → joint (μ, τ) MLE.
   - **Deviance:** `comp.tau.mu.dev.pvals(...)` → deviance and p-values on a (μ, τ) grid; for OR, exact tests use noncentral hypergeometric (e.g. `comp.tau.mu.log.OR.dev.pvals.exact` where used).
   - **Confidence region:** `compute_confidence_region(dev_pvals[[2]], level.ma)` → contour from likelihood-ratio (chi-squared) rule.
   - **μ CI:** `comp.mu.tau.dev.CDF.CI(...)` used for CDF-based CI for μ; result object also stores `lower.mu`, `upper.mu` on the returned list.

4. **Returned object (class `metabiv`):**  
   Contains at least: `y.k`, `sigma.2.k`, `sm`, `mu`, `tau`, `lower.mu`, `upper.mu`, `lower.k`, `upper.k`, `conf_region`, `dev_pvals`, `Q`, `I2`, `H2`, etc., as used by the app.

5. **Downstream use in the app:**
   - **Plots:** `forest.metabiv()`, `confidence_region_shift_plot()`, `comp.eff.harm.plot()` (with `comp.mu.tau.dev.CDF.CI()` for CDF.ci.obj).
   - **Reports:** `R/functions.R` uses the same helpers (`comp.mu.tau.dev.CDF.CI`, `comp.eff.harm.plot`, `confidence_region_shift_plot`, `forest.metabiv`) for PDF/output.

All of the above logic lives in **`R/bivariate_meta.R`**; there is no separate JCR implementation elsewhere in the app.

---

## jcrmeta: future package and current content

- **`jcrmeta/`** is the intended future R package for the same JCR method. The plan is to eventually have the app depend on `library(jcrmeta)` and remove the duplicated code from the app (see `docs/JCR_PACKAGE_MIGRATION.md`).
- **Single implementation file in the package:** `jcrmeta/R/metabiv.R`. It is intended to contain the **same** algorithms and behavior as `R/bivariate_meta.R`.

### Keeping implementations identical

- **Canonical implementation:** `R/bivariate_meta.R` (the app’s current source).
- **Package mirror:** `jcrmeta/R/metabiv.R` must be kept **identical** to `R/bivariate_meta.R` in all calculations and structure (function names, returned elements, validation rules, etc.), so that when the app switches to `library(jcrmeta)`, behavior stays the same.

**Sync rule:** After any change to JCR logic in **`R/bivariate_meta.R`**, update the package by copying that file into **`jcrmeta/R/metabiv.R`** (overwriting the existing file). Then run the package tests (e.g. `devtools::test()` in `jcrmeta/`) and the app tests that use JCR (e.g. `tests/testthat/test-bivariate_meta.R`) to confirm nothing regressed.

A small script is provided to do the copy: **`scripts/sync_jcrmeta_from_app.R`**.

### Using the sync script

From the **meta-app** repo root:

```bash
Rscript scripts/sync_jcrmeta_from_app.R
```

Or copy manually:

```bash
cp R/bivariate_meta.R jcrmeta/R/metabiv.R
```

After syncing, run package tests: `devtools::test(pkg = "jcrmeta")`.

---

## App vs package: same implementation

**Is the jcrmeta package the same as the app's JCR implementation?**  
Yes. `jcrmeta/R/metabiv.R` is kept identical to `R/bivariate_meta.R` (via the sync script). Same calculations (effect measures, MLE, deviance, confidence region, μ CI, efficacy/harm), same returned object structure, and the same plotting helpers: `forest.metabiv`, `confidence_region_shift_plot`, `comp.eff.harm.plot`, `comp.mu.tau.dev.CDF.CI`.

**If we publish jcrmeta, can people get the same JCR as in the UI?**  
Yes. They run `metabiv(...)` with the same arguments (binary RR/OR or SMD) and get the same estimates and CIs; they can use the same plot functions to reproduce the app's JCR tab and report outputs. No algorithmic or structural differences.

**What the app uses from the JCR file (all in `jcrmeta/R/metabiv.R`):** `metabiv()`, `forest.metabiv()`, `confidence_region_shift_plot()`, `comp.mu.tau.dev.CDF.CI()`, `comp.eff.harm.plot()`, plus internal helpers used only inside these.

---

## Summary

| Item | Location | Notes |
|------|----------|--------|
| **App JCR code** | `R/bivariate_meta.R` | Sourced by server.R and functions.R; this is the **canonical** implementation. |
| **Package JCR code** | `jcrmeta/R/metabiv.R` | Must match `R/bivariate_meta.R` exactly. Sync via `Rscript scripts/sync_jcrmeta_from_app.R`. |
| **App uses package at runtime?** | No | App uses only `source("R/bivariate_meta.R")`. |
| **After editing JCR in the app** | Run `Rscript scripts/sync_jcrmeta_from_app.R` | Then run package and app tests. |

This keeps a single source of truth (the app file) and ensures the future package matches the app’s implementation exactly.
