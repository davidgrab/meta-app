## Migration Plan: Move JCR logic from `R/bivariate_meta.R` into `jcrmeta/`

This document outlines a safe, incremental plan to migrate the Joint Confidence Region (JCR) method implementation from the app code (`R/bivariate_meta.R`) into the reusable package located in `jcrmeta/`.

### Goals
- Consolidate JCR method core logic into the `jcrmeta` package.
- Keep the Shiny app functional throughout (no breaking UI).
- Improve testability and pave the way for CRAN-readiness of `jcrmeta`.

### Scope
- Code move/refactor only; no algorithmic changes.
- Public API for the package will be `metabiv()` plus exported helpers.

---

### Phase 1 — Preparation (no behavior change)
1. Audit current entry points used by the app:
   - `metabiv()` main entry
   - plotting helpers: `forest.metabiv`, `plot.mu.tau.CI`, efficacy/harm plot
   - diagnostics: QQ plots, deleted residuals
   - data transforms: log RR/OR, variances, continuity corrections
2. Ensure unit tests cover current behavior:
   - Extend `tests/testthat/` with app-level tests calling `metabiv()` and verifying key outputs (μ̂, τ̂, CI region levels).
3. Define target package API (minimal):
   - `metabiv(...)` (main)
   - `comp_tau_mu_mle(...)`, `comp_tau_mu_dev_pvals(...)`
   - `log_rr(...)`, `variance_log_rr(...)`, `log_or(...)`, `variance_log_or(...)`
   - Plot methods registered as S3 where applicable (e.g., `plot.metabiv`, `forest.metabiv`).

Deliverable: Updated tests; API list frozen in this file.

---

### Phase 2 — Package extraction
1. Move core numerical code into `jcrmeta/R/`:
   - Create focused files (example):
     - `R/core-estimation.R` (MLE, deviance, CI region)
     - `R/effect-measures.R` (RR/OR transforms, variances)
     - `R/metabiv.R` (thin wrapper composing the above)
2. Move S3 plot methods that are package-agnostic into `jcrmeta`:
   - `forest.metabiv`, `plot.mu.tau.CI`, and generic `plot.metabiv` (if present)
   - Leave Shiny-specific layout/containers in the app (`R/functions.R`).
3. Update `NAMESPACE` and roxygen docs accordingly.

Deliverable: `jcrmeta` builds and `devtools::check()` passes locally.

---

### Phase 3 — App integration switch
1. Replace `source("R/bivariate_meta.R")` in `R/server.R` with `library(jcrmeta)`.
2. Update calls to plotting functions:
   - Prefer S3 methods (e.g., `plot(x)` or `forest(x)`) where feasible.
   - Keep Shiny-specific wrappers in `R/functions.R`.
3. Smoke-test all tabs; ensure identical outputs.

Deliverable: App runs only with `jcrmeta` (no direct sourcing).

---

### Phase 4 — De-duplication and cleanup
1. Remove migrated functions from `R/bivariate_meta.R`.
2. Keep a thin shim (optional) that forwards to `jcrmeta` for backward compatibility during transition.
3. Prune dead code and harmonize names/docs.

Deliverable: No duplicated logic; app code is thinner and clearer.

---

### Phase 5 — Packaging and documentation
1. Finalize `jcrmeta` DESCRIPTION and README (JCR method description, examples).
2. Add vignettes (confidence region, efficacy-harm usage, diagnostics).
3. Tag a version, create release notes.

Deliverable: `jcrmeta` ready for wider reuse (internal or CRAN track).

---

### Validation checklist
- μ̂, τ̂, and CI region identical before/after.
- Unit tests green (app + package).
- UI text and help remain accurate (JCR method).

---

### Rollback plan
- Re-enable `source("R/bivariate_meta.R")` and pin to previous commit if a regression is detected.

---

### Ownership & Next Actions
- Owner: David (package) / App team (integration)
- Next: Complete Phase 1 tests, then proceed to Phase 2 extraction.


