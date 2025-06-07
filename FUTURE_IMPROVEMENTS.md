# Future Improvements for Bivariate Meta-Analysis

This document outlines the necessary steps to fully implement the exact, simulation-based test for the **Odds Ratio (OR)** summary measure within the "Bivariate Approach" tab, as described by Saad et al.

The current implementation has temporarily deferred this feature to ensure application stability. When "OR" is selected, the app uses the standard Chi-squared approximation method, which is the same approach used for "Relative Risk (RR)" and "Standardized Mean Difference (SMD)".

---

### Task: Implement the Exact Test for Odds Ratio

**1. The Goal:**
The primary goal is to provide a more accurate analysis for meta-analyses with small study sizes, where the assumptions of the standard Chi-squared test may not hold. This requires implementing the simulation-based "Exact Test" as detailed in the supplementary materials of the Saad et al. paper.

**2. The Problem:**
The existing function `comp.tau.mu.log.OR.dev.pvals.exact` in `R/bivariate_meta.R` is not correctly integrated. It was causing an `unused arguments` error at runtime, indicating a mismatch between the function's definition and how it was being called within the main `metabiv` function.

**3. Implementation Steps:**

*   **Step 1: Debug the Function Signature.**
    *   Thoroughly review the `comp.tau.mu.log.OR.dev.pvals.exact` function in `R/bivariate_meta.R`.
    *   Ensure its arguments are only what it needs. It should likely only require `data.tbl`, `mu.vec.tst`, `tau.vec.tst`, and `N.sig` (the number of simulations). It should calculate the `y.k` and `sigma.2.k` values internally from the `data.tbl`.

*   **Step 2: Validate the Simulation Logic.**
    *   Cross-reference the function's internal logic with the procedure described in the Saad et al. supplementary materials. The correct procedure is:
        1.  For each candidate `(µ₀, τ₀)` on the grid, simulate `N` datasets.
        2.  Each simulation involves generating new "true" effects `θₖ` from `N(µ₀, τ₀²)`.
        3.  From these `θₖ`, simulate new study outcomes (cell counts) using the **noncentral hypergeometric distribution** (e.g., via `rnoncenhypergeom` from the `BiasedUrn` package).
        4.  For each simulated dataset, calculate a new deviance statistic `T*`.
        5.  The "exact" p-value for `(µ₀, τ₀)` is the proportion of simulated `T*` values that are greater than or equal to the originally observed deviance `T`.

*   **Step 3: Re-integrate into `metabiv`.**
    *   Once the `comp.tau.mu.log.OR.dev.pvals.exact` function is debugged and validated, restore the conditional logic within the `metabiv` function:
      ```r
      if (sm == "OR") {
        # Call the now-working exact test function
        dev_pvals <- comp.tau.mu.log.OR.dev.pvals.exact(...)
      } else {
        # The standard method for RR and SMD
        dev_pvals <- comp.tau.mu.dev.pvals(...)
      }
      ```

---
By following these steps, the application will be fully compliant with the advanced methodology proposed in the source articles. 