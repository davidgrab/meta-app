# Future Improvements for Bivariate Meta-Analysis (Enhanced Plan)

This document outlines the definitive, step-by-step tasks required to improve the application's statistical correctness and user interface. This plan is based on a thorough review of the existing codebase and the original source articles.

---

## Part 1: Implement the Exact, Simulation-Based Test for Odds Ratio

**Objective:**
To replace the current, unstable placeholder calculation for Odds Ratio (OR) with the correct, simulation-based "Exact Test" as described in the source literature. This will resolve known convergence failures and produce statistically valid results.

**Key Tasks:**

*   **Task 1.1: Fix the Exact Test Function in `R/bivariate_meta.R`**
    *   **Action:** Locate the dormant function `comp.tau.mu.log.OR.dev.pvals.exact`. Its current content is a placeholder and must be replaced.
    *   **Implementation:** Re-write the function to perform the following logic, derived from the original paper's code:
        1.  The function signature will be `comp.tau.mu.log.OR.dev.pvals.exact(data.tbl, mu.vec.tst, tau.vec.tst, N.sig = 100)`. `N.sig` is the number of simulations; `100` is chosen as a starting point to ensure the application remains fast.
        2.  Calculate the *observed* deviance statistic for each `(µ, τ)` point on the grid using the real data.
        3.  For each `(µ, τ)` point, begin a simulation loop (`for (i in 1:N.sig)`).
        4.  Inside the loop, simulate a new set of "true" log-ORs (`theta.k`) for each study from a normal distribution `N(µ, τ²)`.
        5.  From these `theta.k`, simulate new 2x2 study tables using the noncentral hypergeometric distribution. The `BiasedUrn::rFNCHypergeo.vec` function from the original script is the correct tool for this.
        6.  Calculate the deviance statistic for this *newly simulated* dataset.
        7.  The final p-value for the `(µ, τ)` point is the proportion of simulated deviances that were greater than or equal to the *observed* deviance.
    *   **Outcome:** A correct, functioning exact test that returns a list containing the deviance matrix and the exact p-value matrix.

*   **Task 1.2: Integrate the Exact Test into the Core `metabiv` Function**
    *   **Action:** In `R/bivariate_meta.R`, find the line `dev_pvals <- comp.tau.mu.dev.pvals(...)` within the `metabiv` function.
    *   **Implementation:** Replace that line with a conditional block that correctly routes the analysis based on the summary measure (`sm`):
        ```r
        if (sm == "OR") {
          # Call the new, correct function for Odds Ratio
          dev_pvals <- comp.tau.mu.log.OR.dev.pvals.exact(data.tbl, mu.vec, tau.vec, N.sig = 100)
        } else {
          # Use the existing standard method for RR and SMD
          dev_pvals <- comp.tau.mu.dev.pvals(data.tbl, mu.vec, tau.vec, sm, y.k.in = y.k, sigma.2.k.in = sigma.2.k)
        }
        ```
    *   **Outcome:** The application will now use the exact test when, and only when, the user selects "OR".

*   **Task 1.3: Update the Validation Script `temp.R`**
    *   **Action:** Modify the `run_or_comparison` function in `temp.R` to serve as a valid unit test for the new implementation.
    *   **Implementation:** The "Benchmark" will no longer be `meta::metabin`. It will be a call to the *original paper's* `comp.tau.mu.log.OR.dev.pvals` function to get the benchmark deviance and MLE values. The "Validation" will be a call to our updated `metabiv` function.
    *   **Outcome:** A validation script that provides a true apples-to-apples comparison, confirming that our new implementation faithfully reproduces the original paper's methodology.

---

## Part 2: Add UI Loading Indicators for Enhanced User Experience

**Objective:**
To provide immediate visual feedback to the user during calculations, preventing the UI from appearing frozen and improving the application's perceived responsiveness.

**Key Tasks:**

*   **Task 2.1: Add `shinycssloaders` Library to the UI**
    *   **Action:** Edit `R/ui.R`.
    *   **Implementation:** Add `library(shinycssloaders)` at the top of the file.
    *   **Outcome:** The necessary functions for creating loading spinners will be available to the app.

*   **Task 2.2: Wrap All Plot Outputs with `withSpinner()`**
    *   **Action:** In `R/ui.R`, systematically wrap every `plotOutput()` and `plotlyOutput()` call with the `withSpinner()` function. No change to the UI layout will occur.
    *   **Implementation:** For example, `plotOutput("methodComparisonPlot")` becomes `withSpinner(plotOutput("methodComparisonPlot"))`. This will be applied to all 21 plots identified during the analysis phase.
    *   **Outcome:** Every plot in the application will automatically display a loading spinner while the underlying R code is executing, and the spinner will disappear when the plot is rendered.

---

This plan is complete and actionable. I am ready to proceed with the implementation.
