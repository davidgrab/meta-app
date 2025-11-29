# Efficacy/Harm Plot Smoothing Fix

This note captures exactly how the new smoothing logic eliminates the “bump” that appeared when running the JCR analysis (e.g., the BCG dataset). Keep this handy if you ever need to reason about or tweak the fix.

## Background
- The JCR efficacy/harm plot is driven by `comp.eff.harm.plot()` in `R/bivariate_meta.R`, which transforms the joint CDF of the effect estimate into two ribbons (benefit vs. harm).
- With sparse or heterogeneous data (BCG is the canonical example), the raw CDF sampled from the deviance grid was noisy and occasionally out-of-order. When we interpolated/extended that curve, small local reversals showed up as visible “bumps.”

## Root Cause
1. The CDF grid extracted from `comp.mu.tau.dev.CDF.CI()` is not guaranteed to be sorted monotone after tapering/extending.
2. We were feeding those unsorted points straight into `approx()`, so a single inversion produced a down-then-up shape in the plotted ribbon.

## Fix Overview
`comp.eff.harm.plot()` now rebuilds the CDF with three guard rails before plotting:
1. **Safe interpolation (`safe_approx`)**  
   - Sorts `x` values, drops duplicates, and falls back to a mean if there are <2 unique points.  
   - This alone prevents the biggest jumps when the taper introduces repeated values.
2. **Spline smoothing (`smooth_monotone`)**  
   - Fits `smooth.spline(..., spar = 0.65)` (or linear interpolation if the fit fails).  
   - Clamps to `[0,1]`, then enforces monotonicity using `cummax` (or its reverse for decreasing sections).  
   - Because the ribbons use `1 - cdf` on the harm side, we smooth both the primary and reflected curves.
3. **Consistent bounds**  
   - After smoothing we recompute `cdf.ll`/`cdf.ul` so that the lower band never crosses the point estimate, guaranteeing ribbons stay ordered.

These steps happen for both SMD and OR/RR code paths, so every combination now benefits from the same stability logic.

## Implementation Details
- **Helpers injected at the top of `comp.eff.harm.plot()`:**
  ```r
  safe_approx <- function(...) { ... }
  smooth_monotone <- function(...) { ... }
  clamp_prob <- function(p) pmax(0, pmin(1, p))
  ```
- **SMD branch:**  
  - `cdf.est`, `cdf.ll`, and `cdf.ul` each pass through `safe_approx` ➜ `smooth_monotone`.  
  - We create `cdf.lower <- pmin(cdf.ll, cdf.ul)` and `cdf.upper <- pmax(...)`, then reset the lower/upper envelopes relative to the smoothed estimate.  
  - The rest of the code (building `plot_data`, trimming to non-trivial probabilities, etc.) stays the same, but now consumes well-behaved curves.
- **RR/OR branch:**  
  - Identical smoothing steps happen after the exponential re-scaling (log → original scale).  
  - The probability ribbons for `x < 1` and `x > 1` now reference monotone arrays, so interpolation for `le1.vec`/`mt1.vec` cannot wiggle.

## Validation Flow
1. Run the BCG example (or any dataset) so that `bivariate_result()` is populated.
2. From the console, call `comp.eff.harm.plot(...)` and inspect `ggplot_build(p)$data[[1]]$y`.  
   - `diff(...)` should now be strictly non-negative within tolerance, confirming monotonicity.
3. Visually confirm the bump is gone in the Shiny app. The plot transitions smoothly from efficacy to harm, even with extreme heterogeneity.

## Future Tweaks
- The smoothing strength (`spar = 0.65`) can be exposed as an argument if we ever want user control.  
- If performance becomes a concern, cache the smoothed CDF alongside `CDF.ci.obj` instead of recomputing in every render.  
- Additional diagnostics (e.g., warning when raw CDF had inversions) could be logged to help spot problematic datasets faster.

This document, together with the git diff around `comp.eff.harm.plot()`, should give you all the context needed to revisit or extend the fix later.

