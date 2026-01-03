# App Code Synchronization Guide

## Purpose
This document lists the changes needed to sync the **production app repository** with the **thesis code** in this folder.

**Last Updated:** January 2, 2026

---

## Summary

The thesis code in `/code/application_code/` has been refined and validated during thesis writing. The production app at `https://github.com/davidgrab/meta-app` needs to be updated to match.

---

## Changes Required in Production App

### 1. UI Text Updates (Help Text / Tooltips)

| Location | Current (may have) | Should Say |
|----------|-------------------|------------|
| Efficacy-Harm explanation | Bayesian/posterior language | "Frequentist functional projection" |
| JCR explanation | Deviance-based language | "Likelihood-ratio based, p-value ≥ α" |
| BLUP explanation | May be unclear | "Best Linear Unbiased Predictors (empirical Bayes estimates)" |

**Files to update in production app:**
- `ui.R` - tooltip text, help panels
- Any markdown help files

### 2. JCR Construction Verification

Ensure the production app's `bivariate_meta.R` uses the same approach as thesis code:

**Thesis approach (correct):**
```r
# From bivariate_meta.R, comp.tau.mu.dev.pvals():
pval.mat <- array(1 - pchisq(dev.mat, 2), dim = c(n.mu, n.tau))
# Region = points where p-value >= alpha
```

**Check production app has:**
- ✅ LRT statistic: `T = -2 * (loglik - max_loglik)`
- ✅ P-value from χ²₂: `1 - pchisq(T, 2)`
- ✅ Region: `{(μ,τ): p-value ≥ 0.05}` for 95% CI

### 3. Efficacy-Harm Band Calculation

Ensure production app uses **exact line-JCR intersection**, not bootstrap.

**Thesis approach (correct):**
```r
# From bivariate_meta.R, comp.mu.tau.dev.CDF.CI():
# For each q ∈ [0.01, 0.99], find (μ,τ) pairs where:
# h(μ,τ) = Φ((c - μ)/τ) = q
# This forms a line: μ = c - τ*Φ⁻¹(q)
# CI = {q : line intersects JCR}
```

### 4. BLUP Calculation

**Current implementation (thesis):**
```r
# functions.R, calculate_blups():
blups <- mu_hat + (tau2_hat / (tau2_hat + sigma2_i)) * (y_i - mu_hat)
```

This is the standard empirical Bayes shrinkage formula. **Verify production app matches.**

### 5. Deleted Residuals

**Current implementation (thesis):**
```r
# functions.R, calculate_bivariate_deleted_residuals():
# For each study i:
# 1. Refit model without study i
# 2. Compute: r_i^(-) = (Y_i - μ̂_{(-i)}) / sqrt(σ_i² + τ̂²_{(-i)})
```

This is the "studentized deleted residual" approach from Viechtbauer (2021).

### 6. Axis Labels

Ensure all JCR plots have:
- **X-axis:** "Population Mean Effect μ (log RR)" or "(SMD)"
- **Y-axis:** "Heterogeneity SD τ"

Not "Effect Size" on the x-axis (that's for efficacy-harm plots).

---

## Files to Compare

| Thesis File | Production App File | Action |
|-------------|---------------------|--------|
| `code/application_code/bivariate_meta.R` | `bivariate_meta.R` | Diff and sync |
| `code/application_code/functions.R` | `functions.R` | Diff and sync |
| `code/application_code/ui.R` | `ui.R` | Update help text |
| `code/application_code/server.R` | `server.R` | Diff and sync |

---

## Specific Code Blocks to Verify

### A. metabiv() Function (bivariate_meta.R)

The main JCR function. Check:
1. Grid computation range matches
2. P-value calculation is `1 - pchisq(dev.mat, 2)`
3. Confidence region uses p-value threshold

### B. comp.eff.harm.plot() Function (bivariate_meta.R)

Check:
1. CDF functional correctly implemented
2. Line-JCR intersection algorithm used
3. No bootstrap resampling

### C. calculate_blups() Functions (functions.R)

Check:
1. Formula matches thesis: `μ̂ + (τ̂²/(τ̂² + σᵢ²)) * (Yᵢ - μ̂)`
2. Standardization is done correctly

### D. Deleted Residuals Functions (functions.R)

Check:
1. Leave-one-out refitting is done
2. Formula uses LOO estimates for μ and τ²

---

## Metafor Usage

The thesis code uses `metafor` package for:
- ✅ Meta-regression: `metafor::rma()`, `metafor::rma.mv()`
- ✅ Permutation tests: `metafor::permutest()`
- ✅ Standard heterogeneity estimators (via `meta` package which uses metafor)

The thesis code does **NOT** use `metafor` for:
- ❌ BLUPs (custom implementation)
- ❌ JCR/bivariate analysis (custom implementation)
- ❌ Efficacy-harm bands (custom implementation)
- ❌ Deleted residuals (custom implementation)

---

## Sync Procedure

1. **Backup production app**
   ```bash
   cd /path/to/meta-app
   git checkout -b backup-before-thesis-sync
   git push origin backup-before-thesis-sync
   ```

2. **Copy thesis code files**
   ```bash
   cp /path/to/thesis/code/application_code/bivariate_meta.R .
   cp /path/to/thesis/code/application_code/functions.R .
   ```

3. **Diff and review**
   ```bash
   git diff bivariate_meta.R
   git diff functions.R
   ```

4. **Update UI help text manually** (can't just copy due to potential other changes)

5. **Test thoroughly**
   - Run on BCG dataset
   - Run on CBT dataset
   - Verify JCR contours match thesis figures
   - Verify efficacy-harm bands match thesis figures

6. **Commit**
   ```bash
   git add -A
   git commit -m "Sync with thesis code - align with Saad methodology"
   ```

---

## Validation Checklist

After syncing, verify:

| Check | Status |
|-------|--------|
| [ ] BCG JCR contour matches thesis Figure 6.3 |
| [ ] BCG efficacy-harm matches thesis Figure 6.4 |
| [ ] CBT JCR contour matches thesis Figure 6.8 |
| [ ] CBT efficacy-harm matches thesis Figure 6.9 |
| [ ] Q-Q plots have blue regression lines |
| [ ] Help text mentions "frequentist" and "functional projection" |
| [ ] No Bayesian/posterior language in UI |

---

## Notes

- The thesis code is the **authoritative version** for methodology
- Production app may have additional UI features not in thesis
- Focus sync on: `bivariate_meta.R`, BLUP functions, deleted residual functions
