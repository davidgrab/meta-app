# Comprehensive Normality Diagnostics for Meta-Analytic Models

## Overview

This document describes the implementation of comprehensive normality diagnostics for three meta-analytic models, based on the unified framework you specified. The implementation provides robust tools for assessing the normality assumptions underlying different meta-analytic approaches.

## Theoretical Foundation

The implementation is based on the unified overview of normality assumptions in three meta-analytic models:

1. **Fixed-Effects Meta-Analysis**: Tests if sampling errors follow N(0,σᵢ²)
2. **Standard Random-Effects Meta-Analysis**: Tests both within-study (Y_i|θ_i ~ N(θ_i,σᵢ²)) and between-study (θ_i ~ N(μ,τ²)) normality
3. **Bivariate ("Exact" MLE) Meta-Analysis**: Same distributional assumptions as standard random-effects, but with joint profile-likelihood estimation

## Implementation Details

### 1. Fixed-Effects Meta-Analysis Diagnostics

#### Functions Implemented:
- `qq_plot_fixed_residuals(fixed_results, envelope = TRUE)`
- `shapiro_test_fixed(fixed_results)`

#### Diagnostics Performed:
1. **Q-Q Plot of Standardized Residuals**
   - Computes r_i = (Y_i - θ̂)/σ_i
   - Tests whether residuals follow N(0,1)
   - Includes optional 95% simulation envelope
   - Interpretation:
     - Points along diagonal = Normal distribution
     - S-shaped patterns = Tail deviations
     - Systematic curvature = Skewness

2. **Shapiro-Wilk Test**
   - Formal test for normality (H₀: residuals are Normal)
   - p > 0.05 suggests no strong evidence against normality
   - Note: Low power for small samples

#### Usage in UI:
- New "Comprehensive Normality Diagnostics" tab in Fixed Effects Analysis
- Visual Q-Q plot with envelope
- Text summary of formal test results

### 2. Standard Random-Effects Meta-Analysis Diagnostics

#### Functions Implemented:
- `calculate_blups(random_results)`
- `qq_plot_random_blups(random_results, envelope = TRUE)`
- `calculate_deleted_residuals_random(random_results)`
- `qq_plot_random_deleted_residuals(random_results, envelope = TRUE)`
- `shapiro_test_random(random_results)`

#### Diagnostics Performed:
1. **Q-Q Plot of BLUPs (Best Linear Unbiased Predictors)**
   - Computes θ̂_i = μ̂ + [τ̂²/(τ̂² + σᵢ²)] × (Y_i - μ̂)
   - Tests normality of random effects distribution
   - Standardizes BLUPs for plotting: z_i = (θ̂_i - θ̄)/sd(θ̂)

2. **Q-Q Plot of Standardized Deleted Residuals**
   - For each study i, removes it and re-estimates μ₍₋ᵢ₎ and τ²₍₋ᵢ₎
   - Computes r_i = (Y_i - μ̂₍₋ᵢ₎)/√(σᵢ² + τ̂²₍₋ᵢ₎)
   - More robust assessment of combined assumptions
   - Uses DerSimonian-Laird estimator for leave-one-out estimation

3. **Shapiro-Wilk Tests**
   - Separate tests for both BLUPs and deleted residuals
   - Both should be non-significant for model validity

#### Usage in UI:
- New "Comprehensive Normality Diagnostics" tab in Random Effects Analysis
- Side-by-side Q-Q plots for BLUPs and deleted residuals
- Comprehensive summary of both formal tests

### 3. Bivariate ("Exact" MLE) Meta-Analysis Diagnostics

#### Functions Implemented:
- `calculate_blups_bivariate(bivariate_results)`
- `qq_plot_bivariate_blups(bivariate_results, envelope = TRUE)`
- `calculate_deleted_residuals_bivariate(bivariate_results, data, input)`
- `qq_plot_bivariate_deleted_residuals(bivariate_results, data, input, envelope = TRUE)`
- `shapiro_test_bivariate(bivariate_results, data, input)`

#### Diagnostics Performed:
1. **Q-Q Plot of BLUPs (Bivariate MLE)**
   - Uses jointly estimated μ̂_MLE and τ̂_MLE
   - Computes θ̂_k = μ̂_MLE + [τ̂²_MLE/(τ̂²_MLE + σₖ²)] × (Y_k - μ̂_MLE)
   - More precise than standard random effects due to exact MLE
   - Enhanced simulation envelope using true model parameters

2. **Q-Q Plot of Deleted Residuals (Bivariate MLE)**
   - For each study k, removes it and refits entire bivariate model
   - Computes r_k = (Y_k - μ̂₍₋ₖ₎)/√(σₖ² + τ̂²₍₋ₖ₎)
   - Most computationally intensive but most robust diagnostic
   - Handles both binary and continuous data types

3. **Shapiro-Wilk Tests**
   - Tests both BLUPs and deleted residuals
   - Based on exact MLE estimates rather than approximations
   - Higher precision may detect subtle departures from normality

#### Usage in UI:
- New "Comprehensive Normality Diagnostics" tab in Bivariate Approach
- Side-by-side Q-Q plots for BLUPs and deleted residuals
- Comprehensive summary with bivariate-specific interpretations

## Data Type Support

### Binary Data (2×2 Tables)
- **Supported Effect Measures**: Relative Risk (RR), Odds Ratio (OR)
- **Input Format**: study, ie, it, pe, pt columns
- **Processing**: Raw counts converted to log effect sizes and variances
- **Example**: Hypericum depression trials (response vs. non-response)

### Continuous Data (Standardized Mean Difference)
- **Supported Effect Measures**: SMD (Standardized Mean Difference)
- **Input Format**: study, smd, ci_lower, ci_upper columns
- **Processing**: Standard errors calculated from 95% CIs: se = (ci_upper - ci_lower)/(2×1.96)
- **Example**: Treatment efficacy measured on continuous scales

## Unified Framework Benefits

### 1. Consistency Across Models
- Same diagnostic principles apply regardless of estimation method
- Comparable interpretation guidelines for all three approaches
- Standardized output format and visualization

### 2. Comprehensive Assessment
- Visual diagnostics (Q-Q plots) complemented by formal tests
- Multiple perspectives on normality (BLUPs vs. deleted residuals)
- Simulation envelopes provide robust reference distributions

### 3. Clinical Relevance
- Violations suggest need for alternative distributions
- Guides decisions about transformation or robust methods
- Supports evidence quality assessment (GRADE criteria)

## Interpretation Guidelines

### Q-Q Plot Patterns:
- **Points close to diagonal line**: Normal distribution assumption supported
- **S-shaped curves**: Tail deviations (heavy or light tails)
- **Systematic curvature**: Skewness in the distribution
- **Points outside envelope**: Potential outliers or model misspecification

### Formal Test Results:
- **p > 0.05**: No strong evidence against normality
- **p ≤ 0.05**: Evidence against normality assumption
- **Low power consideration**: Small samples may not detect violations

### Model-Specific Considerations:
- **Fixed Effects**: Focus on sampling error normality
- **Random Effects**: Assess both sampling and random effects distributions
- **Bivariate MLE**: Higher precision may detect subtle violations

## Integration with Existing Application

### New UI Components:
1. **"Comprehensive Normality Diagnostics" tabs** added to all three analysis sections
2. **Interactive help buttons** with detailed explanations
3. **Coordinated plot layouts** for optimal comparison
4. **Formal test summaries** with interpretation guidance

### Backend Integration:
1. **Reactive outputs** for all new diagnostic plots
2. **Error handling** for edge cases and insufficient data
3. **Performance optimization** for computationally intensive diagnostics
4. **Data type detection** for automatic processing

## Compliance with Saad et al. Methodology

### ✅ Successfully Implemented:
1. **Fixed-effects diagnostics** as specified in Section 1
2. **Random-effects diagnostics** as specified in Section 2  
3. **Bivariate MLE diagnostics** as specified in Section 3
4. **Unified interpretation framework** for all model types
5. **Support for both binary and continuous data**
6. **Simulation envelopes** for robust assessment
7. **Formal statistical tests** complementing visual diagnostics

### ✅ Enhanced Beyond Original Specification:
1. **Interactive help system** with detailed mathematical explanations
2. **Automatic data type handling** for seamless user experience
3. **Error handling and fallback options** for robust operation
4. **Coordinated visualization layouts** for efficient comparison
5. **Performance optimization** for large datasets

## Testing and Validation

### Test Script: `test_normality_diagnostics.R`
- Demonstrates functionality with real datasets
- Tests both binary and continuous data workflows
- Validates all three model types
- Provides comprehensive output examples

### Example Datasets:
1. **Hypericum Depression Data**: Binary outcomes (18 RCTs)
2. **SMD Example Data**: Continuous outcomes (multiple studies)

### Validation Results:
- All diagnostic functions execute successfully
- Q-Q plots display appropriate patterns
- Formal tests provide sensible results
- Error handling works for edge cases

## Future Enhancements

### Potential Extensions:
1. **Alternative distributions**: Support for non-Normal random effects
2. **Robust methods**: Implementation when normality fails
3. **Multivariate extensions**: Beyond bivariate to higher dimensions
4. **Bayesian diagnostics**: Posterior predictive checks
5. **Network meta-analysis**: Extension to network models

### Performance Optimizations:
1. **Parallel processing** for deleted residuals computation
2. **Caching mechanisms** for repeated diagnostics
3. **Progressive rendering** for large datasets
4. **Memory optimization** for resource-constrained environments

## Conclusion

The comprehensive normality diagnostics implementation successfully fulfills and extends the Saad et al. methodology requirements. It provides a unified, robust framework for assessing normality assumptions across all three major meta-analytic approaches, with seamless integration into the existing application interface.

The implementation supports both binary and continuous data types, provides both visual and formal diagnostic tools, and includes comprehensive user guidance for interpretation. This enhancement significantly strengthens the application's diagnostic capabilities and supports more informed meta-analytic decision-making. 