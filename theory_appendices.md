# Theory Appendices

This document provides a detailed explanation of the theoretical underpinnings and calculations performed by the Comprehensive Meta-Analysis App.

## Data Input and Pre-processing

The application accepts three main types of data:

1.  **Binary (2x2) Data**: Requires columns `study`, `ie` (intervention events), `it` (intervention total), `pe` (placebo/control events), and `pt` (placebo/control total).
2.  **Continuous (SMD) Data**: Requires columns `study`, `smd` (Standardized Mean Difference), `ci_lower` (lower confidence interval bound), and `ci_upper` (upper confidence interval bound).
3.  **Hazard Ratio (HR) Data**: For time-to-event outcomes. Two formats are accepted:
    *   **Format 1 (HR and Confidence Intervals):** Columns `study`, `hr` (Hazard Ratio), `ci_lower` (lower 95% CI for HR), `ci_upper` (upper 95% CI for HR).
    *   **Format 2 (Log Hazard Ratio and Standard Error):** Columns `study`, `loghr` (natural logarithm of the Hazard Ratio), `se_loghr` (standard error of the log Hazard Ratio).

For SMD data, the standard error (SE) and variance are calculated from the confidence intervals as follows:

\[
SE = \frac{CI_{upper} - CI_{lower}}{2 \times 1.96}
\]
\[
Variance = SE^2
\]

For HR data provided in Format 1 (HR and CIs), the application internally converts these values to the log scale for analysis:
\[
loghr = \ln(hr)
\]
\[
se\_loghr = \frac{\ln(CI_{upper}) - \ln(CI_{lower})}{2 \times \Phi^{-1}(0.975)}
\]
where $\Phi^{-1}(0.975)$ is the 0.975 quantile of the standard normal distribution (approximately 1.96).

The application allows for the removal of rows with missing values.

## Overall Results Tab

### Method Comparison

This plot compares the overall effect size estimates and their 95% confidence intervals from the three different meta-analysis models: Fixed Effects, Random Effects, and Bivariate. This allows for a quick visual assessment of the consistency of the results across different analytical assumptions.

### Summary Table

This table provides a numerical summary of the three models, including:

*   **Effect Size**: The pooled effect estimate (e.g., Odds Ratio, Risk Ratio, or SMD).
*   **Confidence Interval**: The 95% confidence interval for the pooled effect size.
*   **P-value**: The p-value for the overall effect.
*   **Heterogeneity (τ²)**: The estimate of the between-study variance (for random and bivariate models).
*   **I²**: The percentage of total variation across studies that is due to heterogeneity rather than chance.

## Fixed Effects Analysis

The fixed-effects model assumes that all studies in the meta-analysis share a single common effect size. The variation between studies is considered to be due to chance alone. The calculations for the fixed-effects model are performed using the `metabin` (for binary data) or `metagen` (for continuous data) functions from the **`meta`** R package.

### Effect Size and Heterogeneity

*   **Forest Plot**: Displays the effect size and confidence interval for each individual study, along with the pooled fixed-effect estimate represented by a diamond. The size of the square for each study is proportional to its weight in the analysis.
*   **Overall Summary**: Provides the numerical results of the fixed-effects meta-analysis, including the pooled effect size, confidence interval, and z-score.
*   **Model Fit Statistics**: Shows the results of the heterogeneity test, including Cochran's Q statistic, the p-value for the Q statistic, and the I² statistic.

### Model Diagnostics

*   **Radial Plot**: Also known as a Galbraith plot, it is used to assess heterogeneity. It plots the standardized effect sizes against their precision (1/SE). Studies that lie outside the ±2 standard deviation lines are potential outliers.
*   **Q-Q Plot**: A quantile-quantile plot of the standardized residuals. It helps to assess whether the residuals follow a standard normal distribution. If the points fall along the diagonal line, it suggests that the model is a good fit for the data.
*   **Outlier Detection Plot**: Displays the standardized residuals for each study. Studies with residuals outside the ±1.96 lines are considered potential outliers.

### Publication Bias

*   **Funnel Plot**: A scatter plot of the effect estimates from individual studies against a measure of study size or precision. An asymmetric funnel shape can be an indication of publication bias.
*   **Trim and Fill Plot**: A method to estimate the number and outcomes of missing studies due to publication bias. It then "fills in" the funnel plot with these imputed studies and re-computes the overall effect size.
*   **Egger's Test**: A linear regression-based test for funnel plot asymmetry. A significant result suggests the presence of publication bias.

### Sensitivity Analysis

*   **Leave-One-Out Plot**: This analysis iteratively removes one study at a time and recalculates the pooled effect size. It helps to identify influential studies that have a disproportionate impact on the overall result.
*   **Influence Plot (Baujat Plot)**: A plot that helps to identify studies that are influential in the meta-analysis. It plots the contribution of each study to the overall Q-statistic against its influence on the pooled effect size.

## Random Effects Analysis

The random-effects model assumes that the true effect size varies from study to study. The model estimates the mean of the distribution of effect sizes and the variance of this distribution (τ², tau-squared). The calculations for the random-effects model are also performed using the `metabin` and `metagen` functions from the **`meta`** R package.

The plots and summaries in this section are analogous to those in the Fixed Effects Analysis section but are based on the random-effects model assumptions.

## Subgroup Analysis

Subgroup analyses can be performed to investigate heterogeneity and explore whether the overall effect size differs based on certain study characteristics (e.g., patient populations, study settings, intervention types).

In this application, subgroup analyses are conducted by specifying a categorical `byvar` (subgrouping variable) in the `metabin` (for binary data) or `metagen` (for continuous/SMD and HR data) functions from the `meta` package.

When a subgroup analysis is performed:
*   The chosen meta-analysis model (fixed effect or random effects) is applied separately to each subgroup defined by the levels of the `byvar`.
*   An overall test for subgroup differences (also known as a test for interaction) is performed. This test assesses whether there is statistically significant heterogeneity between the subgroup-specific effect estimates. It is typically based on a Q-statistic (Cochran's Q) with degrees of freedom equal to the number of subgroups minus 1. A significant p-value for this test suggests that the effect measure truly varies across the subgroups. The `meta` package provides this test (often denoted $Q_b$) and its p-value.
*   The results will include both the pooled estimates within each subgroup and the overall pooled estimate across all studies (if applicable, depending on the model and settings). Forest plots will also display results by subgroup.

It's important to note that subgroup analyses should be pre-specified whenever possible and interpreted with caution, especially if there are many subgroup variables tested or if subgroups contain few studies, as this can lead to spurious findings. The power to detect genuine subgroup differences is often low.

## Bivariate Approach

The bivariate approach is a more advanced random-effects model that is implemented in this application based on the methodology described in Saad et al. (2019) and the supplementary material provided. This model assumes that the observed effect sizes (`y_k` for study `k`) arise from a normal distribution with a study-specific mean (`θ_k`) and within-study variance (`σ_k^2`). The study-specific means `θ_k` are themselves assumed to be a random sample from a normal distribution with a common mean `μ` (the overall effect size) and between-study variance `τ^2`.

\[
y_k \sim N(\theta_k, \sigma_k^2)
\]
\[
\theta_k \sim N(\mu, \tau^2)
\]

This leads to the marginal distribution of `y_k`:

\[
y_k \sim N(\mu, \sigma_k^2 + \tau^2)
\]

The parameters `μ` and `τ` are estimated using Maximum Likelihood Estimation (MLE).

### Effect Measure Handling

The bivariate model is adapted for different effect measures:

*   **Odds Ratio (OR) and Relative Risk (RR)**: For binary data, the log OR or log RR and their corresponding variances are calculated for each study. A continuity correction of 0.5 is added to all cells of the 2x2 table if any cell contains a zero, to prevent division by zero.
    *   **log(OR)**: `log((n.11 + 0.5) * (n.22 + 0.5) / ((n.12 + 0.5) * (n.21 + 0.5)))`
    *   **Var(log(OR))**: `1/(n.11 + 0.5) + 1/(n.12 + 0.5) + 1/(n.21 + 0.5) + 1/(n.22 + 0.5)`
*   **Standardized Mean Difference (SMD)**: For continuous data, the SMD (`y`) and its variance (`sigma2`) are used directly.

### Effect Size and Heterogeneity

*   **Bivariate Forest Plot**: Similar to the other forest plots, but the overall summary diamond is based on the bivariate model's MLE estimates for `μ` and its confidence interval.
*   **Confidence Region Plot**: This is a key plot in the bivariate approach. It shows the joint confidence region for the two main parameters of the model: the overall effect size `μ` and the between-study standard deviation `τ`. The plot is generated by calculating the deviance for a grid of `μ` and `τ` values and then finding the contour lines corresponding to specific p-values (e.g., p=0.05 for a 95% confidence region). The Maximum Likelihood Estimates (MLE) for `μ` and `τ` are marked on the plot.
*   **Efficacy-Harm Plot**: This plot, based on the supplementary material, visualizes the probabilistic relationship between treatment efficacy and harm. It plots the cumulative distribution function (CDF) of the effect size, allowing one to read the probability that the true effect size is below a certain threshold. For example, for an Odds Ratio, it can show the probability that the OR is less than 1 (efficacy) and the probability that it is greater than 1 (harm).

### Model Diagnostics

*   **Q-Q Plot (Standardized Residuals)**: This plot assesses the normality assumption for the random effects. It plots the quantiles of the standardized residuals against the quantiles of a standard normal distribution. The standardized residual for study `k` is calculated as:
    \[
    r_k = \frac{y_k - \hat{\mu}}{\sqrt{\sigma_k^2 + \hat{\tau}^2}}
    \]
*   **Q-Q Plot (Raw Residuals)**: This plot shows the raw residuals (`y_k - \hat{\mu}`) against the theoretical quantiles. The size of the points is proportional to the study size, giving more visual weight to larger studies.

### Publication Bias

*   **Adapted Funnel Plot**: This is a standard funnel plot of the effect sizes (`y_k`) against their standard errors (`σ_k`), but the interpretation is in the context of the bivariate model.

### Sensitivity Analysis

*   **Confidence Region Shift Plot**: This is a novel plot for sensitivity analysis in the bivariate model. It shows how the 95% confidence region for (`μ`, `τ`) shifts when each study is removed one at a time. This helps to visually identify influential studies that cause a large shift in the confidence region.
*   **Enhanced Baujat Plot**: This plot is adapted for the bivariate model to identify influential studies. It plots a measure of a study's contribution to heterogeneity against its influence on the pooled estimate.

This completes the theoretical overview of the methods implemented in the app. The use of established libraries for standard methods and a custom implementation for the advanced bivariate approach provides a robust and comprehensive tool for meta-analysis.

## Meta-Regression

Meta-regression is used to explore the relationship between study-level covariates (characteristics of the studies) and the effect sizes observed in those studies. It aims to explain some of the heterogeneity between studies by accounting for these covariates.

The basic model for a meta-regression with one covariate can be expressed as:
\[
TE_i = \beta_0 + \beta_1 \times X_{1i} + u_i + e_i
\]
Where:
- $TE_i$ is the observed effect size in study $i$.
- $\beta_0$ is the estimated overall effect size when the covariate $X_{1i}$ is zero (or at its baseline level if categorical).
- $X_{1i}$ is the value of the covariate for study $i$.
- $\beta_1$ is the estimated change in $TE_i$ for each one-unit increase in $X_{1i}$. If $X_{1i}$ is categorical, $\beta_1$ represents the difference compared to a reference category.
- $u_i$ is the random effect for study $i$, representing the deviation of study $i$'s true effect from that predicted by covariates, where $u_i \sim N(0, \tau^2)$. $\tau^2$ is the residual heterogeneity, i.e., the between-study variance not explained by the covariate(s).
- $e_i$ is the within-study error for study $i$, where $e_i \sim N(0, \sigma_i^2)$, and $\sigma_i^2$ is the known within-study variance.

This model can be extended to include multiple covariates.

### Implementation
In this application, meta-regression is performed using the `metareg()` function from the **`meta`** R package. This function takes a pre-existing meta-analysis object (typically a random-effects model from `metagen` or `metabin`) and adds one or more covariates to the model.

### Key Outputs
Common outputs from a meta-regression analysis include:
*   **Coefficients ($\beta$) for each covariate**: These indicate the strength and direction of the relationship between the covariate and the effect size.
*   **Standard Errors (SE) and Confidence Intervals for coefficients**: These provide a measure of precision for the estimated coefficients.
*   **Test of Significance for each coefficient**: Typically a z-test or t-test to determine if the coefficient is statistically significantly different from zero.
*   **Test of Moderators (e.g., QM statistic)**: An omnibus test to assess whether the set of all covariates included in the model significantly explains heterogeneity.
*   **Residual Heterogeneity ($\tau^2$)**: An estimate of the between-study variance that remains unexplained after accounting for the covariates. A reduction in $\tau^2$ compared to the model without covariates suggests that the covariates explain some of the heterogeneity.
*   **R-squared (R²)**: An estimate of the proportion of between-study variance explained by the covariates.

### Bubble Plot
A common visualization for meta-regression (especially with a single continuous covariate) is a bubble plot. In this plot:
*   The x-axis represents the values of the covariate.
*   The y-axis represents the effect sizes of the studies.
*   Each study is represented by a bubble, where the size of the bubble is often proportional to the precision of the study (e.g., inverse of the within-study variance).
*   A regression line is typically overlaid to show the estimated relationship between the covariate and the effect size.

This plot helps to visually assess the relationship and identify studies that may be particularly influential or deviate from the overall trend.