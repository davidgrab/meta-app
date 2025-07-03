# Modern Meta-Analysis Shiny App – User Guide

## 1. App Workflow (Step-by-Step)

1. **Load Data**
   1. Click **"Browse"** on the sidebar and upload either a CSV or Excel file.
   2. Mandatory columns (case-sensitive):
      * **Binary (2 × 2)** – `study`, `ie`, `it`, `pe`, `pt`  
        *(ie = intervention events, it = intervention total, pe = placebo events, pt = placebo total)*
      * **Continuous (SMD)** – `study`, `smd`, `ci_lower`, `ci_upper`
   3. *Optional* columns – any additional variable(s) you intend to use for **Subgroup** or **Meta-Regression** (e.g. `sex`, `dose`, `risk_level`).  Categorical variables will automatically appear in subgroup dropdowns; both categorical and numeric variables appear in meta-regression.
2. **Choose Effect Type** (binary vs continuous) and **Effect Measure** (`RR` or `OR`).
3. Click **"Analyze"** – this unlocks the four primary analysis panels:
   - **Random Effects**
   - **Fixed Effects**
   - **Bivariate Approach** (joint μ/τ estimation)
   - **Meta-Regression** (optional, separate panel)
4. Within each primary analysis, tabs follow a uniform structure:
   | Tab | Purpose | Typical Action |
   |-----|---------|----------------|
   | *Effect Size & Heterogeneity* | Forest plot, pooled estimate, τ²/I² | Inspect overall effect; note heterogeneity |
   | *Model Diagnostics* | Normality checks, Baujat, influence, etc. | Verify model assumptions & influential studies |
   | *Publication Bias* | Funnel, trim-and-fill, Egger's | Evaluate risk of small-study/publication bias |
   | *Sensitivity Analysis* | Leave-one-out, cumulative, GOSH, etc. | Check robustness of conclusions |
   | *Subgroup Analysis* | NEW – pooled effects by subgroup | Select a categorical variable & click **Run Subgroup** |

---

## 2. Subgroup Analysis (All Methods)

1. Navigate to the **Subgroup Analysis** tab inside **Random**, **Fixed**, or **Bivariate** panels.
2. In the sidebar choose a **Subgroup Variable** (must be categorical).
3. Click **Run Subgroup Analysis**.
4. Outputs:
   * **Forest Plot** – studies grouped by subgroup with separate diamonds.
   * **Q-between test** – p-value for subgroup differences.
5. **Interpretation**
   * Non-overlapping diamonds or Q-between p < 0.05 → evidence that treatment effect differs between subgroups.
   * Wide CIs or few studies per subgroup → interpret cautiously.

---

## 3. Meta-Regression

> Located in its own **"Meta-Regression"** panel.

1. **Moderator Selection** – choose one variable (categorical or continuous).  (Multiple moderators not yet supported – use separate runs.)
2. Pick **Variable Type** (auto-detected, but you can override).
3. Decide on **Model** (Random Effects recommended when heterogeneity present).
4. Click **Run Meta-Regression**.

### Outputs
| Tab | Description | Interpretation |
|-----|-------------|----------------|
| Regression Plot | • Continuous → scatter + fitted line + 95 % band  
• Categorical → bar means + error bars + study points | Slope ≠ 0 or level coefficients ≠ 0 indicate moderator explains effect size variation |
| Regression Results | `rma()` table (β, SE, z, p) | Significant β (p < 0.05) ⇒ moderator effect |
| Bubble Plot | Point size ∝ precision | Visual weight of studies along moderator axis |
| Residual Plot | Residuals vs. fitted | Random scatter ↔ good; patterns ↔ model mis-fit |

*"Peak"* – no longer used; previous placeholder removed. Follow standard meta-regression best practices from **metafor**.

---

## 4. Plot & Result Explanations
Each plot includes an info box below it (grey box with blue border) detailing:
* **What it shows**
* **How to interpret**
* **Why it matters**

These texts were vetted to ensure black font for readability in dark-mode UI ( `.plot-explanation { color:#000; }` ).

---

## 5. Removed Features
* **Hazard Ratio** and **Survival Analysis** options have been **temporarily disabled** until full validation is completed.

---

## 6. Feature Overview (Cheat-Sheet)

| Feature | Location | Key Outputs |
|---------|----------|-------------|
| Random Effects | "Random Effects" → tabs | Forest + τ²/I², diagnostics, bias, sensitivity, subgroups |
| Fixed Effects  | "Fixed Effects" → tabs  | Forest + Q, diagnostics, etc. |
| Bivariate Approach | "Bivariate Approach" → tabs | 2-D μ/τ confidence ellipses, etc. |
| Subgroup Analysis | Inside each method | Group-specific pooled effects, Q-between |
| Meta-Regression | Separate panel | Regression coefficients, plots, diagnostics |

Happy meta-analyzing! 🎉 