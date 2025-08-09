# 🚀 **Modern Meta-Analysis**  
### Instant Evidence Synthesis for Busy Clinicians, Researchers & Data-Nerds  

> *Because clinical decisions shouldn’t wait for messy spreadsheets or cryptic R scripts.*

---

## 🌐 Try It Right Now – Zero Install

**Live demo:** <https://modern-meta-analysis.shinyapps.io/meta-app/>  
*(Runs entirely in your browser ­– no R, no setup, no headaches.)*

---

## 🤔 Why the Joint-Confidence-Region (JCR) Method Matters

Classic meta-analysis tells you *the average* effect.  
The **JCR bivariate model** (Saad *et al.*, 2019) answers the question clinicians actually ask:

> **“What’s the probability that the true effect beats my clinical threshold?”**

It models the pooled effect ( \(\mu\) ) **and** between-study heterogeneity ( \(\tau^2\) ) *jointly*, then turns that into:

* **Probability statements** – “There’s an **88 %** chance the risk ratio is ≤ 0.80.”  
* **Confidence ellipses** – benefit & harm in one glance.  
* **Efficiency-Hump plots** – a CDF curve your brain understands.

Result ➜ move from *p-values* to *probabilistic thinking*.

---

## 🧭 Feature Tour (Plain-Language Version)

| Tab | What You Get | Core Engine |
|-----|--------------|-------------|
| **Data Preview** | Drag-&-drop CSV/Excel, instant validation & missing-data flags. | `readxl`, `DT` |
| **Fixed / Random Effects** | DerSimonian-Laird & inverse-variance pooling; forest & funnel plots; Q, I², τ². | `meta`, `metafor` |
| **Subgroup & Sensitivity** | One-click subgroups, leave-one-out, Cook’s D, Baujat plot, toggle outliers. | Handbook 2021 ch 11 |
| **Diagnostics & Bias** | Residual & BLUP QQ, Egger test, trim-&-fill. | Handbook 2021 |
| **Bivariate (JCR)** | Saad MLE, 95 % joint ellipse, probability table for any threshold, efficiency-hump plot. | Custom open-source code |
| **Meta-Regression** | Moderator slopes with permutation p-values; bubble plot + ribbon. | `metafor` (REML) |
| **One-click Report** | PDF/HTML with every plot + session info (reproducible). | R Markdown |

*Dark-mode included because 2 AM happens.*

---

## 📚 Statistical Roots

1. **Saad *et al.* (2019)** – JCR & predictive probabilities.  
2. **Schmid & White, _Handbook of Meta-Analysis_ (2021)** – diagnostics, bias, GRADE cues.  
3. **Borenstein *et al.* (2009)** – FE vs RE, heterogeneity math.  
4. **Cochrane Handbook** – subgroups, risk-of-bias.

Core maths handled by vetted R packages (`meta`, `metafor`); the fancy JCR bits live in `bivariate_meta.R` – 100 % open-source & unit-tested.

---

## ☕ Five-Step Workflow

1. **Upload Data**  
   * Choose data type:  
     * **Binary** – two arms per study (events & totals).  
     * **Continuous** – mean, SD, sample N per arm.  
   * Example templates available in the app.

2. **Select Effect Size**  
   * For binary: pick **Risk Ratio (RR)** or **Odds Ratio (OR)**.  
   * For continuous: pick **Mean Difference (MD)** or **SMD**.

3. **Click “Analyze”** – the app runs fixed- & random-effects models and computes heterogeneity.

4. **Explore Results**  
   * **Bivariate tab** for joint ellipse & probability table.  
   * **Diagnostics tab** to spot outliers or bias.

5. **Download Report** – a polished PDF/HTML with every plot & statistic.

---

## 💻 Open-Source & Hungry for Contributors

MIT-licensed on GitHub – fork it, break it, make it better.  
We’re wrapping the JCR engine into a standalone R package (**`jcrmeta`**) for CRAN release soon.

| You are… | How You Can Help |
|----------|------------------|
| **Clinician** | Test with real trials – do the outputs answer your clinical questions? |
| **Statistician** | Audit the maths · propose new estimators · debate DL vs REML. |
| **Developer** | Grab a *help-wanted* issue: prediction intervals, fail-safe N, GOSH plots… |
| **Any Human** | ⭐ Star the repo, file bugs, request features, or just say hi! |

---

## 📣 Call to Action

Love it? **Try it now** and tell us what would make it indispensable for *your* workflow.  

* Open an issue on GitHub: <https://github.com/davidgrab/meta-app/issues>  
* Or drop us a line: [davidgrabois@mail.tau.ac.il](mailto:davidgrabois@mail.tau.ac.il)  

Your feedback directly shapes the next release!

---

## 🔮 Roadmap Highlights

- **Network meta-analysis** (league tables & ranks)  
- **Bayesian mode** via Stan (posterior probabilities)  
- **GRADE wizard** that nudges downgrades when I² explodes  
- **Localization** – ¡Hola! 你好! Salut!

Vote for what ships first.

---

## 📬 Stay in the Loop

* **GitHub:** <https://github.com/davidgrab/meta-app>  
* **Email:** [davidgrabois@mail.tau.ac.il](mailto:davidgrabois@mail.tau.ac.il)  
* **X / Twitter:** **@ModernMetaApp**

> Evidence synthesis should be **transparent**, **reproducible**, and **fast**.  
> Let’s build that future – together.

<br>

*© 2025 David Grabois – MIT License*