library(shiny)
library(DT)
library(meta)
library(ggplot2)
library(plotly)
library(metafor)
library(BiasedUrn)
library(bslib)
library(shinyjs)
library(bsicons)
library(shinycssloaders)


# Source the functions file
source("R/functions.R")

# Define the custom theme for light mode
light_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#333333",
  primary = "#34495E",  # Dark blue color for the sidebar
  secondary = "#ECF0F1", # Light gray for backgrounds
  base_font = "Arial, sans-serif",
  heading_font = "Arial, sans-serif",
  font_scale = 0.9
)

# Define dark theme (keeping your previous dark theme)
dark_theme <- bs_theme(
  version = 5,
  bg = "#212529",
  fg = "#ffffff",
  primary = "#0062cc",
  secondary = "#6c757d",
  success = "#28a745",
  info = "#17a2b8",
  warning = "#ffc107",
  danger = "#dc3545",
  base_font = "Arial, sans-serif",
  heading_font = "Arial, sans-serif",
  font_scale = 0.9
)

ui <- page_fillable(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  
  fluidRow(
    column(8, h3("Comprehensive Meta-Analysis App", style = "margin-top: 5px;")),
    column(4, 
           style = "text-align: right; margin-top: 5px;",
           actionButton("app_info", "App Info", icon = icon("info-circle"), style = "margin-right: 10px;"),
           input_dark_mode(id = "dark_mode", mode = "light", style="font-size: 0.8em; padding: 0; width: 20px; height: 20px; border-radius: 50%;")
    )
  ),
  hr(),
  
  layout_sidebar(
    sidebar = sidebar(
      radioButtons("data_type", "Select Data Type:",
                   choices = list("Binary (2x2)" = "binary", 
                                  "Continuous (SMD)" = "smd"),
                   selected = "binary"),
      fileInput("datafile", "Upload Data", accept = c(".csv", ".xlsx")),
      selectInput("het_estimator", "Heterogeneity Estimator", choices = c("DL", "PM", "REML", "ML"), selected = "DL"),
      conditionalPanel(
        condition = "input.data_type == 'binary'",
        selectInput("effect_measure", "Effect Measure", choices = c("OR", "RR"), selected = "RR")
      ),
      actionButton("analyze", "Analyze", class = "btn-primary"),
      hr(),
      h4("Data Cleaning"),
      checkboxInput("remove_na", "Remove rows with NA values", value = TRUE),
      hr(),
      conditionalPanel(
        condition = "output.analysisReady == true",
      actionButton("prepareReport", "Download Report", icon = icon("file-pdf"))
      )
    ),
    
    navset_card_tab(
      id = "main_tabs",
      nav_panel("Data Preview", 
                actionButton("data_info", "How to Upload Data", icon = icon("question-circle")),
                fluidRow(
                  column(6, downloadButton("downloadSampleStructure", "Download Sample Structure")),
                  column(6, 
                         div(
                           div(style = "display: flex; align-items: center;",
                             selectInput("exampleDatasetChoice", "Choose Example Dataset:", 
                                        choices = list(
                                          "Hypericum (St. John's Wort) - Depression (Default)" = "default",
                                          "Colditz et al. (1994) - BCG Vaccine" = "colditz",
                                          "Yusuf et al. (1985) - Beta-Blockers" = "yusuf",
                                          "CBT for Depression (SMD)" = "smd"
                                        ), 
                                        selected = "default"),
                             actionButton("dataset_info", "", icon = icon("info-circle"), 
                                          style = "margin-left: 5px; height: 30px;")
                           ),
                           uiOutput("datasetDescription"),
                           actionButton("loadExampleData", "Load Example Dataset"),
                           style = "display: flex; flex-direction: column;"
                         )
                  )
                ),
                br(),
                DTOutput("dataPreview"),
                helpText("This tab displays the uploaded data. Review your data here to ensure it has been correctly loaded and formatted.")
      ),
      # nav_panel("Overall Results",
      #           tabsetPanel(
      #             tabPanel("Method Comparison", 
      #                      actionButton("method_comparison_info", "", icon = icon("info-circle"), class = "help-text"),
      #                      withSpinner(plotOutput("methodComparisonPlot")),
      #                      p("Method comparison plot: Compares effect size estimates and confidence intervals from fixed effects, random effects, and bivariate meta-analysis models. Shows relative precision of each method.", class = "plot-explanation"),
      #                      hr(),
      #                      h4("Summary"),
      #                      verbatimTextOutput("methodComparisonSummary")),
      #             tabPanel("Summary Table", 
      #                      actionButton("summary_table_info", "", icon = icon("info-circle"), class = "help-text"),
      #                      tableOutput("overallSummaryTable")),
      #             # tabPanel("Overall Interpretation", 
      #             #          actionButton("overall_interpretation_info", "", icon = icon("info-circle"), class = "help-text"),
      #             #          verbatimTextOutput("overallInterpretation"))
      #           )
      # ),
      nav_panel("Random Effects Analysis",
                tabsetPanel(
                  # tabPanel("Effect Size and Heterogeneity",
                  #          actionButton("re_effect_size_heterogeneity_info", "", icon = icon("info-circle"), class = "help-text"),
                  #          fluidRow(
                  #            column(6,
                  #                   div(class = "plot-container",
                  #                       plotOutput("randomForestPlot"),
                  #                       p("Forest plot: Shows individual study effects and overall effect size with confidence intervals.", class = "plot-explanation")
                  #                   )
                  #            ),
                  #            column(6,
                  #                   div(class = "plot-container",
                  #                       plotOutput("randomHeterogeneityPlot"),
                  #                       p("Heterogeneity plot: Visualizes the extent of heterogeneity among studies.", class = "plot-explanation")
                  #                   )
                  #            )
                  #          ),
                  #          verbatimTextOutput("randomOverallSummary"),
                  #          verbatimTextOutput("randomHeterogeneitySummary")
                  # ),
                  tabPanel("Effect Size and Heterogeneity", 
                           div(class = "plot-container",
                               style = "max-height: 600px; overflow-y: auto;",
                               withSpinner(plotOutput("randomForestPlot"))
                           ),
                           p(HTML("<strong>What it is:</strong> A forest plot visualizes the effect sizes and confidence intervals for each individual study, along with the overall pooled estimate from the random-effects model. The size of the square for each study is proportional to its weight in the analysis.<br>
                                      <strong>How to interpret:</strong> Look for the overall effect estimate (the diamond) and its confidence interval to understand the main finding. If the diamond does not cross the line of no effect (e.g., 1 for OR/RR), the result is statistically significant. The spread of the individual studies shows the extent of heterogeneity."), class = "plot-explanation"),
                           actionButton("re_effect_size_heterogeneity_info", "", icon = icon("info-circle"), class = "help-text"),
                           verbatimTextOutput("randomOverallSummary"),
                           verbatimTextOutput("randomHeterogeneitySummary")
                  ),
                  tabPanel("Subgroup Analysis",
                           fluidRow(
                             column(4,
                                    h4("Subgroup Configuration"),
                                    conditionalPanel(
                                      condition = "output.hasSubgroupData",
                                      selectInput("random_subgroup_variable", "Select Subgroup Variable:",
                                                 choices = NULL),
                                      br(),
                                      actionButton("run_random_subgroup", "Run Subgroup Analysis", 
                                                  class = "btn-primary", icon = icon("play")),
                                      br(), br(),
                                      helpText("Subgroup analysis using the random effects model will compare effect sizes between different subgroups and test for subgroup differences.")
                                    ),
                                    conditionalPanel(
                                      condition = "!output.hasSubgroupData",
                                      div(class = "alert alert-info",
                                          HTML("<strong>No subgroup data available.</strong><br>
                                               Upload data with categorical variables to perform subgroup analysis."))
                                    )
                             ),
                             column(8,
                                    conditionalPanel(
                                      condition = "input.run_random_subgroup > 0",
                                      div(class = "plot-container",
                                          style = "max-height: 700px; overflow-y: auto;",
                                          withSpinner(plotOutput("randomSubgroupForestPlot"))
                                      ),
                                      p(HTML("<strong>Random Effects Subgroup Analysis:</strong> Forest plot showing studies grouped by the selected variable, with random effects pooled estimates for each subgroup."), class = "plot-explanation"),
                                      br(),
                                      h4("Subgroup Test Results"),
                                      verbatimTextOutput("randomSubgroupTest")
                                    ),
                                    conditionalPanel(
                                      condition = "input.run_random_subgroup == 0 && output.hasSubgroupData",
                                      div(class = "alert alert-secondary text-center",
                                          style = "margin-top: 50px; padding: 30px;",
                                          h4("Ready for Random Effects Subgroup Analysis"),
                                          p("Select a subgroup variable and click 'Run Subgroup Analysis' to begin."))
                                    )
                             )
                           )
                  ),
                  tabPanel("Model Diagnostics",
                           actionButton("re_model_diagnostics_info", "", icon = icon("info-circle"), class = "help-text"),
                           h4("Normality Assessment"),
                           p("These plots assess whether the random effects model assumptions are met:", class = "section-explanation"),
                           
                           # Side-by-side deleted residuals Q-Q plots (NEW - moved up)
                           h5("Deleted Residuals Comparison"),
                           div(class = "plot-container", style = "width: 100%;",
                               withSpinner(plotOutput("randomDeletedResidualsComparisonPlot", height = "400px")),
                               p(HTML("<strong>Side-by-Side Q-Q Plots of Deleted Residuals:</strong><br>
                                      <em>Left panel:</em> Fixed Effects deleted residuals - each point represents the residual when that study is removed and the fixed effect is recalculated.<br>
                                      <em>Right panel:</em> Random Effects deleted residuals - each point represents the residual when that study is removed and the random effects model is refitted.<br>
                                      <br><em>Interpretation:</em> Compare tail behavior between models. If points deviate similarly from the diagonal in both panels, 
                                      it suggests the deviations are due to the data rather than the choice of model. Different patterns suggest model-specific issues."), 
                                 class = "plot-explanation")
                           ),
                           
                           # BLUPs Q-Q plot (moved down)
                           h5("Best Linear Unbiased Predictors (BLUPs)"),
                           div(class = "plot-container",
                               withSpinner(plotOutput("randomQQPlot")),
                               p(HTML("<strong>Q-Q Plot of BLUPs:</strong><br>
                                      This plot tests whether the study-specific effect estimates follow a normal distribution under the random effects model. 
                                      Points should lie along the red diagonal line if normality holds. The gray envelope shows 95% confidence bands.
                                      <br><em>Interpretation:</em> S-shaped curves indicate tail deviations, systematic curvature suggests skewness, 
                                      and points outside the envelope may indicate outliers or model misspecification."), class = "plot-explanation")
                           ),
                           
                           h4("Formal Statistical Tests"),
                           verbatimTextOutput("randomNormalityTestSummary"),
                           h4("Additional Diagnostics"),
                           fluidRow(
                             column(6, 
                                    div(class = "plot-container",
                                        withSpinner(plotOutput("outlierDetectionPlot")),
                                        p(HTML("<strong>Outlier Detection Plot:</strong><br>
                                               Displays standardized residuals for each study with reference lines at ±1.96. 
                                               Studies outside these bounds may be outliers affecting the meta-analysis.
                                               <br><em>Interpretation:</em> Values beyond ±1.96 suggest potential outliers that may need investigation."), class = "plot-explanation")
                                    )
                             ),
                             column(6, 
                                    div(class = "plot-container",
                                        withSpinner(plotOutput("effectDistributionPlot")),
                                        p(HTML("<strong>Effect Distribution Plot:</strong><br>
                                               Histogram showing the distribution of effect sizes across studies with the pooled effect size (red dashed line).
                                               <br><em>Interpretation:</em> Shows the spread of individual study effects around the overall estimate. 
                                               Bimodal or highly skewed distributions may indicate subgroup effects."), class = "plot-explanation")
                                    )
                             )
                           )
                  ),
                  tabPanel("Publication Bias",
                           actionButton("publication_bias_info", "", icon = icon("info-circle"), class = "help-text"),
                           fluidRow(
                             column(6, 
                                    div(class = "plot-container",
                                        withSpinner(plotOutput("randomFunnelPlot")),
                                        p(HTML("<strong>What it is:</strong> A funnel plot is a scatterplot of treatment effect against a measure of study precision. It is used primarily as a visual aid to detecting publication bias.<br>
                                                  <strong>How to interpret:</strong> In the absence of bias, the plot should resemble a symmetrical inverted funnel. An asymmetrical funnel suggests that studies with certain results (usually small, non-significant ones) may be missing from the analysis, which indicates potential publication bias."), class = "plot-explanation")
                                    )
                             ),
                             column(6, 
                                    div(class = "plot-container",
                                        withSpinner(plotOutput("randomTrimFillPlot")),
                                        p(HTML("<strong>What it is:</strong> The trim and fill method is a non-parametric method for estimating the number of missing studies in a meta-analysis and adjusting the overall estimate for their absence. Missing studies are shown as open circles.<br>
                                                  <strong>How to interpret:</strong> This plot shows the original studies (solid circles) and the imputed missing studies (open circles). The adjusted overall estimate (the new diamond) shows how the result might change if the suspected missing studies were included. A large difference between the original and adjusted estimates suggests that publication bias may be affecting the results."), class = "plot-explanation")
                                    )
                             )
                           ),
                           verbatimTextOutput("randomEggerTestResults")
                  ),
                  tabPanel("Sensitivity Analysis",
                           actionButton("sensitivity_analysis_info", "", icon = icon("info-circle"), class = "help-text"),
                           
                           # Leave-One-Out Plot (full width, scrollable)
                           div(class = "plot-container",
                               style = "max-height: 600px; overflow-y: auto;",
                               withSpinner(plotOutput("leaveOneOutPlot"))
                           ),
                           p(HTML("<strong>What it is:</strong> A leave-one-out analysis recalculates the pooled effect estimate by removing one study at a time. This plot shows how the overall estimate changes when each study is omitted.<br>
                                      <strong>How to interpret:</strong> Look for studies whose removal causes a large shift in the overall estimate or its confidence interval. If the overall conclusion changes when a particular study is removed, the results are sensitive to that study, which may warrant further investigation."), class = "plot-explanation"),

                           # Baujat Plot (full width)
                           div(class = "plot-container",
                               withSpinner(plotOutput("baujatPlot"))
                           ),
                           p(HTML("<strong>What it is:</strong> A Baujat plot helps to identify studies that are influential in the meta-analysis. It plots the contribution of each study to the overall heterogeneity statistic (Q) against its influence on the pooled effect estimate.<br>
                                      <strong>How to interpret:</strong> Studies in the top-right quadrant are the most influential, as they contribute highly to both heterogeneity and the overall result. These studies are candidates for further investigation as potential sources of heterogeneity or as outliers."), class = "plot-explanation"),

                           div(class = "plot-container",
                               withSpinner(plotOutput("randomGOSHPlot")),
                               p(HTML("<strong>What it is:</strong> A GOSH (Graphical Display of Heterogeneity) plot analyzes the distribution of heterogeneity by fitting the meta-analysis model to all possible subsets of studies. It plots the overall effect size against a heterogeneity measure (like I-squared) for each subset.<br>
                                      <strong>How to interpret:</strong> A unimodal, symmetrical distribution suggests a homogeneous set of studies. Multiple clusters or a scattered plot indicate that certain subsets of studies have very different effects or levels of heterogeneity, suggesting that a single pooled estimate may not be appropriate for all studies."), class = "plot-explanation")
                           ),
                           verbatimTextOutput("influenceSummary")
                  ),
                  # tabPanel("Quality Assessment",
                  #          actionButton("quality_assessment_info", "", icon = icon("info-circle"), class = "help-text"),
                  #          verbatimTextOutput("randomGradeAssessment")
                  # )
                )
      ),
      nav_panel("Fixed Effects Analysis",
                tabsetPanel(
                  tabPanel("Effect Size and Heterogeneity", 
                           div(class = "plot-container",
                               style = "max-height: 600px; overflow-y: auto;",
                               withSpinner(plotOutput("fixedForestPlot"))
                           ),
                           p(HTML("<strong>What it is:</strong> This forest plot visualizes the effect sizes from individual studies under the fixed-effect model, which assumes all studies share a single, common true effect. The pooled estimate is represented by the diamond.<br>
                                      <strong>How to interpret:</strong> The key assumption is that any differences between studies are due to chance alone. If the confidence intervals of the studies overlap significantly and the heterogeneity test (Q-statistic) is not significant, the fixed-effect model may be appropriate. The diamond represents the best estimate of the common true effect."), class = "plot-explanation"),
                           actionButton("fe_effect_size_heterogeneity_info", "", icon = icon("info-circle"), class = "help-text"),
                           verbatimTextOutput("fixedOverallSummary"),
                           verbatimTextOutput("modelFitStatistics")
                  ),
                  tabPanel("Subgroup Analysis",
                           fluidRow(
                             column(4,
                                    h4("Subgroup Configuration"),
                                    conditionalPanel(
                                      condition = "output.hasSubgroupData",
                                      selectInput("fixed_subgroup_variable", "Select Subgroup Variable:",
                                                 choices = NULL),
                                      br(),
                                      actionButton("run_fixed_subgroup", "Run Subgroup Analysis", 
                                                  class = "btn-primary", icon = icon("play")),
                                      br(), br(),
                                      helpText("Subgroup analysis using the fixed effects model assumes a common effect within each subgroup and tests for differences between subgroups.")
                                    ),
                                    conditionalPanel(
                                      condition = "!output.hasSubgroupData",
                                      div(class = "alert alert-info",
                                          HTML("<strong>No subgroup data available.</strong><br>
                                               Upload data with categorical variables to perform subgroup analysis."))
                                    )
                             ),
                             column(8,
                                    conditionalPanel(
                                      condition = "input.run_fixed_subgroup > 0",
                                      div(class = "plot-container",
                                          style = "max-height: 700px; overflow-y: auto;",
                                          withSpinner(plotOutput("fixedSubgroupForestPlot"))
                                      ),
                                      p(HTML("<strong>Fixed Effects Subgroup Analysis:</strong> Forest plot showing studies grouped by the selected variable, with fixed effects pooled estimates for each subgroup."), class = "plot-explanation"),
                                      br(),
                                      h4("Subgroup Test Results"),
                                      verbatimTextOutput("fixedSubgroupTest")
                                    ),
                                    conditionalPanel(
                                      condition = "input.run_fixed_subgroup == 0 && output.hasSubgroupData",
                                      div(class = "alert alert-secondary text-center",
                                          style = "margin-top: 50px; padding: 30px;",
                                          h4("Ready for Fixed Effects Subgroup Analysis"),
                                          p("Select a subgroup variable and click 'Run Subgroup Analysis' to begin."))
                                    )
                             )
                           )
                  ),
                  tabPanel("Model Diagnostics",
                           actionButton("fe_model_diagnostics_info", "", icon = icon("info-circle"), class = "help-text"),
                           h4("Normality Assessment"),
                           p("This plot assesses whether the fixed effects model assumptions are met:", class = "section-explanation"),
                           div(class = "plot-container",
                               withSpinner(plotOutput("fixedQQPlot")),
                               p(HTML("<strong>Q-Q Plot of Standardized Residuals:</strong><br>
                                      This plot tests whether the residuals r<sub>i</sub> = (Y<sub>i</sub> - θ̂)/σ<sub>i</sub> follow a standard normal distribution. 
                                      Points should lie along the red diagonal line if the fixed effects normality assumption holds. The gray envelope shows 95% confidence bands.
                                      <br><em>Interpretation:</em> If points systematically deviate from the line, this suggests the sampling errors 
                                      do not follow the assumed normal distribution, which may invalidate the fixed effects model."), class = "plot-explanation")
                           ),
                           h4("Formal Statistical Test"),
                           verbatimTextOutput("fixedNormalityTestSummary"),
                           h4("Model Fit and Outlier Detection"),
                           fluidRow(
                             column(6, 
                                    div(class = "plot-container",
                                        withSpinner(plotOutput("fixedModelFitPlot")),
                                        p(HTML("<strong>Radial Plot:</strong><br>
                                               Visualizes the fit of the fixed effects model. Points closer to the central line indicate better fit to the model.
                                               <br><em>Interpretation:</em> Studies far from the line contribute more to heterogeneity and may not fit 
                                               the fixed effects assumption of a common true effect size."), class = "plot-explanation")
                                    )
                             ),
                             column(6, 
                                    div(class = "plot-container",
                                        withSpinner(plotOutput("fixedOutlierDetectionPlot")),
                                        p(HTML("<strong>Outlier Detection:</strong><br>
                                               Identifies potential outliers based on standardized residuals. Studies with large residuals may be outliers.
                                               <br><em>Interpretation:</em> Outliers may indicate studies that don't follow the fixed effects assumption 
                                               or have measurement errors that should be investigated."), class = "plot-explanation")
                                    )
                             )
                           )
                  ),
                  tabPanel("Publication Bias",
                           actionButton("publication_bias_info", "", icon = icon("info-circle"), class = "help-text"),
                           fluidRow(
                             column(6, 
                                    div(class = "plot-container",
                                        withSpinner(plotOutput("fixedFunnelPlot")),
                                        p(HTML("<strong>What it is:</strong> A funnel plot under a fixed-effect model plots the study effect sizes against their standard errors. It's a visual tool to check for publication bias.<br>
                                                  <strong>How to interpret:</strong> For a fixed-effect model, the plot should be symmetrical around the pooled effect estimate (the vertical line). Asymmetry, particularly a gap in the bottom-left or bottom-right of the funnel, suggests that small, non-significant studies may be missing, which could indicate publication bias."), class = "plot-explanation")
                                    )
                             ),
                             column(6, 
                                    div(class = "plot-container",
                                        withSpinner(plotOutput("fixedTrimFillPlot")),
                                        p(HTML("<strong>What it is:</strong> This plot applies the trim and fill method to the fixed-effect model to estimate and adjust for potentially missing studies due to publication bias.<br>
                                                  <strong>How to interpret:</strong> The plot shows the original studies (solid circles) and imputes missing ones (open circles) to create a symmetrical funnel. The adjusted overall estimate (the new diamond) shows how the result might change if publication bias were accounted for. A significant difference between the original and adjusted estimates suggests that the fixed-effect result is sensitive to publication bias."), class = "plot-explanation")
                                    )
                             )
                           ),
                           verbatimTextOutput("fixedEggerTestResults")
                  ),
                  tabPanel("Sensitivity Analysis",
                           actionButton("sensitivity_analysis_info", "", icon = icon("info-circle"), class = "help-text"),

                           # Leave-One-Out Plot (full width, scrollable)
                           div(class = "plot-container",
                               style = "max-height: 600px; overflow-y: auto;",
                               withSpinner(plotOutput("fixedLeaveOneOutPlot"))
                           ),
                           p(HTML("<strong>What it is:</strong> This plot shows how the overall fixed-effect estimate changes when each study is sequentially removed from the meta-analysis.<br>
                                      <strong>How to interpret:</strong> This analysis assesses the robustness of the results. If removing a single study drastically changes the overall estimate or its significance, the findings are considered sensitive to that study. Such influential studies should be examined more closely."), class = "plot-explanation"),

                           # Influence Plot (Baujat) (full width)
                           div(class = "plot-container",
                               withSpinner(plotOutput("fixedInfluencePlot"))
                           ),
                           p(HTML("<strong>What it is:</strong> This is a Baujat plot adapted for a fixed-effect model. It identifies influential studies by plotting each study's contribution to the overall heterogeneity statistic (Q) against its influence on the pooled effect estimate.<br>
                                      <strong>How to interpret:</strong> Since the fixed-effect model assumes no heterogeneity, any study contributing significantly to the Q statistic (x-axis) is a potential outlier or violates the model assumptions. Studies with high influence (y-axis) are those that disproportionately affect the pooled estimate. Studies in the top-right are the most influential overall."), class = "plot-explanation"),

                           verbatimTextOutput("fixedInfluenceSummary")
                  ),
                  # tabPanel("Quality Assessment",
                  #          actionButton("quality_assessment_info", "", icon = icon("info-circle"), class = "help-text"),
                  #          verbatimTextOutput("fixedGradeAssessment")
                  # )
                )
      ),
      nav_panel("Bivariate Approach",
                tabsetPanel(
                  tabPanel("Effect Size and Heterogeneity", 
                           actionButton("biv_effect_size_heterogeneity_info", "", icon = icon("info-circle")),
                           
                           # Bivariate Forest Plot (full width)
                           div(class = "plot-container",
                               style = "max-height: 600px; overflow-y: auto;",
                               withSpinner(plotOutput("bivariateForestPlot"))
                           ),
                           p(HTML("<strong>What it is:</strong> This forest plot displays the results from the bivariate random-effects model. It shows the individual study effect sizes and the overall pooled estimate (diamond) calculated using a more advanced statistical method that jointly estimates the overall effect (μ) and the between-study heterogeneity (τ).<br>
                                      <strong>How to interpret:</strong> The bivariate model often provides a more precise and reliable estimate than standard random-effects models, especially with sparse data. The interpretation is similar to a standard forest plot, but the underlying model is more statistically robust."), class = "plot-explanation"),
                           
                           # Confidence Region Plot (full width)
                           div(class = "plot-container",
                               withSpinner(plotOutput("confidenceRegionPlot"))
                           ),
                           p(HTML("<strong>What it is:</strong> This plot shows the joint confidence region for the two main parameters of the bivariate model: the overall effect size (μ) and the between-study heterogeneity (τ). The different colored regions represent the 50%, 90%, 95%, and 99% confidence levels.<br>
                                      <strong>How to interpret:</strong> The plot illustrates the uncertainty in the estimates of μ and τ simultaneously. A wide region indicates greater uncertainty. The shape of the region shows the correlation between the estimates of the two parameters. The maximum likelihood estimates for both are marked with a cross."), class = "plot-explanation"),
                           
                           div(class = "plot-container",
                               withSpinner(plotOutput("efficacyHarmPlot")),
                               p(HTML("<strong>What it is:</strong> The Efficacy-Harm plot shows the probability of observing a true effect size that is more extreme than a certain threshold. It plots the cumulative distribution function (CDF) for the treatment effect, showing probabilities for both benefit (efficacy) and harm.<br>
                                          <strong>How to interpret:</strong> This plot helps in making clinical decisions. For example, you can use it to determine the probability that the true effect is greater than a minimal clinically important difference, or the probability that it falls into a range considered harmful. The steepness of the curve indicates the level of certainty."), class = "plot-explanation"),
                               
                               # Custom thresholds input
                               br(),
                               div(style = "margin: 15px 0;",
                                   textInput("custom_thresholds", 
                                            label = "Custom Thresholds (comma-separated):", 
                                            value = "", 
                                            placeholder = "e.g., 0.3, 0.7, 1.0",
                                            width = "300px"),
                                   helpText("Optional: Add custom threshold values for probability calculations")
                               ),
                               
                               # Probability table
                               h5("Probability Table for Key Clinical Thresholds"),
                               tableOutput("efficacyHarmProbabilityTable"),
                               p(HTML("<strong>What it shows:</strong> This table displays the exact same data as the Efficacy/Harm plot above. For each threshold value T, it shows the probability P(θ ≥ T) that the true effect size is greater than or equal to that threshold, along with 95% confidence intervals.<br>
                                          <strong>How to use:</strong> These are the numerical values underlying the Efficacy/Harm plot. You can add custom threshold values above to see their exact probabilities from the curve."), class = "plot-explanation")
                           ),
                           verbatimTextOutput("bivariateOverallSummary")
                  ),
                  tabPanel("Subgroup Analysis",
                           fluidRow(
                             column(4,
                                    h4("Subgroup Configuration"),
                                    conditionalPanel(
                                      condition = "output.hasSubgroupData",
                                      selectInput("bivariate_subgroup_variable", "Select Subgroup Variable:",
                                                 choices = NULL),
                                      br(),
                                      actionButton("run_bivariate_subgroup", "Run Subgroup Analysis", 
                                                  class = "btn-primary", icon = icon("play")),
                                      br(), br(),
                                      helpText("Subgroup analysis using the bivariate approach performs separate bivariate meta-analyses for each subgroup, providing joint estimation of effect and heterogeneity parameters.")
                                    ),
                                    conditionalPanel(
                                      condition = "!output.hasSubgroupData",
                                      div(class = "alert alert-info",
                                          HTML("<strong>No subgroup data available.</strong><br>
                                               Upload data with categorical variables to perform subgroup analysis."))
                                    )
                             ),
                             column(8,
                                    conditionalPanel(
                                      condition = "input.run_bivariate_subgroup > 0",
                                      div(class = "plot-container",
                                          style = "max-height: 700px; overflow-y: auto;",
                                          withSpinner(plotOutput("bivariateSubgroupForestPlot"))
                                      ),
                                      p(HTML("<strong>Bivariate Subgroup Analysis:</strong> Forest plot showing studies grouped by the selected variable, with bivariate MLE estimates for each subgroup."), class = "plot-explanation"),
                                      br(),
                                      h4("Subgroup Comparison"),
                                      verbatimTextOutput("bivariateSubgroupTest")
                                    ),
                                    conditionalPanel(
                                      condition = "input.run_bivariate_subgroup == 0 && output.hasSubgroupData",
                                      div(class = "alert alert-secondary text-center",
                                          style = "margin-top: 50px; padding: 30px;",
                                          h4("Ready for Bivariate Subgroup Analysis"),
                                          p("Select a subgroup variable and click 'Run Subgroup Analysis' to begin."))
                                    )
                             )
                           )
                  ),
                  tabPanel("Model Diagnostics",
                           actionButton("biv_model_diagnostics_info", "", icon = icon("info-circle")),
                           h4("Normality Assessment"),
                           p("These plots assess whether the bivariate meta-analysis model assumptions are met using joint MLE estimation:", class = "section-explanation"),
                           
                           # Side-by-side deleted residuals Q-Q plots (NEW - moved up)
                           h5("Deleted Residuals Comparison"),
                           div(class = "plot-container", style = "width: 100%;",
                               withSpinner(plotOutput("bivariateDeletedResidualsComparisonPlot", height = "400px")),
                               p(HTML("<strong>Side-by-Side Q-Q Plots of Deleted Residuals:</strong><br>
                                      <em>Left panel:</em> Bivariate MLE deleted residuals - each point represents the residual when that study is removed and the bivariate model is refitted.<br>
                                      <em>Right panel:</em> Fixed Effects deleted residuals - each point represents the residual when that study is removed and the fixed effect is recalculated.<br>
                                      <br><em>Interpretation:</em> Compare tail behavior between the bivariate MLE and fixed effects approaches. The bivariate model often shows better 
                                      behavior in the tails due to its more flexible modeling of heterogeneity. Different patterns suggest model-specific strengths and weaknesses."), 
                                 class = "plot-explanation")
                           ),
                           
                           # BLUPs Q-Q plot (moved down)
                           h5("Best Linear Unbiased Predictors (BLUPs)"),
                           div(class = "plot-container",
                               withSpinner(plotOutput("qqPlotMu")),
                               p(HTML("<strong>Q-Q Plot of BLUPs (Bivariate MLE):</strong><br>
                                      This plot tests normality of Best Linear Unbiased Predictors using jointly estimated μ̂<sub>MLE</sub> and τ̂<sub>MLE</sub> parameters. 
                                      Points should follow the diagonal line if the random effects distribution is normal. The simulation envelope provides robust assessment.
                                      <br><em>Interpretation:</em> The bivariate approach provides more precise estimates than standard random effects. 
                                      Deviations suggest the random effects may not follow the assumed normal distribution."), class = "plot-explanation")
                           ),
                           
                           h4("Formal Statistical Tests"),
                           verbatimTextOutput("bivariateNormalityTestSummary")
                  ),
                  tabPanel("Publication Bias",
                           actionButton("publication_bias_info", "", icon = icon("info-circle"), class = "help-text"),
                           div(class = "plot-container",
                               withSpinner(plotOutput("bivariateAdaptedFunnelPlot")),
                               p(HTML("<strong>What it is:</strong> This is a funnel plot adapted for the results of the bivariate meta-analysis. It plots the study-specific effect sizes (from the bivariate model) against their standard errors.<br>
                                          <strong>How to interpret:</strong> Similar to a standard funnel plot, it should be symmetrical in the absence of publication bias. Asymmetry may suggest that small studies with non-significant results are missing. Because the bivariate model provides more precise standard errors, this plot can sometimes provide a clearer picture of potential bias than a standard funnel plot."), class = "plot-explanation")
                           ),
                           # verbatimTextOutput("bivariateBiasTestResults")
                  ),
                  tabPanel("Sensitivity Analysis",
                           actionButton("sensitivity_analysis_info", "", icon = icon("info-circle")),
                           fluidRow(
                             column(6, 
                                    div(class = "plot-container",
                                        withSpinner(plotlyOutput("confidenceRegionShiftPlot")),
                                        p(HTML("<strong>What it is:</strong> This plot shows how the joint confidence region of the overall effect (μ) and heterogeneity (τ) shifts when each study is removed one at a time. The solid black line is the confidence region from the full dataset.<br>
                                                  <strong>How to interpret:</strong> This is a powerful sensitivity analysis. If a single study's removal (a colored line) causes a large shift in the confidence region away from the original, it indicates that the main results are highly dependent on that single study. Such influential studies should be carefully examined."), class = "plot-explanation")
                                    )
                             ),
                             column(6,
                                    div(class = "plot-container",
                                        withSpinner(plotlyOutput("enhancedBaujatPlot")),
                                        p(HTML("<strong>What it is:</strong> This is an enhanced Baujat plot specifically for the bivariate model. It identifies influential studies by plotting each study's contribution to heterogeneity against its influence on the pooled effect estimate, using values derived from the more precise bivariate model.<br>
                                                  <strong>How to interpret:</strong> Studies in the top-right quadrant are the most influential. Because this plot uses estimates from the bivariate model, it can provide a more accurate identification of influential studies than a standard Baujat plot. These studies should be reviewed to understand their impact on the overall findings."), class = "plot-explanation")
                                    )
                             )
                           ),
                           verbatimTextOutput("bivariateInfluenceSummary")
                  ),
                  # tabPanel("Quality Assessment",
                  #          actionButton("quality_assessment_info", "", icon = icon("info-circle"), class = "help-text"),
                  #          fluidRow(
                  #            column(6,
                  #                   selectInput("risk_of_bias", "Risk of Bias:",
                  #                               choices = c("Low", "Unclear", "High"),
                  #                               selected = "Unclear")
                  #            ),
                  #            column(6,
                  #                   selectInput("indirectness", "Indirectness:",
                  #                               choices = c("Low", "Unclear", "High"),
                  #                               selected = "Low")
                  #            )
                  #          ),
                  #          verbatimTextOutput("bivariateGRADESummary")
                  # )
                )
      ),
      nav_panel("Meta-Regression",
                fluidRow(
                  column(4,
                         h4("Meta-Regression Configuration"),
                         p("Configure meta-regression analysis with continuous or categorical moderators.", class = "text-muted"),
                         conditionalPanel(
                           condition = "output.hasModeratorData",
                           selectInput("moderator_variable", "Select Moderator Variable:",
                                      choices = NULL),
                           br(),
                           radioButtons("moderator_type", "Variable Type:",
                                       choices = list("Continuous" = "continuous", "Categorical" = "categorical"),
                                       selected = "continuous"),
                           br(),
                           checkboxInput("use_random_effects", "Use Random Effects Model", value = TRUE),
                           helpText("Random effects model accounts for between-study heterogeneity."),
                           br(),
                           checkboxInput("meta_perm_test", "Permutation test (robust p-values)", value = FALSE),
                           helpText("Permutation tests provide robust p-values, particularly when the number of studies is small (< 10)."),
                           br(),
                           actionButton("run_metaregression", "Run Meta-Regression", 
                                       class = "btn-primary", icon = icon("play")),
                                       br(), br(),
                           helpText("Meta-regression investigates how study characteristics (moderators) relate to effect sizes. This helps explain heterogeneity and identify factors that influence treatment effectiveness.")
                         ),
                         conditionalPanel(
                           condition = "!output.hasModeratorData",
                           div(class = "alert alert-info",
                               HTML("<strong>No moderator data available.</strong><br>
                                    To perform meta-regression, upload data that includes additional columns with continuous or categorical variables (e.g., 'moderator1', 'moderator2', etc.)."))
                         )
                  ),
                  column(8,
                         conditionalPanel(
                           condition = "input.run_metaregression > 0",
                           tabsetPanel(
                             id = "metaregression_tabs",
                             tabPanel("Regression Plot",
                                      div(class = "plot-container",
                                          withSpinner(plotOutput("metaregressionPlot"))
                                      ),
                                      p(HTML("<strong>What it is:</strong> A scatter plot showing the relationship between the moderator variable (x-axis) and effect sizes (y-axis). The regression line shows the predicted relationship.<br>
                                             <strong>How to interpret:</strong> The slope indicates how effect size changes per unit increase in the moderator. A significant slope suggests the moderator explains heterogeneity between studies."), class = "plot-explanation")
                             ),
                             tabPanel("Regression Results",
                                      verbatimTextOutput("metaregressionSummary"),
                                      br(),
                                      h4("Model Interpretation"),
                                      verbatimTextOutput("metaregressionInterpretation")
                             ),
                             tabPanel("Bubble Plot",
                                      div(class = "plot-container",
                                          withSpinner(plotOutput("bubblePlot"))
                                      ),
                                      p(HTML("<strong>What it is:</strong> A bubble plot where bubble size represents study precision (inverse variance). Studies with larger bubbles have more influence on the regression.<br>
                                             <strong>How to interpret:</strong> This helps identify whether the relationship is driven by a few large studies or is consistent across studies of different sizes."), class = "plot-explanation")
                             ),
                             tabPanel("Residual Plot",
                                      div(class = "plot-container",
                                          withSpinner(plotOutput("residualPlot"))
                                      ),
                                      p(HTML("<strong>What it is:</strong> A plot of residuals (observed - predicted effect sizes) against fitted values. This helps assess model assumptions.<br>
                                             <strong>How to interpret:</strong> Random scatter suggests good model fit. Patterns may indicate violations of assumptions or need for additional moderators."), class = "plot-explanation")
                             ),
                             tabPanel("Influence Analysis",
                                      div(class = "plot-container",
                                          withSpinner(plotOutput("metaregInfluencePlot"))
                                      ),
                                      p(HTML("<strong>What it is:</strong> Influence diagnostics identify studies that disproportionately affect the meta-regression results. Cook's distance and hat values are plotted.<br>
                                             <strong>How to interpret:</strong> Points to the right/top indicate influential studies. Investigate them for data quality or study-level differences."), class = "plot-explanation"),
                                      br(),
                                      h4("Influence Summary"),
                                      verbatimTextOutput("metaregInfluenceSummary")
                             )
                           )
                         ),
                         conditionalPanel(
                           condition = "input.run_metaregression == 0 && output.hasModeratorData",
                           div(class = "alert alert-secondary text-center",
                               style = "margin-top: 50px; padding: 30px;",
                               h4("Ready for Meta-Regression"),
                               p("Select a moderator variable and click 'Run Meta-Regression' to begin."),
                               p("Meta-regression helps identify study characteristics that may explain differences in effect sizes across studies.")
                           )
                         )
                  )
                )
      )
    )
  )
)