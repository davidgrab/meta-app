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

# Define the custom theme for light mode - Clean, professional colors
light_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#2c3e50",
  primary = "#34495e", # Muted blue-gray for primary actions

  secondary = "#95a5a6", # Soft gray for secondary elements
  success = "#27ae60", # Muted green
  info = "#5d6d7e", # Slate gray for info
  warning = "#f39c12", # Muted orange
  danger = "#c0392b", # Muted red
  base_font = "system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif",
  heading_font = "system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif",
  font_scale = 0.9
)

# Define dark theme - Consistent with light theme colors
dark_theme <- bs_theme(
  version = 5,
  bg = "#1a1d21",
  fg = "#ecf0f1",
  primary = "#5dade2", # Lighter blue for visibility on dark
  secondary = "#7f8c8d", # Muted gray
  success = "#2ecc71", # Brighter green for dark mode
  info = "#74b9ff", # Light blue for info
  warning = "#f1c40f", # Yellow-gold
  danger = "#e74c3c", # Coral red
  base_font = "system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif",
  heading_font = "system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif",
  font_scale = 0.9
)

ui <- page_fillable(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  div(
    class = "app-header",
    fluidRow(
      column(
        8,
        div(
          class = "app-logo-container",
          tags$img(src = "logo_light.svg", alt = "Meta Analysis App logo", class = "app-logo logo-for-light"),
          tags$img(src = "logo_dark.svg", alt = "Meta Analysis App logo", class = "app-logo logo-for-dark")
        )
      ),
      column(4,
        style = "text-align: right;",
        input_dark_mode(
          id = "dark_mode", mode = "light",
          style = "font-size: 0.8em; padding: 0; width: 20px; height: 20px; border-radius: 50%;"
        )
      )
    )
  ),
  # Main content - now accessible immediately
  layout_sidebar(
    sidebar = sidebar(
      radioButtons("data_type", "Select Data Type:",
        choices = list(
          "Binary (2x2)" = "binary",
          "Continuous" = "smd"
        ),
        selected = "binary"
      ),
      fileInput("datafile", "Upload Data", accept = c(".csv", ".xlsx")),
      selectInput("het_estimator", "Heterogeneity Estimator", choices = c("DL", "PM", "REML", "ML"), selected = "DL"),
      conditionalPanel(
        condition = "input.data_type == 'binary'",
        selectInput("effect_measure", "Effect Measure", choices = c("OR", "RR"), selected = "RR")
      ),
      shinyjs::disabled(actionButton("analyze", "Analyze", class = "btn-primary")),
      uiOutput("analyzeHelpText"),
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
      nav_panel(
        "Data Preview",
        actionButton("data_info", "How to Upload Data", icon = icon("question-circle")),
        fluidRow(
          column(6, downloadButton("downloadSampleStructure", "Download Sample Structure")),
          column(
            6,
            div(
              div(
                style = "display: flex; align-items: center;",
                selectInput("exampleDatasetChoice", "Choose Example Dataset:",
                  choices = list(
                    "Hypericum (St. John's Wort) - Depression (Default)" = "default",
                    "Colditz et al. (1994) - BCG Vaccine" = "colditz",
                    "CBT for Depression (Continuous)" = "smd"
                  ),
                  selected = "default"
                ),
                actionButton("dataset_info", "",
                  icon = icon("info-circle"),
                  class = "btn-outline-secondary",
                  style = "margin-left: 8px; width: 32px; height: 32px; padding: 0; display: inline-flex; align-items: center; justify-content: center; border-radius: 50%;"
                )
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
      #                      p("Method comparison plot: Compares effect size estimates and confidence intervals from fixed effects, random effects, and JCR (Joint Confidence Region) meta-analysis models. Shows relative precision of each method.", class = "plot-explanation"),
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
      nav_panel(
        "Random Effects Analysis",
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
          tabPanel(
            "Effect Size and Heterogeneity",
            div(
              class = "plot-container",
              style = "max-height: 600px; overflow-y: auto;",
              withSpinner(plotOutput("randomForestPlot"))
            ),
            p(HTML("<strong>Forest Plot:</strong> Displays effect sizes and confidence intervals for each study, with the pooled random-effects estimate shown as a diamond. Square sizes are proportional to study weights. If the diamond does not cross the line of no effect (1 for OR/RR, 0 for SMD), the pooled result is statistically significant."), class = "plot-explanation"),
            actionButton("re_effect_size_heterogeneity_info", "", icon = icon("info-circle"), class = "help-text"),
            verbatimTextOutput("randomOverallSummary"),
            verbatimTextOutput("randomHeterogeneitySummary")
          ),
          tabPanel(
            "Subgroup Analysis",
            actionButton("re_subgroup_info", "", icon = icon("info-circle"), class = "help-text"),
            fluidRow(
              column(
                4,
                h4("Subgroup Configuration"),
                conditionalPanel(
                  condition = "output.hasSubgroupData",
                  selectInput("random_subgroup_variable", "Select Subgroup Variable:",
                    choices = NULL
                  ),
                  br(),
                  actionButton("run_random_subgroup", "Run Subgroup Analysis",
                    class = "btn-primary", icon = icon("play")
                  ),
                  br(), br(),
                  helpText("Subgroup analysis using the random effects model will compare effect sizes between different subgroups and test for subgroup differences.")
                ),
                conditionalPanel(
                  condition = "!output.hasSubgroupData",
                  div(
                    class = "alert alert-info",
                    HTML("<strong>No subgroup data available.</strong><br>
                                               Upload data with categorical variables to perform subgroup analysis.")
                  )
                )
              ),
              column(
                8,
                conditionalPanel(
                  condition = "input.run_random_subgroup > 0",
                  div(
                    class = "plot-container",
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
                  div(
                    class = "alert alert-secondary text-center",
                    style = "margin-top: 50px; padding: 30px;",
                    h4("Ready for Random Effects Subgroup Analysis"),
                    p("Select a subgroup variable and click 'Run Subgroup Analysis' to begin.")
                  )
                )
              )
            )
          ),
          tabPanel(
            "Model Diagnostics",
            actionButton("re_model_diagnostics_info", "", icon = icon("info-circle"), class = "help-text"),
            h4("Normality Assessment"),
            p("These plots assess whether the random effects model assumptions are met:", class = "section-explanation"),

            # Side-by-side deleted residuals Q-Q plots (NEW - moved up)
            h5("Deleted Residuals Comparison"),
            div(
              class = "plot-container", style = "width: 100%;",
              withSpinner(plotOutput("randomDeletedResidualsComparisonPlot", height = "400px")),
              p(HTML("<strong>Deleted Residuals Q-Q:</strong> Compares fixed effects (left) vs random effects (right) deleted residuals against N(0,1). Red dashed line = identity; gray region = 95% simulation envelope. If RE residuals align better than FE residuals, this provides informal evidence that heterogeneity is present and the RE model may be more appropriate."),
                class = "plot-explanation"
              )
            ),

            # BLUPs Q-Q plot (moved down)
            h5("Best Linear Unbiased Predictors (BLUPs)"),
            div(
              class = "plot-container",
              withSpinner(plotOutput("randomQQPlot")),
              p(HTML("<strong>BLUPs Q-Q:</strong> An informal diagnostic for whether study-specific effects follow a normal distribution. Red dashed line = identity; gray region = 95% envelope. S-shaped curves or points outside the envelope may suggest departures from normality, potential outliers, or unmodeled subgroups."), class = "plot-explanation")
            ),
            h4("Formal Statistical Tests"),
            verbatimTextOutput("randomNormalityTestSummary"),
            h4("Additional Diagnostics"),
            div(
              class = "plot-container",
              withSpinner(plotOutput("outlierDetectionPlot")),
              p(HTML("<strong>Outlier Detection:</strong> Displays standardized residuals for each study with reference lines at ±1.96. Studies outside these bounds may be outliers that warrant investigation."), class = "plot-explanation")
            )
          ),
          tabPanel(
            "Publication Bias",
            actionButton("publication_bias_info", "", icon = icon("info-circle"), class = "help-text"),
            fluidRow(
              column(
                6,
                div(
                  class = "plot-container",
                  withSpinner(plotOutput("randomFunnelPlot")),
                  p(HTML("<strong>Funnel Plot:</strong> Plots effect sizes against precision (1/SE). In the absence of bias, studies should scatter symmetrically around the pooled effect. Asymmetry may suggest publication bias or other small-study effects, though it can also arise from genuine heterogeneity."), class = "plot-explanation")
                )
              ),
              column(
                6,
                div(
                  class = "plot-container",
                  withSpinner(plotOutput("randomTrimFillPlot")),
                  p(HTML("<strong>Trim & Fill:</strong> Estimates and imputes potentially missing studies (shown as open circles) to restore funnel symmetry. The adjusted pooled estimate shows how results might change if suspected missing studies were included. Large differences between original and adjusted estimates suggest sensitivity to potential publication bias."), class = "plot-explanation")
                )
              )
            ),
            verbatimTextOutput("randomEggerTestResults")
          ),
          tabPanel(
            "Sensitivity Analysis",
            actionButton("sensitivity_analysis_info", "", icon = icon("info-circle"), class = "help-text"),

            # Leave-One-Out Plot (full width, scrollable)
            div(
              class = "plot-container",
              style = "max-height: 600px; overflow-y: auto;",
              withSpinner(plotOutput("leaveOneOutPlot"))
            ),
            p(HTML("<strong>Leave-One-Out:</strong> Recalculates the pooled effect by sequentially removing each study. If removing a study substantially changes the overall estimate or its significance, the results may be sensitive to that study, which warrants further investigation."), class = "plot-explanation"),

            # Baujat Plot (full width)
            div(
              class = "plot-container",
              withSpinner(plotOutput("baujatPlot"))
            ),
            p(HTML("<strong>Baujat Plot:</strong> Plots each study's contribution to the overall heterogeneity (Q statistic) against its influence on the pooled effect. Studies in the top-right quadrant contribute most to both heterogeneity and the overall result, and may warrant closer examination."), class = "plot-explanation"),
            verbatimTextOutput("influenceSummary")
          ),
          # tabPanel("Quality Assessment",
          #          actionButton("quality_assessment_info", "", icon = icon("info-circle"), class = "help-text"),
          #          verbatimTextOutput("randomGradeAssessment")
          # )
        )
      ),
      nav_panel(
        "Fixed Effects Analysis",
        tabsetPanel(
          tabPanel(
            "Effect Size and Heterogeneity",
            div(
              class = "plot-container",
              style = "max-height: 600px; overflow-y: auto;",
              withSpinner(plotOutput("fixedForestPlot"))
            ),
            p(HTML("<strong>Fixed Effects Forest Plot:</strong> Displays effect sizes under the fixed-effect model, which assumes all studies share a single true effect. The diamond represents the pooled estimate. This model may be appropriate when heterogeneity is low (non-significant Q-test, low I²) and study confidence intervals largely overlap."), class = "plot-explanation"),
            actionButton("fe_effect_size_heterogeneity_info", "", icon = icon("info-circle"), class = "help-text"),
            verbatimTextOutput("fixedOverallSummary"),
            verbatimTextOutput("modelFitStatistics")
          ),
          tabPanel(
            "Subgroup Analysis",
            actionButton("fe_subgroup_info", "", icon = icon("info-circle"), class = "help-text"),
            fluidRow(
              column(
                4,
                h4("Subgroup Configuration"),
                conditionalPanel(
                  condition = "output.hasSubgroupData",
                  selectInput("fixed_subgroup_variable", "Select Subgroup Variable:",
                    choices = NULL
                  ),
                  br(),
                  actionButton("run_fixed_subgroup", "Run Subgroup Analysis",
                    class = "btn-primary", icon = icon("play")
                  ),
                  br(), br(),
                  helpText("Subgroup analysis using the fixed effects model assumes a common effect within each subgroup and tests for differences between subgroups.")
                ),
                conditionalPanel(
                  condition = "!output.hasSubgroupData",
                  div(
                    class = "alert alert-info",
                    HTML("<strong>No subgroup data available.</strong><br>
                                               Upload data with categorical variables to perform subgroup analysis.")
                  )
                )
              ),
              column(
                8,
                conditionalPanel(
                  condition = "input.run_fixed_subgroup > 0",
                  div(
                    class = "plot-container",
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
                  div(
                    class = "alert alert-secondary text-center",
                    style = "margin-top: 50px; padding: 30px;",
                    h4("Ready for Fixed Effects Subgroup Analysis"),
                    p("Select a subgroup variable and click 'Run Subgroup Analysis' to begin.")
                  )
                )
              )
            )
          ),
          tabPanel(
            "Model Diagnostics",
            actionButton("fe_model_diagnostics_info", "", icon = icon("info-circle"), class = "help-text"),
            h4("Normality Assessment"),
            p("This plot assesses whether the fixed effects model assumptions are met:", class = "section-explanation"),
            div(
              class = "plot-container",
              withSpinner(plotOutput("fixedQQPlot")),
              p(HTML("<strong>Standardized Residuals Q-Q:</strong> An informal diagnostic for whether residuals follow a standard normal distribution. Red dashed line = identity; gray region = 95% envelope. Systematic deviations from the diagonal may suggest non-normality or the presence of heterogeneity not captured by the fixed-effect model."), class = "plot-explanation")
            ),
            h4("Formal Statistical Test"),
            verbatimTextOutput("fixedNormalityTestSummary"),
            h4("Outlier Detection"),
            div(
              class = "plot-container",
              withSpinner(plotOutput("fixedOutlierDetectionPlot")),
              p(HTML("<strong>Outlier Detection:</strong> Identifies potential outliers based on standardized residuals. Studies with large residuals may be outliers or may not follow the fixed-effect assumption."), class = "plot-explanation")
            )
          ),
          tabPanel(
            "Publication Bias",
            actionButton("publication_bias_info", "", icon = icon("info-circle"), class = "help-text"),
            fluidRow(
              column(
                6,
                div(
                  class = "plot-container",
                  withSpinner(plotOutput("fixedFunnelPlot")),
                  p(HTML("<strong>Funnel Plot:</strong> Plots effect sizes against precision. For a fixed-effect model, the plot should be symmetrical around the pooled estimate. Asymmetry, particularly gaps in the bottom corners, may suggest publication bias or small-study effects."), class = "plot-explanation")
                )
              ),
              column(
                6,
                div(
                  class = "plot-container",
                  withSpinner(plotOutput("fixedTrimFillPlot")),
                  p(HTML("<strong>Trim & Fill:</strong> Imputes potentially missing studies (open circles) to restore funnel symmetry. The adjusted estimate shows how results might change if publication bias were present. Large differences between original and adjusted estimates suggest sensitivity to potential bias."), class = "plot-explanation")
                )
              )
            ),
            verbatimTextOutput("fixedEggerTestResults")
          ),
          tabPanel(
            "Sensitivity Analysis",
            actionButton("sensitivity_analysis_info", "", icon = icon("info-circle"), class = "help-text"),

            # Leave-One-Out Plot (full width, scrollable)
            div(
              class = "plot-container",
              style = "max-height: 600px; overflow-y: auto;",
              withSpinner(plotOutput("fixedLeaveOneOutPlot"))
            ),
            p(HTML("<strong>Leave-One-Out:</strong> Shows how the fixed-effect estimate changes when each study is sequentially removed. If removing a study substantially changes the overall estimate or significance, the results are sensitive to that study."), class = "plot-explanation"),

            # Influence Plot (Baujat) (full width)
            div(
              class = "plot-container",
              withSpinner(plotOutput("fixedInfluencePlot"))
            ),
            p(HTML("<strong>Baujat Plot:</strong> Plots each study's contribution to the Q statistic (heterogeneity) against its influence on the pooled effect. Since the fixed-effect model assumes no heterogeneity, studies contributing significantly to Q may be potential outliers or violate model assumptions."), class = "plot-explanation"),
            verbatimTextOutput("fixedInfluenceSummary")
          ),
          # tabPanel("Quality Assessment",
          #          actionButton("quality_assessment_info", "", icon = icon("info-circle"), class = "help-text"),
          #          verbatimTextOutput("fixedGradeAssessment")
          # )
        )
      ),
      nav_panel(
        "JCR Method",
        tabsetPanel(
          tabPanel(
            "Effect Size and Heterogeneity",
            actionButton("biv_effect_size_heterogeneity_info", "", icon = icon("info-circle"), class = "help-text"),

            # JCR Forest Plot (full width)
            div(
              class = "plot-container",
              style = "max-height: 600px; overflow-y: auto;",
              withSpinner(plotOutput("bivariateForestPlot"))
            ),
            p(HTML("<strong>Forest Plot:</strong> Displays individual study effect sizes and the pooled estimate (diamond). The pooled estimate is derived from Maximum Likelihood Estimation, which jointly estimates μ and τ."), class = "plot-explanation"),

            # Confidence Region Plot (full width)
            div(
              class = "plot-container", style = "width: 100%;",
              withSpinner(plotOutput("confidenceRegionPlot", height = "600px"))
            ),
            p(HTML("<strong>Joint Confidence Region:</strong> Shows the joint confidence region for the overall effect (μ) and heterogeneity (τ) at multiple confidence levels (50%, 90%, 95%, 99%). The cross marks the MLE. A wider region indicates greater uncertainty. Unlike traditional methods that treat τ as fixed, this visualizes how uncertainty in μ and τ are interrelated.<br><strong>Note (binary outcomes):</strong> For OR/RR, the model estimates μ on the log scale, but for convenience we display the x-axis on the original OR/RR scale (i.e., exp(μ)) using a log-scaled axis."), class = "plot-explanation"),
            div(
              class = "plot-container", style = "width: 100%;",
              withSpinner(plotOutput("efficacyHarmPlot", height = "500px")),
              p(HTML("<strong>Efficacy-Harm Plot:</strong> Shows the probability that a new study's true effect exceeds (or falls below) clinical thresholds, with confidence bands derived from the joint (μ, τ) uncertainty. The steepness of the curve indicates certainty. This translates statistical uncertainty into clinically interpretable probabilities."), class = "plot-explanation"),

              # Color configuration for beneficial direction
              div(
                style = "margin: 15px 0; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
                radioButtons(
                  "efficacy_direction",
                  label = "Which direction indicates benefit (shown in green)?",
                  choices = c(
                    "Lower values are beneficial (e.g., RR < 1 means treatment reduces risk)" = "left",
                    "Higher values are beneficial (e.g., SMD > 0 means treatment improves outcome)" = "right"
                  ),
                  selected = "left",
                  inline = FALSE
                ),
                helpText("Choose which side of the plot represents a beneficial outcome. The beneficial side will be shown in green, and the harmful side in red.")
              ),

              # Custom thresholds input
              br(),
              div(
                style = "margin: 15px 0;",
                textInput("custom_thresholds",
                  label = "Custom Thresholds (comma-separated):",
                  value = "",
                  placeholder = "e.g., 0.3, 0.7, 1.0",
                  width = "300px"
                ),
                radioButtons(
                  "probability_direction",
                  label = "Report probability as:",
                  choices = c(
                    "P(θ ≥ T) – probability true effect exceeds threshold" = "greater",
                    "P(θ ≤ T) – probability true effect is below threshold" = "less"
                  ),
                  selected = "greater",
                  inline = TRUE
                ),
                helpText("Add custom thresholds and choose whether the table reports the chance of exceeding or staying below each value.")
              ),

              # Probability table
              h5("Probability Table for Key Clinical Thresholds"),
              tableOutput("efficacyHarmProbabilityTable"),
              p(HTML("<strong>What it shows:</strong> This table mirrors the Efficacy/Harm plot. For each threshold value T, it reports whichever probability you selected above (either P(θ ≥ T) or P(θ ≤ T)), along with 95% confidence intervals.<br>
                                         <strong>How to use:</strong> Toggle the probability direction to switch between \"benefit\" and \"harm\" perspectives and use custom thresholds to pull precise numbers from the curve."), class = "plot-explanation")
            )
          ),
          tabPanel(
            "Subgroup Analysis",
            actionButton("biv_subgroup_info", "", icon = icon("info-circle"), class = "help-text"),
            fluidRow(
              column(
                4,
                h4("Subgroup Configuration"),
                conditionalPanel(
                  condition = "output.hasSubgroupData",
                  selectInput("bivariate_subgroup_variable", "Select Subgroup Variable:",
                    choices = NULL
                  ),
                  br(),
                  actionButton("run_bivariate_subgroup", "Run Subgroup Analysis",
                    class = "btn-primary", icon = icon("play")
                  ),
                  br(), br(),
                  helpText("Subgroup analysis using the JCR method performs separate JCR meta-analyses for each subgroup, providing joint MLE estimation of effect and heterogeneity parameters.")
                ),
                conditionalPanel(
                  condition = "!output.hasSubgroupData",
                  div(
                    class = "alert alert-info",
                    HTML("<strong>No subgroup data available.</strong><br>
                                               Upload data with categorical variables to perform subgroup analysis.")
                  )
                )
              ),
              column(
                8,
                conditionalPanel(
                  condition = "input.run_bivariate_subgroup > 0",
                  div(
                    class = "plot-container",
                    style = "max-height: 700px; overflow-y: auto;",
                    withSpinner(plotOutput("bivariateSubgroupForestPlot"))
                  ),
                  p(HTML("<strong>Subgroup Analysis:</strong> Forest plot showing studies grouped by the selected variable, with pooled estimates for each subgroup derived from joint MLE."), class = "plot-explanation"),
                  br(),
                  h4("Subgroup Comparison"),
                  verbatimTextOutput("bivariateSubgroupTest")
                ),
                conditionalPanel(
                  condition = "input.run_bivariate_subgroup == 0 && output.hasSubgroupData",
                  div(
                    class = "alert alert-secondary text-center",
                    style = "margin-top: 50px; padding: 30px;",
                    h4("Ready for JCR Subgroup Analysis"),
                    p("Select a subgroup variable and click 'Run Subgroup Analysis' to begin.")
                  )
                )
              )
            )
          ),
          tabPanel(
            "Model Diagnostics",
            actionButton("biv_model_diagnostics_info", "", icon = icon("info-circle"), class = "help-text"),
            h4("Normality Assessment"),
            p("These plots assess whether the JCR meta-analysis model assumptions are met using joint MLE estimation:", class = "section-explanation"),

            # Side-by-side deleted residuals Q-Q plots (NEW - moved up)
            h5("Deleted Residuals Comparison"),
            div(
              class = "plot-container", style = "width: 100%;",
              withSpinner(plotOutput("bivariateDeletedResidualsComparisonPlot", height = "400px")),
              p(HTML("<strong>Deleted Residuals Q-Q:</strong> Compares fixed effects (left) vs MLE (right) deleted residuals against N(0,1). Red dashed line = identity; gray region = 95% simulation envelope. Comparing the two panels may reveal differences in how well each model captures the data structure."),
                class = "plot-explanation"
              )
            ),

            # BLUPs Q-Q plot (moved down)
            h5("Best Linear Unbiased Predictors (BLUPs)"),
            div(
              class = "plot-container",
              withSpinner(plotOutput("qqPlotMu")),
              p(HTML("<strong>BLUPs Q-Q:</strong> An informal diagnostic for whether study-specific effects follow a normal distribution. Red dashed line = identity; gray region = 95% envelope. Deviations may suggest departures from the assumed normal distribution."), class = "plot-explanation")
            ),
            h4("Formal Statistical Tests"),
            verbatimTextOutput("bivariateNormalityTestSummary")
          ),
          tabPanel(
            "Publication Bias",
            actionButton("publication_bias_info", "", icon = icon("info-circle"), class = "help-text"),
            div(
              class = "plot-container",
              withSpinner(plotOutput("bivariateAdaptedFunnelPlot")),
              p(HTML("<strong>Funnel Plot:</strong> Plots study-specific effect sizes against their standard errors. The plot should be symmetrical in the absence of publication bias. Asymmetry may suggest that small studies with non-significant results are missing."), class = "plot-explanation")
            ),
            # verbatimTextOutput("bivariateBiasTestResults")
          ),
          tabPanel(
            "Sensitivity Analysis",
            actionButton("sensitivity_analysis_info", "", icon = icon("info-circle"), class = "help-text"),
            fluidRow(
              column(
                6,
                div(
                  class = "plot-container", style = "width: 100%;",
                  withSpinner(plotlyOutput("confidenceRegionShiftPlot", height = "600px")),
                  p(HTML("<strong>Confidence Region Shift:</strong> Shows how the joint confidence region for (μ, τ) shifts when each study is removed. The black contour is from the full dataset; colored contours show leave-one-out results. Large shifts indicate that results may be sensitive to that particular study.<br><strong>Note (binary outcomes):</strong> For OR/RR, the x-axis is shown on the original OR/RR scale (exp(μ)) using a log-scaled axis."), class = "plot-explanation")
                )
              ),
              column(
                6,
                div(
                  class = "plot-container", style = "width: 100%;",
                  withSpinner(plotlyOutput("enhancedBaujatPlot", height = "600px")),
                  p(HTML("<strong>Baujat Plot:</strong> Plots each study's contribution to heterogeneity (Q statistic) against its influence on the pooled effect. Studies in the top-right quadrant contribute most to both and may warrant closer examination."), class = "plot-explanation")
                )
              )
            )
          )
        )
      ),
      nav_panel(
        "Replicability Analysis",
        fluidRow(
          column(
            12,
            h4("Replicability Analysis Configuration"),
            div(
              style = "margin-bottom: 10px; font-style: italic; color: #666;",
              uiOutput("analysisSettings")
            ),
            div(
              style = "display: flex; gap: 20px; align-items: center; margin-bottom: 15px;",
              numericInput("rvalue_t", "Significance Threshold (α)", value = 0.05, min = 0.001, max = 0.2, step = 0.01, width = "150px"),
              checkboxInput("rvalue_common_effect", "Common-effect assumption (Pearson)", value = FALSE)
            ),
            actionButton("run_rvalue", "Run Analysis", class = "btn-primary", icon = icon("play")),
            hr(),
            conditionalPanel(
              condition = "input.run_rvalue > 0",

              # Replicability Statement (Figure 8 style)
              div(
                class = "alert alert-light border",
                style = "font-weight: bold; font-size: 1.1em; background-color: #f8f9fa; border-left: 5px solid #007bff;",
                textOutput("replicabilityStatement")
              ),

              # 1) Top summary cards
              layout_column_wrap(
                width = 1 / 3,
                height = "150px",
                value_box(
                  title = "r-value (r(2))",
                  value = textOutput("val_r2"),
                  showcase = bsicons::bs_icon("bar-chart-fill")
                ),
                value_box(
                  title = "Lower Bounds (95% CI)",
                  value = textOutput("val_bounds"),
                  p("u_R / u_L"),
                  showcase = bsicons::bs_icon("graph-up")
                ),
                uiOutput("card_consistency")
              ),


              # 2 & 3) Plot and Mini Table
              fluidRow(
                column(
                  8,
                  div(
                    class = "card",
                    div(class = "card-header", h5("Replicability Sensitivity: r(u) Curve")),
                    div(class = "card-body", withSpinner(plotOutput("ruCurvePlot", height = "300px")))
                  )
                ),
                column(
                  4,
                  div(
                    class = "card",
                    div(class = "card-header", h5("Diagnostic Table (u = 2...k)")),
                    div(class = "card-body", DTOutput("ruTable"))
                  )
                )
              ),


              # 4) Forest plot
              div(
                class = "card",
                div(class = "card-header", h5("Forest Plot with Replicability Annotation")),
                div(
                  class = "card-body",
                  withSpinner(plotOutput("rvalueForestPlot", height = "500px"))
                )
              ),
              br(),

              # 5) How to read this frame
              div(
                class = "alert alert-info",
                h5(icon("info-circle"), "How to read these results"),
                tags$ul(
                  tags$li(strong("r-value ≤ 0.05"), ": Evidence the finding is ", strong("not driven by a single study"), " (at least 2 studies in same direction)."),
                  tags$li(strong("uR / uL"), ": The ", strong("minimum number"), " of studies supporting the increased/decreased direction (with 95% confidence)."),
                  tags$li(strong("r-value > 0.05"), ": Does not disprove replicability; may indicate low power or small number of studies."),
                  tags$li("Consistency is supported if at least 2 studies support one direction and 0 support the opposite.")
                )
              )
            ),
            conditionalPanel(
              condition = "input.run_rvalue == 0",
              div(
                class = "alert alert-secondary text-center",
                style = "margin-top: 50px; padding: 40px;",
                h3("Ready to Analyze"),
                p("Click 'Run Analysis' to quantify replicability and consistency."),
                p("This analysis requires the metarep package (pre-installed).")
              )
            )
          )
        )
      ),
      nav_panel(
        "Meta-Regression",
        actionButton("metaregression_info", "", icon = icon("info-circle"), class = "help-text"),
        fluidRow(
          column(
            4,
            h4("Meta-Regression Configuration"),
            p("Configure meta-regression analysis with continuous or categorical moderators.", class = "text-muted"),
            conditionalPanel(
              condition = "output.hasModeratorData",
              selectInput("moderator_variable", "Select Moderator Variable:",
                choices = NULL
              ),
              br(),
              radioButtons("moderator_type", "Variable Type:",
                choices = list("Continuous" = "continuous", "Categorical" = "categorical"),
                selected = "continuous"
              ),
              br(),
              checkboxInput("use_random_effects", "Use Random Effects Model", value = TRUE),
              helpText("Random effects model accounts for between-study heterogeneity."),
              br(),
              checkboxInput("meta_perm_test", "Permutation test (robust p-values)", value = FALSE),
              helpText("Permutation tests provide robust p-values, particularly when the number of studies is small (< 10)."),
              br(),
              actionButton("run_metaregression", "Run Meta-Regression",
                class = "btn-primary", icon = icon("play")
              ),
              br(), br(),
              helpText("Meta-regression investigates how study characteristics (moderators) relate to effect sizes. This helps explain heterogeneity and identify factors that influence treatment effectiveness.")
            ),
            conditionalPanel(
              condition = "!output.hasModeratorData",
              div(
                class = "alert alert-info",
                HTML("<strong>No moderator data available.</strong><br>
                                    To perform meta-regression, upload data that includes additional columns with continuous or categorical variables (e.g., 'moderator1', 'moderator2', etc.).")
              )
            )
          ),
          column(
            8,
            conditionalPanel(
              condition = "input.run_metaregression > 0",
              tabsetPanel(
                id = "metaregression_tabs",
                tabPanel(
                  "Regression Plot",
                  div(
                    class = "plot-container",
                    withSpinner(plotOutput("metaregressionPlot"))
                  ),
                  p(HTML("<strong>What it is:</strong> A scatter plot showing the relationship between the moderator variable (x-axis) and effect sizes (y-axis). The regression line shows the predicted relationship.<br>
                                             <strong>How to interpret:</strong> The slope indicates how effect size changes per unit increase in the moderator. A significant slope suggests the moderator explains heterogeneity between studies."), class = "plot-explanation")
                ),
                tabPanel(
                  "Regression Results",
                  verbatimTextOutput("metaregressionSummary"),
                  br(),
                  h4("Model Interpretation"),
                  verbatimTextOutput("metaregressionInterpretation")
                ),
                tabPanel(
                  "Bubble Plot",
                  div(
                    class = "plot-container",
                    withSpinner(plotOutput("bubblePlot"))
                  ),
                  p(HTML("<strong>What it is:</strong> A bubble plot where bubble size represents study precision (inverse variance). Studies with larger bubbles have more influence on the regression.<br>
                                             <strong>How to interpret:</strong> This helps identify whether the relationship is driven by a few large studies or is consistent across studies of different sizes."), class = "plot-explanation")
                ),
                tabPanel(
                  "Residual Plot",
                  div(
                    class = "plot-container",
                    withSpinner(plotOutput("residualPlot"))
                  ),
                  p(HTML("<strong>What it is:</strong> A plot of residuals (observed - predicted effect sizes) against fitted values. This helps assess model assumptions.<br>
                                             <strong>How to interpret:</strong> Random scatter suggests good model fit. Patterns may indicate violations of assumptions or need for additional moderators."), class = "plot-explanation")
                ),
                tabPanel(
                  "Influence Analysis",
                  div(
                    class = "plot-container",
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
              div(
                class = "alert alert-secondary text-center",
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
  ) # Close layout_sidebar
)
