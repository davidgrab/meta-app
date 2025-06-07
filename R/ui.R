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
           input_dark_mode(id = "dark_mode", mode = "dark", style="font-size: 0.8em; padding: 0; width: 20px; height: 20px; border-radius: 50%;")
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
      downloadButton("downloadReport", "Download Report")
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
      nav_panel("Overall Results",
                tabsetPanel(
                  tabPanel("Method Comparison", 
                           actionButton("method_comparison_info", "", icon = icon("info-circle"), class = "help-text"),
                           withSpinner(plotOutput("methodComparisonPlot")),
                           p("Method comparison plot: Compares effect size estimates and confidence intervals from fixed effects, random effects, and bivariate meta-analysis models. Shows relative precision of each method.", class = "plot-explanation"),
                           hr(),
                           h4("Summary"),
                           verbatimTextOutput("methodComparisonSummary")),
                  tabPanel("Summary Table", 
                           actionButton("summary_table_info", "", icon = icon("info-circle"), class = "help-text"),
                           tableOutput("overallSummaryTable")),
                  # tabPanel("Overall Interpretation", 
                  #          actionButton("overall_interpretation_info", "", icon = icon("info-circle"), class = "help-text"),
                  #          verbatimTextOutput("overallInterpretation"))
                )
      ),
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
                           p("Forest plot: Shows individual study effect sizes with weights (box size) and confidence intervals (horizontal lines), plus the pooled random effects estimate (diamond).", class = "plot-explanation"),
                           actionButton("re_effect_size_heterogeneity_info", "", icon = icon("info-circle"), class = "help-text"),
                           verbatimTextOutput("randomOverallSummary"),
                           verbatimTextOutput("randomHeterogeneitySummary")
                  ),
                  tabPanel("Model Diagnostics",
                           actionButton("re_model_diagnostics_info", "", icon = icon("info-circle"), class = "help-text"),
                           fluidRow(
                             column(6, 
                                    div(class = "plot-container",
                                        withSpinner(plotOutput("randomQQPlot")),
                                        p("Q-Q plot: Assesses if standardized residuals follow a normal distribution, which is an assumption of random effects models.", class = "plot-explanation")
                                    )
                             ),
                             column(6, 
                                    div(class = "plot-container",
                                        withSpinner(plotOutput("outlierDetectionPlot")),
                                        p("Outlier detection plot: Displays standardized residuals for each study with reference lines at ±1.96, helping identify potential outliers.", class = "plot-explanation")
                                    )
                             )
                           ),
                           div(class = "plot-container",
                               withSpinner(plotOutput("effectDistributionPlot")),
                               p("Effect distribution plot: Histogram showing the distribution of effect sizes across studies with the pooled effect size (red dashed line).", class = "plot-explanation")
                           )
                  ),
                  tabPanel("Publication Bias",
                           actionButton("publication_bias_info", "", icon = icon("info-circle"), class = "help-text"),
                           fluidRow(
                             column(6, 
                                    div(class = "plot-container",
                                        withSpinner(plotOutput("randomFunnelPlot")),
                                        p("Funnel plot: Plots effect sizes against standard errors to help detect publication bias. Asymmetry suggests potential publication bias.", class = "plot-explanation")
                                    )
                             ),
                             column(6, 
                                    div(class = "plot-container",
                                        withSpinner(plotOutput("randomTrimFillPlot")),
                                        p("Trim and fill plot: Estimates and adds potentially missing studies (in white) to adjust for publication bias.", class = "plot-explanation")
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
                           p("Leave-one-out plot: Shows how overall effect changes when each study is removed.", class = "plot-explanation"),

                           # Baujat Plot (full width)
                           div(class = "plot-container",
                               withSpinner(plotOutput("baujatPlot"))
                           ),
                           p("Baujat plot: Identifies studies contributing to heterogeneity and influencing overall result.", class = "plot-explanation"),

                           div(class = "plot-container",
                               withSpinner(plotOutput("randomGOSHPlot")),
                               p("Effect size vs. standard error plot: Shows relationship between effect size and precision for each study.", class = "plot-explanation")
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
                           p("Fixed effects forest plot: Shows individual study effect sizes and the pooled fixed effect estimate, assuming a common true effect size.", class = "plot-explanation"),
                           actionButton("fe_effect_size_heterogeneity_info", "", icon = icon("info-circle"), class = "help-text"),
                           verbatimTextOutput("fixedOverallSummary"),
                           verbatimTextOutput("modelFitStatistics")
                  ),
                  tabPanel("Model Diagnostics",
                           actionButton("fe_model_diagnostics_info", "", icon = icon("info-circle"), class = "help-text"),
                           fluidRow(
                             column(6, 
                                    div(class = "plot-container",
                                        withSpinner(plotOutput("fixedModelFitPlot")),
                                        p("Radial plot: Visualizes the fit of the fixed effects model, with points closer to the line indicating better fit.", class = "plot-explanation")
                                    )
                             ),
                             column(6, 
                                    div(class = "plot-container",
                                        withSpinner(plotOutput("fixedQQPlot")),
                                        p("Q-Q plot: Assesses normality of residuals in fixed effects model, testing whether sampling error alone explains study differences.", class = "plot-explanation")
                                    )
                             )
                           ),
                           div(class = "plot-container",
                               withSpinner(plotOutput("fixedOutlierDetectionPlot")),
                               p("Outlier detection: Identifies potential outliers in fixed effects model based on standardized residuals.", class = "plot-explanation")
                           )
                  ),
                  tabPanel("Publication Bias",
                           actionButton("publication_bias_info", "", icon = icon("info-circle"), class = "help-text"),
                           fluidRow(
                             column(6, 
                                    div(class = "plot-container",
                                        withSpinner(plotOutput("fixedFunnelPlot")),
                                        p("Funnel plot: Plots effect sizes against standard errors to help detect publication bias in the fixed effects model.", class = "plot-explanation")
                                    )
                             ),
                             column(6, 
                                    div(class = "plot-container",
                                        withSpinner(plotOutput("fixedTrimFillPlot")),
                                        p("Trim and fill plot: Adjusts for potential publication bias by estimating and adding missing studies to the fixed effects model.", class = "plot-explanation")
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
                           p("Leave-one-out plot: Shows sensitivity of fixed effects model to individual studies by sequentially removing each study.", class = "plot-explanation"),

                           # Influence Plot (Baujat) (full width)
                           div(class = "plot-container",
                               withSpinner(plotOutput("fixedInfluencePlot"))
                           ),
                           p("Influence plot: Identifies studies with disproportionate impact on fixed effects model.", class = "plot-explanation"),

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
                           p("Bivariate forest plot: Shows study effects and overall effect estimated using a bivariate random effects model.", class = "plot-explanation"),
                           
                           # Confidence Region Plot (full width)
                           div(class = "plot-container",
                               withSpinner(plotOutput("confidenceRegionPlot"))
                           ),
                           p("Confidence region plot: Shows the joint confidence region for the effect size (μ) and between-study variability (τ).", class = "plot-explanation"),
                           
                           div(class = "plot-container",
                               withSpinner(plotOutput("efficacyHarmPlot")),
                               p("Efficacy-harm plot: Visualizes probabilistic relationship between efficacy and harm for different thresholds.", class = "plot-explanation")
                           ),
                           verbatimTextOutput("bivariateOverallSummary")
                  ),
                  tabPanel("Model Diagnostics",
                           actionButton("biv_model_diagnostics_info", "", icon = icon("info-circle")),
                           fluidRow(
                             column(6, 
                                    div(class = "plot-container",
                                        withSpinner(plotOutput("qqPlotMu")),
                                        p("Q-Q plot (standardized residuals): Assesses normality assumption for the random effects distribution.", class = "plot-explanation")
                                    )
                             ),
                             column(6, 
                                    div(class = "plot-container",
                                        withSpinner(plotOutput("qqPlotMuRaw")),
                                        p("Q-Q plot (raw residuals): Assesses normality of raw residuals, with point size proportional to study size.", class = "plot-explanation")
                                    )
                             )
                           )
                  ),
                  tabPanel("Publication Bias",
                           actionButton("publication_bias_info", "", icon = icon("info-circle"), class = "help-text"),
                           div(class = "plot-container",
                               withSpinner(plotOutput("bivariateAdaptedFunnelPlot")),
                               p("Adapted funnel plot: Visualizes potential publication bias for the bivariate model using transformed effect sizes.", class = "plot-explanation")
                           ),
                           # verbatimTextOutput("bivariateBiasTestResults")
                  ),
                  tabPanel("Sensitivity Analysis",
                           actionButton("sensitivity_analysis_info", "", icon = icon("info-circle")),
                           fluidRow(
                             column(6, 
                                    div(class = "plot-container",
                                        withSpinner(plotlyOutput("confidenceRegionShiftPlot")),
                                        p("Confidence region shift plot: Visualizes how the confidence region changes with study removal.", class = "plot-explanation")
                                    )
                             ),
                             column(6,
                                    div(class = "plot-container",
                                        withSpinner(plotlyOutput("enhancedBaujatPlot")),
                                        p("Enhanced Baujat plot: Identifies influential studies in the bivariate model.", class = "plot-explanation")
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
      )
    )
  )
)