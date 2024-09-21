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
      fileInput("datafile", "Upload Data (csv)", accept = c(".csv")),
      selectInput("het_estimator", "Heterogeneity Estimator", choices = c("DL", "PM", "REML", "ML"), selected = "DL"),
      selectInput("effect_measure", "Effect Measure", choices = c("OR", "RR"), selected = "RR"),
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
                  column(6, actionButton("loadExampleData", "Load Example Dataset"))
                ),
                br(),
                DTOutput("dataPreview"),
                helpText("This tab displays the uploaded data. Review your data here to ensure it has been correctly loaded and formatted.")
      ),
      nav_panel("Overall Results",
                tabsetPanel(
                  tabPanel("Method Comparison", 
                           actionButton("method_comparison_info", "", icon = icon("info-circle"), class = "help-text"),
                           plotOutput("methodComparisonPlot"),
                           p("Method comparison plot: Compares results from different meta-analysis methods.", class = "plot-explanation"),
                           hr(),
                           h4("Summary"),
                           verbatimTextOutput("methodComparisonSummary")),
                  tabPanel("Summary Table", 
                           actionButton("summary_table_info", "", icon = icon("info-circle"), class = "help-text"),
                           tableOutput("overallSummaryTable")),
                  tabPanel("Overall Interpretation", 
                           actionButton("overall_interpretation_info", "", icon = icon("info-circle"), class = "help-text"),
                           verbatimTextOutput("overallInterpretation"))
                )
      ),
      nav_panel("Random Effects Analysis",
                tabsetPanel(
                  tabPanel("Effect Size and Heterogeneity", 
                           actionButton("re_effect_size_heterogeneity_info", "", icon = icon("info-circle"), class = "help-text"),
                           fluidRow(
                             column(6, 
                                    div(class = "plot-container",
                                        plotOutput("randomForestPlot"),
                                        p("Forest plot: Shows individual study effects and overall effect size with confidence intervals.", class = "plot-explanation")
                                    )
                             ),
                             column(6, 
                                    div(class = "plot-container",
                                        plotOutput("randomHeterogeneityPlot"),
                                        p("Heterogeneity plot: Visualizes the extent of heterogeneity among studies.", class = "plot-explanation")
                                    )
                             )
                           ),
                           verbatimTextOutput("randomOverallSummary"),
                           verbatimTextOutput("randomHeterogeneitySummary")
                  ),
                  tabPanel("Model Diagnostics",
                           actionButton("re_model_diagnostics_info", "", icon = icon("info-circle"), class = "help-text"),
                           fluidRow(
                             column(6, 
                                    div(class = "plot-container",
                                        plotOutput("randomQQPlot"),
                                        p("Q-Q plot: Assesses if effect sizes are normally distributed.", class = "plot-explanation")
                                    )
                             ),
                             column(6, 
                                    div(class = "plot-container",
                                        plotOutput("outlierDetectionPlot"),
                                        p("Outlier detection plot: Identifies potential outliers in the meta-analysis.", class = "plot-explanation")
                                    )
                             )
                           ),
                           div(class = "plot-container",
                               plotOutput("effectDistributionPlot"),
                               p("Effect distribution plot: Shows the distribution of effect sizes across studies.", class = "plot-explanation")
                           )
                  ),
                  tabPanel("Publication Bias",
                           actionButton("publication_bias_info", "", icon = icon("info-circle"), class = "help-text"),
                           fluidRow(
                             column(6, 
                                    div(class = "plot-container",
                                        plotOutput("randomFunnelPlot"),
                                        p("Funnel plot: Helps visualize potential publication bias.", class = "plot-explanation")
                                    )
                             ),
                             column(6, 
                                    div(class = "plot-container",
                                        plotOutput("randomTrimFillPlot"),
                                        p("Trim and fill plot: Adjusts for potential publication bias.", class = "plot-explanation")
                                    )
                             )
                           ),
                           verbatimTextOutput("randomEggerTestResults")
                  ),
                  tabPanel("Sensitivity Analysis",
                           actionButton("sensitivity_analysis_info", "", icon = icon("info-circle"), class = "help-text"),
                           fluidRow(
                             column(6, 
                                    div(class = "plot-container",
                                        plotOutput("leaveOneOutPlot"),
                                        p("Leave-one-out plot: Shows how overall effect changes when each study is removed.", class = "plot-explanation")
                                    )
                             ),
                             column(6, 
                                    div(class = "plot-container",
                                        plotOutput("baujatPlot"),
                                        p("Baujat plot: Identifies studies contributing to heterogeneity and influencing overall result.", class = "plot-explanation")
                                    )
                             )
                           ),
                           div(class = "plot-container",
                               plotOutput("randomGOSHPlot"),
                               p("GOSH plot: Visualizes the stability of meta-analysis results.", class = "plot-explanation")
                           ),
                           verbatimTextOutput("influenceSummary")
                  ),
                  tabPanel("Quality Assessment",
                           actionButton("quality_assessment_info", "", icon = icon("info-circle"), class = "help-text"),
                           verbatimTextOutput("randomGradeAssessment")
                  )
                )
      ),
      nav_panel("Fixed Effects Analysis",
                tabsetPanel(
                  tabPanel("Effect Size and Heterogeneity", 
                           div(class = "plot-container",
                               plotOutput("fixedForestPlot"),
                               p("Fixed effects forest plot: Shows individual and overall fixed effect sizes.", class = "plot-explanation")
                           ),
                           actionButton("fe_effect_size_heterogeneity_info", "", icon = icon("info-circle"), class = "help-text"),
                           verbatimTextOutput("fixedOverallSummary"),
                           verbatimTextOutput("modelFitStatistics")
                  ),
                  tabPanel("Model Diagnostics",
                           actionButton("fe_model_diagnostics_info", "", icon = icon("info-circle"), class = "help-text"),
                           fluidRow(
                             column(6, 
                                    div(class = "plot-container",
                                        plotOutput("fixedModelFitPlot"),
                                        p("Model fit plot: Visualizes the goodness of fit for the fixed effects model.", class = "plot-explanation")
                                    )
                             ),
                             column(6, 
                                    div(class = "plot-container",
                                        plotOutput("fixedQQPlot"),
                                        p("Q-Q plot: Assesses normality of residuals in fixed effects model.", class = "plot-explanation")
                                    )
                             )
                           ),
                           div(class = "plot-container",
                               plotOutput("fixedOutlierDetectionPlot"),
                               p("Outlier detection: Identifies potential outliers in fixed effects model.", class = "plot-explanation")
                           )
                  ),
                  tabPanel("Publication Bias",
                           actionButton("publication_bias_info", "", icon = icon("info-circle"), class = "help-text"),
                           fluidRow(
                             column(6, 
                                    div(class = "plot-container",
                                        plotOutput("fixedFunnelPlot"),
                                        p("Funnel plot: Assesses publication bias in fixed effects model.", class = "plot-explanation")
                                    )
                             ),
                             column(6, 
                                    div(class = "plot-container",
                                        plotOutput("fixedTrimFillPlot"),
                                        p("Trim and fill plot: Adjusts for potential publication bias in fixed effects model.", class = "plot-explanation")
                                    )
                             )
                           ),
                           verbatimTextOutput("fixedEggerTestResults")
                  ),
                  tabPanel("Sensitivity Analysis",
                           actionButton("sensitivity_analysis_info", "", icon = icon("info-circle"), class = "help-text"),
                           fluidRow(
                             column(6, 
                                    div(class = "plot-container",
                                        plotOutput("fixedLeaveOneOutPlot"),
                                        p("Leave-one-out plot: Shows sensitivity of fixed effects model to individual studies.", class = "plot-explanation")
                                    )
                             ),
                             column(6, 
                                    div(class = "plot-container",
                                        plotOutput("fixedInfluencePlot"),
                                        p("Influence plot: Identifies influential studies in the fixed effects model.", class = "plot-explanation")
                                    )
                             )
                           ),
                           verbatimTextOutput("fixedInfluenceSummary")
                  ),
                  tabPanel("Quality Assessment",
                           actionButton("quality_assessment_info", "", icon = icon("info-circle"), class = "help-text"),
                           verbatimTextOutput("fixedGradeAssessment")
                  )
                )
      ),
      nav_panel("Bivariate Approach",
                tabsetPanel(
                  tabPanel("Effect Size and Heterogeneity", 
                           actionButton("biv_effect_size_heterogeneity_info", "", icon = icon("info-circle"), class = "help-text"),
                           fluidRow(
                             column(6, 
                                    div(class = "plot-container",
                                        plotOutput("bivariateForestPlot"),
                                        p("Bivariate forest plot: Displays effect sizes for two outcomes simultaneously.", class = "plot-explanation")
                                    )
                             ),
                             column(6, 
                                    div(class = "plot-container",
                                        plotOutput("confidenceRegionPlot"),
                                        p("Confidence region plot: Shows the joint confidence region for the two outcomes.", class = "plot-explanation")
                                    )
                             )
                           ),
                           verbatimTextOutput("bivariateOverallSummary")
                  ),
                  tabPanel("Model Diagnostics",
                           actionButton("biv_model_diagnostics_info", "", icon = icon("info-circle"), class = "help-text"),
                           fluidRow(
                             column(6, 
                                    div(class = "plot-container",
                                        plotOutput("qqPlotMu"),
                                        p("Q-Q plot (μ): Assesses normality of residuals for the first outcome.", class = "plot-explanation")
                                    )
                             ),
                             column(6, 
                                    div(class = "plot-container",
                                        plotOutput("qqPlotTau"),
                                        p("Q-Q plot (τ): Assesses normality of residuals for the second outcome.", class = "plot-explanation")
                                    )
                             )
                           ),
                           div(class = "plot-container",
                               plotOutput("efficacyHarmPlot"),
                               p("Efficacy-harm plot: Visualizes the relationship between efficacy and harm outcomes.", class = "plot-explanation")
                           )
                  ),
                  tabPanel("Publication Bias",
                           actionButton("publication_bias_info", "", icon = icon("info-circle"), class = "help-text"),
                           div(class = "plot-container",
                               plotOutput("bivariateAdaptedFunnelPlot"),
                               p("Adapted funnel plot: Visualizes potential publication bias in the bivariate context.", class = "plot-explanation")
                           ),
                           verbatimTextOutput("bivariateBiasTestResults")
                  ),
                  tabPanel("Sensitivity Analysis",
                           actionButton("sensitivity_analysis_info", "", icon = icon("info-circle"), class = "help-text"),
                           fluidRow(
                             column(6, 
                                    div(class = "plot-container",
                                        plotlyOutput("confidenceRegionShiftPlot"),
                                        p("Confidence region shift plot: Visualizes how the confidence region changes with study removal.", class = "plot-explanation")
                                    )
                             ),
                             column(6, 
                                    div(class = "plot-container",
                                        plotlyOutput("enhancedBaujatPlot"),
                                        p("Enhanced Baujat plot: Shows each study's contribution to heterogeneity and influence on results.", class = "plot-explanation")
                                    )
                             )
                           ),
                           div(class = "plot-container",
                               plotlyOutput("bivariateGOSHPlot"),
                               p("Bivariate GOSH plot: Assesses stability of results in the bivariate model.", class = "plot-explanation")
                           ),
                           verbatimTextOutput("bivariateInfluenceSummary")
                  ),
                  tabPanel("Quality Assessment",
                           actionButton("quality_assessment_info", "", icon = icon("info-circle"), class = "help-text"),
                           fluidRow(
                             column(6,
                                    selectInput("risk_of_bias", "Risk of Bias:",
                                                choices = c("Low", "Unclear", "High"),
                                                selected = "Unclear")
                             ),
                             column(6,
                                    selectInput("indirectness", "Indirectness:",
                                                choices = c("Low", "Unclear", "High"),
                                                selected = "Low")
                             )
                           ),
                           verbatimTextOutput("bivariateGRADESummary")
                  )
                )
      )
    )
  )
)