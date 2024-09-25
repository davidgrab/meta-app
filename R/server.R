#install.packages(c("shiny", "DT", "meta", "ggplot2", "plotly", "metafor", "BiasedUrn","DTO"))
#list.of.packages <- c("shiny", "meta", "metafor", "ggplot2", "plotly", "DT", "httr", "jsonlite", "bslib", "sf", "MASS","httr","shinyjs")
#install.packages(list.of.packages )
#install.packages(c("bslib", "shinyjs"))
#install.packages("readxl")

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
library(readxl)
library(httr)
library(jsonlite)
library(sf)
library(MASS)
library(grDevices)
library(grid)
library(rmarkdown)
library(knitr)



source("R/ui.R")
# Source external function files
source("R/functions.R")  # Make sure to update this file with new functions
source("R/bivariate_meta.R")# 

# server.R
server <- function(input, output, session) {
  print("Server function started")
  

  
  print("Functions sourced")
  
  # Initially hide all tabs except Data Preview
  observe({
    print("Hiding tabs")
    lapply(c("Overall Results", "Random Effects Analysis", "Fixed Effects Analysis", "Bivariate Approach"), function(tab) {
      hideTab(inputId = "main_tabs", target = tab)
    })
  })
  
  # Show all tabs when Analyze button is clicked
  observeEvent(input$analyze, {
    print("Analyze button clicked")
    lapply(c("Overall Results", "Random Effects Analysis", "Fixed Effects Analysis", "Bivariate Approach"), function(tab) {
      showTab(inputId = "main_tabs", target = tab)
    })
  })
  
  # React to dark mode toggle
  observeEvent(input$dark_mode, {
    print(paste("Dark mode toggled:", input$dark_mode))
    if (input$dark_mode == "light") {
      showNotification("Welcome to the light side!")
      # session$setCurrentTheme(
      #   #bs_theme(version = 5, bootswatch = "vapor")
      #   light_theme
      # )
    }else {
      showNotification("Welcome to the dark side!")
      #session$setCurrentTheme(
      #bs_theme(version = 5, bootswatch = "flatly")
      #dark_theme
      #)
      
    }
  })
  
  
  
  # App Info popup
  observeEvent(input$info, {
    print("Info button clicked")
    showModal(modalDialog(
      title = "About this App",
      "This app performs advanced meta-analysis with GRADE assessment. Upload your data, choose analysis options, and explore the results across different models.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  showInfoModal <- function(title, message) {
    showModal(modalDialog(
      title = title,
      p(message),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  }
  
  # Data Upload Info popup
  observeEvent(input$data_info, {
    print("Data info button clicked")
    showModal(modalDialog(
      title = "How to Upload Data",
      HTML(
        paste0(
          "1. Prepare your CSV file with columns: study, ie (intervention events), it (intervention total), pe (placebo events), pt (placebo total).<br><br>",
          "2. Click 'Browse' in the 'Upload Data (CSV)' section.<br><br>",
          "3. Select your prepared CSV file.<br><br>",
          "4. The data will automatically load and display in the 'Data Preview' tab."
        )
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Data loading
  # Modified data reactive
  data <- reactive({
    req(currentData())
    df <- currentData()
    if (input$remove_na) {
      df <- na.omit(df)
    }
    names(df) <- c("study", "ie", "it", "pe", "pt")
    df
  })
  
  # New download
  output$downloadSampleStructure <- downloadHandler(
    filename = function() {
      "sample_data_structure.csv"
    },
    content = function(file) {
      write.csv(sampleDataStructure, file, row.names = FALSE)
    }
  )
  
  # New reactive value
  currentData <- reactiveVal(NULL)
  
  # New observe events
  observeEvent(input$loadExampleData, {
    currentData(exampleData)
  })
  
  observeEvent(input$datafile, {
    req(input$datafile)
    currentData(read.csv(input$datafile$datapath, stringsAsFactors = FALSE))
  })
  
  output$dataPreview <- renderDT({
    print("Rendering data preview")
    data()
  })
  
  # New data structures
  sampleDataStructure <- data.frame(
    study = character(),
    Intervention_effected = integer(),
    Intervention_total = integer(),
    Placebo_effected = integer(),
    Placebo_total = integer(),
    stringsAsFactors = FALSE
  )
  
  # Example dataset
  exampleData <- read.csv(text = "study,Intervention_effected,Intervention_total,Placebo_effected,Placebo_total
1,22,54,21,55
2,17,45,9,43
3,35,53,12,55
4,23,37,15,35
5,24,49,16,49
6,4,20,11,26
7,45,80,12,79
8,26,98,19,102
9,41,70,4,70
10,64,109,48,109
11,71,131,51,130
12,46,113,56,116
13,159,243,26,81
14,98,186,80,189
15,55,123,57,124
16,67,106,22,47
17,46,70,34,70
18,34,48,25,49")
  
  # Combined analysis
  combinedResults <- eventReactive(input$analyze, {
    req(data())
    
    random_model <- metabin(event.e = data()$ie, 
                            n.e = data()$it,
                            event.c = data()$pe,
                            n.c = data()$pt,
                            studlab = data()$study,
                            sm = input$effect_measure, 
                            method.tau = input$het_estimator,
                            fixed = FALSE,
                            random = TRUE)
    
    fixed_model <- metabin(event.e = data()$ie, 
                           n.e = data()$it,
                           event.c = data()$pe,
                           n.c = data()$pt,
                           studlab = data()$study,
                           sm = input$effect_measure, 
                           fixed = TRUE,
                           random = FALSE)
    
    bivariate_model <- metabiv(event.e = data()$ie, 
                               n.e = data()$it, 
                               event.c = data()$pe, 
                               n.c = data()$pt,
                               studlab = data()$study,
                               sm = input$effect_measure)
    
    list(random = random_model, fixed = fixed_model, bivariate = bivariate_model)
  })  
  # Overall Results Tab
  
  # output$methodComparisonPlot <- renderPlot({
  #   req(combinedResults())
  #   method_comparison_plot(combinedResults()$random, 
  #                          combinedResults()$fixed, 
  #                          combinedResults()$bivariate)
  # })
  
  
  output$overallSummaryTable <- renderTable({
    req(combinedResults())
    compare_models(combinedResults())
  })
  
  output$overallInterpretation <- renderText({
    req(combinedResults())
    interpret_results(combinedResults())
  })
  
  # Random Effects Analysis Tab
  
  output$randomForestPlot <- renderPlot({
    req(combinedResults()$random)
    forest(combinedResults()$random, 
           leftlabs = c("Study", "TE", "seTE"),
           rightlabs = c("TE", "95%-CI", "Weight"),
           fontsize = 10, 
           xlab = paste("Effect Size (", combinedResults()$random$sm, ")", sep=""))
  })
  
  output$randomHeterogeneityPlot <- renderPlot({
    req(combinedResults()$random)
    heterogeneity_plot(combinedResults()$random)
  })
  
  output$randomOverallSummary <- renderPrint({
    req(combinedResults()$random)
    summary(combinedResults()$random)
  })
  
  output$leaveOneOutPlot <- renderPlot({
    req(combinedResults()$random)
    inf <- metainf(combinedResults()$random)
    forest(inf, 
           leftlabs = c("Omitted Study", "Effect Size", "95% CI"),
           xlab = "Effect Size",
           title = "Leave-One-Out Analysis")
  })
  
  output$baujatPlot <- renderPlot({
    req(combinedResults()$random)
    baujat(combinedResults()$random)
  })
  
  output$influenceSummary <- renderPrint({
    req(combinedResults()$random)
    influence_analysis(combinedResults()$random)
  })
  
  output$randomQQPlot <- renderPlot({
    req(combinedResults())
    random_model <- combinedResults()$random
    
    # Calculate standardized residuals
    residuals <- calculate_random_residuals(random_model)
    
    # Check if we have valid residuals
    if (!is.null(residuals) && !all(is.na(residuals))) {
      qqnorm(residuals, main = "Q-Q Plot of Random Effects Standardized Residuals")
      qqline(residuals, col = "red")
    } else {
      plot(1, type = "n", xlab = "", ylab = "", main = "Q-Q Plot Unavailable")
      text(1, 1, "Insufficient data for Q-Q plot", cex = 1.2)
    }
  })
  
  output$outlierDetectionPlot <- renderPlot({
    req(combinedResults()$random)
    outlier_detection_plot(combinedResults()$random)
  })
  
  output$effectDistributionPlot <- renderPlot({
    req(combinedResults()$random)
    effect_distribution_plot(combinedResults()$random)
  })
  
  output$randomGOSHPlot <- renderPlot({
    req(combinedResults()$random)
    gosh_plot(combinedResults()$random)
  })
  
  output$randomFunnelPlot <- renderPlot({
    req(combinedResults()$random)
    funnel(combinedResults()$random)
  })
  
  output$randomEggerTestResults <- renderPrint({
    req(combinedResults()$random)
    metabias(combinedResults()$random, method = "Egger")
  })
  
  output$randomTrimFillPlot <- renderPlot({
    req(combinedResults()$random)
    tf <- trimfill(combinedResults()$random)
    funnel(tf, 
           studlab = FALSE,
           xlab = combinedResults()$random$sm,
           ylab = "Standard Error",
           main = "Trim and Fill Funnel Plot")
  })
  
  output$randomGradeAssessment <- renderPrint({
    req(combinedResults()$random)
    grade_assessment(combinedResults()$random, "Random Effects")
  })
  
  
  
  # Fixed Effects Analysis Tab
  
  output$fixedForestPlot <- renderPlot({
    req(combinedResults()$fixed)
    forest(combinedResults()$fixed, 
           leftlabs = c("Study", "TE", "seTE"),
           rightlabs = c("TE", "95%-CI", "Weight"),
           fontsize = 10, 
           xlab = paste("Effect Size (", combinedResults()$fixed$sm, ")", sep=""))
  })
  
  output$fixedModelFitPlot <- renderPlot({
    req(combinedResults()$fixed)
    radial(combinedResults()$fixed)
  })
  
  output$fixedOverallSummary <- renderPrint({
    req(combinedResults()$fixed)
    summary(combinedResults()$fixed)
  })
  
  output$modelFitStatistics <- renderPrint({
    req(combinedResults()$fixed)
    model_fit_statistics(combinedResults()$fixed)
  })
  
  output$fixedLeaveOneOutPlot <- renderPlot({
    req(combinedResults()$fixed)
    inf <- metainf(combinedResults()$fixed)
    forest(inf, 
           leftlabs = c("Omitted Study", "Effect Size", "95% CI"),
           xlab = "Effect Size",
           title = "Leave-One-Out Analysis")
  })
  
  output$fixedInfluencePlot <- renderPlot({
    req(combinedResults()$fixed)
    baujat(combinedResults()$fixed)
  })
  
  output$fixedInfluenceSummary <- renderPrint({
    req(combinedResults()$fixed)
    influence_analysis(combinedResults()$fixed)
  })
  
  output$fixedQQPlot <- renderPlot({
    req(combinedResults())
    fixed_model <- combinedResults()$fixed
    
    # Calculate residuals
    residuals <- calculate_residuals(fixed_model)
    
    # Check if we have valid residuals
    if (!is.null(residuals) && !all(is.na(residuals))) {
      qqnorm(residuals, main = "Q-Q Plot of Fixed Effects Residuals")
      qqline(residuals, col = "red")
    } else {
      plot(1, type = "n", xlab = "", ylab = "", main = "Q-Q Plot Unavailable")
      text(1, 1, "Insufficient data for Q-Q plot", cex = 1.2)
    }
  })
  
  output$fixedOutlierDetectionPlot <- renderPlot({
    req(combinedResults()$fixed)
    outlier_detection_plot(combinedResults()$fixed)
  })
  
  output$fixedFunnelPlot <- renderPlot({
    req(combinedResults()$fixed)
    funnel(combinedResults()$fixed)
  })
  
  output$fixedTrimFillPlot <- renderPlot({
    req(combinedResults()$fixed)
    tf <- trimfill(combinedResults()$fixed)
    funnel(tf, 
           studlab = FALSE,
           xlab = combinedResults()$fixed$sm,
           ylab = "Standard Error",
           main = "Trim and Fill Funnel Plot")
  })
  
  output$fixedEggerTestResults <- renderPrint({
    req(combinedResults()$fixed)
    metabias(combinedResults()$fixed, method = "Egger")
  })
  
  output$fixedGradeAssessment <- renderPrint({
    req(combinedResults()$fixed)
    grade_assessment(combinedResults()$fixed, "Fixed Effects")
  })
  
  
  output$fixedSummaryText <- renderPrint({
    print("Rendering fixed summary text")
    req(fixedEffectsResult())
    result <- fixedEffectsResult()
    cat("Fixed Effects Model Summary:\n\n")
    cat("Overall effect size (OR):", exp(result$TE.fixed), "\n")
    cat("95% CI:", exp(result$lower.fixed), "to", exp(result$upper.fixed), "\n")
    cat("p-value:", result$pval.fixed, "\n\n")
    cat("Heterogeneity:\n")
    cat("I^2 =", result$I2, "%\n")
    cat("Q =", result$Q, "(p =", result$pval.Q, ")\n\n")
    cat("Interpretation: [Add your interpretation here based on the results]\n")
  })
  
  
  
  # Bivariate analysis results
  bivariate_result <- eventReactive(input$analyze, {
    req(data())
    metabiv(event.e = data()$ie, 
            n.e = data()$it, 
            event.c = data()$pe, 
            n.c = data()$pt,
            studlab = data()$study,
            sm = input$effect_measure)
  })
  
  # Bivariate Forest Plot
  output$bivariateForestPlot <- renderPlot({
    req(bivariate_result())
    forest.metabiv(bivariate_result())
  })
  
  # Confidence Region Plot
  output$confidenceRegionPlot <- renderPlot({
    req(bivariate_result())
    plot.mu.tau.CI(bivariate_result()$dev_pvals[[1]], 
                   bivariate_result()$dev_pvals[[2]], 
                   mlb = "Confidence Region for (μ, τ)")
  })
  
  # Bivariate Overall Summary
  output$bivariateOverallSummary <- renderPrint({
    req(bivariate_result())
    summary(bivariate_result())
  })
  
  # Confidence Region Shift Plot
  output$confidenceRegionShiftPlot <- renderPlotly({
    req(bivariate_result())
    confidence_region_shift_plot(bivariate_result())
  })
  
  # Enhanced Baujat Plot
  output$enhancedBaujatPlot <- renderPlotly({
    req(bivariate_result())
    influence <- lapply(1:length(bivariate_result()$y.k), function(i) {
      res_i <- metabiv(event.e = data()$ie[-i], 
                       n.e = data()$it[-i], 
                       event.c = data()$pe[-i], 
                       n.c = data()$pt[-i],
                       studlab = data()$study[-i],
                       sm = input$effect_measure)
      c(contribution = (bivariate_result()$y.k[i] - res_i$mu)^2 / (bivariate_result()$sigma.2.k[i] + res_i$tau^2),
        influence = (bivariate_result()$y.k[i] - res_i$mu)^2 / (bivariate_result()$sigma.2.k[i] + res_i$tau^2)^2)
    })
    influence_df <- do.call(rbind, influence)
    plot_ly(x = influence_df[,1], y = influence_df[,2], text = bivariate_result()$studlab,
            type = "scatter", mode = "markers+text") %>%
      layout(title = "Enhanced Baujat Plot",
             xaxis = list(title = "Contribution to Q"),
             yaxis = list(title = "Influence on pooled estimate"))
  })
  
  # Bivariate Influence Summary
  output$bivariateInfluenceSummary <- renderPrint({
    req(bivariate_result())
    cat("Bivariate Influence Summary\n")
    influence <- lapply(1:length(bivariate_result()$y.k), function(i) {
      res_i <- metabiv(event.e = data()$ie[-i], 
                       n.e = data()$it[-i], 
                       event.c = data()$pe[-i], 
                       n.c = data()$pt[-i],
                       studlab = data()$study[-i],
                       sm = input$effect_measure)
      c(mu_change = res_i$mu - bivariate_result()$mu,
        tau_change = res_i$tau - bivariate_result()$tau)
    })
    influence_df <- do.call(rbind, influence)
    rownames(influence_df) <- bivariate_result()$studlab
    print(influence_df)
  })
  
  # Q-Q Plot for μ
  output$qqPlotMu <- renderPlot({
    req(bivariate_result())
    qqnorm(bivariate_result()$y.k, main = "Q-Q Plot for μ")
    qqline(bivariate_result()$y.k, col = "red")
  })
  
  # Q-Q Plot for τ
  output$qqPlotTau <- renderPlot({
    req(bivariate_result())
    qqnorm(sqrt(bivariate_result()$sigma.2.k), main = "Q-Q Plot for τ")
    qqline(sqrt(bivariate_result()$sigma.2.k), col = "red")
  })
  
  # Efficacy-Harm Plot
  output$efficacyHarmPlot <- renderPlot({
    req(bivariate_result())
    CDF.ci.obj <- comp.mu.tau.dev.CDF.CI(bivariate_result()$dev_pvals)
    comp.eff.harm.plot(CDF.ci.obj,
                       efficacy.is.OR.le1 = (bivariate_result()$sm == "OR"),
                       mlb = paste("Efficacy/Harm plot for", bivariate_result()$sm))
  })
  
  # Bivariate GOSH Plot
  output$bivariateGOSHPlot <- renderPlotly({
    req(bivariate_result())
    # Implement GOSH plot
    subsets <- replicate(1000, sample(1:nrow(data()), size = nrow(data())/2, replace = FALSE))
    gosh_results <- apply(subsets, 2, function(subset) {
      res <- metabiv(event.e = data()$ie[subset], 
                     n.e = data()$it[subset], 
                     event.c = data()$pe[subset], 
                     n.c = data()$pt[subset],
                     studlab = data()$study[subset],
                     sm = input$effect_measure)
      c(mu = res$mu, tau = res$tau)
    })
    gosh_df <- as.data.frame(t(gosh_results))
    plot_ly(data = gosh_df, x = ~mu, y = ~tau, type = "scatter", mode = "markers",
            marker = list(size = 3, opacity = 0.5)) %>%
      layout(title = "GOSH Plot",
             xaxis = list(title = "μ"),
             yaxis = list(title = "τ"))
  })
  
  # Bivariate Adapted Funnel Plot
  output$bivariateAdaptedFunnelPlot <- renderPlot({
    req(bivariate_result())
    # Assuming bivariate_result() returns a list with y.k and sigma.2.k
    y.k <- exp(bivariate_result()$y.k)
    se.k <- sqrt(bivariate_result()$sigma.2.k) # Standard errors
    sm<-bivariate_result()$sm
    # Create a meta-analysis object
    meta_analysis <- metagen(TE = y.k, seTE = se.k)
    funnel(meta_analysis, sm = sm, xlab = "Effect Size",
           ylab = "Standard Error")
    
    # plot_ly(x = bivariate_result()$y.k, y = sqrt(bivariate_result()$sigma.2.k), type = "scatter", mode = "markers") %>%
    #   add_trace(x = bivariate_result()$mu, y = 0, type = "scatter", mode = "markers",
    #             marker = list(size = 10, color = "red", symbol = "diamond")) %>%
    #   layout(title = "Adapted Funnel Plot",
    #          xaxis = list(title = "Effect Size"),
    #          yaxis = list(title = "Standard Error", autorange = "reversed"))
  })
  
  # Bivariate Bias Test Results
  output$bivariateBiasTestResults <- renderPrint({
    req(bivariate_result())
    cat("Bivariate Bias Test Results\n")
    # Implement Egger's test for the bivariate case
    egger_test <- lm(bivariate_result()$y.k ~ sqrt(bivariate_result()$sigma.2.k))
    print(summary(egger_test))
    cat("\nInterpretation: If the intercept is significantly different from zero,\n",
        "this may indicate the presence of small-study effects or publication bias.\n")
  })
  
  # Bivariate GRADE Summary
  output$bivariateGRADESummary <- renderPrint({
    req(bivariate_result(), input$risk_of_bias, input$indirectness)
    cat("Bivariate GRADE Assessment\n\n")
    cat("Risk of Bias:", input$risk_of_bias, "\n")
    cat("Indirectness:", input$indirectness, "\n")
    cat("Inconsistency:", ifelse(bivariate_result()$tau > 0.5, "High", 
                                 ifelse(bivariate_result()$tau > 0.3, "Moderate", "Low")), "\n")
    cat("Imprecision:", ifelse(abs(bivariate_result()$mu) / bivariate_result()$tau < 2, "High", "Low"), "\n")
    cat("Publication Bias:", ifelse(abs(coef(lm(bivariate_result()$y.k ~ sqrt(bivariate_result()$sigma.2.k)))[1]) > 0.1, "Suspected", "Not suspected"), "\n\n")
    
    grade_score <- 4  # Start with high quality
    if (input$risk_of_bias == "High") grade_score <- grade_score - 1
    if (input$indirectness == "High") grade_score <- grade_score - 1
    if (bivariate_result()$tau > 0.5) grade_score <- grade_score - 2
    else if (bivariate_result()$tau > 0.3) grade_score <- grade_score - 1
    if (abs(bivariate_result()$mu) / bivariate_result()$tau < 2) grade_score <- grade_score - 1
    if (abs(coef(lm(bivariate_result()$y.k ~ sqrt(bivariate_result()$sigma.2.k)))[1]) > 0.1) grade_score <- grade_score - 1
    
    cat("Overall GRADE:", switch(as.character(max(0, grade_score)),
                                 "4" = "High",
                                 "3" = "Moderate",
                                 "2" = "Low",
                                 "1" = "Very Low",
                                 "0" = "Very Low"), "\n")
  })
  
  # Method Comparison Plot
  output$methodComparisonPlot <- renderPlot({
    req(bivariate_result())
    # Traditional meta-analysis (random effects)
    trad_meta_random <- metabin(event.e = data()$ie, 
                                n.e = data()$it, 
                                event.c = data()$pe, 
                                n.c = data()$pt,
                                studlab = data()$study,
                                sm = input$effect_measure,
                                method = "Inverse",
                                fixed = FALSE,
                                random = TRUE)
    
    # Traditional meta-analysis (fixed effects)
    trad_meta_fixed <- metabin(event.e = data()$ie, 
                               n.e = data()$it, 
                               event.c = data()$pe, 
                               n.c = data()$pt,
                               studlab = data()$study,
                               sm = input$effect_measure,
                               method = "Inverse",
                               fixed = TRUE,
                               random = FALSE)
    
    # Combine results
    results <- data.frame(
      Method = c("Fixed Effects", "Random Effects", "Bivariate"),
      Estimate = c(trad_meta_fixed$TE.fixed, trad_meta_random$TE.random, bivariate_result()$mu),
      Lower = c(trad_meta_fixed$lower.fixed, trad_meta_random$lower.random, bivariate_result()$mu - qnorm(0.975)*bivariate_result()$tau),
      Upper = c(trad_meta_fixed$upper.fixed, trad_meta_random$upper.random, bivariate_result()$mu + qnorm(0.975)*bivariate_result()$tau)
    )
    
    
    # Create forest plot
    forest(metagen(TE = Estimate, 
                   lower = Lower, 
                   upper = Upper, 
                   studlab = Method, 
                   data = results,
                   sm = input$effect_measure),
           leftlabs = c("Method", "Estimate", "95% CI"),
           xlab = paste("Effect Size (", input$effect_measure, ")", sep=""),
           weight.study = "same",
           prediction = FALSE,
           print.tau2 = FALSE,
           print.I2 = FALSE,
           hetstat= FALSE,
           overall= FALSE)
  })
  
  # App Info
  observeEvent(input$app_info, {
    showModal(modalDialog(
      title = "Comprehensive Meta-Analysis App: User Guide",
      HTML(paste0(
        "This app provides a comprehensive tool for conducting and interpreting meta-analyses. Here's how to use it:<br><br>",
        
        "<b>1. Data Input:</b><br>",
        "- Upload your data CSV file using the 'Upload Data' button in the sidebar<br>",
        "- Ensure your data includes columns for study name, intervention events, intervention total, placebo events, and placebo total<br>",
        "- You can use the 'Load Example Dataset' button to see the required format<br><br>",
        
        "<b>2. Analysis Settings:</b><br>",
        "- Choose your heterogeneity estimator and effect measure in the sidebar<br>",
        "- Click 'Analyze' to run the meta-analysis<br><br>",
        
        "<b>3. Results Tabs:</b><br>",
        "- <i>Data Preview:</i> Check your uploaded data<br>",
        "- <i>Overall Results:</i> Compare results across all methods<br>",
        "- <i>Random Effects Analysis:</i> Detailed random effects model results<br>",
        "- <i>Fixed Effects Analysis:</i> Detailed fixed effects model results<br>",
        "- <i>Bivariate Approach:</i> Results from the bivariate analysis<br><br>",
        
        "<b>4. Key Features in Each Analysis Tab:</b><br>",
        "- Effect size and heterogeneity assessment<br>",
        "- Model diagnostics<br>",
        "- Publication bias evaluation<br>",
        "- Sensitivity analysis<br>",
        "- Quality assessment (GRADE)<br><br>",
        
        "<b>5. Generating Reports:</b><br>",
        "- Use the 'Download Report' button to create a comprehensive HTML report of all analyses<br><br>",
        
        "Throughout the app, look for info buttons (?) for additional guidance on interpreting results and using features.<br><br>",
        
        "This tool is designed to help researchers thoroughly examine their meta-analytic data and draw robust conclusions from their analyses."
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  
  # Data Preview Info
  observeEvent(input$data_preview_info, {
    showModal(modalDialog(
      title = "Data Preview",
      HTML(paste0(
        "This tab displays your uploaded data. Proper data formatting is crucial for accurate meta-analysis results.<br><br>",
        "Key points:<br>",
        "1. Review your data carefully to ensure correct loading and formatting.<br>",
        "2. The data should include columns for:<br>",
        "   - Study identifiers<br>",
        "   - Effect sizes<br>",
        "   - Standard errors or confidence intervals<br><br>",
        "Expected column names:<br>",
        "- study: Study identifier<br>",
        "- ie: Intervention events<br>",
        "- it: Intervention total<br>",
        "- pe: Placebo/control events<br>",
        "- pt: Placebo/control total<br><br>",
        "If your data doesn't match this format, please reformat it before proceeding with the analysis."
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Method Comparison Info
  observeEvent(input$method_comparison_info, {
    showModal(modalDialog(
      title = "Method Comparison",
      HTML(paste0(
        "The Method Comparison Plot visually compares results from different meta-analysis methods:<br><br>",
        "- Random Effects<br>",
        "- Fixed Effects<br>",
        "- Bivariate Approach<br><br>",
        "This plot shows effect size estimates and their confidence intervals for each method, allowing you to easily see differences in results across approaches.<br><br>",
        "Key points to consider:<br>",
        "1. Consistency across methods<br>",
        "2. Width of confidence intervals<br>",
        "3. Direction and magnitude of effects"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$combined_forest_info, {
    showInfoModal("Combined Forest Plot", 
                  "This forest plot shows results from all methods, allowing for a visual comparison of effect sizes and confidence intervals. Each study is represented by a box (size proportional to study weight) and a horizontal line (95% CI). The overall effect for each method is shown as a diamond at the bottom. This plot helps visualize the consistency of results across different meta-analytic approaches.")
  })
  
  # Summary Table Info
  observeEvent(input$summary_table_info, {
    showModal(modalDialog(
      title = "Summary Table",
      HTML(paste0(
        "The Summary Table provides a numerical comparison of key statistics across all methods:<br><br>",
        "- Effect size estimates<br>",
        "- Confidence intervals<br>",
        "- Heterogeneity measures (I², τ²)<br>",
        "- Model fit statistics (where applicable)<br><br>",
        "This table allows for a quick comparison of results from different meta-analytic methods, helping you identify similarities and differences in outcomes across approaches."
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$overall_interpretation_info, {
    showInfoModal("Overall Interpretation", 
                  "This section provides an overall interpretation of the meta-analysis results, highlighting key findings and differences between methods. It discusses the consistency of results across methods and potential implications of any discrepancies. This helps in drawing overall conclusions from the meta-analysis.")
  })
  
  # Random Effects Overall Results Info
  observeEvent(input$random_overall_info, {
    showModal(modalDialog(
      title = "Random Effects: Overall Results and Heterogeneity",
      HTML(paste0(
        "This section presents key results from the random effects meta-analysis:<br><br>",
        "1. Forest Plot:<br>",
        "   - Shows individual study effects and the overall effect size<br>",
        "   - Confidence intervals for each study and the pooled effect<br>",
        "   - Study weights indicated by box sizes<br><br>",
        "2. Heterogeneity Plot:<br>",
        "   - Visualizes the extent of between-study variability<br>",
        "   - Helps identify studies contributing most to overall heterogeneity<br><br>",
        "3. Summary Statistics:<br>",
        "   - Overall effect size and its confidence interval<br>",
        "   - Heterogeneity measures (I², τ², Q statistic)<br>",
        "   - P-value for the test of heterogeneity"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Random Effects Sensitivity and Influence Info
  observeEvent(input$random_sensitivity_info, {
    showModal(modalDialog(
      title = "Random Effects: Sensitivity and Influence",
      HTML(paste0(
        "This section presents analyses to assess the robustness of results and identify influential studies:<br><br>",
        "1. Leave-One-Out Plot:<br>",
        "   - Shows how the overall effect changes when each study is removed<br>",
        "   - Helps identify studies with a large impact on the pooled result<br><br>",
        "2. Baujat Plot:<br>",
        "   - Visualizes each study's contribution to overall heterogeneity and influence on the pooled result<br>",
        "   - X-axis: Contribution to heterogeneity<br>",
        "   - Y-axis: Influence on pooled result<br>",
        "   - Studies in the upper-right corner are most influential<br><br>",
        "3. Influence Summary:<br>",
        "   - Provides numerical details on the influence of each study<br>",
        "   - Includes changes in effect size, heterogeneity, and other key statistics when each study is omitted"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Random Effects Residuals and Effect Distribution Info
  observeEvent(input$random_residuals_info, {
    showModal(modalDialog(
      title = "Random Effects: Residuals and Effect Distribution",
      HTML(paste0(
        "This section helps assess model assumptions and effect size distribution:<br><br>",
        "1. Q-Q Plot:<br>",
        "   - Assesses whether effect sizes are normally distributed<br>",
        "   - Points should roughly follow the diagonal line for normality<br><br>",
        "2. Outlier Detection Plot:<br>",
        "   - Identifies potential outliers based on standardized residuals<br>",
        "   - Studies outside the dashed lines may be considered outliers<br><br>",
        "3. Effect Distribution Plot:<br>",
        "   - Histogram showing the distribution of effect sizes across studies<br>",
        "   - Helps visualize the central tendency and spread of effects<br>",
        "   - Can indicate skewness or multi-modality in effect sizes"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Random Effects Stability and Publication Bias Info
  observeEvent(input$random_stability_info, {
    showModal(modalDialog(
      title = "Random Effects: Stability and Publication Bias",
      HTML(paste0(
        "This section assesses the stability of results and potential publication bias:<br><br>",
        "1. GOSH Plot:<br>",
        "   - Graphical Display of Study Heterogeneity<br>",
        "   - Shows stability of results across different subsets of studies<br>",
        "   - Clustering suggests robust results<br><br>",
        "2. Funnel Plot:<br>",
        "   - Helps visualize potential publication bias<br>",
        "   - X-axis: Effect size; Y-axis: Standard error or precision<br>",
        "   - Asymmetry may indicate presence of bias<br><br>",
        "3. Egger's Test Results:<br>",
        "   - Statistical test for funnel plot asymmetry<br>",
        "   - Significant p-value suggests potential publication bias<br><br>",
        "4. Trim and Fill Plot:<br>",
        "   - Adjusts for potential publication bias by imputing missing studies<br>",
        "   - Provides an adjusted effect size estimate"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Random Effects Quality Assessment Info
  observeEvent(input$random_quality_info, {
    showModal(modalDialog(
      title = "Random Effects: Quality Assessment",
      HTML(paste0(
        "This section provides an overall assessment of the evidence quality:<br><br>",
        "1. GRADE Assessment:<br>",
        "   - Grading of Recommendations, Assessment, Development and Evaluations<br>",
        "   - Evaluates the quality of evidence based on several factors:<br>",
        "     a) Risk of bias<br>",
        "     b) Inconsistency<br>",
        "     c) Indirectness<br>",
        "     d) Imprecision<br>",
        "     e) Publication bias<br><br>",
        "2. Overall Interpretation:<br>",
        "   - Summarizes the main findings of the random effects meta-analysis<br>",
        "   - Considers effect size, heterogeneity, and quality of evidence<br>",
        "   - Provides context for clinical or practical significance of results"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Fixed Effects Overall Results Info
  observeEvent(input$fixed_overall_info, {
    showModal(modalDialog(
      title = "Fixed Effects: Overall Results and Model Fit",
      HTML(paste0(
        "This section presents key results from the fixed effects meta-analysis:<br><br>",
        "1. Forest Plot:<br>",
        "   - Shows individual study effects and the overall fixed effect size<br>",
        "   - Confidence intervals for each study and the pooled effect<br>",
        "   - Study weights indicated by box sizes<br><br>",
        "2. Model Fit Plot:<br>",
        "   - Visualizes the goodness of fit for the fixed effects model<br>",
        "   - Helps assess whether the fixed effects assumption is appropriate<br><br>",
        "3. Summary Statistics:<br>",
        "   - Overall fixed effect size and its confidence interval<br>",
        "   - Model fit statistics (e.g., Q statistic, p-value)<br><br>",
        "4. Model Fit Statistics:<br>",
        "   - Detailed numerical assessment of model fit<br>",
        "   - Includes Q statistic, degrees of freedom, and p-value"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Fixed Effects Sensitivity and Influence Info
  observeEvent(input$fixed_sensitivity_info, {
    showModal(modalDialog(
      title = "Fixed Effects: Sensitivity and Influence",
      HTML(paste0(
        "This section assesses the robustness of fixed effects results:<br><br>",
        "1. Leave-One-Out Plot:<br>",
        "   - Shows how the overall effect changes when each study is removed<br>",
        "   - Helps identify influential studies in the fixed effects context<br><br>",
        "2. Influence Plot:<br>",
        "   - Visualizes each study's influence on the overall fixed effect<br>",
        "   - Helps identify studies with disproportionate impact on results<br><br>",
        "3. Influence Summary:<br>",
        "   - Provides numerical details on the influence of each study<br>",
        "   - Includes changes in effect size and other key statistics when each study is omitted"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Fixed Effects Residuals and Effect Distribution Info
  observeEvent(input$fixed_residuals_info, {
    showModal(modalDialog(
      title = "Fixed Effects: Residuals and Effect Distribution",
      HTML(paste0(
        "This section helps assess model assumptions for the fixed effects model:<br><br>",
        "1. Q-Q Plot:<br>",
        "   - Assesses whether residuals are normally distributed<br>",
        "   - Points should roughly follow the diagonal line for normality<br><br>",
        "2. Outlier Detection Plot:<br>",
        "   - Identifies potential outliers in the fixed effects model<br>",
        "   - Studies outside the dashed lines may be considered outliers<br><br>",
        "3. Effect Distribution:<br>",
        "   - Visualizes the distribution of effect sizes across studies<br>",
        "   - Helps assess the appropriateness of the fixed effects assumption"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Fixed Effects Publication Bias Info
  observeEvent(input$fixed_bias_info, {
    showModal(modalDialog(
      title = "Fixed Effects: Publication Bias",
      HTML(paste0(
        "This section assesses potential publication bias in the fixed effects context:<br><br>",
        "1. Funnel Plot:<br>",
        "   - Helps visualize potential publication bias<br>",
        "   - X-axis: Effect size; Y-axis: Standard error or precision<br>",
        "   - Asymmetry may indicate presence of bias<br><br>",
        "2. Trim and Fill Plot:<br>",
        "   - Adjusts for potential publication bias by imputing missing studies<br>",
        "   - Provides an adjusted effect size estimate<br><br>",
        "3. Egger's Test Results:<br>",
        "   - Statistical test for funnel plot asymmetry<br>",
        "   - Significant p-value suggests potential publication bias"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Fixed Effects Quality Assessment Info
  observeEvent(input$fixed_quality_info, {
    showModal(modalDialog(
      title = "Fixed Effects: Quality Assessment",
      HTML(paste0(
        "This section provides an overall assessment of the evidence quality:<br><br>",
        "1. GRADE Assessment:<br>",
        "   - Adapted for the fixed effects model context<br>",
        "   - Evaluates the quality of evidence based on several factors<br><br>",
        "2. Overall Interpretation:<br>",
        "   - Summarizes the main findings of the fixed effects meta-analysis<br>",
        "   - Considers effect size, model fit, and quality of evidence<br>",
        "   - Provides context for clinical or practical significance of results"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Bivariate Approach Overall Results Info
  observeEvent(input$bivariate_overall_info, {
    showModal(modalDialog(
      title = "Bivariate Approach: Overall Results and Heterogeneity",
      HTML(paste0(
        "This section presents key results from the bivariate meta-analysis approach:<br><br>",
        "1. Bivariate Forest Plot:<br>",
        "   - Displays effect sizes for two outcomes simultaneously<br>",
        "   - Allows for a more comprehensive view of the results<br><br>",
        "2. Confidence Region Plot:<br>",
        "   - Shows the joint confidence region for the two outcomes (μ and τ)<br>",
        "   - Provides a visual representation of the uncertainty in both parameters<br><br>",
        "3. Overall Summary:<br>",
        "   - Provides numerical results for the bivariate analysis<br>",
        "   - Includes estimates for both outcomes and their correlation"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Bivariate Approach Sensitivity and Influence Info
  observeEvent(input$bivariate_sensitivity_info, {
    showModal(modalDialog(
      title = "Bivariate Approach: Sensitivity and Influence",
      HTML(paste0(
        "This section assesses the robustness of bivariate results:<br><br>",
        "1. Confidence Region Shift Plot:<br>",
        "   - Novel plot showing how the confidence region changes when each study is removed<br>",
        "   - Helps identify influential studies in the bivariate context<br><br>",
        "2. Enhanced Baujat Plot:<br>",
        "   - Adapted for the bivariate approach<br>",
        "   - Shows each study's contribution to heterogeneity and influence on results<br><br>",
        "3. Influence Summary:<br>",
        "   - Provides numerical details on the influence of each study<br>",
        "   - Includes changes in effect sizes and other key statistics when each study is omitted"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Bivariate Approach Residuals and Effect Distribution Info
  observeEvent(input$bivariate_residuals_info, {
    showModal(modalDialog(
      title = "Bivariate Approach: Residuals and Effect Distribution",
      HTML(paste0(
        "This section helps assess model assumptions for the bivariate approach:<br><br>",
        "1. Q-Q Plot (μ):<br>",
        "   - Assesses normality of residuals for the first outcome<br><br>",
        "2. Q-Q Plot (τ):<br>",
        "   - Assesses normality of residuals for the second outcome<br><br>",
        "3. Efficacy-Harm Plot:<br>",
        "   - Visualizes the relationship between efficacy and harm outcomes<br>",
        "   - Helps balance benefits and risks in decision-making"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Bivariate Approach Stability and Publication Bias Info
  observeEvent(input$bivariate_stability_info, {
    showModal(modalDialog(
      title = "Bivariate Approach: Stability and Publication Bias",
      HTML(paste0(
        "This section assesses result stability and potential publication bias in the bivariate context:<br><br>",
        "1. Bivariate GOSH Plot:<br>",
        "   - Assesses stability of results across different subsets of studies<br>",
        "   - Adapted for the bivariate approach<br><br>",
        "2. Adapted Funnel Plot:<br>",
        "   - Visualizes potential publication bias in the bivariate context<br>",
        "   - Interpretation may differ from traditional funnel plots<br><br>",
        "3. Bivariate Bias Test Results:<br>",
        "   - Provides statistical assessment of potential bias<br>",
        "   - Adapted for the bivariate meta-analysis framework"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Bivariate Approach Quality Assessment Info
  observeEvent(input$bivariate_quality_info, {
    showModal(modalDialog(
      title = "Bivariate Approach: Quality Assessment",
      HTML(paste0(
        "This section provides an overall assessment of the evidence quality using the bivariate approach:<br><br>",
        "1. Modified GRADE Assessment:<br>",
        "   - Adapted for the bivariate meta-analysis context<br>",
        "   - Evaluates the quality of evidence considering both outcomes simultaneously<br><br>",
        "2. Risk of Bias and Indirectness:<br>",
        "   - User-specified inputs for these GRADE components<br>",
        "   - Helps tailor the assessment to the specific meta-analysis context<br><br>",
        "3. Overall Summary:<br>",
        "   - Provides a comprehensive evaluation of evidence quality<br>",
        "   - Considers all aspects of the bivariate analysis in the final assessment"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$re_effect_size_heterogeneity_info, {
    showModal(modalDialog(
      title = "Random Effects: Effect Size and Heterogeneity",
      HTML(paste0(
        "This section presents key results from the random effects meta-analysis:<br><br>",
        "1. Forest Plot:<br>",
        "   - Shows individual study effects and the overall random effects estimate<br>",
        "   - Confidence intervals for each study and the pooled effect<br>",
        "   - Study weights indicated by box sizes<br><br>",
        "2. Heterogeneity Plot:<br>",
        "   - Visualizes the extent of between-study variability<br>",
        "   - Helps identify studies contributing most to overall heterogeneity<br><br>",
        "3. Summary Statistics:<br>",
        "   - Overall effect size and its confidence interval<br>",
        "   - Heterogeneity measures (I², τ², Q statistic)<br>",
        "   - P-value for the test of heterogeneity"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$re_model_diagnostics_info, {
    showModal(modalDialog(
      title = "Random Effects: Model Diagnostics",
      HTML(paste0(
        "This section helps assess model assumptions and effect size distribution:<br><br>",
        "1. Q-Q Plot:<br>",
        "   - Assesses whether effect sizes are normally distributed<br>",
        "   - Points should roughly follow the diagonal line for normality<br><br>",
        "2. Outlier Detection Plot:<br>",
        "   - Identifies potential outliers based on standardized residuals<br>",
        "   - Studies outside the dashed lines may be considered outliers<br><br>",
        "3. Effect Distribution Plot:<br>",
        "   - Histogram showing the distribution of effect sizes across studies<br>",
        "   - Helps visualize the central tendency and spread of effects<br>",
        "   - Can indicate skewness or multi-modality in effect sizes"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$fe_effect_size_heterogeneity_info, {
    showModal(modalDialog(
      title = "Fixed Effects: Effect Size and Heterogeneity",
      HTML(paste0(
        "This section presents key results from the fixed effects meta-analysis:<br><br>",
        "1. Forest Plot:<br>",
        "   - Shows individual study effects and the overall fixed effect estimate<br>",
        "   - Confidence intervals for each study and the pooled effect<br>",
        "   - Study weights indicated by box sizes<br><br>",
        "2. Model Fit Plot:<br>",
        "   - Visualizes the goodness of fit for the fixed effects model<br>",
        "   - Helps assess whether the fixed effects assumption is appropriate<br><br>",
        "3. Summary Statistics:<br>",
        "   - Overall fixed effect size and its confidence interval<br>",
        "   - Model fit statistics (e.g., Q statistic, p-value)<br><br>",
        "4. Model Fit Statistics:<br>",
        "   - Detailed numerical assessment of model fit<br>",
        "   - Includes Q statistic, degrees of freedom, and p-value"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$fe_model_diagnostics_info, {
    showModal(modalDialog(
      title = "Fixed Effects: Model Diagnostics",
      HTML(paste0(
        "This section helps assess model assumptions for the fixed effects model:<br><br>",
        "1. Q-Q Plot:<br>",
        "   - Assesses whether residuals are normally distributed<br>",
        "   - Points should roughly follow the diagonal line for normality<br><br>",
        "2. Outlier Detection Plot:<br>",
        "   - Identifies potential outliers in the fixed effects model<br>",
        "   - Studies outside the dashed lines may be considered outliers<br><br>",
        "3. Effect Distribution:<br>",
        "   - Visualizes the distribution of effect sizes across studies<br>",
        "   - Helps assess the appropriateness of the fixed effects assumption"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$biv_effect_size_heterogeneity_info, {
    showModal(modalDialog(
      title = "Bivariate Approach: Effect Size and Heterogeneity",
      HTML(paste0(
        "This section presents key results from the bivariate meta-analysis approach:<br><br>",
        "1. Bivariate Forest Plot:<br>",
        "   - Displays effect sizes for two outcomes simultaneously<br>",
        "   - Allows for a more comprehensive view of the results<br><br>",
        "2. Confidence Region Plot:<br>",
        "   - Shows the joint confidence region for the two outcomes (μ and τ)<br>",
        "   - Provides a visual representation of the uncertainty in both parameters<br><br>",
        "3. Overall Summary:<br>",
        "   - Provides numerical results for the bivariate analysis<br>",
        "   - Includes estimates for both outcomes and their correlation"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$biv_model_diagnostics_info, {
    showModal(modalDialog(
      title = "Bivariate Approach: Model Diagnostics",
      HTML(paste0(
        "This section helps assess model assumptions for the bivariate approach:<br><br>",
        "1. Q-Q Plot (μ):<br>",
        "   - Assesses normality of residuals for the first outcome<br><br>",
        "2. Q-Q Plot (τ):<br>",
        "   - Assesses normality of residuals for the second outcome<br><br>",
        "3. Efficacy-Harm Plot:<br>",
        "   - Visualizes the relationship between efficacy and harm outcomes<br>",
        "   - Helps balance benefits and risks in decision-making"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$publication_bias_info, {
    showModal(modalDialog(
      title = "Publication Bias",
      HTML(paste0(
        "This section assesses potential publication bias in the meta-analysis:<br><br>",
        "1. Funnel Plot:<br>",
        "   - Helps visualize potential publication bias<br>",
        "   - X-axis: Effect size; Y-axis: Standard error or precision<br>",
        "   - Asymmetry may indicate presence of bias<br><br>",
        "2. Trim and Fill Plot:<br>",
        "   - Adjusts for potential publication bias by imputing missing studies<br>",
        "   - Provides an adjusted effect size estimate<br><br>",
        "3. Egger's Test Results:<br>",
        "   - Statistical test for funnel plot asymmetry<br>",
        "   - Significant p-value suggests potential publication bias"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$sensitivity_analysis_info, {
    showModal(modalDialog(
      title = "Sensitivity Analysis",
      HTML(paste0(
        "This section assesses the robustness of meta-analysis results:<br><br>",
        "1. Leave-One-Out Plot:<br>",
        "   - Shows how the overall effect changes when each study is removed<br>",
        "   - Helps identify influential studies<br><br>",
        "2. Baujat Plot:<br>",
        "   - Visualizes each study's contribution to overall heterogeneity and influence on results<br>",
        "   - X-axis: Contribution to heterogeneity<br>",
        "   - Y-axis: Influence on pooled result<br>",
        "   - Studies in the upper-right corner are most influential<br><br>",
        "3. Influence Summary:<br>",
        "   - Provides numerical details on the influence of each study<br>",
        "   - Includes changes in effect size, heterogeneity, and other key statistics when each study is omitted"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$quality_assessment_info, {
    showModal(modalDialog(
      title = "Quality Assessment",
      HTML(paste0(
        "This section provides an overall assessment of the evidence quality:<br><br>",
        "1. GRADE Assessment:<br>",
        "   - Grading of Recommendations, Assessment, Development and Evaluations<br>",
        "   - Evaluates the quality of evidence based on several factors:<br>",
        "     a) Risk of bias<br>",
        "     b) Inconsistency<br>",
        "     c) Indirectness<br>",
        "     d) Imprecision<br>",
        "     e) Publication bias<br><br>",
        "2. Overall Interpretation:<br>",
        "   - Summarizes the main findings of the meta-analysis<br>",
        "   - Considers effect size, heterogeneity, and quality of evidence<br>",
        "   - Provides context for clinical or practical significance of results"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Modify or add these reactive expressions
  random_results <- reactive({
    req(input$analyze)
    # Your existing code to generate random effects results
    combinedResults()$random
  })
  
  fixed_results <- reactive({
    req(input$analyze)
    # Your existing code to generate fixed effects results
    combinedResults()$fixed
  })
  
  bivariate_results <- reactive({
    req(input$analyze)
    # Your existing code to generate bivariate results
    combinedResults()$bivariate
  })
  # Add this new output for the download report button
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("meta_analysis_report_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      withProgress(message = 'Generating report...', value = 0, {
        # Increment the progress bar
        incProgress(1/3)
        
        # Render the report
        output_file <- render_report(
          random_results(), 
          fixed_results(), 
          bivariate_results()
        )
        
        # Increment the progress bar
        incProgress(1/3)
        
        # Copy the output file to the destination
        file.copy(output_file, file)
        
        # Increment the progress bar
        incProgress(1/3)
      })
    }
  )
  
}


