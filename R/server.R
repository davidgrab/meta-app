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
  
  # Load example datasets from CSV files
  exampleData <- read.csv("data/hypericum_depression_default.csv", stringsAsFactors = FALSE)
  colditzData <- read.csv("data/colditz_1994_bcg_vaccine.csv", stringsAsFactors = FALSE)
  yusufData <- read.csv("data/yusuf_1985_beta_blockers.csv", stringsAsFactors = FALSE)
  smdData <- as.data.frame(read_excel("data/CBT_versus_other_therapies_formatted.xlsx"))

  print("Functions sourced")
  
  # Initially hide all tabs except Data Preview
  observe({
    print("Hiding tabs")
    lapply(c("Overall Results", "Random Effects Analysis", "Fixed Effects Analysis", "JCR Method", "Meta-Regression"), function(tab) {
      hideTab(inputId = "main_tabs", target = tab)
    })
  })
  
  # Show all tabs when Analyze button is clicked
  observeEvent(input$analyze, {
    print("Analyze button clicked")
    lapply(c("Overall Results", "Random Effects Analysis", "Fixed Effects Analysis", "JCR Method", "Meta-Regression"), function(tab) {
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
    
    binary_instructions <- HTML(
      paste0(
        "<h4>Binary (2x2) Data</h4>",
        "1. Prepare your CSV or Excel file with the following columns:<br>",
        "<b>Required columns:</b> <b>study, ie, it, pe, pt</b><br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>study</b>: Study label<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>ie</b>: Intervention group events<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>it</b>: Intervention group total<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>pe</b>: Placebo/control group events<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>pt</b>: Placebo/control group total<br><br>",
        "<b>Optional columns for subgroup analysis and meta-regression:</b><br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>subgroup</b>: Categorical variable for subgroup analysis<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>moderator1, moderator2, ...</b>: Continuous or categorical variables for meta-regression<br><br>",
        "2. Click 'Browse' to select your file.<br><br>",
        "3. The data will load and display in the 'Data Preview' tab."
      )
    )
    
    smd_instructions <- HTML(
      paste0(
        "<h4>Continuous (SMD) Data</h4>",
        "1. Prepare your CSV or Excel file with the following columns:<br>",
        "<b>Required columns:</b> <b>study, smd, ci_lower, ci_upper</b><br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>study</b>: Study label<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>smd</b>: Standardized Mean Difference<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>ci_lower</b>: Lower bound of the 95% confidence interval<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>ci_upper</b>: Upper bound of the 95% confidence interval<br>",
        "<br><b>Note:</b> The SMD column may also appear as <b>CoNC</b> or <b>HeadGrid-G</b>. All are interpreted as SMD for now.<br><br>",
        "<b>Optional columns for subgroup analysis and meta-regression:</b><br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>subgroup</b>: Categorical variable for subgroup analysis<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>moderator1, moderator2, ...</b>: Continuous or categorical variables for meta-regression<br><br>",
        "2. Click 'Browse' to select your file.<br><br>",
        "3. The app will calculate the standard error and variance for you."
      )
    )
    
    modal_content <- if (input$data_type == "smd") smd_instructions else binary_instructions
    
    showModal(modalDialog(
      title = "How to Upload Data",
      modal_content,
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Dataset info popup
  observeEvent(input$dataset_info, {
    showModal(modalDialog(
      title = "Example Datasets Information",
      HTML(
        paste0(
          "<h4>Colditz et al. (1994) - BCG Vaccine Dataset</h4>",
          "<p>This dataset contains results from ", nrow(colditzData), " studies examining the effectiveness of the Bacillus Calmette-Guerin (BCG) vaccine against tuberculosis. It shows substantial heterogeneity between studies, potentially related to the geographic latitude where the studies were conducted.</p>",
          "<p>Source: Available in the metadat R package as dat.colditz1994</p>",
          "<hr>",
          "<h4>Yusuf et al. (1985) - Beta-Blockers Dataset</h4>",
          "<p>This dataset contains results from ", nrow(yusufData), " studies on the effectiveness of beta blockers for reducing mortality after myocardial infarction. It is from Table 6 of the original publication and demonstrates clear treatment effects with studies of varying sizes.</p>",
          "<p>Source: Available in the metafor R package as dat.yusuf1985</p>",
          "<hr>",
          "<h4>Hypericum (St. John's Wort) - Depression Dataset</h4>",
          "<p>This dataset comes from a Cochrane systematic review of randomized controlled trials comparing Hypericum extracts (St. John's Wort) to placebo in patients with major depressive disorder. It includes ", nrow(exampleData), " RCTs with binary outcomes measuring response to treatment (responder vs. non-responder) reported as relative risk (RR).</p>",
          "<p>Hypericum extracts are herbal remedies used for treating depression symptoms, and this dataset demonstrates the effectiveness comparison against placebo treatments.</p>",
          "<hr>",
          "<h4>CBT for Depression (SMD) Dataset</h4>",
          "<p>This dataset contains ", nrow(smdData), " studies comparing cognitive-behavioral therapy (CBT) to control conditions in depression, reported as standardized mean differences (SMD) with 95% confidence intervals.</p>",
          "<p>Source: Included with the app as an Excel file (data/CBT_versus_other_therapies_formatted.xlsx).</p>"
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
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
    
    # Handle variable column counts for subgroup and meta-regression data
    if (input$data_type == "binary") {
      if (ncol(df) >= 5) {
        # Ensure basic columns have correct names
        names(df)[1:5] <- c("study", "ie", "it", "pe", "pt")
        # Keep additional columns for subgroup/meta-regression analysis
      }
    } else if (input$data_type == "smd") {
      if (ncol(df) >= 4) {
        # Ensure basic columns have correct names
        names(df)[1:4] <- c("study", "smd", "ci_lower", "ci_upper")
        # Keep additional columns for subgroup/meta-regression analysis
      }
    }
    df
  })
  
  # New download
  output$downloadSampleStructure <- downloadHandler(
    filename = function() {
      if (input$data_type == "smd") {
        "sample_data_structure_continuous.csv"
      } else {
        "sample_data_structure_binary.csv"
      }
    },
    content = function(file) {
      if (input$data_type == "smd") {
        write.csv(sampleDataStructure_continuous, file, row.names = FALSE)
        # Append note to the file
        cat("\n# Note: The 'smd' column may also appear as 'CoNC' or 'HeadGrid-G'. All are interpreted as SMD for now.\n", file = file, append = TRUE)
      } else {
        write.csv(sampleDataStructure_binary, file, row.names = FALSE)
      }
    }
  )
  
  # New reactive value
  currentData <- reactiveVal(NULL)
  
  # Dataset description
  output$datasetDescription <- renderUI({
    dataset_choice <- input$exampleDatasetChoice
    
    description <- switch(dataset_choice,
      "colditz" = paste(nrow(colditzData), "studies on BCG vaccine effectiveness against tuberculosis. Classic dataset with substantial heterogeneity and potential moderators (latitude)."),
      "yusuf" = paste(nrow(yusufData), "studies on beta-blockers for reducing mortality after myocardial infarction. Widely used dataset with clear treatment effects and varying study sizes."),
      "default" = paste("Cochrane review of", nrow(exampleData), "RCTs comparing Hypericum (St. John's Wort) to placebo in major depressive disorder. Binary outcome (response to treatment) reported as relative risk (RR)."),
      "smd" = paste(nrow(smdData), "studies comparing cognitive-behavioral therapy (CBT) to control conditions for depression, with outcomes reported as Hedges' g standardized mean difference (SMD).")
    )
    
    HTML(paste("<div style='font-size: 0.85em; margin-bottom: 10px; color: #666;'>", description, "</div>"))
  })
  
  # New observe events
  observeEvent(input$loadExampleData, {
    dataset_choice <- input$exampleDatasetChoice
    
    if (dataset_choice == "colditz") {
      currentData(colditzData)
      updateRadioButtons(session, "data_type", selected = "binary")
      showNotification("Loaded Colditz et al. (1994) BCG Vaccine Dataset", type = "message")
    } else if (dataset_choice == "yusuf") {
      currentData(yusufData)
      updateRadioButtons(session, "data_type", selected = "binary")
      showNotification("Loaded Yusuf et al. (1985) Beta-Blockers Dataset", type = "message")
    } else if (dataset_choice == "smd") {
      currentData(smdData)
      updateRadioButtons(session, "data_type", selected = "smd")
      showNotification("Loaded CBT for Depression (SMD) Dataset", type = "message")
    } else {
      currentData(exampleData)
      updateRadioButtons(session, "data_type", selected = "binary")
      showNotification("Loaded Default Example Dataset", type = "message")
    }
  })
  
  observeEvent(input$datafile, {
    req(input$datafile)
    ext <- tools::file_ext(input$datafile$name)
    df <- switch(ext,
                 csv  = read.csv(input$datafile$datapath, stringsAsFactors = FALSE),
                 xlsx = read_excel(input$datafile$datapath),
                 validate("Invalid file type. Please upload a .csv or .xlsx file."))

    # --- NEW: auto-select data type ------------------------------------------------
    colnames_lower <- tolower(names(df))
    if (all(c("ie", "it", "pe", "pt") %in% colnames_lower)) {
      updateRadioButtons(session, "data_type", selected = "binary")
    } else if ("smd" %in% colnames_lower &&
               all(c("ci_lower", "ci_upper") %in% colnames_lower)) {
      updateRadioButtons(session, "data_type", selected = "smd")
    }
    # ------------------------------------------------------------------------------

    currentData(df)
  })
  
  output$dataPreview <- renderDT({
    print("Rendering data preview")
    data()
  })
  
  # New data structures
  sampleDataStructure_binary <- data.frame(
    study = c("Study 1", "Study 2", "Study 3"),
    ie = c(12, 8, 15),
    it = c(100, 95, 80),
    pe = c(10, 7, 12),
    pt = c(98, 93, 75),
    subgroup = c("Group A", "Group B", "Group A"),
    moderator1 = c(2.5, 3.1, 2.8),
    moderator2 = c("High", "Low", "Medium"),
    stringsAsFactors = FALSE
  )
  
  sampleDataStructure_continuous <- data.frame(
    study = c("Study 1", "Study 2", "Study 3"),
    smd = c(0.45, -0.12, 0.28),
    ci_lower = c(0.10, -0.30, 0.05),
    ci_upper = c(0.80, 0.06, 0.51),
    subgroup = c("Group A", "Group B", "Group A"),
    moderator1 = c(2.5, 3.1, 2.8),
    moderator2 = c("High", "Low", "Medium"),
    stringsAsFactors = FALSE
  )
  attr(sampleDataStructure_continuous, "note") <- "Note: The 'smd' column may also appear as 'CoNC' or 'HeadGrid-G'. All are interpreted as SMD for now."
  
  # Example dataset
  # exampleData <- read.csv(text = "study,Intervention_effected,Intervention_total,Placebo_effected,Placebo_total
#1,22,54,21,55
#2,17,45,9,43
#3,35,53,12,55
#4,23,37,15,35
#5,24,49,16,49
#6,4,20,11,26
#7,45,80,12,79
#8,26,98,19,102
#9,41,70,4,70
#10,64,109,48,109
#11,71,131,51,130
#12,46,113,56,116
#13,159,243,26,81
#14,98,186,80,189
#15,55,123,57,124
#16,67,106,22,47
#17,46,70,34,70
#18,34,48,25,49")
  
  # Colditz et al. (1994) - BCG Vaccine Dataset
  # colditzData <- read.csv(text = "study,Intervention_effected,Intervention_total,Placebo_effected,Placebo_total
#1,4,123,11,139
#2,6,306,29,303
#3,3,231,11,220
#4,62,13598,248,12867
#5,33,5069,47,5808
#6,180,1361,372,1079
#7,8,2545,10,629
#8,505,87886,499,87892
#9,29,7470,45,7232
#10,17,1699,65,1600
#11,186,50634,141,27338
#12,5,2493,3,2338
#13,27,16886,29,17825")
  
  # Yusuf et al. (1985) - Beta-Blockers Dataset (from Table 6)
  # yusufData <- read.csv(text = "study,Intervention_effected,Intervention_total,Placebo_effected,Placebo_total
#1,14,56,15,58
#2,18,66,19,64
#3,15,100,12,95
#4,10,52,12,47
#5,21,226,24,228
#6,3,38,6,31
#7,2,20,3,20
#8,19,76,15,67
#9,15,106,9,114
#10,5,62,4,57
#11,0,9,0,8
#12,8,133,11,127
#13,3,48,3,49
#14,0,16,0,13
#15,1,42,1,46
#16,0,25,3,25
#17,14,221,15,228
#18,0,11,0,11
#19,8,259,7,129
#20,6,157,4,158
#21,3,177,2,136")
  
  # Combined analysis
  combinedResults <- eventReactive(input$analyze, {
    req(data())
    df <- data()
    
    if (input$data_type == "binary" && ncol(df) >= 5) {
      random_model <- metabin(event.e = df$ie, 
                              n.e = df$it,
                              event.c = df$pe,
                              n.c = df$pt,
                              studlab = df$study,
                              sm = input$effect_measure, 
                              method.tau = input$het_estimator,
                              common = FALSE,
                              random = TRUE)
      
      fixed_model <- metabin(event.e = df$ie, 
                             n.e = df$it,
                             event.c = df$pe,
                             n.c = df$pt,
                             studlab = df$study,
                             sm = input$effect_measure, 
                             common = TRUE,
                             random = FALSE)
      
      bivariate_model <- metabiv(event.e = df$ie, 
                                 n.e = df$it, 
                                 event.c = df$pe, 
                                 n.c = df$pt,
                                 studlab = df$study,
                                 sm = input$effect_measure)
      
    } else if (input$data_type == "smd" && ncol(df) >= 4) {
      
      # Calculate SE and variance from CI
      se <- (df$ci_upper - df$ci_lower) / (2 * 1.96)
      var <- se^2
      
      random_model <- metagen(TE = df$smd,
                              seTE = se,
                              studlab = df$study,
                              sm = "SMD",
                              method.tau = input$het_estimator,
                              common = FALSE,
                              random = TRUE)
      
      fixed_model <- metagen(TE = df$smd,
                             seTE = se,
                             studlab = df$study,
                             sm = "SMD",
                             common = TRUE,
                             random = FALSE)
      
      bivariate_model <- metabiv(studlab = df$study,
                                 sm = "SMD",
                                 y = df$smd,
                                 sigma2 = var)
    } else {
      # Handle cases where data and type don't match, maybe show a warning
      showNotification("Data columns do not match selected data type.", type = "warning")
      return(NULL)
    }
    
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
  
  # Helper to get effect measure label for plots
  effect_measure_label <- reactive({
    if (input$data_type == "smd") {
      "SMD"
    } else {
      switch(input$effect_measure,
             OR = "Odds Ratio",
             RR = "Risk Ratio",
             input$effect_measure)
    }
  })
  
  # Dynamic height for forest plots
  forest_plot_height <- reactive({
    req(data())
    # Base height of 400px, plus 20px for each study
    400 + nrow(data()) * 20
  })
  
  output$randomForestPlot <- renderPlot({
    req(combinedResults()$random)
    meta::forest(combinedResults()$random, 
           leftlabs = c("Study", "TE", "seTE"),
           rightlabs = c("TE", "95%-CI", "Weight"),
           fontsize = 10, 
           xlab = paste0("Effect Size (", effect_measure_label(), ")"),
           main = paste0("Random Effects Forest Plot (", effect_measure_label(), ")"))
  }, height = function() forest_plot_height())
  
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
    tryCatch({
      inf <- metainf(combinedResults()$random)
      
      # Create a data frame and remove non-finite results to prevent errors
      inf_df <- data.frame(TE = inf$TE, seTE = inf$seTE, studlab = inf$studlab)
      inf_df_clean <- subset(inf_df, is.finite(TE) & is.finite(seTE))
      
      if (nrow(inf_df_clean) == 0) {
        plot(1, type="n", axes=FALSE, xlab="", ylab="", main="Leave-One-Out plot cannot be generated\n(no valid studies to plot)")
      } else {
        # Create a new meta-object from the cleaned data for robust plotting
        m_clean <- metagen(TE = TE, seTE = seTE, studlab = studlab, data = inf_df_clean)
        meta::forest(m_clean, 
               leftlabs = c("Omitted Study"),
               xlab = paste0("Effect Size (", effect_measure_label(), ")"),
               main = "Leave-One-Out Analysis (Random Effects)")
      }
    }, error = function(e) {
      plot(1, type="n", axes=FALSE, xlab="", ylab="", main=paste("Leave-One-Out plot failed:", e$message))
    })
  }, height = function() forest_plot_height())
  
  output$baujatPlot <- renderPlot({
    req(combinedResults()$random)
    tryCatch({
      baujat(combinedResults()$random)
    }, error = function(e) {
      plot(1, type="n", axes=FALSE, xlab="", ylab="", main="Baujat plot failed")
      text(1, 1, paste("Error:", e$message))
    })
  })
  
  output$influenceSummary <- renderPrint({
    req(combinedResults()$random)
    influence_analysis(combinedResults()$random)
  })
  
  output$randomQQPlot <- renderPlot({
    req(combinedResults())
    random_model <- combinedResults()$random
    
    tryCatch({
      # Use the new comprehensive normality diagnostic for BLUPs
      qq_plot_random_blups(random_model, envelope = TRUE)
    }, error = function(e) {
      plot(1, type = "n", xlab = "", ylab = "", main = "Q-Q Plot Unavailable")
      text(1, 1, "Insufficient data for comprehensive Q-Q plot", cex = 1.2)
    })
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
    meta::forest(combinedResults()$fixed, 
           leftlabs = c("Study", "TE", "seTE"),
           rightlabs = c("TE", "95%-CI", "Weight"),
           fontsize = 10, 
           xlab = paste0("Effect Size (", effect_measure_label(), ")"),
           main = paste0("Fixed Effects Forest Plot (", effect_measure_label(), ")"))
  }, height = function() forest_plot_height())
  
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
    tryCatch({
      inf <- metainf(combinedResults()$fixed)
      
      # Create a data frame and remove non-finite results to prevent errors
      inf_df <- data.frame(TE = inf$TE, seTE = inf$seTE, studlab = inf$studlab)
      inf_df_clean <- subset(inf_df, is.finite(TE) & is.finite(seTE))
      
      if (nrow(inf_df_clean) == 0) {
        plot(1, type="n", axes=FALSE, xlab="", ylab="", main="Leave-One-Out plot cannot be generated\n(no valid studies to plot)")
      } else {
        # Create a new meta-object from the cleaned data for robust plotting
        m_clean <- metagen(TE = TE, seTE = seTE, studlab = studlab, data = inf_df_clean)
        meta::forest(m_clean, 
               leftlabs = c("Omitted Study"),
               xlab = paste0("Effect Size (", effect_measure_label(), ")"),
               main = "Leave-One-Out Analysis (Fixed Effects)")
      }
    }, error = function(e) {
      plot(1, type="n", axes=FALSE, xlab="", ylab="", main=paste("Leave-One-Out plot failed:", e$message))
    })
  }, height = function() forest_plot_height())
  
  output$fixedInfluencePlot <- renderPlot({
    req(combinedResults()$fixed)
    tryCatch({
      baujat(combinedResults()$fixed)
    }, error = function(e) {
      plot(1, type="n", axes=FALSE, xlab="", ylab="", main="Baujat plot failed")
      text(1, 1, paste("Error:", e$message))
    })
  })
  
  output$fixedInfluenceSummary <- renderPrint({
    req(combinedResults()$fixed)
    influence_analysis(combinedResults()$fixed)
  })
  
  output$fixedQQPlot <- renderPlot({
    req(combinedResults())
    fixed_model <- combinedResults()$fixed
    
    tryCatch({
      # Use the new comprehensive normality diagnostic for fixed effects
      qq_plot_fixed_residuals(fixed_model, envelope = TRUE)
    }, error = function(e) {
      plot(1, type = "n", xlab = "", ylab = "", main = "Q-Q Plot Unavailable")
      text(1, 1, "Insufficient data for comprehensive Q-Q plot", cex = 1.2)
    })
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
  
  
  output$fixedSummary <- renderPrint({
    req(combinedResults()$fixed)
    result <- combinedResults()$fixed
    cat("Fixed Effects Model Summary\n")
    cat("-------------------------\n")
    cat("Overall effect size (OR):", exp(result$TE.common), "\n")
    cat("95% CI:", exp(result$lower.common), "to", exp(result$upper.common), "\n")
    cat("P-value:", result$pval.common, "\n")
    cat("Heterogeneity test (Q):", result$Q, "\n")
    cat("Heterogeneity p-value:", result$pval.Q, "\n")
    cat("I² (inconsistency):", result$I2, "%\n")
  })
  
  
  
  # JCR method analysis results
  bivariate_result <- reactive({
    req(input$analyze)
    # Handle both data types
    df <- data()
    if (input$data_type == "smd") {
        se <- (df$ci_upper - df$ci_lower) / (2 * 1.96)
        var <- se^2
        metabiv(studlab = df$study, sm = "SMD", y = df$smd, sigma2 = var, verbose = TRUE)
    } else {
        metabiv(event.e = df$ie, 
                n.e = df$it, 
                event.c = df$pe, 
                n.c = df$pt,
                studlab = df$study,
                sm = input$effect_measure, verbose = TRUE)
    }
  })
  
  # JCR Forest Plot
  output$bivariateForestPlot <- renderPlot({
    req(bivariate_result())
    forest.metabiv(bivariate_result(),
                   title = paste("JCR Forest Plot (", effect_measure_label(), ")"),
                   xlab = paste("Effect Size (", effect_measure_label(), ")"))
  }, height = function() forest_plot_height())
  
  # Confidence Region Plot
  output$confidenceRegionPlot <- renderPlot({
    req(bivariate_result())
    
    # Get the effect measure label
    effect_label <- effect_measure_label()
    
    # Generate a dynamic title and xlab
    plot_title <- paste("Confidence Region for (", effect_label, ", τ)")
    x_label <- paste("Effect Size (", effect_label, ")")
    
    plot.mu.tau.CI(bivariate_result()$dev_pvals[[1]], 
                   bivariate_result()$dev_pvals[[2]], 
                   mlb = plot_title,
                   xlab = x_label,  # Pass xlab to the plotting function
                   mu_mle = bivariate_result()$mu,
                   tau_mle = bivariate_result()$tau,
                   sm = bivariate_result()$sm)
  }, height = 600)  # Set explicit height for better visibility
  
  
  # JCR Overall Summary
  # COMMENTED OUT: The summary shows mu and tau values that don't match what's displayed in plots
  # TODO: Fix the scale consistency - should use same scale as plots (exponentiated for OR/RR)
  # The mu estimate here appears to be in log scale while plots show exponentiated values
  # output$bivariateOverallSummary <- renderPrint({
  #   req(bivariate_result())
  #   summary(bivariate_result())
  # })
  
  # Confidence Region Shift Plot
  output$confidenceRegionShiftPlot <- renderPlotly({
    req(bivariate_result())
    confidence_region_shift_plot(bivariate_result())
  })
  
  # Enhanced Baujat Plot
  output$enhancedBaujatPlot <- renderPlotly({
    req(bivariate_result())
    
    influence_data <- calculate_influence_bivariate(bivariate_result(), data(), input)
    
    p <- plot_ly(data = influence_data, 
                 x = ~contribution, 
                 y = ~influence, 
                 text = ~study,
                 type = "scatter", 
                 mode = "markers+text",
                 textposition = "top right",
                 marker = list(size = 10, opacity = 0.7, line = list(width=1, color = 'black'))) %>%
      layout(title = paste("Enhanced Baujat Plot (", effect_measure_label(), ")"),
             xaxis = list(title = "Contribution to Q (Heterogeneity)"),
             yaxis = list(title = "Influence on Pooled Estimate"),
             shapes = list(
               list(type = 'line', 
                    x0 = mean(influence_data$contribution), 
                    y0 = 0, 
                    x1 = mean(influence_data$contribution), 
                    y1 = max(influence_data$influence), 
                    line = list(dash = 'dash', color = 'red')),
               list(type = 'line', 
                    x0 = 0, 
                    y0 = mean(influence_data$influence), 
                    x1 = max(influence_data$contribution), 
                    y1 = mean(influence_data$influence), 
                    line = list(dash = 'dash', color = 'red'))
             ))
    p
  })
  
  # Helper function to calculate influence for bivariate model
  calculate_influence_bivariate <- function(biv_res, full_data, input) {
    studylab <- biv_res$studlab
    influence <- lapply(1:length(studylab), function(i) {
      loo_data <- full_data[full_data$study != studylab[i], ]
      
      res_i <- if (input$data_type == "smd") {
        se <- (loo_data$ci_upper - loo_data$ci_lower) / (2 * 1.96)
        var <- se^2
        metabiv(studlab = loo_data$study, sm = "SMD", y = loo_data$smd, sigma2 = var, verbose = FALSE)  # No logs
      } else {
        metabiv(event.e = loo_data$ie, n.e = loo_data$it, 
                event.c = loo_data$pe, n.c = loo_data$pt,
                studlab = loo_data$study, sm = input$effect_measure, verbose = FALSE)  # No logs
      }
      
      # Calculate contribution and influence
      contribution <- (biv_res$y.k[i] - res_i$mu)^2 / (biv_res$sigma.2.k[i] + res_i$tau^2)
      influence_val <- (biv_res$y.k[i] - res_i$mu)^2 / (biv_res$sigma.2.k[i] + res_i$tau^2)^2
      c(contribution = contribution, influence = influence_val)
    })
    
    influence_df <- as.data.frame(do.call(rbind, influence))
    influence_df$study <- studylab
    return(influence_df)
  }
  
  # JCR Influence Summary
  # COMMENTED OUT: Removed as requested - not needed for now
  # output$bivariateInfluenceSummary <- renderPrint({
  #   req(bivariate_result())
  #   cat("Bivariate Influence Summary\n")
  #   influence <- lapply(1:length(bivariate_result()$y.k), function(i) {
  #       df <- data()[-i, ]
  #       if (input$data_type == "smd") {
  #           se <- (df$ci_upper - df$ci_lower) / (2 * 1.96)
  #           var <- se^2
  #           res_i <- metabiv(studlab = df$study, sm = "SMD", y = df$smd, sigma2 = var, verbose = FALSE)  # No logs
  #       } else {
  #           res_i <- metabiv(event.e = df$ie, n.e = df$it, event.c = df$pe, n.c = df$pt,
  #                            studlab = df$study, sm = input$effect_measure, verbose = FALSE)  # No logs
  #       }
  #     c(mu_change = res_i$mu - bivariate_result()$mu,
  #       tau_change = res_i$tau - bivariate_result()$tau)
  #   })
  #   influence_df <- do.call(rbind, influence)
  #   rownames(influence_df) <- bivariate_result()$studlab
  #   print(influence_df)
  # })
  
  # Q-Q Plot for μ
  output$qqPlotMu <- renderPlot({
    req(bivariate_result())
    
    tryCatch({
      # Use the new comprehensive normality diagnostic for bivariate BLUPs
      qq_plot_bivariate_blups(bivariate_result(), envelope = TRUE)
    }, error = function(e) {
      # Fallback to the old implementation
    qq_plot_with_ci(y_k = bivariate_result()$y.k, 
                    mu = bivariate_result()$mu,
                    sigma_2_k = bivariate_result()$sigma.2.k,
                    tau_2 = bivariate_result()$tau^2,
                    title = "Q-Q Plot for Standardized Residuals (Normal Random Effects)")
    })
  })
  
  # Side-by-side deleted residuals comparison plot
  output$bivariateDeletedResidualsComparisonPlot <- renderPlot({
    req(bivariate_result(), combinedResults()$fixed, data())
    tryCatch({
      qq_plot_bivariate_vs_fixed_deleted(bivariate_result(), combinedResults()$fixed, data(), input)
    }, error = function(e) {
      plot(1, type="n", main="Deleted Residuals Comparison Unavailable", xlab="", ylab="")
      text(1, 1, paste("Error:", e$message), cex=0.8)
    })
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
    
    # Get the effect measure label
    effect_label <- effect_measure_label()
    plot_title <- paste("Efficacy/Harm Plot for", effect_label)
    
    CDF.ci.obj <- comp.mu.tau.dev.CDF.CI(bivariate_result()$dev_pvals, sm = bivariate_result()$sm)
    comp.eff.harm.plot(CDF.ci.obj,
                       efficacy.is.OR.le1 = (bivariate_result()$sm == "OR"),
                       mlb = plot_title,
                       xlb = paste("Effect Size (", effect_label, ")"),
                       sm = bivariate_result()$sm)
  }, height = 500)  # Set explicit height for better visibility
  
  # Probability Table for Clinical Thresholds
  output$efficacyHarmProbabilityTable <- renderTable({
    req(bivariate_result())
    
    # Use the EXACT same CDF object as the Efficacy/Harm plot
    CDF.ci.obj <- comp.mu.tau.dev.CDF.CI(bivariate_result()$dev_pvals, sm = bivariate_result()$sm)
    
    # Parse custom thresholds from input
    custom_thresholds <- NULL
    if (!is.null(input$custom_thresholds) && input$custom_thresholds != "") {
      # Split by comma and convert to numeric
      threshold_strings <- strsplit(input$custom_thresholds, ",")[[1]]
      threshold_strings <- trimws(threshold_strings)  # Remove whitespace
      custom_thresholds <- suppressWarnings(as.numeric(threshold_strings))
      custom_thresholds <- custom_thresholds[!is.na(custom_thresholds)]
    }
    
    # Calculate probability table using the exact same CDF data as the plot
    prob_table <- calculate_threshold_probabilities_from_cdf(CDF.ci.obj, custom_thresholds, bivariate_result()$sm)
    
    # Format for display (simple numeric display)
    prob_table$`P(θ ≥ T)` <- sprintf("%.3f", prob_table$Probability)
    prob_table$`95% CI Lower` <- sprintf("%.3f", prob_table$CI_Lower)
    prob_table$`95% CI Upper` <- sprintf("%.3f", prob_table$CI_Upper)
    prob_table$`Threshold (T)` <- sprintf("%.3f", prob_table$Threshold)
    
    # Select and rename columns for display (just numbers, no descriptions)
    display_table <- prob_table[, c("Threshold (T)", "P(θ ≥ T)", "95% CI Lower", "95% CI Upper")]
    
    return(display_table)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # JCR GOSH Plot
  output$bivariateGOSHPlot <- renderPlotly({
    req(bivariate_result())
    # Implement GOSH plot
    df <- data()
    subsets <- replicate(1000, sample(1:nrow(df), size = nrow(df)/2, replace = FALSE))
    gosh_results <- apply(subsets, 2, function(subset) {
        df_sub <- df[subset, ]
        if (input$data_type == "smd") {
            se <- (df_sub$ci_upper - df_sub$ci_lower) / (2 * 1.96)
            var <- se^2
            res <- metabiv(studlab = df_sub$study, sm = "SMD", y = df_sub$smd, sigma2 = var)
        } else {
            res <- metabiv(event.e = df_sub$ie, n.e = df_sub$it, event.c = df_sub$pe, n.c = df_sub$pt,
                           studlab = df_sub$study, sm = input$effect_measure)
        }
      c(mu = res$mu, tau = res$tau)
    })
    gosh_df <- as.data.frame(t(gosh_results))
    plot_ly(data = gosh_df, x = ~mu, y = ~tau, type = "scatter", mode = "markers",
            marker = list(size = 3, opacity = 0.5)) %>%
      layout(title = "GOSH Plot",
             xaxis = list(title = "μ"),
             yaxis = list(title = "τ"))
  })
  
  # JCR Adapted Funnel Plot
  output$bivariateAdaptedFunnelPlot <- renderPlot({
    req(bivariate_result())
    
    y.k <- bivariate_result()$y.k
    se.k <- sqrt(bivariate_result()$sigma.2.k) # Standard errors
    
    # Use effect measure for sm and xlab
    effect_label <- effect_measure_label()
    
    # Create a meta-analysis object
    # For OR and RR, we exponentiate for the plot
    if (bivariate_result()$sm %in% c("OR", "RR")) {
      meta_analysis <- metagen(TE = y.k, seTE = se.k, sm = bivariate_result()$sm)
      funnel(meta_analysis, 
             xlab = paste("Effect Size (", effect_label, ")"),
             main = paste("Funnel Plot (", effect_label, ")"))
    } else { # For SMD, plot on original scale
      meta_analysis <- metagen(TE = y.k, seTE = se.k, sm = "SMD")
      funnel(meta_analysis, 
             xlab = paste("Effect Size (", effect_label, ")"),
             main = paste("Funnel Plot (", effect_label, ")"))
    }
  })
  
  # JCR Bias Test Results
  output$bivariateBiasTestResults <- renderPrint({
    req(bivariate_result())
    cat("Bivariate Bias Test Results\n")
    # Implement Egger's test for the bivariate case
    egger_test <- lm(bivariate_result()$y.k ~ sqrt(bivariate_result()$sigma.2.k))
    print(summary(egger_test))
    cat("\nInterpretation: If the intercept is significantly different from zero,\n",
        "this may indicate the presence of small-study effects or publication bias.\n")
  })
  
  # JCR GRADE Summary
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
    
    df <- data()
    if (input$data_type == "smd") {
        se <- (df$ci_upper - df$ci_lower) / (2 * 1.96)
        trad_meta_random <- metagen(TE = df$smd, seTE = se, studlab = df$study, sm = "SMD", common = FALSE, random = TRUE)
        trad_meta_fixed <- metagen(TE = df$smd, seTE = se, studlab = df$study, sm = "SMD", common = TRUE, random = FALSE)
    } else {
        # Traditional meta-analysis (random effects)
        trad_meta_random <- metabin(event.e = df$ie, 
                                    n.e = df$it, 
                                    event.c = df$pe, 
                                    n.c = df$pt,
                                    studlab = df$study,
                                    sm = input$effect_measure,
                                    method = "Inverse",
                                    common = FALSE, # Corrected
                                    random = TRUE)
        
        # Traditional meta-analysis (fixed effects)
        trad_meta_fixed <- metabin(event.e = df$ie, 
                                   n.e = df$it, 
                                   event.c = df$pe, 
                                   n.c = df$pt,
                                   studlab = df$study,
                                   sm = input$effect_measure,
                                   method = "Inverse",
                                   common = TRUE,
                                   random = FALSE)
    }
    

    # Combine results
    results <- data.frame(
      Method = c("Fixed Effects", "Random Effects", "JCR Method"),
      Estimate = c(trad_meta_fixed$TE.common, trad_meta_random$TE.random, bivariate_result()$mu),
      Lower = c(trad_meta_fixed$lower.common, trad_meta_random$lower.random, bivariate_result()$lower),
      Upper = c(trad_meta_fixed$upper.common, trad_meta_random$upper.random, bivariate_result()$upper)
    )
    
    
    # Create forest plot
    meta::forest(metagen(TE = Estimate, 
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
        "- Upload your data CSV file using the 'Upload Data' button in the sidebar.<br>",
        "- Supported data types:<br>",
        "&nbsp;&nbsp;• <b>Binary (2x2):</b> Columns: study, ie, it, pe, pt<br>",
        "&nbsp;&nbsp;• <b>Continuous (SMD):</b> Columns: study, smd, ci_lower, ci_upper<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>Note:</b> The SMD column may also appear as <b>CoNC</b> or <b>HeadGrid-G</b>. All are interpreted as SMD for now.<br>",
        "- You can use the 'Download Sample Structure' button to see the required format for each data type.<br><br>",
        
        "<b>2. Analysis Settings:</b><br>",
        "- Choose your heterogeneity estimator and effect measure in the sidebar.<br>",
        "- For binary data, select the effect measure (Odds Ratio, Risk Ratio, etc.).<br>",
        "- For continuous data, the effect measure is always SMD (regardless of column label).<br>",
        "- Click 'Analyze' to run the meta-analysis.<br><br>",
        
        "<b>3. Results Tabs:</b><br>",
        "- <i>Data Preview:</i> Check your uploaded data.<br>",
        "- <i>Overall Results:</i> Compare results across all methods.<br>",
        "- <i>Random Effects Analysis:</i> Detailed random effects model results.<br>",
        "- <i>Fixed Effects Analysis:</i> Detailed fixed effects model results.<br>",
        "- <i>JCR Method:</i> Results from the joint MLE meta-analysis with a joint confidence region.<br><br>",
        
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
        "<b>Binary (2x2):</b> study, ie, it, pe, pt<br>",
        "<b>Continuous (SMD):</b> study, smd, ci_lower, ci_upper<br>",
        "<b>Note:</b> For continuous data, the SMD column may also appear as <b>CoNC</b> or <b>HeadGrid-G</b>. All are interpreted as SMD for now.<br><br>",
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
        "- JCR Method<br><br>",
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
  
  # JCR Method Overall Results Info
  observeEvent(input$bivariate_overall_info, {
    showModal(modalDialog(
      title = "JCR Method: Overall Results and Heterogeneity",
      HTML(paste0(
        "This section presents key results from the bivariate meta-analysis approach:<br><br>",
        "1. JCR Forest Plot:<br>",
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
  
  # JCR Method Sensitivity and Influence Info
  observeEvent(input$bivariate_sensitivity_info, {
    showModal(modalDialog(
      title = "JCR Method: Sensitivity and Influence",
      HTML(paste0(
        "This section assesses the robustness of bivariate results:<br><br>",
        "1. Confidence Region Shift Plot:<br>",
        "   - Novel plot showing how the confidence region changes when each study is removed<br>",
        "   - Helps identify influential studies in the bivariate context<br><br>",
        "2. Enhanced Baujat Plot:<br>",
        "   - Adapted for the JCR method<br>",
        "   - Shows each study's contribution to heterogeneity and influence on results<br><br>",
        "3. Influence Summary:<br>",
        "   - Provides numerical details on the influence of each study<br>",
        "   - Includes changes in effect sizes and other key statistics when each study is omitted"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # JCR Method Residuals and Effect Distribution Info
  observeEvent(input$bivariate_residuals_info, {
    showModal(modalDialog(
      title = "JCR Method: Residuals and Effect Distribution",
      HTML(paste0(
        "This section helps assess model assumptions for the JCR method:<br><br>",
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
  
  # JCR Method Stability and Publication Bias Info
  observeEvent(input$bivariate_stability_info, {
    showModal(modalDialog(
      title = "JCR Method: Stability and Publication Bias",
      HTML(paste0(
        "This section assesses result stability and potential publication bias in the bivariate context:<br><br>",
        "1. JCR GOSH Plot:<br>",
        "   - Assesses stability of results across different subsets of studies<br>",
        "   - Adapted for the JCR method<br><br>",
        "2. Adapted Funnel Plot:<br>",
        "   - Visualizes potential publication bias in the bivariate context<br>",
        "   - Interpretation may differ from traditional funnel plots<br><br>",
        "3. JCR Bias Test Results:<br>",
        "   - Provides statistical assessment of potential bias<br>",
        "   - Adapted for the bivariate meta-analysis framework"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # JCR Method Quality Assessment Info
  observeEvent(input$bivariate_quality_info, {
    showModal(modalDialog(
      title = "JCR Method: Quality Assessment",
      HTML(paste0(
        "This section provides an overall assessment of the evidence quality using the JCR method:<br><br>",
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
      title = "JCR Method: Effect Size and Heterogeneity",
      HTML(paste0(
        "This section presents key results from the bivariate meta-analysis approach:<br><br>",
        "1. JCR Forest Plot:<br>",
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
      title = "JCR Method: Model Diagnostics",
      HTML(paste0(
        "This section helps assess model assumptions for the JCR method:<br><br>",
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
  
  # Subgroup Analysis Information Buttons
  observeEvent(input$re_subgroup_info, {
    showModal(modalDialog(
      title = "Random Effects: Subgroup Analysis",
      HTML(paste0(
        "This section performs subgroup analysis using the random effects model:<br><br>",
        "1. Purpose:<br>",
        "   - Investigates whether treatment effects differ between subgroups<br>",
        "   - Helps explain sources of heterogeneity<br>",
        "   - Identifies populations that may benefit more or less from treatment<br><br>",
        "2. Forest Plot:<br>",
        "   - Shows studies grouped by the selected categorical variable<br>",
        "   - Displays separate pooled estimates for each subgroup<br>",
        "   - Uses random effects model within each subgroup<br><br>",
        "3. Statistical Test:<br>",
        "   - Q-statistic tests for differences between subgroups<br>",
        "   - Significant p-value (< 0.05) indicates subgroup differences<br>",
        "   - Helps determine if subgroup-specific recommendations are warranted"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$fe_subgroup_info, {
    showModal(modalDialog(
      title = "Fixed Effects: Subgroup Analysis",
      HTML(paste0(
        "This section performs subgroup analysis using the fixed effects model:<br><br>",
        "1. Purpose:<br>",
        "   - Investigates whether treatment effects differ between subgroups<br>",
        "   - Assumes a common true effect within each subgroup<br>",
        "   - More appropriate when heterogeneity within subgroups is minimal<br><br>",
        "2. Forest Plot:<br>",
        "   - Shows studies grouped by the selected categorical variable<br>",
        "   - Displays separate pooled estimates for each subgroup<br>",
        "   - Uses fixed effects model within each subgroup<br><br>",
        "3. Statistical Test:<br>",
        "   - Q-statistic tests for differences between subgroups<br>",
        "   - Significant p-value (< 0.05) indicates subgroup differences<br>",
        "   - Results may be more precise but less generalizable than random effects"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$biv_subgroup_info, {
    showModal(modalDialog(
      title = "JCR Method: Subgroup Analysis",
      HTML(paste0(
        "This section performs subgroup analysis using the JCR method:<br><br>",
        "1. Purpose:<br>",
        "   - Investigates whether treatment effects differ between subgroups<br>",
        "   - Uses bivariate meta-analysis for joint estimation within subgroups<br>",
        "   - Particularly useful for diagnostic test accuracy or paired outcomes<br><br>",
        "2. Forest Plot:<br>",
        "   - Shows studies grouped by the selected categorical variable<br>",
        "   - Displays separate bivariate estimates for each subgroup<br>",
        "   - Provides joint confidence regions for paired parameters<br><br>",
        "3. Comparison:<br>",
        "   - Visual comparison of confidence intervals between subgroups<br>",
        "   - Formal statistical test for subgroup differences not implemented<br>",
        "   - Interpretation based on overlap of confidence regions"
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$metaregression_info, {
    showModal(modalDialog(
      title = "Meta-Regression Analysis",
      HTML(paste0(
        "This section performs meta-regression to investigate sources of heterogeneity:<br><br>",
        "1. Purpose:<br>",
        "   - Examines how study characteristics (moderators) relate to effect sizes<br>",
        "   - Helps explain between-study heterogeneity<br>",
        "   - Identifies factors that influence treatment effectiveness<br><br>",
        "2. Regression Plot:<br>",
        "   - Scatter plot showing relationship between moderator and effect size<br>",
        "   - Regression line indicates predicted relationship<br>",
        "   - Bubble size represents study precision (inverse variance)<br><br>",
        "3. Statistical Results:<br>",
        "   - Slope coefficient indicates effect size change per unit moderator<br>",
        "   - R² shows proportion of heterogeneity explained by moderator<br>",
        "   - Permutation tests provide robust p-values for small samples<br><br>",
        "4. Model Diagnostics:<br>",
        "   - Residual plots assess model assumptions<br>",
        "   - Influence diagnostics identify influential studies<br>",
        "   - Cook's distance and hat values highlight potential outliers"
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
  if (FALSE) {
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
          bivariate_results(),
          data()  # Add the data parameter
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
  
  # ============================================================================
  # COMPREHENSIVE NORMALITY DIAGNOSTICS OUTPUTS
  # ============================================================================
  


  output$fixedNormalityTestSummary <- renderPrint({
    req(combinedResults()$fixed)
    tryCatch({
      print_normality_summary(
        run_all_normality_diagnostics("fixed", combinedResults()$fixed),
        "fixed"
      )
    }, error = function(e) {
      cat("Fixed Effects Normality Test Summary Unavailable\n")
      cat("Error:", e$message, "\n")
    })
  })
  
  # ----------------------------------------------------------------------------
  # RANDOM EFFECTS COMPREHENSIVE NORMALITY DIAGNOSTICS
  # ----------------------------------------------------------------------------
  


  # Side-by-side deleted residuals comparison plot
  output$randomDeletedResidualsComparisonPlot <- renderPlot({
    req(combinedResults()$random, combinedResults()$fixed)
    tryCatch({
      qq_plot_random_vs_fixed_deleted(combinedResults()$random, combinedResults()$fixed)
    }, error = function(e) {
      plot(1, type="n", main="Deleted Residuals Comparison Unavailable", xlab="", ylab="")
      text(1, 1, paste("Error:", e$message), cex=0.8)
    })
  })

  output$randomNormalityTestSummary <- renderPrint({
    req(combinedResults()$random)
    tryCatch({
      print_normality_summary(
        run_all_normality_diagnostics("random", combinedResults()$random),
        "random"
      )
    }, error = function(e) {
      cat("Random Effects Normality Test Summary Unavailable\n")
      cat("Error:", e$message, "\n")
    })
  })
  
  # ----------------------------------------------------------------------------
  # BIVARIATE MLE COMPREHENSIVE NORMALITY DIAGNOSTICS
  # ----------------------------------------------------------------------------
  
  
  
  output$bivariateNormalityTestSummary <- renderPrint({
    req(bivariate_result())
    tryCatch({
      print_normality_summary(
        run_all_normality_diagnostics("bivariate", bivariate_result(), data(), input),
        "bivariate"
      )
    }, error = function(e) {
    cat("JCR MLE Normality Test Summary Unavailable\n")
      cat("Error:", e$message, "\n")
    })
  })
  
  # ============================================================================
  # SUBGROUP ANALYSIS AND META-REGRESSION LOGIC
  # ============================================================================
  
  # Detect available subgroup variables
  output$hasSubgroupData <- reactive({
    req(data())
    df <- data()
    # Check if we have additional columns beyond the required ones
    min_cols <- if(input$data_type == "binary") 5 else 4
    has_extra <- ncol(df) > min_cols
    
    if (has_extra) {
      # Check if any additional columns are categorical/character
      extra_cols <- df[, (min_cols + 1):ncol(df), drop = FALSE]
      has_categorical <- any(sapply(extra_cols, function(x) is.character(x) || is.factor(x)))
      return(has_categorical)
    }
    return(FALSE)
  })
  
  outputOptions(output, "hasSubgroupData", suspendWhenHidden = FALSE)
  
  # Detect available moderator variables
  output$hasModeratorData <- reactive({
    req(data())
    df <- data()
    # Check if we have additional columns beyond the required ones
    min_cols <- if(input$data_type == "binary") 5 else 4
    return(ncol(df) > min_cols)
  })
  
  outputOptions(output, "hasModeratorData", suspendWhenHidden = FALSE)
  
  # Update subgroup variable choices for all analysis types
  observe({
    req(data())
    df <- data()
    min_cols <- if(input$data_type == "binary") 5 else 4
    
    if (ncol(df) > min_cols) {
      extra_cols <- names(df)[(min_cols + 1):ncol(df)]
      # Filter for categorical variables
      categorical_cols <- extra_cols[sapply(df[extra_cols], function(x) is.character(x) || is.factor(x))]
      
      # Update all subgroup variable selectors
      updateSelectInput(session, "random_subgroup_variable", 
                       choices = setNames(categorical_cols, categorical_cols))
      updateSelectInput(session, "fixed_subgroup_variable", 
                       choices = setNames(categorical_cols, categorical_cols))
      updateSelectInput(session, "bivariate_subgroup_variable", 
                       choices = setNames(categorical_cols, categorical_cols))
    }
  })
  
  # Update moderator variable choices
  observe({
    req(data())
    df <- data()
    min_cols <- if(input$data_type == "binary") 5 else 4
    
    if (ncol(df) > min_cols) {
      extra_cols <- names(df)[(min_cols + 1):ncol(df)]
      
      updateSelectInput(session, "moderator_variable", 
                       choices = setNames(extra_cols, extra_cols))
    }
  })
  
  # Random Effects Subgroup Analysis
  random_subgroup_results <- eventReactive(input$run_random_subgroup, {
    req(data(), input$random_subgroup_variable)
    df <- data()
    
    if (input$data_type == "binary") {
      res <- metabin(event.e = df$ie, 
                     n.e = df$it,
                     event.c = df$pe,
                     n.c = df$pt,
                     studlab = df$study,
                     sm = input$effect_measure,
                     method.tau = input$het_estimator,
                     byvar = df[[input$random_subgroup_variable]],
                     common = FALSE,
                     random = TRUE,
                     print.byvar = FALSE)
    } else {
      se <- (df$ci_upper - df$ci_lower) / (2 * 1.96)
      res <- metagen(TE = df$smd,
                     seTE = se,
                     studlab = df$study,
                     sm = "SMD",
                     method.tau = input$het_estimator,
                     byvar = df[[input$random_subgroup_variable]],
                     common = FALSE,
                     random = TRUE,
                     print.byvar = FALSE)
    }
    res
  })
  
  output$randomSubgroupForestPlot <- renderPlot({
    req(random_subgroup_results())
    meta::forest(random_subgroup_results(),
                 test.overall.random = TRUE,
                 test.subgroup.random = TRUE,
                 print.byvar = FALSE,
                 fontsize = 10,
                 xlab = paste0("Effect Size (", effect_measure_label(), ")"),
                 main = paste0("Random Effects Subgroup Analysis by ", input$random_subgroup_variable))
  }, height = function() {
    req(data())
    400 + nrow(data()) * 25
  })
  
  output$randomSubgroupTest <- renderPrint({
    req(random_subgroup_results())
    cat("Random Effects Subgroup Analysis Results\n")
    cat("=========================================\n\n")
    cat("Test for Subgroup Differences:\n")
    cat("Q-statistic between groups:", random_subgroup_results()$Q.b.random, "\n")
    cat("Degrees of freedom:", random_subgroup_results()$df.Q.b, "\n")
    cat("P-value:", random_subgroup_results()$pval.Q.b.random, "\n\n")
    
    if (random_subgroup_results()$pval.Q.b.random < 0.05) {
      cat("Interpretation: There is statistically significant evidence of subgroup differences (p < 0.05).\n")
      cat("The effect size varies significantly between the subgroups using the random effects model.\n")
    } else {
      cat("Interpretation: There is no statistically significant evidence of subgroup differences (p ≥ 0.05).\n")
      cat("The effect size does not vary significantly between the subgroups.\n")
    }
  })
  
  # Fixed Effects Subgroup Analysis
  fixed_subgroup_results <- eventReactive(input$run_fixed_subgroup, {
    req(data(), input$fixed_subgroup_variable)
    df <- data()
    
    if (input$data_type == "binary") {
      res <- metabin(event.e = df$ie, 
                     n.e = df$it,
                     event.c = df$pe,
                     n.c = df$pt,
                     studlab = df$study,
                     sm = input$effect_measure,
                     method.tau = input$het_estimator,
                     byvar = df[[input$fixed_subgroup_variable]],
                     common = TRUE,
                     random = FALSE,
                     print.byvar = FALSE)
    } else {
      se <- (df$ci_upper - df$ci_lower) / (2 * 1.96)
      res <- metagen(TE = df$smd,
                     seTE = se,
                     studlab = df$study,
                     sm = "SMD",
                     method.tau = input$het_estimator,
                     byvar = df[[input$fixed_subgroup_variable]],
                     common = TRUE,
                     random = FALSE,
                     print.byvar = FALSE)
    }
    res
  })
  
  output$fixedSubgroupForestPlot <- renderPlot({
    req(fixed_subgroup_results())
    meta::forest(fixed_subgroup_results(),
                 test.overall.common = TRUE,
                 test.subgroup.common = TRUE,
                 print.byvar = FALSE,
                 fontsize = 10,
                 xlab = paste0("Effect Size (", effect_measure_label(), ")"),
                 main = paste0("Fixed Effects Subgroup Analysis by ", input$fixed_subgroup_variable))
  }, height = function() {
    req(data())
    400 + nrow(data()) * 25
  })
  
  output$fixedSubgroupTest <- renderPrint({
    req(fixed_subgroup_results())
    cat("Fixed Effects Subgroup Analysis Results\n")
    cat("=======================================\n\n")
    cat("Test for Subgroup Differences:\n")
    cat("Q-statistic between groups:", fixed_subgroup_results()$Q.b.common, "\n")
    cat("Degrees of freedom:", fixed_subgroup_results()$df.Q.b, "\n")
    cat("P-value:", fixed_subgroup_results()$pval.Q.b.common, "\n\n")
    
    if (fixed_subgroup_results()$pval.Q.b.common < 0.05) {
      cat("Interpretation: There is statistically significant evidence of subgroup differences (p < 0.05).\n")
      cat("The effect size varies significantly between the subgroups using the fixed effects model.\n")
    } else {
      cat("Interpretation: There is no statistically significant evidence of subgroup differences (p ≥ 0.05).\n")
      cat("The effect size does not vary significantly between the subgroups.\n")
    }
  })
  
  # JCR Subgroup Analysis
  bivariate_subgroup_results <- eventReactive(input$run_bivariate_subgroup, {
    req(data(), input$bivariate_subgroup_variable)
    df <- data()
    
    # Get unique subgroup levels
    subgroup_levels <- unique(df[[input$bivariate_subgroup_variable]])
    results <- list()
    
    for (level in subgroup_levels) {
      subset_data <- df[df[[input$bivariate_subgroup_variable]] == level, ]
      
      if (input$data_type == "binary") {
        res <- metabiv(event.e = subset_data$ie, 
                       n.e = subset_data$it, 
                       event.c = subset_data$pe, 
                       n.c = subset_data$pt,
                       studlab = subset_data$study,
                       sm = input$effect_measure)
      } else {
        se <- (subset_data$ci_upper - subset_data$ci_lower) / (2 * 1.96)
        var <- se^2
        res <- metabiv(studlab = subset_data$study,
                       sm = "SMD",
                       y = subset_data$smd,
                       sigma2 = var)
      }
      results[[level]] <- res
    }
    
    list(results = results, variable = input$bivariate_subgroup_variable, data = df)
  })
  
  output$bivariateSubgroupForestPlot <- renderPlot({
    req(bivariate_subgroup_results())
    results <- bivariate_subgroup_results()$results
    
    # Create a combined forest plot
    par(mfrow = c(length(results), 1), mar = c(4, 4, 2, 2))
    
    for (i in seq_along(results)) {
      level_name <- names(results)[i]
      result <- results[[i]]
      
      forest.metabiv(result,
                     title = paste("Subgroup:", level_name),
                     xlab = paste("Effect Size (", effect_measure_label(), ")"))
    }
    
    par(mfrow = c(1, 1))
  }, height = function() {
    req(bivariate_subgroup_results())
    length(bivariate_subgroup_results()$results) * 400
  })
  
  output$bivariateSubgroupTest <- renderPrint({
    req(bivariate_subgroup_results())
    results <- bivariate_subgroup_results()$results
    
    cat("JCR Subgroup Analysis Results\n")
    cat("===================================\n\n")
    
    for (i in seq_along(results)) {
      level_name <- names(results)[i]
      result <- results[[i]]
      
      cat("Subgroup:", level_name, "\n")
      cat("Effect estimate (μ):", round(result$mu, 4), "\n")
      cat("95% CI: [", round(result$lower, 4), ", ", round(result$upper, 4), "]\n")
      cat("Heterogeneity (τ):", round(result$tau, 4), "\n")
      cat("I²:", round(result$I2, 1), "%\n\n")
    }
    
    cat("Note: Formal statistical test for subgroup differences is not implemented for the JCR method.\n")
    cat("Compare confidence intervals and effect estimates between subgroups.\n")
  })
  
  # Meta-Regression Results
  metaregression_results <- eventReactive(input$run_metaregression, {
    req(data(), input$moderator_variable)
    df <- data()
    
    # Remove rows with missing moderator data
    complete_cases <- !is.na(df[[input$moderator_variable]])
    df_complete <- df[complete_cases, ]
    
    if (nrow(df_complete) < 3) {
      stop("Insufficient data for meta-regression. Need at least 3 studies with complete moderator data.")
    }
    
    if (nrow(df_complete) < 10) {
      showNotification("Warning: Fewer than 10 studies – meta-regression results (p-values, R²) may be unreliable.",
                      type = "warning", duration = 8)
    }
    
    if (input$data_type == "binary") {
      base_model <- metabin(event.e = df_complete$ie, 
                           n.e = df_complete$it,
                           event.c = df_complete$pe,
                           n.c = df_complete$pt,
                           studlab = df_complete$study,
                           sm = input$effect_measure,
                           method.tau = input$het_estimator,
                           common = FALSE,
                           random = input$use_random_effects)
      
      # Extract effect sizes and standard errors for metareg
      TE <- base_model$TE
      seTE <- base_model$seTE
    } else {
      se <- (df_complete$ci_upper - df_complete$ci_lower) / (2 * 1.96)
      TE <- df_complete$smd
      seTE <- se
    }
    
    # Prepare moderator variable
    moderator_var <- df_complete[[input$moderator_variable]]
    
    # Handle categorical vs continuous
    if (input$moderator_type == "categorical" || is.character(moderator_var) || is.factor(moderator_var)) {
      # Convert to factor if not already
      moderator_var <- as.factor(moderator_var)
      
      # Determine test statistic (Knapp-Hartung for random effects)
      test_method <- if (input$use_random_effects) "knha" else "z"
      # Map heterogeneity estimator choice to metafor::rma method argument
      rma_method <- if (input$use_random_effects) switch(input$het_estimator,
                                                         "DL" = "DL",
                                                         "PM" = "PM",
                                                         "REML" = "REML",
                                                         "ML" = "ML",
                                                         "DL") else "FE"
      
      # Run meta-regression with categorical moderator
      metareg_result <- metafor::rma(yi = TE, vi = seTE^2, 
                                    mods = ~ moderator_var, 
                                    method = rma_method,
                                    test  = test_method)
    } else {
      # Continuous moderator
      moderator_var <- as.numeric(moderator_var)
      
      # Run meta-regression with continuous moderator
      metareg_result <- metafor::rma(yi = TE, vi = seTE^2, 
                                    mods = ~ moderator_var, 
                                    method = if(input$use_random_effects) "REML" else "FE",
                                    test  = test_method)
    }
    
    # Optional permutation test for robust p-values
    perm_result <- NULL
    if (isTRUE(input$meta_perm_test)) {
      perm_result <- tryCatch({
        metafor::permutest(metareg_result, iter = 2000)
      }, error = function(e) {
        showNotification(paste("Permutation test failed:", e$message), type = "error")
        NULL
      })
    }
    
    list(
      model = metareg_result,
      moderator_data = moderator_var,
      moderator_name = input$moderator_variable,
      moderator_type = input$moderator_type,
      effect_sizes = TE,
      standard_errors = seTE,
      study_names = df_complete$study,
      use_random_effects = input$use_random_effects,
      perm_result = perm_result
    )
  })
  
  # Meta-Regression Plot
  output$metaregressionPlot <- renderPlot({
    req(metaregression_results())
    results <- metaregression_results()
    
    if (results$moderator_type == "categorical" || is.factor(results$moderator_data)) {
      # Categorical moderator - create a grouped plot
      moderator_levels <- levels(as.factor(results$moderator_data))
      
      # Calculate means and SEs for each group
      group_means <- tapply(results$effect_sizes, results$moderator_data, mean)
      group_ses <- tapply(results$standard_errors, results$moderator_data, function(x) sqrt(mean(x^2)))
      
      # Create bar plot with error bars
      bp <- barplot(group_means, 
                   xlab = results$moderator_name,
                   ylab = paste0("Effect Size (", effect_measure_label(), ")"),
                   main = paste0("Meta-Regression: ", results$moderator_name, " (Categorical)"),
                   col = "lightblue",
                   ylim = c(min(group_means - 1.96 * group_ses), max(group_means + 1.96 * group_ses)))
      
      # Add error bars
      arrows(bp, group_means - 1.96 * group_ses,
             bp, group_means + 1.96 * group_ses,
             length = 0.05, angle = 90, code = 3, col = "red", lwd = 2)
      
      # Add individual points
      for (i in seq_along(moderator_levels)) {
        level <- moderator_levels[i]
        indices <- which(results$moderator_data == level)
        jittered_x <- rep(bp[i], length(indices)) + runif(length(indices), -0.1, 0.1)
        points(jittered_x, results$effect_sizes[indices], pch = 19, cex = 0.8)
      }
      
    } else {
      # Continuous moderator - scatter plot with regression line
      plot(results$moderator_data, results$effect_sizes,
           xlab = results$moderator_name,
           ylab = paste0("Effect Size (", effect_measure_label(), ")"),
           main = paste0("Meta-Regression: ", results$moderator_name, " (Continuous)"),
           pch = 19, cex = 1.2)
      
      # Add study labels
      text(results$moderator_data, results$effect_sizes, 
           labels = results$study_names, pos = 3, cex = 0.7, col = "blue")
      
      # Add regression line and confidence band
      mod_seq <- seq(min(results$moderator_data, na.rm = TRUE), 
                     max(results$moderator_data, na.rm = TRUE), 
                     length.out = 100)
      pred_results <- predict(results$model, newmods = mod_seq)
      lines(mod_seq, pred_results$pred, col = "red", lwd = 2)
      
      # Add confidence bands if available
      if (!is.null(pred_results$se)) {
        lines(mod_seq, pred_results$pred + 1.96 * pred_results$se, col = "red", lty = 2)
        lines(mod_seq, pred_results$pred - 1.96 * pred_results$se, col = "red", lty = 2)
      }
      
      # Add confidence intervals as error bars for individual studies
      arrows(results$moderator_data, 
             results$effect_sizes - 1.96 * results$standard_errors,
             results$moderator_data, 
             results$effect_sizes + 1.96 * results$standard_errors,
             length = 0.05, angle = 90, code = 3, col = "gray50")
    }
  })
  
  # Meta-Regression Summary
  output$metaregressionSummary <- renderPrint({
    req(metaregression_results())
    res <- metaregression_results()
    print(summary(res$model))
    if (!is.null(res$perm_result)) {
      cat("\nPermutation Test (robust p-values):\n")
      print(res$perm_result)
    }
  })
  
  # Meta-Regression Interpretation
  output$metaregressionInterpretation <- renderPrint({
    req(metaregression_results())
    results <- metaregression_results()
    model <- results$model
    
    cat("Meta-Regression Interpretation\n")
    cat("==============================\n\n")
    
    cat("Model Type:", if(results$use_random_effects) "Random Effects" else "Fixed Effects", "\n")
    cat("Moderator Variable:", results$moderator_name, "\n")
    cat("Moderator Type:", results$moderator_type, "\n")
    cat("Number of Studies:", length(results$effect_sizes), "\n\n")
    
    # Check for overall significance of moderator
    moderator_significant <- any(model$pval[-1] < 0.05, na.rm = TRUE)
    
    if (moderator_significant) {
      cat("SIGNIFICANT MODERATOR EFFECT DETECTED\n")
      cat("=====================================\n")
      cat("The moderator variable '", results$moderator_name, "' significantly explains heterogeneity (p < 0.05).\n\n")
      
      if (results$moderator_type == "continuous") {
        slope <- model$beta[2]
        cat("Regression Coefficient (slope):", round(slope, 4), "\n")
        cat("Interpretation: For every 1-unit increase in ", results$moderator_name, 
            ", the effect size changes by ", round(slope, 4), " units.\n\n")
        
        if (slope > 0) {
          cat("Direction: Positive association - higher values of ", results$moderator_name, 
              " are associated with larger effect sizes.\n")
        } else {
          cat("Direction: Negative association - higher values of ", results$moderator_name, 
              " are associated with smaller effect sizes.\n")
        }
      } else {
        cat("Categorical Moderator Effects:\n")
        cat("Reference group: ", names(model$beta)[1], "\n")
        for (i in 2:length(model$beta)) {
          coef_name <- names(model$beta)[i]
          coef_val <- model$beta[i]
          p_val <- model$pval[i]
          cat(coef_name, ": ", round(coef_val, 4), " (p = ", round(p_val, 3), ")\n")
        }
      }
    } else {
      cat("NO SIGNIFICANT MODERATOR EFFECT\n")
      cat("===============================\n")
      cat("The moderator variable '", results$moderator_name, "' does not significantly explain heterogeneity (p ≥ 0.05).\n")
      cat("This suggests that ", results$moderator_name, " does not account for differences between studies.\n")
    }
    
    cat("\nModel Fit Statistics:\n")
    cat("--------------------\n")
    if (!is.null(model$I2)) {
      cat("Residual heterogeneity (I²):", round(model$I2, 1), "%\n")
      if (moderator_significant) {
        # Calculate explained variance (approximate)
        cat("Approximate variance explained:", round(100 - model$I2, 1), "%\n")
      }
    }
    
    cat("tau² (residual heterogeneity):", round(model$tau2, 4), "\n")
    cat("R² (proportion of variance explained):", if(!is.null(model$R2)) round(model$R2, 3) else "Not available", "\n")
    
    cat("\nModel Recommendation:\n")
    cat("--------------------\n")
    if (moderator_significant) {
      cat("✓ The moderator provides valuable insight into study heterogeneity.\n")
      cat("✓ Consider this variable when interpreting meta-analysis results.\n")
      if (results$moderator_type == "continuous") {
        cat("✓ Consider dose-response relationships in clinical application.\n")
      } else {
        cat("✓ Consider subgroup-specific recommendations.\n")
      }
    } else {
      cat("• The moderator does not explain heterogeneity in this analysis.\n")
      cat("• Consider other potential moderators or sources of heterogeneity.\n")
      cat("• Results suggest the overall effect may be generalizable across ", results$moderator_name, " levels.\n")
    }
    
    # Report omnibus test for moderators
    if (!is.null(model$QM)) {
      cat("Test of Moderator (QM):", round(model$QM,3), "on", model$df.QM, "df  (p =", sprintf("%.3f", model$pval.QM), ")\n")
    }
    if (!is.null(model$QE)) {
      cat("Residual Heterogeneity (QE):", round(model$QE,3), "on", model$df.QE, "df  (p =", sprintf("%.3f", model$pval.QE), ")\n\n")
    }
  })
  
  # Bubble Plot
  output$bubblePlot <- renderPlot({
    req(metaregression_results())
    results <- metaregression_results()
    
    # Calculate bubble sizes based on precision (inverse variance)
    weights <- 1 / (results$standard_errors^2)
    bubble_sizes <- sqrt(weights / max(weights)) * 3  # Scale for visibility
    
    if (results$moderator_type == "categorical" || is.factor(results$moderator_data)) {
      # For categorical variables, use numeric positions
      x_pos <- as.numeric(as.factor(results$moderator_data))
      plot(x_pos, results$effect_sizes,
           cex = bubble_sizes,
           pch = 19, col = alpha("blue", 0.6),
           xlab = results$moderator_name,
           ylab = paste0("Effect Size (", effect_measure_label(), ")"),
           main = paste0("Bubble Plot: ", results$moderator_name, " (Categorical)"),
           xaxt = "n")
      axis(1, at = unique(x_pos), labels = levels(as.factor(results$moderator_data)))
    } else {
      # Continuous variable
      plot(results$moderator_data, results$effect_sizes,
           cex = bubble_sizes,
           pch = 19, col = alpha("blue", 0.6),
           xlab = results$moderator_name,
           ylab = paste0("Effect Size (", effect_measure_label(), ")"),
           main = paste0("Bubble Plot: ", results$moderator_name, " (Continuous)"))
      
      # Add regression line
      mod_seq <- seq(min(results$moderator_data, na.rm = TRUE), 
                     max(results$moderator_data, na.rm = TRUE), 
                     length.out = 100)
      pred_results <- predict(results$model, newmods = mod_seq)
      lines(mod_seq, pred_results$pred, col = "red", lwd = 2)
    }
    
    # Add legend for bubble sizes
    legend("topright", 
           legend = c("Large studies", "Small studies"),
           pch = 19, 
           pt.cex = c(2, 0.5),
           col = alpha("blue", 0.6),
           title = "Study Precision")
  })
  
  # Residual Plot for Meta-Regression
  output$residualPlot <- renderPlot({
    req(metaregression_results())
    results <- metaregression_results()
    
    # Calculate residuals
    fitted_values <- fitted(results$model)
    residuals <- residuals(results$model)
    
    plot(fitted_values, residuals,
         xlab = "Fitted Values",
         ylab = "Residuals",
         main = "Residual Plot for Meta-Regression",
         pch = 19, cex = 1.2)
    
    # Add horizontal line at 0
    abline(h = 0, col = "red", lty = 2, lwd = 2)
    
    # Add study labels for outliers (residuals > 2 SD)
    residual_sd <- sd(residuals, na.rm = TRUE)
    outliers <- abs(residuals) > 2 * residual_sd
    if (any(outliers)) {
      text(fitted_values[outliers], residuals[outliers], 
           labels = results$study_names[outliers], 
           pos = 3, cex = 0.8, col = "red")
    }
    
    # Add smoother to detect patterns
    if (length(fitted_values) > 3) {
      smooth_line <- lowess(fitted_values, residuals)
      lines(smooth_line, col = "blue", lwd = 2)
    }
    
    # Add text explanation
    mtext("Look for random scatter around zero. Patterns may indicate model issues.", 
          side = 1, line = 4, cex = 0.8, col = "gray50")
  })
  
  # Influence diagnostics for Meta-Regression
  output$metaregInfluencePlot <- renderPlot({
    req(metaregression_results())
    tryCatch({
      inf <- stats::influence(metaregression_results()$model)
      plot(inf, label = TRUE)
    }, error = function(e){
      plot(1, type="n", axes=FALSE, xlab="", ylab="", main="Influence diagnostics unavailable")
      text(1,1, paste("Error:", e$message))
    })
  })
  
  output$metaregInfluenceSummary <- renderPrint({
    req(metaregression_results())
    tryCatch({
      inf <- stats::influence(metaregression_results()$model)
      print(inf)
    }, error = function(e){
      cat("Influence diagnostics unavailable:\n", e$message)
    })
  })
  
  # Combined forest plot for JCR subgroups
  output$bivariateSubgroupForestPlot <- renderPlot({
    req(bivariate_subgroup_results())
    results <- bivariate_subgroup_results()$results
    # Load grid packages
    library(grid)
    library(gridExtra)
    plots <- lapply(names(results), function(nm){
      grid::grid.grabExpr({
        tryCatch({
          forest.metabiv(results[[nm]], title = paste("Subgroup:", nm),
                         xlab = paste("Effect Size (", effect_measure_label(), ")"))
        }, error = function(e){
          plot(1, type="n", main = paste("Plot unavailable (", nm, ")")); text(1,1,e$message)
        })
      })
    })
    do.call(gridExtra::grid.arrange, c(plots, ncol=1))
  }, height = function(){
    req(bivariate_subgroup_results())
    length(bivariate_subgroup_results()$results)*400
  })
  
  observeEvent(input$prepareReport, {
    showModal(modalDialog(
      title = "Generate Report",
      checkboxInput("include_subgroup_report", "Include Subgroup Analysis Sections", value = FALSE),
      conditionalPanel(
        condition = "input.include_subgroup_report == true",
        selectInput("report_subgroup_variable", "Subgroup Variable:", 
                   choices = reactive({
                     req(data())
                     cols <- names(data())
                     # Exclude standard columns based on data type
                     if (input$data_type == "binary") {
                       cols <- setdiff(cols, c("study", "ie", "it", "pe", "pt"))
                     } else {
                       cols <- setdiff(cols, c("study", "smd", "ci_lower", "ci_upper"))
                     }
                     cols
                   })()),
        helpText("Select the subgroup variable to use in the report.")
      ),
      checkboxInput("include_metareg_report", "Include Meta-Regression Section", value = FALSE),
      conditionalPanel(
        condition = "input.include_metareg_report == true",
        selectInput("report_mod_variable", "Moderator Variable:", 
                   choices = reactive({
                     req(data())
                     cols <- names(data())
                     # Exclude standard columns based on data type
                     if (input$data_type == "binary") {
                       cols <- setdiff(cols, c("study", "ie", "it", "pe", "pt"))
                     } else {
                       cols <- setdiff(cols, c("study", "smd", "ci_lower", "ci_upper"))
                     }
                     cols
                   })()),
        radioButtons("report_mod_type", "Variable Type:", choices = list("Continuous"="continuous","Categorical"="categorical"), inline=TRUE),
        checkboxInput("report_use_random", "Use Random Effects Model", TRUE),
        checkboxInput("report_perm_test", "Permutation Test (robust p-values)", FALSE)
      ),
      footer = tagList(
        modalButton("Cancel"),
        downloadButton("downloadReportFile", "Generate Report", class = "btn-primary")
      ),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
  # New download handler that respects options chosen in the modal
  output$downloadReportFile <- downloadHandler(
    filename = function() {
      paste("meta_analysis_report_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      withProgress(message = 'Generating report...', value = 0, {
        incProgress(0.3)
        
        # Get the selected variables
        subgroup_var <- if (isTRUE(input$include_subgroup_report)) input$report_subgroup_variable else NULL
        moderator_var <- if (isTRUE(input$include_metareg_report)) input$report_mod_variable else NULL
        moderator_type <- if (isTRUE(input$include_metareg_report)) input$report_mod_type else NULL

        output_file <- render_report(
          random_results(),
          fixed_results(),
          bivariate_results(),
          data(),
          include_subgroup = isTRUE(input$include_subgroup_report),
          include_metareg = isTRUE(input$include_metareg_report),
          subgroup_var = subgroup_var,
          moderator_var = moderator_var,
          moderator_type = moderator_type
        )

        incProgress(0.6)
        file.copy(output_file, file)
        incProgress(1)
      })
    }
  )
  
  # ------------------------------------------------------------------
  # Flag to indicate that data are loaded and Analyze was clicked
  # This drives visibility of the "Download Report" button in the UI
  # ------------------------------------------------------------------
  output$analysisReady <- reactive({
    req(input$analyze)        # ensure button was pressed at least once
    d <- tryCatch(data(), error=function(e) NULL)
    return(!is.null(d) && nrow(d) > 0)
  })
  outputOptions(output, "analysisReady", suspendWhenHidden = FALSE)
}


