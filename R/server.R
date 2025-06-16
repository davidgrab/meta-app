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
  smdData <- read.csv("data/smd_example.csv", stringsAsFactors = FALSE)

  print("Functions sourced")
  
  # Initially hide all tabs except Data Preview
  observe({
    print("Hiding tabs")
    # Also hide Meta-Regression tab initially
    lapply(c("Overall Results", "Random Effects Analysis", "Fixed Effects Analysis", "Bivariate Approach", "Meta-Regression"), function(tab) {
      hideTab(inputId = "main_tabs", target = tab)
    })
  })
  
  # Show all tabs when Analyze button is clicked
  observeEvent(input$analyze, {
    print("Analyze button clicked")
    # Show common tabs first
    common_tabs <- c("Overall Results", "Random Effects Analysis", "Fixed Effects Analysis")
    lapply(common_tabs, function(tab) {
      showTab(inputId = "main_tabs", target = tab)
    })

    subgroup_selected <- !is.null(input$subgroup_var) && input$subgroup_var != ""
    metareg_selected <- !is.null(input$metareg_vars) && length(input$metareg_vars) > 0

    # Conditionally show or hide Bivariate Approach tab
    if (input$data_type == "hr" || subgroup_selected || metareg_selected) {
      hideTab(inputId = "main_tabs", target = "Bivariate Approach")
      if (input$data_type == "hr") print("Data type is HR, hiding Bivariate Approach tab.")
      if (subgroup_selected) print("Subgroup analysis active, hiding Bivariate Approach tab.")
      if (metareg_selected && !subgroup_selected && input$data_type != "hr") print("Meta-regression active, hiding Bivariate Approach tab.")
    } else {
      showTab(inputId = "main_tabs", target = "Bivariate Approach")
      print(paste("Data type is", input$data_type, ", no subgroup analysis, and no meta-regression, showing Bivariate Approach tab."))
    }

    # Notification for meta-regression and subgroup analysis conflict
    if (subgroup_selected && metareg_selected) {
      showNotification("Subgroup analysis and meta-regression are both selected. Meta-regression will be skipped. Bivariate tab will also be hidden.",
                       type = "warning", duration = 10)
    }

    # Conditionally show or hide Meta-Regression tab
    if (metareg_selected && !subgroup_selected) {
      showTab(inputId = "main_tabs", target = "Meta-Regression")
      print("Meta-regression selected, showing Meta-Regression tab.")
    } else {
      hideTab(inputId = "main_tabs", target = "Meta-Regression")
      if (metareg_selected && subgroup_selected) print("Meta-regression will be skipped due to active subgroup analysis; hiding Meta-Regression tab.")
      else if (!metareg_selected) print("No meta-regression covariates selected, hiding Meta-Regression tab.")
    }
  })
  
  # React to dark mode toggle
  observeEvent(input$dark_mode, {
    print(paste("Dark mode toggled:", input$dark_mode))
    if (input$dark_mode == "light") {
      showNotification("Welcome to the light side!")
    }else {
      showNotification("Welcome to the dark side!")
    }
  })
  
  # App Info popup
  observeEvent(input$info, {
    print("Info button clicked")
    showModal(modalDialog(
      title = "About this App",
      HTML(paste0("This app performs advanced meta-analysis with GRADE assessment. Upload your data, choose analysis options, and explore the results across different models.",
                  " Key features include support for various data types (binary, continuous/SMD, Hazard Ratio), heterogeneity assessment, publication bias diagnostics, sensitivity analyses, and GRADE quality assessment. ",
                  "The app also supports subgroup analyses to investigate sources of heterogeneity based on study-level covariates. ",
                  "Additionally, meta-regression can be performed to investigate how study characteristics (covariates) influence the observed effect sizes.")),
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
        "1. Prepare your CSV or Excel file with the following columns: <b>study, ie, it, pe, pt</b>.<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>study</b>: Study label<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>ie</b>: Intervention group events<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>it</b>: Intervention group total<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>pe</b>: Placebo/control group events<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>pt</b>: Placebo/control group total<br><br>",
        "2. Click 'Browse' to select your file.<br><br>",
        "3. The data will load and display in the 'Data Preview' tab.<br><br>",
        "4. Optionally, include additional columns with categorical study characteristics (e.g., study setting, patient demographics) for subgroup analysis. Ensure these columns have a limited number of distinct categories.<br><br>",
        "5. For meta-regression, ensure your dataset includes columns for potential covariates (moderators). These can be numeric (e.g., average age, year of publication) or categorical (e.g., study setting, intervention type)."
      )
    )
    
    smd_instructions <- HTML(
      paste0(
        "<h4>Continuous (SMD) Data</h4>",
        "1. Prepare your CSV or Excel file with the following columns: <b>study, smd, ci_lower, ci_upper</b>.<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>study</b>: Study label<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>smd</b>: Standardized Mean Difference<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>ci_lower</b>: Lower bound of the 95% confidence interval<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;• <b>ci_upper</b>: Upper bound of the 95% confidence interval<br>",
        "<br><b>Note:</b> The SMD column may also appear as <b>CoNC</b> or <b>HeadGrid-G</b>. All are interpreted as SMD for now.<br><br>",
        "2. Click 'Browse' to select your file.<br><br>",
        "3. The app will calculate the standard error and variance for you.<br><br>",
        "4. Optionally, include additional columns with categorical study characteristics for subgroup analysis.<br><br>",
        "5. For meta-regression, ensure your dataset includes columns for potential covariates (moderators). These can be numeric (e.g., average age, year of publication) or categorical (e.g., study setting, intervention type)."
      )
    )
    
    hr_instructions <- HTML(
      paste0(
        "<h4>Hazard Ratio (HR) Data</h4>",
        "1. Prepare your CSV or Excel file with one of the following column structures:<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;<b>Format 1 (HR and Confidence Intervals):</b> study, hr, ci_lower, ci_upper<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;• <b>study</b>: Study label<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;• <b>hr</b>: Hazard Ratio<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;• <b>ci_lower</b>: Lower bound of the 95% confidence interval for HR<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;• <b>ci_upper</b>: Upper bound of the 95% confidence interval for HR<br><br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;<b>Format 2 (Log Hazard Ratio and Standard Error):</b> study, loghr, se_loghr<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;• <b>study</b>: Study label<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;• <b>loghr</b>: Natural logarithm of the Hazard Ratio<br>",
        "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;• <b>se_loghr</b>: Standard error of the log Hazard Ratio<br><br>",
        "2. Click 'Browse' to select your file.<br><br>",
        "3. The data will load and display in the 'Data Preview' tab. The app will convert Format 1 to logHR and SE for analysis.<br><br>",
        "4. Optionally, include additional columns with categorical study characteristics for subgroup analysis.<br><br>",
        "5. For meta-regression, ensure your dataset includes columns for potential covariates (moderators). These can be numeric (e.g., average age, year of publication) or categorical (e.g., study setting, intervention type)."
      )
    )

    modal_content <- if (input$data_type == "smd") {
      smd_instructions
    } else if (input$data_type == "hr") {
      hr_instructions
    } else { # binary
      # Add the meta-regression point to the binary_instructions string directly
      HTML(
        paste0(
          binary_instructions, # This already has points 1-4
          "<br><br>5. For meta-regression, ensure your dataset includes columns for potential covariates (moderators). These can be numeric (e.g., average age, year of publication) or categorical (e.g., study setting, intervention type)."
        )
      )
    }
    # If it's SMD or HR, the variables smd_instructions and hr_instructions already contain point 5.
    # If it's binary, we append point 5.
    # This avoids duplicating point 5 if modal_content is one of the pre-defined ones.
    # However, the current structure assigns one of the *_instructions to modal_content first, then shows it.
    # A cleaner way is to append to each instruction string first.

    # Re-evaluate modal_content with appended meta-regression info for all types
    general_metareg_note <- "<br><br><b>Meta-Regression Note:</b> For meta-regression, ensure your dataset includes columns for potential covariates (moderators). These can be numeric (e.g., average age, year of publication) or categorical (e.g., study setting, intervention type)."

    modal_content_final <- if (input$data_type == "smd") {
      HTML(paste0(smd_instructions)) # Already updated, but ensure it is used.
    } else if (input$data_type == "hr") {
      HTML(paste0(hr_instructions)) # Already updated
    } else { # binary
       HTML(paste0(
         "<h4>Binary (2x2) Data</h4>",
         "1. Prepare your CSV or Excel file with the following columns: <b>study, ie, it, pe, pt</b>.<br>",
         "&nbsp;&nbsp;&nbsp;&nbsp;• <b>study</b>: Study label<br>",
         "&nbsp;&nbsp;&nbsp;&nbsp;• <b>ie</b>: Intervention group events<br>",
         "&nbsp;&nbsp;&nbsp;&nbsp;• <b>it</b>: Intervention group total<br>",
         "&nbsp;&nbsp;&nbsp;&nbsp;• <b>pe</b>: Placebo/control group events<br>",
         "&nbsp;&nbsp;&nbsp;&nbsp;• <b>pt</b>: Placebo/control group total<br><br>",
         "2. Click 'Browse' to select your file.<br><br>",
         "3. The data will load and display in the 'Data Preview' tab.<br><br>",
         "4. Optionally, include additional columns with categorical study characteristics (e.g., study setting, patient demographics) for subgroup analysis. Ensure these columns have a limited number of distinct categories.",
         general_metareg_note # Append the general note
      ))
    }


    
    showModal(modalDialog(
      title = "How to Upload Data",
      modal_content_final, # Use the final version with the note
      easyClose = TRUE,
      footer = NULL
    ))
  })

  # Meta-Regression Info Popup
  observeEvent(input$metareg_info, {
    showModal(modalDialog(
      title = "Meta-Regression Information",
      HTML(paste0(
        "<h4>What is Meta-Regression?</h4>",
        "<p>Meta-regression is an extension to meta-analysis that allows you to investigate whether specific study characteristics (covariates or moderators) are associated with the magnitude of the effect size. It helps explore sources of heterogeneity between studies.</p>",
        "<h4>How to Select Covariates:</h4>",
        "<p>From the sidebar, you can select one or more covariates from your uploaded dataset. These can be:",
        "<ul><li><b>Numeric:</b> e.g., average age of participants, year of publication, dosage.</li>",
        "<li><b>Categorical:</b> e.g., study setting (hospital vs. community), type of intervention, patient population (male vs. female). Categorical variables will be dummy coded for the analysis.</li></ul></p>",
        "<h4>Interpreting the Summary Output:</h4>",
        "<ul><li><b>Coefficients (estimate):</b> For a numeric covariate, the coefficient indicates the change in the (log) effect size for each one-unit increase in the covariate. For a categorical covariate, it represents the difference in (log) effect size for that category compared to the reference category.</li>",
        "<li><b>p-value (pval):</b> A small p-value (typically < 0.05) for a coefficient suggests that the covariate is significantly associated with the effect size.</li>",
        "<li><b>Test of Moderators (QM):</b> This tests the overall significance of all covariates in the model. A significant p-value suggests that at least one covariate influences the effect size.</li>",
        "<li><b>Adjusted R-squared (R2.adj):</b> Indicates the proportion of between-study variance (heterogeneity) explained by the covariates in the model.</li>",
        "<li><b>Residual Heterogeneity (tau^2):</b> The amount of between-study variance remaining after accounting for the covariates.</li></ul>",
        "<h4>Interpreting the Bubble Plot:</h4>",
        "<p>The bubble plot visualizes the relationship between a selected numeric covariate (on the x-axis) and the study effect sizes (on the y-axis). Each bubble represents a study, and its size is typically proportional to the precision of the study (inverse of its variance). A regression line shows the trend. For HR/OR/RR, effect sizes are plotted on the log scale.</p>",
        "<h4>Interaction with Subgroup Analysis:</h4>",
        "<p>Currently, meta-regression will be skipped if a subgroup analysis is also selected. Please choose one or the other for analysis.</p>"
      )),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Dataset info popup
  observeEvent(input$dataset_info, {
    showModal(modalDialog(
      title = "Example Datasets Information",
      HTML(
        paste0(
          "<h4>Colditz et al. (1994) - BCG Vaccine Dataset</h4>",
          "<p>This dataset contains results from 13 studies examining the effectiveness of the Bacillus Calmette-Guerin (BCG) vaccine against tuberculosis. It shows substantial heterogeneity between studies, potentially related to the geographic latitude where the studies were conducted.</p>",
          "<p>Source: Available in the metadat R package as dat.colditz1994</p>",
          "<hr>",
          "<h4>Yusuf et al. (1985) - Beta-Blockers Dataset</h4>",
          "<p>This dataset contains results from 22 studies on the effectiveness of beta blockers for reducing mortality after myocardial infarction. It is from Table 6 of the original publication and demonstrates clear treatment effects with studies of varying sizes.</p>",
          "<p>Source: Available in the metafor R package as dat.yusuf1985</p>",
          "<hr>",
          "<h4>Hypericum (St. John's Wort) - Depression Dataset</h4>",
          "<p>This dataset comes from a Cochrane systematic review of randomized controlled trials comparing Hypericum extracts (St. John's Wort) to placebo in patients with major depressive disorder. It includes 18 RCTs with binary outcomes measuring response to treatment (responder vs. non-responder) reported as relative risk (RR).</p>",
          "<p>Hypericum extracts are herbal remedies used for treating depression symptoms, and this dataset demonstrates the effectiveness comparison against placebo treatments.</p>"
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Data loading
  data <- reactive({
    req(currentData())
    raw_df_snapshot <- currentData() # Keep a snapshot of currentData before it's modified by na.omit

    df_to_process <- currentData()
    if (input$remove_na) {
      # Important: na.omit removes rows based on ANY NA in that row.
      # This means if a subgroup or covariate column has NAs, rows might be removed
      # even if the main data columns (ie, it, smd, etc.) are complete for that row.
      # This is standard behavior but users should be aware.
      df_to_process <- na.omit(df_to_process)
      if(nrow(df_to_process) == 0) {
        showNotification("All rows removed after NA handling. Please check your data or 'Remove NA' option.", type="error", duration=10)
        return(NULL)
      }
    }

    processed_df <- NULL
    # Standardized column names for core data based on data_type
    if (input$data_type == "binary" && ncol(df_to_process) >= 5) {
      # Assume the first 5 columns are the core data if more columns exist (e.g. subgroup/covariates)
      temp_df <- df_to_process[, 1:5, drop = FALSE]
      names(temp_df) <- c("study", "ie", "it", "pe", "pt")
      temp_df$ie <- as.numeric(temp_df$ie)
      temp_df$it <- as.numeric(temp_df$it)
      temp_df$pe <- as.numeric(temp_df$pe)
      temp_df$pt <- as.numeric(temp_df$pt)
      processed_df <- temp_df
      # Now, add back any other original columns (subgroup, covariates) from df_to_process
      # This ensures they underwent the same NA filtering if applied
      other_cols <- names(df_to_process)[!names(df_to_process) %in% names(temp_df)[-1]] # exclude "study" to avoid dup
      if(length(other_cols) > 0 && "study" %in% names(processed_df)) {
         processed_df <- cbind(processed_df, df_to_process[, other_cols[other_cols != "study"], drop=FALSE])
      } else if (length(other_cols) > 0) {
         processed_df <- cbind(processed_df, df_to_process[, other_cols, drop=FALSE])
      }


    } else if (input$data_type == "smd" && ncol(df_to_process) >= 4) {
      temp_df <- df_to_process[, 1:4, drop = FALSE]
      names(temp_df) <- c("study", "smd", "ci_lower", "ci_upper")
      temp_df$smd <- as.numeric(temp_df$smd)
      temp_df$ci_lower <- as.numeric(temp_df$ci_lower)
      temp_df$ci_upper <- as.numeric(temp_df$ci_upper)
      processed_df <- temp_df
      other_cols <- names(df_to_process)[!names(df_to_process) %in% names(temp_df)[-1]]
       if(length(other_cols) > 0 && "study" %in% names(processed_df)) {
         processed_df <- cbind(processed_df, df_to_process[, other_cols[other_cols != "study"], drop=FALSE])
      } else if (length(other_cols) > 0) {
         processed_df <- cbind(processed_df, df_to_process[, other_cols, drop=FALSE])
      }

    } else if (input$data_type == "hr") {
      original_hr_names <- names(df_to_process) # Save original names before potential modification
      if (ncol(df_to_process) >= 4 && all(c("hr", "ci_lower", "ci_upper") %in% original_hr_names)) {
        temp_df_hr <- df_to_process[,c("study", "hr", "ci_lower", "ci_upper")]

        temp_df_hr$hr <- suppressWarnings(as.numeric(temp_df_hr$hr))
        temp_df_hr$ci_lower <- suppressWarnings(as.numeric(temp_df_hr$ci_lower))
        temp_df_hr$ci_upper <- suppressWarnings(as.numeric(temp_df_hr$ci_upper))

        if (any(temp_df_hr$hr <= 0, na.rm = TRUE) || any(temp_df_hr$ci_lower <= 0, na.rm = TRUE) || any(temp_df_hr$ci_upper <= 0, na.rm = TRUE) || any(temp_df_hr$ci_lower >= temp_df_hr$ci_upper, na.rm=TRUE)) {
          showNotification("HR and CIs must be positive and CIs logical. Please check input data.", type = "error", duration = 10)
          return(NULL)
        }
        if (any(is.na(temp_df_hr$hr)) || any(is.na(temp_df_hr$ci_lower)) || any(is.na(temp_df_hr$ci_upper))) {
          showNotification("Non-numeric HR or CI values detected after conversion. Please check input data.", type = "error", duration = 10)
          return(NULL)
        }
        loghr <- log(temp_df_hr$hr)
        se_loghr <- (log(temp_df_hr$ci_upper) - log(temp_df_hr$ci_lower)) / (2 * qnorm(0.975))
        processed_df <- data.frame(study = temp_df_hr$study, loghr = loghr, se_loghr = se_loghr)

        # Add back other columns
        other_cols_names <- original_hr_names[!original_hr_names %in% c("hr", "ci_lower", "ci_upper", "study")]
        if(length(other_cols_names) > 0){
            processed_df <- cbind(processed_df, df_to_process[, other_cols_names, drop=FALSE])
        }

      } else if (ncol(df_to_process) >= 3 && all(c("loghr", "se_loghr") %in% original_hr_names)) {
        temp_df_loghr <- df_to_process[,c("study", "loghr", "se_loghr")]
        temp_df_loghr$loghr <- suppressWarnings(as.numeric(temp_df_loghr$loghr))
        temp_df_loghr$se_loghr <- suppressWarnings(as.numeric(temp_df_loghr$se_loghr))
        if (any(is.na(temp_df_loghr$loghr)) || any(is.na(temp_df_loghr$se_loghr))) {
          showNotification("Non-numeric logHR or SE_logHR values detected. Please check input data.", type = "error", duration = 10)
          return(NULL)
        }
        processed_df <- temp_df_loghr
        # Add back other columns
        other_cols_names <- original_hr_names[!original_hr_names %in% c("loghr", "se_loghr", "study")]
         if(length(other_cols_names) > 0){
            processed_df <- cbind(processed_df, df_to_process[, other_cols_names, drop=FALSE])
        }
      } else {
        showNotification("For HR data, column names must be 'study', 'hr', 'ci_lower', 'ci_upper' OR 'study', 'loghr', 'se_loghr'. Additional columns for subgroup/meta-regression are allowed.", type = "error", duration = 15)
        return(NULL)
      }
    } else if (ncol(df_to_process) < 3) { # General check for too few columns
        showNotification("Uploaded data has too few columns for the selected data type.", type = "error", duration = 10)
        return(NULL)
    } else { # Data type not binary, smd, or hr, but has enough columns, assume it might be for other types later
        # Or, more likely, it's a mismatch with expected columns for selected type
        showNotification("Data type and column structure mismatch. Please ensure first columns match expected format (e.g., study, ie, it, pe, pt for binary).", type = "warning", duration=10)
        # Try to preserve original columns if no specific processing matched
        # but this is risky as downstream functions expect specific names.
        # For now, return NULL if no specific processing path was taken but data type was selected.
        if(input$data_type %in% c("binary", "smd", "hr")) return(NULL)
        processed_df <- df_to_process # Pass through if not a known type with specific processing
    }

    if (is.null(processed_df)) {
      showNotification("Data processing failed. Please check data format and column names.", type = "error", duration = 10)
      return(NULL)
    }

    # Ensure subgroup and meta-regression variables are correctly typed and appended
    # These variables are taken from 'df_to_process' which has undergone na.omit if selected
    # This ensures that the rows align with 'processed_df' if no rows were dropped during main data col processing
    if (!is.null(input$subgroup_var) && input$subgroup_var != "" && input$subgroup_var %in% names(df_to_process)) {
      if (nrow(processed_df) == nrow(df_to_process)) { # Check row alignment
        processed_df[[input$subgroup_var]] <- df_to_process[[input$subgroup_var]]
      } else {
        showNotification(paste("Could not align subgroup variable '", input$subgroup_var, "' due to row changes during NA removal or data processing. Subgroup analysis may be affected."), type="warning", duration=10)
      }
    }

    if (!is.null(input$metareg_vars) && length(input$metareg_vars) > 0) {
      for (cov_name in input$metareg_vars) {
        if (cov_name %in% names(df_to_process)) {
          if (nrow(processed_df) == nrow(df_to_process)) { # Check row alignment
            processed_df[[cov_name]] <- df_to_process[[cov_name]]
          } else {
             showNotification(paste("Could not align covariate '", cov_name, "' due to row changes. Meta-regression may be affected."), type="warning", duration=10)
          }
        }
      }
    }

    return(processed_df)
  })
  
  # New download
  output$downloadSampleStructure <- downloadHandler(
    filename = function() {
      if (input$data_type == "smd") {
        "sample_data_structure_continuous.csv"
      } else if (input$data_type == "hr") {
        "sample_data_structure_hr.csv"
      } else {
        "sample_data_structure_binary.csv"
      }
    },
    content = function(file) {
      if (input$data_type == "smd") {
        write.csv(sampleDataStructure_continuous, file, row.names = FALSE)
        cat("\n# Note: The 'smd' column may also appear as 'CoNC' or 'HeadGrid-G'. All are interpreted as SMD for now.\n", file = file, append = TRUE)
      } else if (input$data_type == "hr") {
        write.csv(sampleDataStructure_hr, file, row.names = FALSE)
        cat("\n# Note: Alternatively, you can provide columns named 'study', 'loghr', and 'se_loghr' (natural log of HR and its standard error).\n", file = file, append = TRUE)
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
      "colditz" = "13 studies on BCG vaccine effectiveness against tuberculosis. Classic dataset with substantial heterogeneity and potential moderators (latitude).",
      "yusuf" = "22 studies on beta-blockers for reducing mortality after myocardial infarction. Widely used dataset with clear treatment effects and varying study sizes.",
      "default" = "Cochrane review of 18 RCTs comparing Hypericum (St. John's Wort) to placebo in major depressive disorder. Binary outcome (response to treatment) reported as relative risk (RR).",
      "smd" = "87 studies comparing cognitive-behavioral therapy (CBT) to control conditions for depression, with outcomes reported as Hedges' g standardized mean difference (SMD)."
    )
    
    HTML(paste("<div style='font-size: 0.85em; margin-bottom: 10px; color: #666;'>", description, "</div>"))
  })
  
  # UI for subgroup variable selection
  output$subgroup_var_ui <- renderUI({
    req(currentData()) # Ensure data is loaded
    df_raw <- currentData()
    original_col_names <- names(df_raw)

    suitable_for_subgrouping <- sapply(original_col_names, function(col_name) {
      col_data <- df_raw[[col_name]]
      is_char_factor <- is.character(col_data) || is.factor(col_data)
      # Allow numeric if it has few unique values (more than 1, less than or equal to 7, and not all unique)
      is_sensible_numeric_subgroup <- is.numeric(col_data) &&
                                      length(unique(na.omit(col_data))) > 1 &&
                                      length(unique(na.omit(col_data))) <= 7 &&
                                      length(unique(na.omit(col_data))) < nrow(df_raw)
      return(is_char_factor || is_sensible_numeric_subgroup)
    })

    choices_for_subgroup <- original_col_names[suitable_for_subgrouping]

    selectInput("subgroup_var",
                "Select Subgroup Variable (Optional):",
                choices = c("None" = "", choices_for_subgroup),
                selected = "")
  })

  # UI for meta-regression covariate selection
  output$metareg_var_ui <- renderUI({
    req(currentData()) # Ensure data is loaded
    df_raw <- currentData()
    original_col_names <- names(df_raw)

    choices_for_metareg <- original_col_names[!original_col_names %in% c("study")]
    choices_for_metareg <- choices_for_metareg[sapply(choices_for_metareg, function(col) !all(is.na(df_raw[[col]])))]

    suitable_choices <- c()
    if (length(choices_for_metareg) > 0) {
        suitable_choices <- choices_for_metareg[sapply(choices_for_metareg, function(col_name) {
            col_data <- df_raw[[col_name]]
            is.numeric(col_data) || is.character(col_data) || is.factor(col_data)
        })]
    }

    selectizeInput("metareg_vars",
                   "Select Covariate(s) for Meta-Regression:",
                   choices = suitable_choices,
                   multiple = TRUE,
                   options = list(placeholder = 'None (overall analysis only)'))
  })

  # New observe events
  observeEvent(input$loadExampleData, {
    dataset_choice <- input$exampleDatasetChoice
    
    if (dataset_choice == "colditz") {
      currentData(colditzData)
      showNotification("Loaded Colditz et al. (1994) BCG Vaccine Dataset", type = "message")
    } else if (dataset_choice == "yusuf") {
      currentData(yusufData)
      showNotification("Loaded Yusuf et al. (1985) Beta-Blockers Dataset", type = "message")
    } else if (dataset_choice == "smd") {
      currentData(smdData)
      updateRadioButtons(session, "data_type", selected = "smd")
      showNotification("Loaded CBT for Depression (SMD) Dataset", type = "message")
    } else {
      currentData(exampleData)
      showNotification("Loaded Default Example Dataset", type = "message")
    }
  })
  
  observeEvent(input$datafile, {
    req(input$datafile)
    ext <- tools::file_ext(input$datafile$name)
    df <- switch(ext,
                 csv = read.csv(input$datafile$datapath, stringsAsFactors = FALSE),
                 xlsx = read_excel(input$datafile$datapath),
                 validate("Invalid file type. Please upload a .csv or .xlsx file.")
    )
    currentData(df)
  })
  
  output$dataPreview <- renderDT({
    print("Rendering data preview")
    data()
  })
  
  # New data structures
  sampleDataStructure_binary <- data.frame(
    study = c("Study 1", "Study 2"),
    ie = c(12, 8),
    it = c(100, 95),
    pe = c(10, 7),
    pt = c(98, 93),
    stringsAsFactors = FALSE
  )
  
  sampleDataStructure_continuous <- data.frame(
    study = c("Study 1", "Study 2"),
    smd = c(0.45, -0.12),
    ci_lower = c(0.10, -0.30),
    ci_upper = c(0.80, 0.06),
    stringsAsFactors = FALSE
  )
  attr(sampleDataStructure_continuous, "note") <- "Note: The 'smd' column may also appear as 'CoNC' or 'HeadGrid-G'. All are interpreted as SMD for now."

  sampleDataStructure_hr <- data.frame(
    study = c("Study H1", "Study H2"),
    hr = c(0.75, 1.25),
    ci_lower = c(0.60, 0.90),
    ci_upper = c(0.95, 1.75),
    stringsAsFactors = FALSE
  )
  
  # Combined analysis
  combinedResults <- eventReactive(input$analyze, {
    req(data())
    df <- data() # This now potentially includes the subgroup variable and covariates
    subgroup_variable_name <- input$subgroup_var
    
    random_model <- NULL
    fixed_model <- NULL
    bivariate_model <- NULL

    perform_subgroup_analysis <- !is.null(subgroup_variable_name) && subgroup_variable_name != "" && subgroup_variable_name %in% names(df)

    if (perform_subgroup_analysis) {
      print(paste("Performing subgroup analysis by:", subgroup_variable_name))
      bivariate_model <- NULL
      
      if (input$data_type == "binary") {
        req(df$study, df$ie, df$it, df$pe, df$pt, df[[subgroup_variable_name]])
        random_model <- metabin(event.e = df$ie, n.e = df$it, event.c = df$pe, n.c = df$pt,
                                studlab = df$study, sm = input$effect_measure, method.tau = input$het_estimator,
                                byvar = df[[subgroup_variable_name]], common = FALSE, random = TRUE)
        fixed_model <- metabin(event.e = df$ie, n.e = df$it, event.c = df$pe, n.c = df$pt,
                               studlab = df$study, sm = input$effect_measure,
                               byvar = df[[subgroup_variable_name]], common = TRUE, random = FALSE)
      } else if (input$data_type == "smd") {
        req(df$study, df$smd, df$ci_lower, df$ci_upper, df[[subgroup_variable_name]])
        se <- (df$ci_upper - df$ci_lower) / (2 * 1.96)
        random_model <- metagen(TE = df$smd, seTE = se, studlab = df$study, sm = "SMD",
                                method.tau = input$het_estimator, byvar = df[[subgroup_variable_name]],
                                common = FALSE, random = TRUE)
        fixed_model <- metagen(TE = df$smd, seTE = se, studlab = df$study, sm = "SMD",
                               byvar = df[[subgroup_variable_name]], common = TRUE, random = FALSE)
      } else if (input$data_type == "hr") {
        req(df$study, df$loghr, df$se_loghr, df[[subgroup_variable_name]])
        random_model <- metagen(TE = df$loghr, seTE = df$se_loghr, studlab = df$study, sm = "HR",
                                method.tau = input$het_estimator, byvar = df[[subgroup_variable_name]],
                                common = FALSE, random = TRUE)
        fixed_model <- metagen(TE = df$loghr, seTE = df$se_loghr, studlab = df$study, sm = "HR",
                               byvar = df[[subgroup_variable_name]], common = TRUE, random = FALSE)
      } else {
        showNotification("Subgroup analysis selected, but data type or columns are not as expected.", type = "error")
        return(NULL)
      }
    } else { # Overall analysis
      print("Performing overall analysis (no subgroup variable selected or found).")
      if (input$data_type == "binary" && all(c("study", "ie", "it", "pe", "pt") %in% names(df))) {
        random_model <- metabin(event.e = df$ie, n.e = df$it, event.c = df$pe, n.c = df$pt,
                                studlab = df$study, sm = input$effect_measure, method.tau = input$het_estimator,
                                common = FALSE, random = TRUE)
        fixed_model <- metabin(event.e = df$ie, n.e = df$it, event.c = df$pe, n.c = df$pt,
                               studlab = df$study, sm = input$effect_measure, common = TRUE, random = FALSE)
        if(is.null(input$metareg_vars) || length(input$metareg_vars) == 0) { # Bivariate only if no metareg
             bivariate_model <- metabiv(event.e = df$ie, n.e = df$it, event.c = df$pe, n.c = df$pt,
                                   studlab = df$study, sm = input$effect_measure)
        }
      } else if (input$data_type == "smd" && all(c("study", "smd", "ci_lower", "ci_upper") %in% names(df))) {
        se <- (df$ci_upper - df$ci_lower) / (2 * 1.96)
        var <- se^2
        random_model <- metagen(TE = df$smd, seTE = se, studlab = df$study, sm = "SMD",
                                method.tau = input$het_estimator, common = FALSE, random = TRUE)
        fixed_model <- metagen(TE = df$smd, seTE = se, studlab = df$study, sm = "SMD",
                               common = TRUE, random = FALSE)
        if(is.null(input$metareg_vars) || length(input$metareg_vars) == 0) { # Bivariate only if no metareg
            bivariate_model <- metabiv(studlab = df$study, sm = "SMD", y = df$smd, sigma2 = var)
        }
      } else if (input$data_type == "hr" && all(c("study", "loghr", "se_loghr") %in% names(df))) {
        random_model <- metagen(TE = df$loghr, seTE = df$se_loghr, studlab = df$study, sm = "HR",
                                method.tau = input$het_estimator, common = FALSE, random = TRUE)
        fixed_model <- metagen(TE = df$loghr, seTE = df$se_loghr, studlab = df$study, sm = "HR",
                               common = TRUE, random = FALSE)
        bivariate_model <- NULL
      } else {
        showNotification("Data columns do not match selected data type for overall analysis.", type = "warning")
        return(NULL)
      }
    }

    if (is.null(random_model) || is.null(fixed_model)) {
        if (!perform_subgroup_analysis || (is.null(bivariate_model) && input$data_type != "hr" && !perform_subgroup_analysis && (is.null(input$metareg_vars) || length(input$metareg_vars) == 0) )) {
             showNotification("Meta-analysis model generation failed. Please check data and settings.", type = "error")
             return(NULL)
        }
    }
    
    list(random = random_model, fixed = fixed_model, bivariate = bivariate_model)
  })  
  
  # Overall Results Tab
  output$overallSummaryTable <- renderTable({
    req(combinedResults())
    compare_models(combinedResults())
  })
  
  output$overallInterpretation <- renderText({
    req(combinedResults())
    interpret_results(combinedResults())
  })
  
  output$subgroup_test_output <- renderPrint({
    req(combinedResults())

    subgroup_var_selected <- input$subgroup_var
    if (is.null(subgroup_var_selected) || subgroup_var_selected == "" || (!is.null(input$metareg_vars) && length(input$metareg_vars) > 0)) {
      if (!is.null(input$metareg_vars) && length(input$metareg_vars) > 0 && !is.null(subgroup_var_selected) && subgroup_var_selected != "") {
        return(cat("Subgroup test is not shown when meta-regression is active."))
      }
      return(cat("No subgroup analysis performed or subgroup variable not selected."))
    }

    model_random <- combinedResults()$random
    model_fixed <- combinedResults()$fixed

    if (!is.null(model_random) && !is.null(model_random$k.byvar) && length(model_random$k.byvar) > 0 && !is.null(model_random$Q.b)) {
      cat("Test for Subgroup Differences (Random Effects Model):\n")
      cat(sprintf("  Q_b (between groups) = %.2f, df = %d, p = %.4f\n", model_random$Q.b, model_random$df.Q.b, model_random$pval.Q.b))
      if (!is.null(model_random$I2.b)) {
        cat(sprintf("  I^2 (variation between subgroups) = %.1f%%\n", model_random$I2.b * 100))
      }
      cat("\nNote: A significant p-value (e.g., < 0.05) suggests that the effect differs significantly between subgroups.\n")
    } else if (!is.null(model_fixed) && !is.null(model_fixed$k.byvar) && length(model_fixed$k.byvar) > 0 && !is.null(model_fixed$Q.b)) {
      cat("Test for Subgroup Differences (Fixed Effects Model):\n")
      cat(sprintf("  Q_b (between groups) = %.2f, df = %d, p = %.4f\n", model_fixed$Q.b, model_fixed$df.Q.b, model_fixed$pval.Q.b))
      cat("\nNote: A significant p-value (e.g., < 0.05) suggests that the effect differs significantly between subgroups.\n")
    } else {
      cat("Subgroup analysis was selected ('", subgroup_var_selected, "'), but the test for subgroup differences could not be computed or is not applicable.\n",
          "This can occur if:\n",
          "- There is only one subgroup level with data.\n",
          "- Studies are not sufficiently distributed across multiple subgroup levels.\n",
          "- The subgroup variable resulted in no valid subgroups for comparison.\n",
          "Please check the distribution of studies across the levels of your selected subgroup variable.\n")
      if(!is.null(model_random) && !is.null(model_random$k.byvar) && length(model_random$k.byvar) > 0){
        cat(paste("Number of studies per subgroup level (Random Effects Model):", paste(model_random$k.byvar, collapse=", "), "\n"))
      }
       if(!is.null(model_fixed) && !is.null(model_fixed$k.byvar) && length(model_fixed$k.byvar) > 0){
        cat(paste("Number of studies per subgroup level (Fixed Effects Model):", paste(model_fixed$k.byvar, collapse=", "), "\n"))
      }
    }
  })

  # Meta-Regression Results Reactive
  metaRegressionResults <- reactive({
    req(input$analyze, combinedResults()$random, !is.null(input$metareg_vars) && length(input$metareg_vars) > 0)

    if (!is.null(input$subgroup_var) && input$subgroup_var != "") {
      return(NULL)
    }

    selected_covariates <- input$metareg_vars
    if (is.null(selected_covariates) || length(selected_covariates) == 0) {
      return(NULL)
    }

    model_for_metareg <- combinedResults()$random

    df_for_metareg <- if (!is.null(model_for_metareg$data) && all(selected_covariates %in% names(model_for_metareg$data))) {
                         model_for_metareg$data
                       } else {
                         data()
                       }

    if (is.null(df_for_metareg)) {
         showNotification("Data for meta-regression is not available.", type="error", duration=10)
         return(NULL)
    }

    missing_covariates <- setdiff(selected_covariates, names(df_for_metareg))
    if (length(missing_covariates) > 0) {
        showNotification(paste("Error: Covariate(s)", paste(missing_covariates, collapse=", "), "not found in the analysis data. Meta-regression cannot proceed."), type="error", duration=15)
        return(NULL)
    }

    formula_str <- paste("TE ~", paste(selected_covariates, collapse = " + "))

    meta_reg_model <- tryCatch({
      meta::metareg(model_for_metareg, as.formula(formula_str), data = df_for_metareg)
    }, error = function(e) {
      showNotification(paste("Meta-regression error:", e$message), type = "error", duration = 10)
      return(NULL)
    })

    return(meta_reg_model)
  })

  # Meta-Regression Tab Outputs
  output$metareg_summary_output <- renderPrint({
    model_results <- metaRegressionResults() # Store in a variable to avoid re-calling
    if (is.null(model_results)) {
      subgroup_active <- !is.null(input$subgroup_var) && input$subgroup_var != ""
      covariates_selected <- !is.null(input$metareg_vars) && length(input$metareg_vars) > 0

      if (subgroup_active && covariates_selected) {
        cat("Meta-regression was not performed because subgroup analysis is active.\nPlease clear the subgroup selection if you wish to perform meta-regression.")
      } else if (!covariates_selected) {
        cat("No covariates selected for meta-regression. Please select one or more covariates from the sidebar.")
      } else {
        cat("Meta-regression results are not available. This might be due to an error during calculation or other conflicting settings.")
      }
      return()
    }
    req(model_results) # Ensure model_results is not NULL before proceeding

    cat("Meta-Regression Model Summary\n")
    cat("-----------------------------\n\n")

    # Print the summary of the meta-regression model
    print(summary(model_results))

    cat("\n\nInterpretation Notes:\n")
    cat("- 'estimate': Change in the effect size for a one-unit increase in the covariate.\n")
    cat("- 'se': Standard error of the estimate.\n")
    cat("- 'zval', 'pval': Z-value and p-value for testing if the estimate is significantly different from zero.\n")
    cat("- 'ci.lb', 'ci.ub': Confidence interval for the estimate.\n")
    cat("Test of Moderators:\n")
    cat("- QM: Test statistic for the hypothesis that all coefficients (except intercept) are zero.\n")
    cat("- df: Degrees of freedom for QM.\n")
    cat("- p-value: P-value for the test of moderators. A small p-value suggests at least one covariate is significantly related to the effect size.\n")
    cat("Variance Accounted For (R^2):\n")
    cat("- R^2 (adj.): Proportion of between-study variance explained by the covariates, adjusted for the number of covariates.\n")

    if (input$data_type == "hr") {
        cat("\nNote: For Hazard Ratios (HR), the 'estimate' and its CI are on the log-HR scale.\n")
        cat("To interpret on the HR scale, exponentiate these values (e.g., exp(estimate)).\n")
    }
  })

  output$metareg_plot_output <- renderPlot({
    model_results <- metaRegressionResults()
    selected_covariates <- input$metareg_vars

    if (is.null(model_results) || is.null(selected_covariates) || length(selected_covariates) == 0) {
      plot(1, type="n", axes=FALSE, xlab="", ylab="", main="Meta-Regression Plot Not Available")
      text(1, 1, "Please select at least one covariate and ensure meta-regression has run.", cex=1.2)
      return()
    }

    # Try to find the first numeric covariate among those used in the model
    # model_results$X contains the design matrix, intercept is first column
    # model_results$model$data might be more reliable if it exists and contains original covariates

    cov_to_plot <- NULL
    cov_data_values <- NULL

    # Check if model_results$data exists and contains the covariates
    # The actual covariates used in metareg are named in model_results$X.names

    if(!is.null(model_results$X.names) && length(model_results$X.names) > 0) {
        # Exclude intercept if present
        potential_covs <- model_results$X.names[model_results$X.names != "(Intercept)"]

        # Find the first numeric one from the original data used by the model
        # model_results$data should be the data frame used by metareg
        df_for_plot <- model_results$data
        if (is.null(df_for_plot)) { # Fallback to the main data() if model$data is not populated
            df_for_plot <- data()
        }

        for(cov_name in potential_covs) {
            if(cov_name %in% names(df_for_plot) && is.numeric(df_for_plot[[cov_name]])) {
                cov_to_plot <- cov_name
                cov_data_values <- df_for_plot[[cov_name]] # Get the actual data values
                break
            }
        }
    }

    if (is.null(cov_to_plot)) {
      plot(1, type="n", axes=FALSE, xlab="", ylab="", main="Meta-Regression Plot Not Available")
      text(1, 1, "No suitable numeric covariate found for plotting.\nPlease select a numeric covariate.", cex=1.2)
      return()
    }

    tryCatch({
      # Determine xlab based on data type
      effect_label_val <- effect_measure_label()
      plot_xlab <- paste("Effect Size (", effect_label_val, ")")
      if (input$data_type == "hr") {
          plot_xlab <- "log(Hazard Ratio)" # TE is logHR for HR data
      }

      meta::bubble(model_results,
                   xlab = plot_xlab,
                   main = paste("Meta-Regression Bubble Plot for Covariate:", cov_to_plot),
                   studlab = TRUE, # Show study labels
                   col.line = "blue", # Color for the regression line
                   regline = TRUE) # Ensure regression line is drawn

      if (input$data_type == "hr") {
          mtext("Note: Effect sizes (Y-axis) are on the log-HR scale.", side=1, line=4, adj=1, cex=0.8)
      }

    }, error = function(e) {
      plot(1, type="n", axes=FALSE, xlab="", ylab="", main="Error in Generating Meta-Regression Plot")
      text(1, 1, paste("Could not generate bubble plot:", e$message), cex=1.2)
    })
  })

  # Random Effects Analysis Tab
  
  # Helper to get effect measure label for plots
  effect_measure_label <- reactive({
    if (input$data_type == "smd") {
      "SMD"
    } else if (input$data_type == "hr") {
      "Hazard Ratio"
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
    if (input$data_type == "hr") {
      meta::forest(combinedResults()$random,
             leftlabs = c("Study", "TE", "seTE"), # TE and seTE are on logHR scale
             rightlabs = c("HR", "95%-CI", "Weight"), # forest will backtransform TE
             fontsize = 10,
             xlab = "Hazard Ratio (HR)",
             main = paste0("Random Effects Forest Plot (", effect_measure_label(), ")"),
             backtransf = exp,
             sm = "HR") # Explicitly set sm for clarity with backtransf
    } else {
      meta::forest(combinedResults()$random,
             leftlabs = c("Study", "TE", "seTE"),
             rightlabs = c("TE", "95%-CI", "Weight"),
             fontsize = 10,
             xlab = paste0("Effect Size (", effect_measure_label(), ")"),
             main = paste0("Random Effects Forest Plot (", effect_measure_label(), ")"))
    }
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
    
    # Calculate standardized residuals
    residuals <- calculate_random_residuals(random_model)
    
    # Check if we have valid residuals
    if (!is.null(residuals) && !all(is.na(residuals))) {
      qqnorm(residuals, main = if (input$data_type == "hr") "Q-Q Plot of Random Effects Standardized Residuals (logHR)" else "Q-Q Plot of Random Effects Standardized Residuals")
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
    current_model <- combinedResults()$random
    current_xlab <- if (input$data_type == "hr") "log(Hazard Ratio)" else paste("Effect Size (", current_model$sm, ")")
    effect_distribution_plot(current_model, xlab_label = current_xlab)
  })
  
  output$randomGOSHPlot <- renderPlot({
    req(combinedResults()$random)
    gosh_plot(combinedResults()$random)
  })
  
  output$randomFunnelPlot <- renderPlot({
    req(combinedResults()$random)
    current_model <- combinedResults()$random
    current_xlab <- if (input$data_type == "hr") "log(Hazard Ratio)" else current_model$sm
    funnel(current_model, xlab = current_xlab)
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
    if (input$data_type == "hr") {
      meta::forest(combinedResults()$fixed,
             leftlabs = c("Study", "TE", "seTE"), # TE and seTE are on logHR scale
             rightlabs = c("HR", "95%-CI", "Weight"), # forest will backtransform TE
             fontsize = 10,
             xlab = "Hazard Ratio (HR)",
             main = paste0("Fixed Effects Forest Plot (", effect_measure_label(), ")"),
             backtransf = exp,
             sm = "HR") # Explicitly set sm for clarity with backtransf
    } else {
      meta::forest(combinedResults()$fixed,
             leftlabs = c("Study", "TE", "seTE"),
             rightlabs = c("TE", "95%-CI", "Weight"),
             fontsize = 10,
             xlab = paste0("Effect Size (", effect_measure_label(), ")"),
             main = paste0("Fixed Effects Forest Plot (", effect_measure_label(), ")"))
    }
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
    
    # Calculate residuals
    residuals <- calculate_residuals(fixed_model)
    
    # Check if we have valid residuals
    if (!is.null(residuals) && !all(is.na(residuals))) {
      qqnorm(residuals, main = if (input$data_type == "hr") "Q-Q Plot of Fixed Effects Residuals (logHR)" else "Q-Q Plot of Fixed Effects Residuals")
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
    current_model <- combinedResults()$fixed
    current_xlab <- if (input$data_type == "hr") "log(Hazard Ratio)" else current_model$sm
    funnel(current_model, xlab = current_xlab)
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
  
  
  
  # Bivariate analysis results
  bivariate_result <- eventReactive(input$analyze, {
    req(data())
    # Handle both data types
    df <- data()
    if (input$data_type == "smd") {
        se <- (df$ci_upper - df$ci_lower) / (2 * 1.96)
        var <- se^2
        metabiv(studlab = df$study, sm = "SMD", y = df$smd, sigma2 = var)
    } else {
        metabiv(event.e = df$ie, 
                n.e = df$it, 
                event.c = df$pe, 
                n.c = df$pt,
                studlab = df$study,
                sm = input$effect_measure)
    }
  })
  
  # Bivariate Forest Plot
  output$bivariateForestPlot <- renderPlot({
    req(bivariate_result())
    forest.metabiv(bivariate_result(),
                   title = paste("Bivariate Forest Plot (", effect_measure_label(), ")"),
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
                   tau_mle = bivariate_result()$tau)
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
        metabiv(studlab = loo_data$study, sm = "SMD", y = loo_data$smd, sigma2 = var)
      } else {
        metabiv(event.e = loo_data$ie, n.e = loo_data$it, 
                event.c = loo_data$pe, n.c = loo_data$pt,
                studlab = loo_data$study, sm = input$effect_measure)
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
  
  # Bivariate Influence Summary
  output$bivariateInfluenceSummary <- renderPrint({
    req(bivariate_result())
    cat("Bivariate Influence Summary\n")
    influence <- lapply(1:length(bivariate_result()$y.k), function(i) {
        df <- data()[-i, ]
        if (input$data_type == "smd") {
            se <- (df$ci_upper - df$ci_lower) / (2 * 1.96)
            var <- se^2
            res_i <- metabiv(studlab = df$study, sm = "SMD", y = df$smd, sigma2 = var)
        } else {
            res_i <- metabiv(event.e = df$ie, n.e = df$it, event.c = df$pe, n.c = df$pt,
                             studlab = df$study, sm = input$effect_measure)
        }
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
    # browser()
    #qqnorm(bivariate_result()$y.k, main = "Q-Q Plot for μ")
    #qqline(bivariate_result()$y.k, col = "red")
    qq_plot_with_ci(y_k = bivariate_result()$y.k, 
                    mu = bivariate_result()$mu,
                    sigma_2_k = bivariate_result()$sigma.2.k,
                    tau_2 = bivariate_result()$tau^2,
                    title = "Q-Q Plot for Standardized Residuals (Normal Random Effects)")
  })
  
  output$qqPlotMuRaw <- renderPlot({
    req(bivariate_result())
    qq_plot_with_ci_raw(y_k = bivariate_result()$y.k, 
                    mu = bivariate_result()$mu,
                    sigma_2_k = bivariate_result()$sigma.2.k,n_k=bivariate_result()$n_k,
                    tau_2 = bivariate_result()$tau^2,
                    title = "Q-Q Plot of Raw Residuals")
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
    
    CDF.ci.obj <- comp.mu.tau.dev.CDF.CI(bivariate_result()$dev_pvals)
    comp.eff.harm.plot(CDF.ci.obj,
                       efficacy.is.OR.le1 = (bivariate_result()$sm == "OR"),
                       mlb = plot_title)
  })
  
  # Bivariate GOSH Plot
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
  
  # Bivariate Adapted Funnel Plot
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
      Method = c("Fixed Effects", "Random Effects", "Bivariate"),
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
        "&nbsp;&nbsp;• <b>Hazard Ratio (HR):</b> Columns: study, hr, ci_lower, ci_upper OR study, loghr, se_loghr<br>",
        "- You can use the 'Download Sample Structure' button to see the required format for each data type.<br><br>",
        
        "<b>2. Analysis Settings:</b><br>",
        "- Choose your heterogeneity estimator in the sidebar.<br>",
        "- For binary data, select the effect measure (Odds Ratio, Risk Ratio, etc.).<br>",
        "- For continuous data (SMD) and Hazard Ratio (HR) data, the effect measure is fixed (SMD and HR respectively).<br>",
        "- Click 'Analyze' to run the meta-analysis.<br><br>",
        
        "<b>3. Results Tabs:</b><br>",
        "- <i>Data Preview:</i> Check your uploaded data.<br>",
        "- <i>Overall Results:</i> Compare results across all methods.<br>",
        "- <i>Random Effects Analysis:</i> Detailed random effects model results.<br>",
        "- <i>Fixed Effects Analysis:</i> Detailed fixed effects model results.<br>",
        "- <i>Bivariate Approach:</i> Results from the bivariate analysis.<br><br>",
        
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
          random_results = random_results(),
          fixed_results = fixed_results(),
          bivariate_results = bivariate_results(),
          data = data(),
          input_subgroup_var = input$subgroup_var,
          # Add new parameters for meta-regression
          input_metareg_results = metaRegressionResults(),
          input_metareg_vars = input$metareg_vars,
          input_effect_measure_label = effect_measure_label(),
          input_data_type = input$data_type
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



[end of R/server.R]
