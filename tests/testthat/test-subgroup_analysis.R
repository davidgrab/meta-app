library(testthat)
library(shiny)
library(meta)

# Source functions (server.R logic will be mimicked in helper functions for easier testing)
source("../../R/functions.R", local = TRUE)

context("Subgroup Analysis Functionality")

# 2. Sample Data and Preparation
# Binary data with a character subgroup variable
subgroup_test_data_binary_raw <- read.csv("../../data/hypericum_depression_default.csv", stringsAsFactors = FALSE)
# 'studytype' can be used as subgroup. Let's ensure it has more than one level.
# For this dataset, 'studytype' has multiple levels like "RCT", "RCT KRAMER", etc.
# We might need to simplify it for clearer subgroup testing if levels are too sparse.
# For now, assume 'studytype' is suitable.

# SMD data - create a subgroup variable
subgroup_test_data_smd_raw <- read.csv("../../data/smd_example.csv", stringsAsFactors = FALSE)
subgroup_test_data_smd_raw$condition <- factor(rep(c("GroupA", "GroupB", "GroupC"), length.out = nrow(subgroup_test_data_smd_raw)))
# Add some NAs to test NA handling in subgroup variable
# subgroup_test_data_smd_raw$condition[c(1, 5, 10)] <- NA


# HR data - create a subgroup variable
subgroup_test_data_hr_raw <- data.frame(
  study = paste0("Study", 1:10),
  hr = exp(rnorm(10, mean = -0.2, sd = 0.3)),
  ci_lower = exp(rnorm(10, mean = -0.5, sd = 0.2)),
  ci_upper = exp(rnorm(10, mean = 0.1, sd = 0.2)),
  setting = factor(sample(c("Hospital", "Community", "Mixed"), 10, replace = TRUE)),
  stringsAsFactors = FALSE
)
# Ensure CIs are logical
subgroup_test_data_hr_raw$ci_lower <- pmin(subgroup_test_data_hr_raw$hr * 0.5, subgroup_test_data_hr_raw$ci_lower)
subgroup_test_data_hr_raw$ci_upper <- pmax(subgroup_test_data_hr_raw$hr * 1.5, subgroup_test_data_hr_raw$ci_upper)
# subgroup_test_data_hr_raw$setting[c(2,6)] <- NA


# 3. Test `data()` Reactive Logic for Subgroup Column
# Helper function mimicking relevant data() reactive logic from server.R
test_data_reactive_subgroup <- function(raw_df, data_type, selected_subgroup_var, remove_na_val) {
  # Mimic currentData()
  current_data_val <- raw_df

  # Mimic na.omit based on input
  df_intermediate <- if (remove_na_val) na.omit(current_data_val) else current_data_val

  # This part mimics the data processing section of data() reactive
  processed_df <- NULL
  if (data_type == "binary" && ncol(df_intermediate) >= 5) { # Check >=5 because subgroup might be there
    # Assume first 5 are the main data
    temp_df <- df_intermediate[,1:5]
    names(temp_df) <- c("study", "ie", "it", "pe", "pt")
    temp_df$ie <- as.numeric(temp_df$ie)
    temp_df$it <- as.numeric(temp_df$it)
    temp_df$pe <- as.numeric(temp_df$pe)
    temp_df$pt <- as.numeric(temp_df$pt)
    processed_df <- temp_df
  } else if (data_type == "smd" && ncol(df_intermediate) >= 4) {
    temp_df <- df_intermediate[,1:4]
    names(temp_df) <- c("study", "smd", "ci_lower", "ci_upper")
    temp_df$smd <- as.numeric(temp_df$smd)
    temp_df$ci_lower <- as.numeric(temp_df$ci_lower)
    temp_df$ci_upper <- as.numeric(temp_df$ci_upper)
    processed_df <- temp_df
  } else if (data_type == "hr") {
    if (ncol(df_intermediate) >= 4 && all(c("hr", "ci_lower", "ci_upper") %in% names(df_intermediate))) {
        # Assuming 4 main columns for HR+CI, then subgroup
        temp_df_hr_ci <- df_intermediate[, c("study", "hr", "ci_lower", "ci_upper")]
        temp_df_hr_ci$hr <- suppressWarnings(as.numeric(temp_df_hr_ci$hr))
        temp_df_hr_ci$ci_lower <- suppressWarnings(as.numeric(temp_df_hr_ci$ci_lower))
        temp_df_hr_ci$ci_upper <- suppressWarnings(as.numeric(temp_df_hr_ci$ci_upper))

        if (any(temp_df_hr_ci$hr <= 0, na.rm = TRUE) || any(temp_df_hr_ci$ci_lower <= 0, na.rm = TRUE) || any(temp_df_hr_ci$ci_upper <= 0, na.rm = TRUE) || any(temp_df_hr_ci$ci_lower >= temp_df_hr_ci$ci_upper, na.rm=TRUE)) {
          return(NULL)
        }
        if (any(is.na(temp_df_hr_ci$hr)) || any(is.na(temp_df_hr_ci$ci_lower)) || any(is.na(temp_df_hr_ci$ci_upper))) {
          return(NULL)
        }
        loghr <- log(temp_df_hr_ci$hr)
        se_loghr <- (log(temp_df_hr_ci$ci_upper) - log(temp_df_hr_ci$ci_lower)) / (2 * qnorm(0.975))
        processed_df <- data.frame(study = temp_df_hr_ci$study, loghr = loghr, se_loghr = se_loghr)

    } else if (ncol(df_intermediate) >= 3 && all(c("loghr", "se_loghr") %in% names(df_intermediate))) {
        # Assuming 3 main columns for logHR+SE, then subgroup
        temp_df_loghr <- df_intermediate[, c("study", "loghr", "se_loghr")]
        temp_df_loghr$loghr <- suppressWarnings(as.numeric(temp_df_loghr$loghr))
        temp_df_loghr$se_loghr <- suppressWarnings(as.numeric(temp_df_loghr$se_loghr))
        if (any(is.na(temp_df_loghr$loghr)) || any(is.na(temp_df_loghr$se_loghr))) {
          return(NULL)
        }
        processed_df <- temp_df_loghr
    } else {
      return(NULL)
    }
  } else {
    return(NULL) # Unrecognized type or not enough columns
  }

  if (is.null(processed_df)) return(NULL)

  # Append subgroup variable if selected and valid
  if (!is.null(selected_subgroup_var) && selected_subgroup_var != "") {
    if (selected_subgroup_var %in% names(df_intermediate)) {
      # Add the subgroup column to the processed_df
      # This assumes df_intermediate (which had NAs removed if specified) has the subgroup column
      # and its rows align with processed_df. This is true because processed_df was derived from df_intermediate.
      processed_df[[selected_subgroup_var]] <- df_intermediate[[selected_subgroup_var]]
    } else {
      # Subgroup variable not found in the (potentially NA-omitted) data
      # This case should ideally be handled by UI not offering it, but good to be defensive
      return(processed_df) # Return processed data without subgroup col
    }
  }
  return(processed_df)
}


test_that("data() reactive correctly appends subgroup column (Binary)", {
  # Using 'type' from hypericum_depression_default as subgroup.
  # Note: 'type' is actually 'Study Type' in the example, let's use 'supplements' as a proxy if it exists,
  # or we can rename one for the test. For simplicity, let's assume 'studytype' is a valid column name.
  # The actual `hypericum_depression_default.csv` has 'Study.type'. We'll use that.
  data_with_subgroup <- test_data_reactive_subgroup(subgroup_test_data_binary_raw, "binary", "Study.type", FALSE)
  expect_true("Study.type" %in% names(data_with_subgroup))
  expect_equal(nrow(data_with_subgroup), nrow(subgroup_test_data_binary_raw)) # Assuming no NAs in key cols

  # Test with NA removal (if 'Study.type' had NAs, or other key cols did)
  # For this dataset, 'Study.type' does not have NAs.
  # If we add NAs to other columns, na.omit would remove those rows.
  # For this test, it's more about ensuring the column is present.
  data_with_subgroup_na <- test_data_reactive_subgroup(subgroup_test_data_binary_raw, "binary", "Study.type", TRUE)
  expect_true("Study.type" %in% names(data_with_subgroup_na))
})


test_that("data() reactive correctly appends subgroup column (SMD)", {
  data_with_subgroup <- test_data_reactive_subgroup(subgroup_test_data_smd_raw, "smd", "condition", FALSE)
  expect_true("condition" %in% names(data_with_subgroup))
  expect_equal(nrow(data_with_subgroup), nrow(subgroup_test_data_smd_raw))

  # Test NA removal - original smd_example.csv has no NAs in data part.
  # 'condition' was added without NAs for this test.
  data_with_subgroup_na <- test_data_reactive_subgroup(subgroup_test_data_smd_raw, "smd", "condition", TRUE)
  expect_true("condition" %in% names(data_with_subgroup_na))
  expect_equal(nrow(data_with_subgroup_na), nrow(na.omit(subgroup_test_data_smd_raw[,1:4]))) # if NAs were in main data cols
})

test_that("data() reactive correctly appends subgroup column (HR)", {
  data_with_subgroup <- test_data_reactive_subgroup(subgroup_test_data_hr_raw, "hr", "setting", FALSE)
  expect_true("setting" %in% names(data_with_subgroup))
  expect_equal(nrow(data_with_subgroup), nrow(subgroup_test_data_hr_raw))

  data_with_subgroup_na <- test_data_reactive_subgroup(subgroup_test_data_hr_raw, "hr", "setting", TRUE)
  expect_true("setting" %in% names(data_with_subgroup_na))
})


# 4. Test `combinedResults()` Reactive Logic for Subgroup Analysis
# Helper function mimicking combinedResults() logic for subgroup analysis
test_combined_results_subgroup <- function(processed_df_with_subgroup, data_type,
                                           effect_measure_input = NULL, # Only for binary
                                           het_estimator_input, subgroup_var_name) {
  df <- processed_df_with_subgroup

  if (is.null(df) || !subgroup_var_name %in% names(df)) {
    # If subgroup variable isn't in df, run overall analysis
    # This mimics the behavior where perform_subgroup_analysis would be FALSE
    # For simplicity, we'll just return NULL here, as the main path is subgroup.
    # The "No Subgroup" fallback test will cover overall analysis.
    return(NULL)
  }

  random_model <- NULL
  fixed_model <- NULL
  bivariate_model <- NULL # Always NULL for subgroup analysis as per current app logic

  if (data_type == "binary") {
    random_model <- metabin(event.e = df$ie, n.e = df$it, event.c = df$pe, n.c = df$pt,
                            studlab = df$study, sm = effect_measure_input, method.tau = het_estimator_input,
                            byvar = df[[subgroup_var_name]], common = FALSE, random = TRUE, warn=FALSE)
    fixed_model <- metabin(event.e = df$ie, n.e = df$it, event.c = df$pe, n.c = df$pt,
                           studlab = df$study, sm = effect_measure_input,
                           byvar = df[[subgroup_var_name]], common = TRUE, random = FALSE, warn=FALSE)
  } else if (data_type == "smd") {
    se <- (df$ci_upper - df$ci_lower) / (2 * qnorm(0.975)) # qnorm(0.975) is more precise than 1.96
    random_model <- metagen(TE = df$smd, seTE = se, studlab = df$study, sm = "SMD",
                            method.tau = het_estimator_input, byvar = df[[subgroup_var_name]],
                            common = FALSE, random = TRUE, warn=FALSE)
    fixed_model <- metagen(TE = df$smd, seTE = se, studlab = df$study, sm = "SMD",
                           byvar = df[[subgroup_var_name]], common = TRUE, random = FALSE, warn=FALSE)
  } else if (data_type == "hr") {
    # Assumes df already has loghr and se_loghr
    random_model <- metagen(TE = df$loghr, seTE = df$se_loghr, studlab = df$study, sm = "HR",
                            method.tau = het_estimator_input, byvar = df[[subgroup_var_name]],
                            common = FALSE, random = TRUE, warn=FALSE)
    fixed_model <- metagen(TE = df$loghr, seTE = df$se_loghr, studlab = df$study, sm = "HR",
                           byvar = df[[subgroup_var_name]], common = TRUE, random = FALSE, warn=FALSE)
  }
  return(list(random = random_model, fixed = fixed_model, bivariate = bivariate_model))
}

test_that("combinedResults: Binary subgroup analysis by 'Study.type'", {
  # Prepare data
  df_binary_processed <- test_data_reactive_subgroup(subgroup_test_data_binary_raw, "binary", "Study.type", FALSE)
  req(df_binary_processed) # Ensure data is processed

  res_binary_subgroup <- test_combined_results_subgroup(df_binary_processed, "binary", "RR", "DL", "Study.type")

  expect_false(is.null(res_binary_subgroup$random))
  expect_true(res_binary_subgroup$random$k.byvar > 0)
  expect_true(!is.null(res_binary_subgroup$random$Q.b))
  expect_null(res_binary_subgroup$bivariate)

  # Manual comparison
  manual_random <- metabin(event.e = ie, n.e = it, event.c = pe, n.c = pt,
                           studlab = study, sm = "RR", method.tau = "DL",
                           byvar = Study.type, data = subgroup_test_data_binary_raw, # Use raw for direct comparison
                           common = FALSE, random = TRUE, warn=FALSE)

  expect_equal(res_binary_subgroup$random$Q.b, manual_random$Q.b, tolerance = 1e-6)
  expect_equal(res_binary_subgroup$random$pval.Q.b, manual_random$pval.Q.b, tolerance = 1e-6)
  # Compare TE.random.byvar if it exists and is a list/vector of same length
  if(!is.null(res_binary_subgroup$random$TE.random.byvar) && !is.null(manual_random$TE.random.byvar)){
    expect_equal(res_binary_subgroup$random$TE.random.byvar, manual_random$TE.random.byvar, tolerance = 1e-6)
  }
})


test_that("combinedResults: SMD subgroup analysis by 'condition'", {
  df_smd_processed <- test_data_reactive_subgroup(subgroup_test_data_smd_raw, "smd", "condition", FALSE)
  req(df_smd_processed)

  res_smd_subgroup <- test_combined_results_subgroup(df_smd_processed, "smd", NULL, "DL", "condition")

  expect_false(is.null(res_smd_subgroup$random))
  expect_true(res_smd_subgroup$random$k.byvar > 0)
  expect_true(!is.null(res_smd_subgroup$random$Q.b))
  expect_null(res_smd_subgroup$bivariate)

  # Manual comparison
  # Need to calculate SE for the manual run from the raw data
  smd_data_for_manual <- subgroup_test_data_smd_raw
  smd_data_for_manual$seTE <- (smd_data_for_manual$ci_upper - smd_data_for_manual$ci_lower) / (2 * qnorm(0.975))

  manual_random_smd <- metagen(TE = smd, seTE = seTE, studlab = study, sm = "SMD",
                               method.tau = "DL", byvar = condition, data = smd_data_for_manual,
                               common = FALSE, random = TRUE, warn=FALSE)

  expect_equal(res_smd_subgroup$random$Q.b, manual_random_smd$Q.b, tolerance = 1e-6)
  expect_equal(res_smd_subgroup$random$pval.Q.b, manual_random_smd$pval.Q.b, tolerance = 1e-6)
})

test_that("combinedResults: HR subgroup analysis by 'setting'", {
  df_hr_processed <- test_data_reactive_subgroup(subgroup_test_data_hr_raw, "hr", "setting", FALSE)
  req(df_hr_processed)

  res_hr_subgroup <- test_combined_results_subgroup(df_hr_processed, "hr", NULL, "DL", "setting")

  expect_false(is.null(res_hr_subgroup$random))
  expect_true(res_hr_subgroup$random$k.byvar > 0)
  expect_true(!is.null(res_hr_subgroup$random$Q.b))
  expect_null(res_hr_subgroup$bivariate)

  # Manual comparison
  # Need loghr and se_loghr for the manual run from the raw data
  hr_data_for_manual <- subgroup_test_data_hr_raw
  hr_data_for_manual$loghr    <- log(hr_data_for_manual$hr)
  hr_data_for_manual$se_loghr <- (log(hr_data_for_manual$ci_upper) - log(hr_data_for_manual$ci_lower)) / (2 * qnorm(0.975))

  manual_random_hr <- metagen(TE = loghr, seTE = se_loghr, studlab = study, sm = "HR",
                              method.tau = "DL", byvar = setting, data = hr_data_for_manual,
                              common = FALSE, random = TRUE, warn=FALSE)

  expect_equal(res_hr_subgroup$random$Q.b, manual_random_hr$Q.b, tolerance = 1e-6)
  expect_equal(res_hr_subgroup$random$pval.Q.b, manual_random_hr$pval.Q.b, tolerance = 1e-6)
})


# 5. Test output$subgroup_test_output Logic
# Helper function mimicking output$subgroup_test_output
test_render_subgroup_test_output <- function(model_random, model_fixed, subgroup_var_selected_val) {
  if (is.null(subgroup_var_selected_val) || subgroup_var_selected_val == "") {
    return("No subgroup analysis performed.")
  }

  output_text <- ""
  if (!is.null(model_random) && !is.null(model_random$k.byvar) && model_random$k.byvar > 0 && !is.null(model_random$Q.b)) {
    output_text <- paste(output_text,
                         "Test for Subgroup Differences (Random Effects Model):\n",
                         sprintf("  Q_b (between groups) = %.2f, df = %d, p = %.4f\n", model_random$Q.b, model_random$df.Q.b, model_random$pval.Q.b))
    if (!is.null(model_random$I2.b)) {
      output_text <- paste(output_text, sprintf("  I^2 (variation between subgroups) = %.1f%%\n", model_random$I2.b * 100))
    }
    output_text <- paste(output_text, "\nNote: A significant p-value (e.g., < 0.05) suggests that the effect differs significantly between subgroups.\n")
  } else if (!is.null(model_fixed) && !is.null(model_fixed$k.byvar) && model_fixed$k.byvar > 0 && !is.null(model_fixed$Q.b)) {
    output_text <- paste(output_text,
                         "Test for Subgroup Differences (Fixed Effects Model):\n",
                         sprintf("  Q_b (between groups) = %.2f, df = %d, p = %.4f\n", model_fixed$Q.b, model_fixed$df.Q.b, model_fixed$pval.Q.b))
    output_text <- paste(output_text, "\nNote: A significant p-value (e.g., < 0.05) suggests that the effect differs significantly between subgroups.\n")
  } else {
    output_text <- paste(output_text, "Subgroup analysis was selected ('", subgroup_var_selected_val, "'), but the test for subgroup differences could not be computed or is not applicable.\n")
  }
  return(output_text)
}

test_that("output$subgroup_test_output displays correctly with subgroup results", {
  # Use results from a previous subgroup test
  df_binary_processed <- test_data_reactive_subgroup(subgroup_test_data_binary_raw, "binary", "Study.type", FALSE)
  req(df_binary_processed)
  res_binary_subgroup <- test_combined_results_subgroup(df_binary_processed, "binary", "RR", "DL", "Study.type")

  output_str <- capture.output(cat(test_render_subgroup_test_output(res_binary_subgroup$random, res_binary_subgroup$fixed, "Study.type")))
  output_str_collapsed <- paste(output_str, collapse="\n")

  expect_match(output_str_collapsed, "Test for Subgroup Differences")
  expect_match(output_str_collapsed, "Q_b")
  expect_match(output_str_collapsed, "p =")
})

test_that("output$subgroup_test_output displays correctly when no subgroup analysis", {
  output_str <- capture.output(cat(test_render_subgroup_test_output(NULL, NULL, "")))
  expect_match(paste(output_str, collapse="\n"), "No subgroup analysis performed.")
})

test_that("output$subgroup_test_output displays correctly when subgroup test not computable", {
  # Create a dummy model that implies subgrouping but lacks Q.b
  dummy_model_random <- list(k.byvar = 1, Q.b = NULL, sm = "SMD") # k.byvar > 0 but no Q.b
  output_str <- capture.output(cat(test_render_subgroup_test_output(dummy_model_random, NULL, "some_var")))
  expect_match(paste(output_str, collapse="\n"), "test for subgroup differences could not be computed")
})


# 6. Test "No Subgroup" Fallback in `combinedResults()`
# This involves testing the main path of combinedResults when subgroup_var is empty
# We can adapt test_combined_results_subgroup or use a more direct mimicry of combinedResults
test_that("combinedResults performs overall analysis when no subgroup is selected", {
  # Using binary data, no subgroup
  df_binary_processed_overall <- test_data_reactive_subgroup(subgroup_test_data_binary_raw, "binary", "", FALSE) # No subgroup var
  req(df_binary_processed_overall)

  # Mimic combinedResults structure for overall analysis
  res_overall <- if (TRUE) { # Assuming this path is taken when subgroup_variable_name is ""
    random_model <- metabin(event.e = df_binary_processed_overall$ie, n.e = df_binary_processed_overall$it,
                            event.c = df_binary_processed_overall$pe, n.c = df_binary_processed_overall$pt,
                            studlab = df_binary_processed_overall$study, sm = "RR", method.tau = "DL",
                            common = FALSE, random = TRUE, warn=FALSE)
    fixed_model <- metabin(event.e = df_binary_processed_overall$ie, n.e = df_binary_processed_overall$it,
                           event.c = df_binary_processed_overall$pe, n.c = df_binary_processed_overall$pt,
                           studlab = df_binary_processed_overall$study, sm = "RR",
                           common = TRUE, random = FALSE, warn=FALSE)
    # Bivariate model would also be calculated here in actual server.R
    list(random = random_model, fixed = fixed_model, bivariate = NULL) # Bivariate simplified for this test
  } else { NULL }

  expect_false(is.null(res_overall$random))
  expect_null(res_overall$random$Q.b) # Q.b should not exist for overall analysis
  expect_true(!is.null(res_overall$random$TE.random)) # Overall effect should exist
})
