# tests/testthat/test-metaregression.R

library(testthat)
library(shiny)
library(meta)
# Ensure that the functions.R file is sourced relative to the test file's location
# Adjust the path if your test file is in a subdirectory of tests/testthat/
source(file.path(normalizePath(dirname(rstudioapi::getSourceEditorContext()$path)), "..", "..", "R", "functions.R"), local = TRUE)


# Sample Data Preparation (SMD)
metareg_test_data_smd_raw <- data.frame(
  study = paste("Study", 1:10),
  smd = rnorm(10, 0.5, 0.3),
  ci_lower = rnorm(10, 0.2, 0.1),
  ci_upper = rnorm(10, 0.8, 0.1),
  stringsAsFactors = FALSE
)
# Ensure CIs are logical
metareg_test_data_smd_raw$ci_lower <- pmin(metareg_test_data_smd_raw$smd - 0.1, metareg_test_data_smd_raw$ci_lower)
metareg_test_data_smd_raw$ci_upper <- pmax(metareg_test_data_smd_raw$smd + 0.1, metareg_test_data_smd_raw$ci_upper)
metareg_test_data_smd_raw$ci_lower[1] <- metareg_test_data_smd_raw$smd[1] - 0.2 # ensure some variation
metareg_test_data_smd_raw$ci_upper[1] <- metareg_test_data_smd_raw$smd[1] + 0.2


set.seed(123) # for reproducibility
metareg_test_data_smd_raw$avg_age <- round(rnorm(nrow(metareg_test_data_smd_raw), mean = 45, sd = 5))
metareg_test_data_smd_raw$setting <- factor(sample(c("Hospital", "Community", "Mixed"), nrow(metareg_test_data_smd_raw), replace = TRUE))
metareg_test_data_smd_raw$year <- sample(2000:2010, nrow(metareg_test_data_smd_raw), replace = TRUE) # Another numeric covariate


# Prepare data as it would be after basic processing by data() reactive (TE, seTE for metagen)
# This is a simplified version for testing, assuming data() correctly calculates TE/seTE
# For SMD, TE is smd, seTE is (ci_upper - ci_lower) / (2 * qnorm(0.975))
metareg_test_data_smd_processed <- metareg_test_data_smd_raw
metareg_test_data_smd_processed$TE <- metareg_test_data_smd_processed$smd
metareg_test_data_smd_processed$seTE <- (metareg_test_data_smd_processed$ci_upper - metareg_test_data_smd_processed$ci_lower) / (2 * qnorm(0.975))

# Colditz data (binary, for HR-like tests if we adapt it, or general metareg tests)
# For actual HR, we'd need loghr and se_loghr
colditz_data_metareg <- data.frame(
    study = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"),
    ablat = c(13,13,13,16,19,27,33,33,34,38,42,42,52), # Latitude - numeric covariate
    alloc = c("random","random","random","alternate","random","random","random","random","systematic","random","random","alternate","random"), # Categorical
    loghr = log(c(0.30,0.32,0.63,0.20,0.61,0.90,0.46,0.59,1.00,0.71,0.91,1.58,2.74)), # Example logHR
    se_loghr = c(0.33,0.32,0.23,0.20,0.17,0.17,0.18,0.18,0.15,0.12,0.10,0.15,0.16)   # Example SE for logHR
)
colditz_data_metareg$TE <- colditz_data_metareg$loghr
colditz_data_metareg$seTE <- colditz_data_metareg$se_loghr


context("Meta-Regression Functionality")

test_that("Sample data for meta-regression is prepared correctly", {
  expect_true("avg_age" %in% names(metareg_test_data_smd_raw))
  expect_true(is.numeric(metareg_test_data_smd_raw$avg_age))
  expect_true("setting" %in% names(metareg_test_data_smd_raw))
  expect_true(is.factor(metareg_test_data_smd_raw$setting))

  expect_true("TE" %in% names(metareg_test_data_smd_processed))
  expect_true("seTE" %in% names(metareg_test_data_smd_processed))
  expect_equal(nrow(metareg_test_data_smd_processed), 10)

  expect_true("ablat" %in% names(colditz_data_metareg))
  expect_true(is.numeric(colditz_data_metareg$ablat))
  expect_true("TE" %in% names(colditz_data_metareg))
  expect_true("seTE" %in% names(colditz_data_metareg))
})

# Helper function to mimic data() reactive for covariate inclusion
test_data_reactive_metareg <- function(raw_df, data_type_input, selected_metareg_vars_input, remove_na_input = FALSE) {

  df_to_process <- raw_df
  if (remove_na_input) {
    df_to_process <- na.omit(df_to_process)
    if(nrow(df_to_process) == 0) return(NULL)
  }

  processed_df <- NULL
  original_names <- names(df_to_process)

  if (data_type_input == "smd" && ncol(df_to_process) >= 4) {
    # Simplified SMD processing from server.R
    temp_df <- df_to_process[, 1:4, drop = FALSE]
    names(temp_df) <- c("study", "smd", "ci_lower", "ci_upper")
    temp_df$smd <- as.numeric(temp_df$smd)
    temp_df$ci_lower <- as.numeric(temp_df$ci_lower)
    temp_df$ci_upper <- as.numeric(temp_df$ci_upper)
    # Actual TE/seTE calculation for metagen
    temp_df$TE <- temp_df$smd
    temp_df$seTE <- (temp_df$ci_upper - temp_df$ci_lower) / (2 * qnorm(0.975))
    processed_df <- temp_df[, c("study", "TE", "seTE")] # Keep only essential + study

    # Add back other original columns that are not the core smd columns
    other_cols_names <- original_names[!original_names %in% c("smd", "ci_lower", "ci_upper")]
    # Ensure 'study' is not duplicated if it was part of other_cols_names
    other_cols_names <- other_cols_names[other_cols_names != "study"]
    if(length(other_cols_names) > 0){
        processed_df <- cbind(processed_df, df_to_process[, other_cols_names, drop=FALSE])
    }

  } else if (data_type_input == "hr_loghr" && ncol(df_to_process) >=3) { # Assuming loghr, se_loghr provided
    temp_df <- df_to_process[, 1:3, drop=FALSE]
    names(temp_df)[1:3] <- c("study", "loghr", "se_loghr") # Assume first 3 are these
    temp_df$loghr <- as.numeric(temp_df$loghr)
    temp_df$se_loghr <- as.numeric(temp_df$se_loghr)
    processed_df <- data.frame(study = temp_df$study, TE = temp_df$loghr, seTE = temp_df$se_loghr)

    other_cols_names <- original_names[!original_names %in% c("loghr", "se_loghr")]
    other_cols_names <- other_cols_names[other_cols_names != "study"]
    if(length(other_cols_names) > 0){
        processed_df <- cbind(processed_df, df_to_process[, other_cols_names, drop=FALSE])
    }
  }
  # Add other data type processing if needed for more tests later

  if (is.null(processed_df)) return(NULL)

  # Append selected meta-regression variables
  # This part mimics how server.R's data() would add them back from df_to_process
  # (which has undergone NA removal if selected)
  if (!is.null(selected_metareg_vars_input) && length(selected_metareg_vars_input) > 0) {
    for (cov_name in selected_metareg_vars_input) {
      if (cov_name %in% names(df_to_process)) {
        # In the real data() reactive, processed_df might have fewer rows than df_to_process
        # if NAs were removed from core data columns but not from covariates yet.
        # However, for this test helper, we assume df_to_process is the version of raw_df
        # that has already been subjected to na.omit if remove_na_input was TRUE.
        # So, processed_df (derived from df_to_process) should have same row count as df_to_process.
        if (nrow(processed_df) == nrow(df_to_process)) {
             if (!cov_name %in% names(processed_df)) { # Avoid re-adding if already there
                processed_df[[cov_name]] <- df_to_process[[cov_name]]
             }
        } else {
           # This case implies NAs were removed from essential columns (smd, ci_lower etc)
           # and processed_df now has fewer rows. We need to align covariates.
           # This is a complex part of server.R; for testing, we focus on covariates being present.
           # A more robust test would require the full data() logic.
           # For now, if rows differ, we assume this test helper doesn't need to handle that specific alignment.
           # The key is that the *selected* covariates from *df_to_process* are added.
           warning("Row mismatch in test_data_reactive_metareg - covariate alignment might be simplified.")
           if (!cov_name %in% names(processed_df)) {
             # This simplified logic assumes df_to_process is the source of truth for covariates
             # and processed_df is what we are building. If rows were removed from processed_df
             # due to NA in essential analytical columns, this won't perfectly mimic server.R's merge.
             # However, the goal here is to test if covariates are added.
             # A full test of data() is beyond this helper's scope.
             # processed_df[[cov_name]] <- df_to_process[[cov_name]] # This would fail if rows differ.
           }
        }
      }
    }
  }
  return(processed_df)
}


test_that("data() reactive correctly includes selected meta-regression covariates (SMD)", {
  selected_vars <- c("avg_age", "setting", "year")

  # Test with remove_na = FALSE
  processed_data_no_na <- test_data_reactive_metareg(
    raw_df = metareg_test_data_smd_raw,
    data_type_input = "smd",
    selected_metareg_vars_input = selected_vars,
    remove_na_input = FALSE
  )

  expect_true(!is.null(processed_data_no_na))
  expect_true("avg_age" %in% names(processed_data_no_na))
  expect_equal(processed_data_no_na$avg_age, metareg_test_data_smd_raw$avg_age)
  expect_true("setting" %in% names(processed_data_no_na))
  expect_equal(processed_data_no_na$setting, metareg_test_data_smd_raw$setting)
  expect_true("year" %in% names(processed_data_no_na))
  expect_equal(processed_data_no_na$year, metareg_test_data_smd_raw$year)
  expect_equal(nrow(processed_data_no_na), nrow(metareg_test_data_smd_raw))

  # Test with remove_na = TRUE (no NAs initially in these covariates)
  processed_data_with_na_option <- test_data_reactive_metareg(
    raw_df = metareg_test_data_smd_raw,
    data_type_input = "smd",
    selected_metareg_vars_input = selected_vars,
    remove_na_input = TRUE # No NAs in avg_age or setting in this dataset
  )
  expect_true(!is.null(processed_data_with_na_option))
  expect_equal(nrow(processed_data_with_na_option), nrow(metareg_test_data_smd_raw)) # No rows should be dropped

  # Test with remove_na = TRUE and actual NAs in a covariate
  metareg_test_data_smd_raw_with_na <- metareg_test_data_smd_raw
  metareg_test_data_smd_raw_with_na$avg_age[c(2, 5)] <- NA # Introduce NAs

  processed_data_actual_na <- test_data_reactive_metareg(
    raw_df = metareg_test_data_smd_raw_with_na,
    data_type_input = "smd",
    selected_metareg_vars_input = selected_vars, # including avg_age
    remove_na_input = TRUE
  )
  expect_true(!is.null(processed_data_actual_na))
  expect_equal(nrow(processed_data_actual_na), nrow(metareg_test_data_smd_raw_with_na) - 2) # 2 rows with NA in avg_age should be dropped
  expect_true("avg_age" %in% names(processed_data_actual_na))
  # Check that the remaining avg_age values are correct (NAs removed)
  expect_equal(processed_data_actual_na$avg_age, na.omit(metareg_test_data_smd_raw_with_na)$avg_age)
  expect_true("setting" %in% names(processed_data_actual_na))
  expect_equal(processed_data_actual_na$setting, na.omit(metareg_test_data_smd_raw_with_na)$setting)
})


test_that("data() reactive correctly includes selected meta-regression covariates (HR - loghr format)", {
  colditz_raw_for_test <- colditz_data_metareg[, !names(colditz_data_metareg) %in% c("TE", "seTE")] # Use original columns
  selected_hr_vars <- c("ablat", "alloc")

  processed_data_hr_no_na <- test_data_reactive_metareg(
    raw_df = colditz_raw_for_test,
    data_type_input = "hr_loghr",
    selected_metareg_vars_input = selected_hr_vars,
    remove_na_input = FALSE
  )
  expect_true(!is.null(processed_data_hr_no_na))
  expect_true("ablat" %in% names(processed_data_hr_no_na))
  expect_equal(processed_data_hr_no_na$ablat, colditz_raw_for_test$ablat)
  expect_true("alloc" %in% names(processed_data_hr_no_na))
  expect_equal(processed_data_hr_no_na$alloc, colditz_raw_for_test$alloc)
  expect_equal(nrow(processed_data_hr_no_na), nrow(colditz_raw_for_test))
})

# Helper function to mimic the core of metaRegressionResults()
# It takes a base meta-analysis model, a formula string, and the data containing covariates.
test_run_metaregression <- function(base_model, formula_string, data_for_metareg) {
  if (is.null(formula_string) || formula_string == "" || is.null(data_for_metareg)) {
    return(NULL)
  }

  formula_obj <- tryCatch(as.formula(formula_string), error = function(e) NULL)
  if (is.null(formula_obj)) return(NULL) # Invalid formula

  # Ensure covariates in formula are in data_for_metareg
  covars_in_formula <- all.vars(formula_obj[[3]]) # Get RHS variables
  if(!all(covars_in_formula %in% names(data_for_metareg))) {
    # warning(paste("Not all covariates in formula are present in the data for metaregression:",
                  # paste(setdiff(covars_in_formula, names(data_for_metareg)), collapse=", ")))
    return(NULL) # Mimic server logic of not proceeding if covariates are missing
  }

  meta_reg_model <- tryCatch({
    meta::metareg(base_model, formula_obj, data = data_for_metareg)
  }, error = function(e) {
    # message(paste("Meta-regression error in test_run_metaregression:", e$message))
    return(NULL)
  })
  return(meta_reg_model)
}

# Prepare base models for testing metaRegressionResults()
# SMD Data
base_model_smd <- metagen(TE = TE, seTE = seTE, studlab = study, data = metareg_test_data_smd_processed, sm = "SMD")

# HR Data (Colditz)
base_model_hr <- metagen(TE = TE, seTE = seTE, studlab = study, data = colditz_data_metareg, sm = "HR")


test_that("metaRegressionResults() logic with single numeric covariate (SMD)", {
  selected_covariate <- "avg_age"
  formula_str <- paste("~", selected_covariate) # Formula for metareg is ~ cov

  # Data for metareg should be the one that includes TE, seTE, and the covariate
  data_for_metareg_smd <- metareg_test_data_smd_processed[metareg_test_data_smd_processed$study %in% base_model_smd$studlab, ]

  # Ensure 'avg_age' is in data_for_metareg_smd, which it should be from its definition
  expect_true("avg_age" %in% names(data_for_metareg_smd))

  res_metareg_custom_helper <- test_run_metaregression(base_model_smd, formula_str, data_for_metareg_smd)
  expect_false(is.null(res_metareg_custom_helper), "Meta-regression with custom helper failed for SMD numeric.")
  expect_s3_class(res_metareg_custom_helper, "metareg")

  # Manual calculation for comparison
  res_metareg_manual <- meta::metareg(base_model_smd, avg_age, data = data_for_metareg_smd)

  # Compare key results
  expect_equal(res_metareg_custom_helper$coefficients, res_metareg_manual$coefficients, tolerance = 1e-6)
  expect_equal(res_metareg_custom_helper$se, res_metareg_manual$se, tolerance = 1e-6)
  expect_equal(res_metareg_custom_helper$tau2, res_metareg_manual$tau2, tolerance = 1e-6)
  expect_equal(res_metareg_custom_helper$I2, res_metareg_manual$I2, tolerance = 1e-6)
  expect_equal(res_metareg_custom_helper$R2, res_metareg_manual$R2, tolerance = 1e-6)
  expect_equal(res_metareg_custom_helper$QM, res_metareg_manual$QM, tolerance = 1e-6)
  expect_equal(res_metareg_custom_helper$pval.QM, res_metareg_manual$pval.QM, tolerance = 1e-6)
})

test_that("metaRegressionResults() logic with single numeric covariate (HR - Colditz)", {
  selected_covariate <- "ablat" # Latitude
  formula_str <- paste("~", selected_covariate)

  data_for_metareg_hr <- colditz_data_metareg[colditz_data_metareg$study %in% base_model_hr$studlab, ]
  expect_true("ablat" %in% names(data_for_metareg_hr))

  res_metareg_custom_helper_hr <- test_run_metaregression(base_model_hr, formula_str, data_for_metareg_hr)
  expect_false(is.null(res_metareg_custom_helper_hr), "Meta-regression with custom helper failed for HR numeric.")
  expect_s3_class(res_metareg_custom_helper_hr, "metareg")

  # Manual calculation for comparison
  res_metareg_manual_hr <- meta::metareg(base_model_hr, ablat, data = data_for_metareg_hr)

  # Compare key results
  expect_equal(res_metareg_custom_helper_hr$coefficients, res_metareg_manual_hr$coefficients, tolerance = 1e-6)
  expect_equal(res_metareg_custom_helper_hr$se, res_metareg_manual_hr$se, tolerance = 1e-6)
  expect_equal(res_metareg_custom_helper_hr$tau2, res_metareg_manual_hr$tau2, tolerance = 1e-6)
  # I2 might be NULL if tau2 is zero, handle this
  if(!is.null(res_metareg_custom_helper_hr$I2) || !is.null(res_metareg_manual_hr$I2)) {
    expect_equal(res_metareg_custom_helper_hr$I2, res_metareg_manual_hr$I2, tolerance = 1e-6)
  }
  if(!is.null(res_metareg_custom_helper_hr$R2) || !is.null(res_metareg_manual_hr$R2)) {
   expect_equal(res_metareg_custom_helper_hr$R2, res_metareg_manual_hr$R2, tolerance = 1e-6)
  }
  expect_equal(res_metareg_custom_helper_hr$QM, res_metareg_manual_hr$QM, tolerance = 1e-6)
  expect_equal(res_metareg_custom_helper_hr$pval.QM, res_metareg_manual_hr$pval.QM, tolerance = 1e-6)
})

test_that("metaRegressionResults() logic with single categorical covariate (SMD)", {
  selected_covariate <- "setting" # Factor variable
  formula_str <- paste("~", selected_covariate)

  data_for_metareg_smd <- metareg_test_data_smd_processed[metareg_test_data_smd_processed$study %in% base_model_smd$studlab, ]
  expect_true("setting" %in% names(data_for_metareg_smd))
  # Ensure 'setting' is a factor for metareg to correctly create dummy variables
  data_for_metareg_smd$setting <- as.factor(data_for_metareg_smd$setting)

  res_metareg_custom_helper_smd_cat <- test_run_metaregression(base_model_smd, formula_str, data_for_metareg_smd)
  expect_false(is.null(res_metareg_custom_helper_smd_cat), "Meta-regression with custom helper failed for SMD categorical.")
  expect_s3_class(res_metareg_custom_helper_smd_cat, "metareg")

  # Manual calculation for comparison
  res_metareg_manual_smd_cat <- meta::metareg(base_model_smd, setting, data = data_for_metareg_smd)

  # Compare key results - coefficients will include dummy variables
  expect_equal(res_metareg_custom_helper_smd_cat$coefficients, res_metareg_manual_smd_cat$coefficients, tolerance = 1e-6)
  expect_equal(res_metareg_custom_helper_smd_cat$se, res_metareg_manual_smd_cat$se, tolerance = 1e-6)
  expect_equal(res_metareg_custom_helper_smd_cat$tau2, res_metareg_manual_smd_cat$tau2, tolerance = 1e-6)
   if(!is.null(res_metareg_custom_helper_smd_cat$I2) || !is.null(res_metareg_manual_smd_cat$I2)) {
    expect_equal(res_metareg_custom_helper_smd_cat$I2, res_metareg_manual_smd_cat$I2, tolerance = 1e-6)
  }
  if(!is.null(res_metareg_custom_helper_smd_cat$R2) || !is.null(res_metareg_manual_smd_cat$R2)) {
   expect_equal(res_metareg_custom_helper_smd_cat$R2, res_metareg_manual_smd_cat$R2, tolerance = 1e-6)
  }
  expect_equal(res_metareg_custom_helper_smd_cat$QM, res_metareg_manual_smd_cat$QM, tolerance = 1e-6)
  expect_equal(res_metareg_custom_helper_smd_cat$pval.QM, res_metareg_manual_smd_cat$pval.QM, tolerance = 1e-6)
})

test_that("metaRegressionResults() logic with single categorical covariate (HR - Colditz)", {
  selected_covariate <- "alloc" # Categorical variable
  formula_str <- paste("~", selected_covariate)

  data_for_metareg_hr <- colditz_data_metareg[colditz_data_metareg$study %in% base_model_hr$studlab, ]
  expect_true("alloc" %in% names(data_for_metareg_hr))
  data_for_metareg_hr$alloc <- as.factor(data_for_metareg_hr$alloc)

  res_metareg_custom_helper_hr_cat <- test_run_metaregression(base_model_hr, formula_str, data_for_metareg_hr)
  expect_false(is.null(res_metareg_custom_helper_hr_cat), "Meta-regression with custom helper failed for HR categorical.")
  expect_s3_class(res_metareg_custom_helper_hr_cat, "metareg")

  # Manual calculation for comparison
  res_metareg_manual_hr_cat <- meta::metareg(base_model_hr, alloc, data = data_for_metareg_hr)

  # Compare key results
  expect_equal(res_metareg_custom_helper_hr_cat$coefficients, res_metareg_manual_hr_cat$coefficients, tolerance = 1e-6)
  expect_equal(res_metareg_custom_helper_hr_cat$se, res_metareg_manual_hr_cat$se, tolerance = 1e-6)
  expect_equal(res_metareg_custom_helper_hr_cat$tau2, res_metareg_manual_hr_cat$tau2, tolerance = 1e-6)
  if(!is.null(res_metareg_custom_helper_hr_cat$I2) || !is.null(res_metareg_manual_hr_cat$I2)) {
    expect_equal(res_metareg_custom_helper_hr_cat$I2, res_metareg_manual_hr_cat$I2, tolerance = 1e-6)
  }
  if(!is.null(res_metareg_custom_helper_hr_cat$R2) || !is.null(res_metareg_manual_hr_cat$R2)) {
   expect_equal(res_metareg_custom_helper_hr_cat$R2, res_metareg_manual_hr_cat$R2, tolerance = 1e-6)
  }
  expect_equal(res_metareg_custom_helper_hr_cat$QM, res_metareg_manual_hr_cat$QM, tolerance = 1e-6)
  expect_equal(res_metareg_custom_helper_hr_cat$pval.QM, res_metareg_manual_hr_cat$pval.QM, tolerance = 1e-6)
})

test_that("metaRegressionResults() logic with multiple covariates (SMD)", {
  selected_covariates <- c("avg_age", "setting")
  formula_str <- paste("~", paste(selected_covariates, collapse = " + "))

  data_for_metareg_smd <- metareg_test_data_smd_processed[metareg_test_data_smd_processed$study %in% base_model_smd$studlab, ]
  expect_true(all(selected_covariates %in% names(data_for_metareg_smd)))
  data_for_metareg_smd$setting <- as.factor(data_for_metareg_smd$setting)

  res_metareg_custom_helper_smd_multi <- test_run_metaregression(base_model_smd, formula_str, data_for_metareg_smd)
  expect_false(is.null(res_metareg_custom_helper_smd_multi), "Meta-regression with custom helper failed for SMD multiple covariates.")
  expect_s3_class(res_metareg_custom_helper_smd_multi, "metareg")

  # Manual calculation for comparison
  res_metareg_manual_smd_multi <- meta::metareg(base_model_smd, ~ avg_age + setting, data = data_for_metareg_smd)

  # Compare key results
  expect_equal(res_metareg_custom_helper_smd_multi$coefficients, res_metareg_manual_smd_multi$coefficients, tolerance = 1e-6)
  expect_equal(res_metareg_custom_helper_smd_multi$se, res_metareg_manual_smd_multi$se, tolerance = 1e-6)
  expect_equal(res_metareg_custom_helper_smd_multi$tau2, res_metareg_manual_smd_multi$tau2, tolerance = 1e-6)
  if(!is.null(res_metareg_custom_helper_smd_multi$I2) || !is.null(res_metareg_manual_smd_multi$I2)) {
    expect_equal(res_metareg_custom_helper_smd_multi$I2, res_metareg_manual_smd_multi$I2, tolerance = 1e-6)
  }
  if(!is.null(res_metareg_custom_helper_smd_multi$R2) || !is.null(res_metareg_manual_smd_multi$R2)) {
   expect_equal(res_metareg_custom_helper_smd_multi$R2, res_metareg_manual_smd_multi$R2, tolerance = 1e-6)
  }
  expect_equal(res_metareg_custom_helper_smd_multi$QM, res_metareg_manual_smd_multi$QM, tolerance = 1e-6)
  expect_equal(res_metareg_custom_helper_smd_multi$pval.QM, res_metareg_manual_smd_multi$pval.QM, tolerance = 1e-6)
})

test_that("metaRegressionResults() logic with multiple covariates (HR - Colditz)", {
  selected_covariates <- c("ablat", "alloc")
  formula_str <- paste("~", paste(selected_covariates, collapse = " + "))

  data_for_metareg_hr <- colditz_data_metareg[colditz_data_metareg$study %in% base_model_hr$studlab, ]
  expect_true(all(selected_covariates %in% names(data_for_metareg_hr)))
  data_for_metareg_hr$alloc <- as.factor(data_for_metareg_hr$alloc)

  res_metareg_custom_helper_hr_multi <- test_run_metaregression(base_model_hr, formula_str, data_for_metareg_hr)
  expect_false(is.null(res_metareg_custom_helper_hr_multi), "Meta-regression with custom helper failed for HR multiple covariates.")
  expect_s3_class(res_metareg_custom_helper_hr_multi, "metareg")

  # Manual calculation for comparison
  res_metareg_manual_hr_multi <- meta::metareg(base_model_hr, ~ ablat + alloc, data = data_for_metareg_hr)

  # Compare key results
  expect_equal(res_metareg_custom_helper_hr_multi$coefficients, res_metareg_manual_hr_multi$coefficients, tolerance = 1e-6)
  expect_equal(res_metareg_custom_helper_hr_multi$se, res_metareg_manual_hr_multi$se, tolerance = 1e-6)
  expect_equal(res_metareg_custom_helper_hr_multi$tau2, res_metareg_manual_hr_multi$tau2, tolerance = 1e-6)
  if(!is.null(res_metareg_custom_helper_hr_multi$I2) || !is.null(res_metareg_manual_hr_multi$I2)) {
    expect_equal(res_metareg_custom_helper_hr_multi$I2, res_metareg_manual_hr_multi$I2, tolerance = 1e-6)
  }
  if(!is.null(res_metareg_custom_helper_hr_multi$R2) || !is.null(res_metareg_manual_hr_multi$R2)) {
   expect_equal(res_metareg_custom_helper_hr_multi$R2, res_metareg_manual_hr_multi$R2, tolerance = 1e-6)
  }
  expect_equal(res_metareg_custom_helper_hr_multi$QM, res_metareg_manual_hr_multi$QM, tolerance = 1e-6)
  expect_equal(res_metareg_custom_helper_hr_multi$pval.QM, res_metareg_manual_hr_multi$pval.QM, tolerance = 1e-6)
})

test_that("metaRegressionResults() logic with no/invalid formula or missing covariates", {
  data_for_metareg_smd <- metareg_test_data_smd_processed[metareg_test_data_smd_processed$study %in% base_model_smd$studlab, ]

  # No formula string
  expect_null(test_run_metaregression(base_model_smd, NULL, data_for_metareg_smd))
  expect_null(test_run_metaregression(base_model_smd, "", data_for_metareg_smd))

  # Invalid formula string (metareg would error, helper should return NULL)
  expect_null(test_run_metaregression(base_model_smd, "~", data_for_metareg_smd))
  expect_null(test_run_metaregression(base_model_smd, "~ .", data_for_metareg_smd)) # Dot might be tricky

  # Non-existent covariate in formula
  expect_null(test_run_metaregression(base_model_smd, "~ non_existent_var", data_for_metareg_smd))

  # Formula valid, but covariate data missing from the `data` argument passed to metareg
  # The helper `test_run_metaregression` checks if covars_in_formula are in names(data_for_metareg)
  # So, if data_for_metareg does not have 'avg_age', it should return NULL.
  data_missing_cov <- data_for_metareg_smd[, !names(data_for_metareg_smd) %in% "avg_age"]
  expect_null(test_run_metaregression(base_model_smd, "~ avg_age", data_missing_cov))
})

# Mimic of the full metaRegressionResults reactive for testing subgroup interaction
test_metaRegressionResults_full_logic <- function(base_model,
                                                 selected_covs,
                                                 is_subgroup_active,
                                                 processed_data_with_covs) {
  if (is_subgroup_active) {
    return(NULL)
  }
  if (is.null(selected_covs) || length(selected_covs) == 0) {
    return(NULL)
  }

  # Simplified data source logic for testing: assume processed_data_with_covs is correctly passed
  # In server, it tries model$data first, then data()
  df_for_metareg <- processed_data_with_covs

  if (is.null(df_for_metareg)) {
    # message("Data for meta-regression (processed_data_with_covs) is not available in test.")
    return(NULL)
  }

  missing_covariates <- setdiff(selected_covs, names(df_for_metareg))
  if (length(missing_covariates) > 0) {
    # message(paste("Error: Covariate(s)", paste(missing_covariates, collapse=", "),
                  # "not found in the analysis data for test."))
    return(NULL)
  }

  formula_str <- paste("~", paste(selected_covs, collapse = " + "))

  # Use the already tested helper for the core metareg call
  return(test_run_metaregression(base_model, formula_str, df_for_metareg))
}

test_that("metaRegressionResults() logic correctly handles interaction with active subgroup analysis", {
  selected_covs <- c("avg_age")
  data_for_metareg_smd <- metareg_test_data_smd_processed[metareg_test_data_smd_processed$study %in% base_model_smd$studlab, ]

  # Scenario: Subgroup analysis IS active
  res_with_subgroup <- test_metaRegressionResults_full_logic(
    base_model = base_model_smd,
    selected_covs = selected_covs,
    is_subgroup_active = TRUE,
    processed_data_with_covs = data_for_metareg_smd
  )
  expect_null(res_with_subgroup, "Expected NULL when subgroup analysis is active.")

  # Scenario: Subgroup analysis IS NOT active
  res_without_subgroup <- test_metaRegressionResults_full_logic(
    base_model = base_model_smd,
    selected_covs = selected_covs,
    is_subgroup_active = FALSE,
    processed_data_with_covs = data_for_metareg_smd
  )
  expect_false(is.null(res_without_subgroup), "Expected a metareg object when subgroup analysis is not active.")
  expect_s3_class(res_without_subgroup, "metareg")

  # Scenario: No covariates selected, subgroup not active
  res_no_covs_no_subgroup <- test_metaRegressionResults_full_logic(
    base_model = base_model_smd,
    selected_covs = character(0), # Empty character vector
    is_subgroup_active = FALSE,
    processed_data_with_covs = data_for_metareg_smd
  )
  expect_null(res_no_covs_no_subgroup, "Expected NULL when no covariates are selected.")

  # Scenario: Covariates selected, subgroup not active, but data for metareg is NULL
   res_null_data <- test_metaRegressionResults_full_logic(
    base_model = base_model_smd,
    selected_covs = selected_covs,
    is_subgroup_active = FALSE,
    processed_data_with_covs = NULL # Simulate data not being available
  )
  expect_null(res_null_data, "Expected NULL when data for metareg is NULL.")

  # Scenario: Covariate specified not in data
  res_missing_cov_in_data <- test_metaRegressionResults_full_logic(
      base_model = base_model_smd,
      selected_covs = c("non_existent_cov"),
      is_subgroup_active = FALSE,
      processed_data_with_covs = data_for_metareg_smd
  )
  expect_null(res_missing_cov_in_data, "Expected NULL when covariate not in provided data.")

})

# Helper for testing metareg_summary_output logic
test_render_metareg_summary <- function(model_results, is_subgroup_active_input, selected_covs_input, data_type_input = "SMD") {
  # Capture output of a simplified renderPrint logic
  capture.output({
    if (is.null(model_results)) {
      subgroup_active <- is_subgroup_active_input
      covariates_selected <- !is.null(selected_covs_input) && length(selected_covs_input) > 0

      if (subgroup_active && covariates_selected) {
        cat("Meta-regression was not performed because subgroup analysis is active.\nPlease clear the subgroup selection if you wish to perform meta-regression.")
      } else if (!covariates_selected) {
        cat("No covariates selected for meta-regression. Please select one or more covariates from the sidebar.")
      } else {
        # This case implies model_results is NULL for other reasons (e.g. error in metareg, data issues)
        cat("Meta-regression results are not available. This might be due to an error during calculation or other conflicting settings.")
      }
    } else {
      cat("Meta-Regression Model Summary\n")
      cat("-----------------------------\n\n")
      print(summary(model_results)) # This will print a lot, good for checking if it runs
      cat("\n\nInterpretation Notes:\n") # Check for this part
      if (data_type_input == "hr") {
        cat("\nNote: For Hazard Ratios (HR), the 'estimate' and its CI are on the log-HR scale.\n")
      }
    }
  })
}

test_that("output$metareg_summary_output logic is correct", {
  # Scenario 1: Valid metareg model (use a previously generated one)
  res_metareg_manual_smd <- meta::metareg(base_model_smd, avg_age, data = metareg_test_data_smd_processed)
  output_valid_model <- test_render_metareg_summary(res_metareg_manual_smd, FALSE, c("avg_age"))

  expect_true(any(grepl("Meta-Regression Model Summary", output_valid_model)))
  expect_true(any(grepl("avg_age", output_valid_model))) # Check if covariate name appears
  expect_true(any(grepl("tau\\^2", output_valid_model))) # Check for tau-squared symbol (escaped)
  expect_true(any(grepl("Interpretation Notes:", output_valid_model)))

  # Scenario 1b: Valid HR model
  res_metareg_manual_hr <- meta::metareg(base_model_hr, ablat, data = colditz_data_metareg)
  output_valid_hr_model <- test_render_metareg_summary(res_metareg_manual_hr, FALSE, c("ablat"), data_type_input = "hr")
  expect_true(any(grepl("Note: For Hazard Ratios \\(HR\\)", output_valid_hr_model)))


  # Scenario 2: Subgroup analysis active
  output_subgroup_active <- test_render_metareg_summary(NULL, TRUE, c("avg_age"))
  expect_match(paste(output_subgroup_active, collapse="\n"),
               "Meta-regression was not performed because subgroup analysis is active", fixed = TRUE)

  # Scenario 3: No covariates selected
  output_no_covs <- test_render_metareg_summary(NULL, FALSE, character(0))
  expect_match(paste(output_no_covs, collapse="\n"),
               "No covariates selected for meta-regression", fixed = TRUE)

  # Scenario 4: Null model for other reasons (e.g., error in metareg calculation)
  # Simulate this by passing selected_covs but a NULL model_results
  output_other_null <- test_render_metareg_summary(NULL, FALSE, c("avg_age"))
  expect_match(paste(output_other_null, collapse="\n"),
               "Meta-regression results are not available", fixed = TRUE)
})


# Conceptual helper for metareg_plot_output logic
test_metareg_plot_logic <- function(model_results, selected_covariates_input, data_for_plot_input) {
  if (is.null(model_results)) {
    return("No plot - model is NULL.")
  }
  if (is.null(selected_covariates_input) || length(selected_covariates_input) == 0) {
    return("No plot - no covariates selected.")
  }

  cov_to_plot <- NULL
  # Simplified logic from server.R to find first numeric covariate *used in model*
  if(!is.null(model_results$X.names) && length(model_results$X.names) > 0) {
      potential_covs <- model_results$X.names[model_results$X.names != "(Intercept)"]
      # model_results$data should be the data frame used by metareg
      df_model_data <- model_results$data
      if (is.null(df_model_data)) { # Fallback if model$data is not populated (e.g. older meta version)
          df_model_data <- data_for_plot_input # This is a simplification
      }

      for(cov_name in potential_covs) {
          # Check if this covariate (from model's X.names) is numeric in the provided data
          if(cov_name %in% names(df_model_data) && is.numeric(df_model_data[[cov_name]])) {
              cov_to_plot <- cov_name
              break
          }
      }
  }

  if (is.null(cov_to_plot)) {
    return("No numeric covariate found for plotting.")
  } else {
    # In a real scenario, meta::bubble would be called here.
    # For testing, we just confirm which covariate would be plotted.
    return(paste("Plot attempted for covariate:", cov_to_plot))
  }
}

test_that("output$metareg_plot_output conceptual logic is correct", {
  res_metareg_smd_num <- meta::metareg(base_model_smd, avg_age, data = metareg_test_data_smd_processed)
  res_metareg_smd_cat <- meta::metareg(base_model_smd, setting, data = metareg_test_data_smd_processed)
  res_metareg_smd_multi <- meta::metareg(base_model_smd, ~ avg_age + setting, data = metareg_test_data_smd_processed)

  # Valid model with numeric covariate
  expect_equal(
    test_metareg_plot_logic(res_metareg_smd_num, c("avg_age"), metareg_test_data_smd_processed),
    "Plot attempted for covariate: avg_age"
  )

  # Valid model with only categorical covariate (model$X.names will list dummy vars)
  # The helper needs to correctly identify that 'setting' itself is not directly numeric for plotting by bubble
  # if bubble expects a single numeric column name.
  # However, model_results$X.names for a factor 'setting' will be like 'settingCommunity', 'settingHospital'.
  # The current helper logic might pick one of these if they are considered "numeric" (0/1).
  # This depends on how strictly "is.numeric" is applied to dummy variables.
  # For the server's bubble plot, it relies on meta::bubble to handle the metareg object.
  # The test helper is simplified. Let's assume for now it looks for original column.
  # A more robust test for the plot would directly check if meta::bubble is called.

  # If 'setting' is the *only* covariate in selected_covariates_input, and it's categorical.
  # The current logic in server.R for metareg_plot_output tries to find the *first numeric* covariate.
  # If 'setting' (factor) is the only one, it should result in "no suitable numeric covariate".
   expect_equal(
     test_metareg_plot_logic(res_metareg_smd_cat, c("setting"), metareg_test_data_smd_processed),
     "No numeric covariate found for plotting." # Because 'setting' is factor, not numeric.
                                                # And dummy vars like 'settingCommunity' are not in selected_covariates_input
   )


  # Multiple covariates, first is numeric
  expect_equal(
    test_metareg_plot_logic(res_metareg_smd_multi, c("avg_age", "setting"), metareg_test_data_smd_processed),
    "Plot attempted for covariate: avg_age"
  )

  # Multiple covariates, first is categorical, second is numeric (order in selected_covariates_input matters for the test helper if it iterates through that)
  # The server logic iterates through model$X.names which might have a different order.
  # For robustness, let's ensure the test helper also respects model$X.names order for finding first numeric.
  # The current helper iterates `potential_covs <- model_results$X.names...` so it should be fine.
  res_metareg_smd_multi_cat_first <- meta::metareg(base_model_smd, ~ setting + avg_age, data = metareg_test_data_smd_processed)
   expect_equal(
     test_metareg_plot_logic(res_metareg_smd_multi_cat_first, c("setting", "avg_age"), metareg_test_data_smd_processed),
     "Plot attempted for covariate: avg_age" # avg_age is numeric, even if setting is listed first in formula
   )


  # Model is NULL
  expect_equal(test_metareg_plot_logic(NULL, c("avg_age"), metareg_test_data_smd_processed), "No plot - model is NULL.")

  # No covariates selected
  expect_equal(test_metareg_plot_logic(res_metareg_smd_num, character(0), metareg_test_data_smd_processed), "No plot - no covariates selected.")

  # Covariate selected, but it's not in the model's X.names (e.g. error, or not actually used)
  # Create a model where 'year' was not actually included.
  res_metareg_smd_no_year <- meta::metareg(base_model_smd, avg_age, data = metareg_test_data_smd_processed)
  expect_equal(
    test_metareg_plot_logic(res_metareg_smd_no_year, c("year"), metareg_test_data_smd_processed),
    "No numeric covariate found for plotting." # Because 'year' isn't in X.names of this specific model
  )
})
