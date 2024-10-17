library(testthat)
library(meta)
library(metafor)
library(ggplot2)
library(plotly)

source("R/bivariate_meta.R")
source("R/functions.R")

# Test data based on the provided Hypericum vs. Placebo study
test_data <- data.frame(
  studlab = c("Study 1", "Study 2", "Study 3", "Study 4", "Study 5", "Study 6", "Study 7", "Study 8", 
              "Study 9", "Study 10", "Study 11", "Study 12", "Study 13", "Study 14", "Study 15", 
              "Study 16", "Study 17", "Study 18"),
  event.e = c(22, 17, 35, 23, 24, 4, 45, 26, 41, 64, 71, 46, 159, 98, 55, 67, 46, 34),
  n.e = c(54, 45, 53, 37, 49, 20, 80, 98, 70, 109, 131, 113, 243, 186, 123, 106, 70, 48),
  event.c = c(21, 9, 12, 15, 16, 11, 12, 19, 4, 48, 51, 56, 26, 80, 57, 22, 34, 25),
  n.c = c(55, 43, 55, 35, 49, 26, 79, 102, 70, 109, 130, 116, 81, 189, 124, 47, 70, 49)
)
random_result <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                         event.c = test_data$event.c, n.c = test_data$n.c,
                         studlab = test_data$studlab,
                         sm = "RR", method = "Inverse", random = TRUE)

fixed_result <- metabin(event.e = test_data$event.e, n.e = test_data$n.e,
                        event.c = test_data$event.c, n.c = test_data$n.c,
                        studlab = test_data$studlab,
                        sm = "RR", method = "Inverse", fixed = TRUE)

bivariate_result <- metabiv(event.e = test_data$event.e, n.e = test_data$n.e,
                            event.c = test_data$event.c, n.c = test_data$n.c,
                            studlab = test_data$studlab, sm = "RR")

output_file <- render_report(random_result, fixed_result, bivariate_result)

expect_true(file.exists(output_file))
expect_true(grepl("\\.html$", output_file))


