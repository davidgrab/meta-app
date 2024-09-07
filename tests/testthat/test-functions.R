library(testthat)
library(meta)

source("../../R/functions.R")

test_that("random_forest_plot works", {
  data <- metabin(event.e = c(10, 15), n.e = c(100, 100),
                  event.c = c(5, 10), n.c = c(100, 100),
                  studlab = c("Study 1", "Study 2"),
                  sm = "RR", method = "Inverse")
  
  plot <- random_forest_plot(data)
  expect_s3_class(plot, "forest")
})

test_that("heterogeneity_plot works", {
  data <- metabin(event.e = c(10, 15), n.e = c(100, 100),
                  event.c = c(5, 10), n.c = c(100, 100),
                  studlab = c("Study 1", "Study 2"),
                  sm = "RR", method = "Inverse")
  
  plot <- heterogeneity_plot(data)
  expect_s3_class(plot, "ggplot")
})

test_that("influence_analysis works", {
  data <- metabin(event.e = c(10, 15, 20), n.e = c(100, 100, 100),
                  event.c = c(5, 10, 15), n.c = c(100, 100, 100),
                  studlab = c("Study 1", "Study 2", "Study 3"),
                  sm = "RR", method = "Inverse")
  
  result <- capture.output(influence_analysis(data))
  expect_true(any(grepl("Influence Analysis", result)))
})

# Add more tests for other functions