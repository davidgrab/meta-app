# Modern Meta-Analysis Shiny Apprsconnect::setAccountInfo;
# This app provides tools for conducting and visualizing meta-analyses.
# Run this app using: shiny::runApp()
# Run tests testthat::test_dir("tests/testthat")

source("R/ui.R")
source("R/server.R")

shinyApp(ui = ui, server = server)
