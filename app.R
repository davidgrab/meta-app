# Modern Meta-Analysis Shiny App
# This app provides tools for conducting and visualizing meta-analyses.
# Run this app using: shiny::runApp()

source("R/ui.R")
source("R/server.R")

shinyApp(ui = ui, server = server)