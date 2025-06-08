# Configure Pandoc for R
library(rmarkdown)

# Print current Pandoc configuration
cat("Current Pandoc configuration:\n")
print(rmarkdown::find_pandoc())

# Set Pandoc path if needed
if (!rmarkdown::pandoc_available()) {
  Sys.setenv(RSTUDIO_PANDOC = "/usr/local/bin")
  cat("\nSet Pandoc path to /usr/local/bin\n")
}

# Verify Pandoc is now available
cat("\nVerifying Pandoc availability:\n")
print(rmarkdown::pandoc_available())
print(rmarkdown::find_pandoc()) 