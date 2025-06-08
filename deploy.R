# Set SSL verification options
options(rsconnect.check.certificate = FALSE)


# Deploy the app
rsconnect::deployApp() 
