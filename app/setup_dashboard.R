# Setup script for Vehicle Trends Dashboard
# Run this script first to install required packages

# List of required packages
packages <- c(
  "shiny",           # Core Shiny framework
  "shinydashboard",  # Dashboard layout
  "ggplot2",         # Plotting
  "plotly",          # Interactive plots
  "DT",              # Interactive data tables
  "dplyr",           # Data manipulation
  "colourpicker",    # Color input widget
  "arrow",           # Read parquet files
  "here"             # File path management
)

# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Install packages that are not already installed
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(paste("Installing", pkg, "...\n"))
    install.packages(pkg, dependencies = TRUE, quiet = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    cat(paste("✓", pkg, "already installed\n"))
  }
}

# Install all required packages
cat("Installing required packages for Vehicle Trends Dashboard...\n")
sapply(packages, install_if_missing)

cat("\n All packages installed successfully!\n")
cat(" You can now run the dashboard with: source('vehicle_dashboard.R')\n")
cat("Or run: shiny::runApp('vehicle_dashboard.R')\n")