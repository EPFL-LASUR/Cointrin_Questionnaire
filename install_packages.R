# Installation script for required R packages
# Run this script before launching the questionnaire application

# Function to install packages if not already installed
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# List of required packages
required_packages <- c(
  "shiny",
  "shinyjs"
)

# Install required packages
cat("Installing required packages...\n")
for (pkg in required_packages) {
  cat(paste0("Checking ", pkg, "...\n"))
  install_if_missing(pkg)
}

cat("\nAll required packages are installed!\n")
cat("You can now run the questionnaire with: shiny::runApp('app.R')\n")
