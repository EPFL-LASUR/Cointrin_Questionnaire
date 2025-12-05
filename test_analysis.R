# Test script for analysis functions
# This script tests the analysis functionality using example data

cat("=== Testing Analysis Functions ===\n\n")

# Source the analysis script
source("analysis.R")

# Check if example data exists
if (!file.exists("example_response.csv")) {
  stop("example_response.csv not found. Please ensure the file exists.")
}

cat("Testing with example data...\n\n")

# Test the analyze_responses function
cat("1. Testing analyze_responses() function:\n")
cat("----------------------------------------\n")
data <- analyze_responses("example_response.csv")

cat("\n2. Testing create_visualizations() function:\n")
cat("--------------------------------------------\n")
tryCatch({
  create_visualizations("example_response.csv")
  cat("SUCCESS: Visualizations created in plots/ directory\n")
}, error = function(e) {
  cat("Note: Visualization creation requires ggplot2 package.\n")
  cat("Install with: install.packages('ggplot2')\n")
  cat("Error message:", e$message, "\n")
})

cat("\n3. Verifying data structure:\n")
cat("----------------------------\n")
cat("Columns in dataset:", paste(colnames(data), collapse=", "), "\n")
cat("Number of rows:", nrow(data), "\n")
cat("Number of columns:", ncol(data), "\n")

# Check data types
cat("\nData types:\n")
str(data)

cat("\n=== Test Complete ===\n")
cat("If no errors appeared above, the analysis functions are working correctly.\n")
