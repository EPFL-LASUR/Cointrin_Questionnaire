# Analysis script for Cointrin Questionnaire responses
# This script provides basic analysis of the collected survey data

# Load required libraries
library(ggplot2)

# Function to load and analyze questionnaire responses
analyze_responses <- function(file_path = "questionnaire_responses.csv") {
  
  # Check if file exists
  if (!file.exists(file_path)) {
    stop("No responses file found. Please ensure questionnaire_responses.csv exists.")
  }
  
  # Load data
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  cat("=== Cointrin Questionnaire - Data Analysis ===\n\n")
  
  # Basic statistics
  cat("Total number of responses:", nrow(data), "\n\n")
  
  # Age distribution
  cat("Age Distribution:\n")
  age_table <- table(data$age_group)
  print(age_table)
  cat("\n")
  
  # Residence distribution
  cat("Residence Distribution:\n")
  residence_table <- table(data$residence)
  print(residence_table)
  cat("\n")
  
  # Visit frequency
  cat("Visit Frequency:\n")
  frequency_table <- table(data$visit_frequency)
  print(frequency_table)
  cat("\n")
  
  # Average ratings
  cat("Average Ratings:\n")
  cat("- Accessibility:", mean(data$accessibility, na.rm = TRUE), "\n")
  cat("- Safety:", mean(data$safety, na.rm = TRUE), "\n")
  cat("- Attractiveness:", mean(data$attractiveness, na.rm = TRUE), "\n\n")
  
  # Return the data for further analysis
  invisible(data)
}

# Function to create visualizations
create_visualizations <- function(file_path = "questionnaire_responses.csv") {
  
  if (!file.exists(file_path)) {
    stop("No responses file found.")
  }
  
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Create output directory for plots
  if (!dir.exists("plots")) {
    dir.create("plots")
  }
  
  # Plot 1: Age distribution
  p1 <- ggplot(data, aes(x = age_group)) +
    geom_bar(fill = "steelblue") +
    labs(title = "Age Distribution of Respondents",
         x = "Age Group", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("plots/age_distribution.png", p1, width = 8, height = 6)
  
  # Plot 2: Visit frequency
  p2 <- ggplot(data, aes(x = visit_frequency)) +
    geom_bar(fill = "coral") +
    labs(title = "Visit Frequency to Cointrin Area",
         x = "Frequency", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("plots/visit_frequency.png", p2, width = 8, height = 6)
  
  # Plot 3: Average ratings
  ratings <- data.frame(
    Category = c("Accessibility", "Safety", "Attractiveness"),
    Average = c(mean(data$accessibility, na.rm = TRUE),
                mean(data$safety, na.rm = TRUE),
                mean(data$attractiveness, na.rm = TRUE))
  )
  
  p3 <- ggplot(ratings, aes(x = Category, y = Average)) +
    geom_bar(stat = "identity", fill = "seagreen") +
    ylim(0, 5) +
    labs(title = "Average Ratings for Cointrin Area",
         x = "Category", y = "Average Rating (1-5)") +
    theme_minimal() +
    geom_hline(yintercept = 3, linetype = "dashed", color = "red")
  
  ggsave("plots/average_ratings.png", p3, width = 8, height = 6)
  
  cat("Visualizations saved in the 'plots' directory.\n")
}

# Example usage:
# data <- analyze_responses()
# create_visualizations()
