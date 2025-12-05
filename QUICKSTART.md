# Quick Start Guide

## For First-Time Users

### Step 1: Install R
Download and install R from [CRAN](https://cran.r-project.org/)

### Step 2: Install RStudio (Optional but Recommended)
Download and install RStudio from [RStudio.com](https://www.rstudio.com/products/rstudio/download/)

### Step 3: Install Required Packages
Open R or RStudio and run:
```r
source("install_packages.R")
```

### Step 4: Launch the Questionnaire
```r
shiny::runApp("app.R")
```

The questionnaire will open in your web browser!

## For Survey Administrators

### Starting a New Survey Session
1. Ensure `questionnaire_responses.csv` does not exist, or back it up
2. Launch the application with `shiny::runApp("app.R")`
3. Share the local URL with participants (e.g., http://127.0.0.1:XXXX)

### Collecting Data
- The app automatically saves responses to `questionnaire_responses.csv`
- Each participant submission adds a new row
- Monitor the file to track response count

### Analyzing Results
```r
source("analysis.R")
data <- analyze_responses()
create_visualizations()
```

Check the `plots/` folder for generated charts.

## Deploying to a Server

For wider distribution, you can deploy to:
- [shinyapps.io](https://www.shinyapps.io/) (free tier available)
- Your own Shiny Server
- RStudio Connect

### Example deployment to shinyapps.io:
```r
install.packages("rsconnect")
library(rsconnect)

# Configure your account (first time only)
rsconnect::setAccountInfo(name='your-account',
                         token='your-token',
                         secret='your-secret')

# Deploy the app
rsconnect::deployApp()
```

## Troubleshooting

### "Package not found" error
Run `install_packages.R` again or manually install:
```r
install.packages(c("shiny", "shinyjs", "ggplot2"))
```

### Application doesn't open in browser
Try accessing manually: http://127.0.0.1:XXXX (port number shown in R console)

### Data not saving
- Check write permissions in the directory
- Ensure all required fields are filled before submission

## Support
For issues or questions, please open an issue on GitHub.
