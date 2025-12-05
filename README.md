# Cointrin Questionnaire

Interactive survey application for collecting data about the Cointrin area in Geneva, Switzerland.

## Description

This project provides an interactive web-based questionnaire built with R Shiny to collect opinions and experiences from people regarding the Cointrin area. The questionnaire covers:

- Demographic information
- Area usage patterns
- Transportation modes
- Perception of accessibility, safety, and attractiveness
- Suggestions for improvements

## Features

- **Bilingual interface** (French/English)
- **Interactive web application** using Shiny
- **Data validation** to ensure quality responses
- **Automatic data storage** in CSV format
- **Analysis scripts** for processing collected data
- **Visualization tools** for results presentation

## Requirements

- R (>= 3.5.0)
- RStudio (recommended)

### Required R Packages

- `shiny` (>= 1.7.0)
- `shinyjs` (>= 2.0.0)
- `ggplot2` (for data analysis and visualization)

## Installation

1. Clone this repository:
```bash
git clone https://github.com/EPFL-LASUR/Cointrin_Questionnaire.git
cd Cointrin_Questionnaire
```

2. Install required R packages:
```r
source("install_packages.R")
```

Or manually install packages:
```r
install.packages(c("shiny", "shinyjs", "ggplot2"))
```

## Usage

### Running the Questionnaire

1. Open R or RStudio
2. Set working directory to the project folder
3. Run the application:

```r
shiny::runApp("app.R")
```

The application will open in your default web browser.

### Collecting Responses

- Responses are automatically saved to `questionnaire_responses.csv`
- Each submission appends a new row to the CSV file
- The CSV file is created automatically on first submission

### Analyzing Data

After collecting responses, you can analyze the data:

```r
source("analysis.R")

# View basic statistics
data <- analyze_responses()

# Create visualizations
create_visualizations()
```

Visualizations will be saved in the `plots/` directory.

## Project Structure

```
Cointrin_Questionnaire/
├── app.R                      # Main Shiny application
├── analysis.R                 # Data analysis script
├── install_packages.R         # Package installation script
├── DESCRIPTION                # Package metadata
├── Cointrin_Questionnaire.Rproj  # RStudio project file
├── README.md                  # This file
├── .gitignore                 # Git ignore file
└── questionnaire_responses.csv  # Generated data file (after first submission)
```

## Data Privacy

- All data is stored locally
- No personal identifying information is collected
- Responses are timestamped for analysis purposes
- Data should be handled according to relevant privacy regulations (e.g., GDPR)

## Questionnaire Sections

1. **Demographic Information**: Age group and residence location
2. **Area Usage**: Visit frequency and reasons for visiting
3. **Transportation**: Modes of transport used
4. **Perception**: Ratings for accessibility, safety, and attractiveness
5. **Feedback**: Open-ended suggestions and comments

## Customization

To customize the questionnaire:

1. Open `app.R` in a text editor
2. Modify questions in the UI section
3. Update data collection logic in the server section if needed
4. Adjust the bilingual text as required

## Contributing

This is a project by EPFL-LASUR (Laboratory of Urbanism, EPFL). For contributions or questions, please contact the project maintainers.

## License

MIT License

## Authors

EPFL-LASUR (École Polytechnique Fédérale de Lausanne - Laboratory of Urbanism)

## Contact

For questions or support, please open an issue in this repository.

---

*Note: This questionnaire is designed for research purposes related to urban planning and development in the Cointrin area.*