![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)
[![Project Status](https://img.shields.io/badge/status-under%20development-yellow)](https://github.com/EPFL-LASUR/Cointrin_Questionnaire)
![GitHub License](https://img.shields.io/github/license/EPFL-LASUR/Cointrin_Questionnaire)

# Cointrin_Questionnaire
This repository contains the code to preprocess and generate plots for the Cointrin questionnaire made by LASUR lab from EPFL.

## Project structure
The project is structured like so :<br>
[R/](R) : contains the notebooks and R code in general<br>
[data/raw](data/raw) : contains the raw data from the questionnaire and shapefiles for map plots. Data from questionnaire is expected to be stored in an sav file.<br>
[data/processed](data/processed/) : contains processed data. It could be cleaned data or any output of scripts written in [R](R) folder.

## Install

To set up the environment, you need [RStudio](https://posit.co/download/rstudio-desktop/) with the [renv](https://rstudio.github.io/renv/) package installed.

First, clone the repository. Then, open the `Cointrin_Questionnaire.Rproj` file with RStudio and restore the dependencies from the lockfile using

```r
renv::restore()
```

## Usage

To use this project, you first have to drop your data in [data/raw](data/raw) folder. When this is done, you are ready to start using the notebooks. <br>
For an optimal usage, the notebooks should be run in that order : <br>
1. [data_cleaning.Rmd](R/data_cleaning.Rmd). This notebook cleans unwanted users from the dataset.
2. [regrouping.Rmd](R/regrouping.Rmd). This notebook recodes variables in a more usable way.
3. [plots.Rmd](R/plots.Rmd). This notebook creates plots from given data.

You might have to change input file names in the notebooks. This file name is always in the first code block of every notebook. The file name should the exact name of the file that you want to use.
