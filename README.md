# StatProg II Group 0

This repository contains a Statistical Programming II group project on demographic change in Munich. The project is descriptive and focuses on three related dimensions: age structure, migration dynamics, and household composition.

## Group Members

- Youpeng Jiang
- Xiaohan Liu
- Yang Xi

## Dataset

The project uses the dataset *Monatszahlen Bevoelkerung* from the Open Data Portal of the City of Munich. The dataset is provided by the Statistical Office of Munich and contains monthly population-related indicators for Munich.

The raw dataset is organised in long format. Each row describes one demographic topic and subcategory for a specific year and month. The main row identifiers are:

- `MONATSZAHL`: main demographic topic, for example age groups or migration indicators
- `AUSPRAEGUNG`: subcategory within the topic, for example a specific age group
- `JAHR`: year of the observation
- `MONAT`: month of the observation, usually in `YYYYMM` format

The main value column is `WERT`. The dataset also contains comparison variables such as previous-year values and percentage changes. The project excludes 2025 from the main analysis because the available monthly observations are incomplete.

## Repository Structure

- `code/01_import_clean.R`: imports and prepares the raw population data
- `code/02_eda.R`: exploratory age-structure analysis
- `code/03_analysis.R`: simple statistical model for the 65+ population share
- `code/04_migration_analysis.R`: migration and internationalisation analysis
- `code/05_household_analysis.R`: household composition analysis
- `code/06_age_migration_comparison.R`: exploratory age-migration comparison
- `data/processed/`: processed CSV outputs used by the proposal, report, and plots
- `figures/`: generated plots
- `proposal.qmd`: proposal and initial data analysis
- `report.qmd`: final report draft
- `docs/`: rendered Quarto website

## Reproduce the Analysis

Run the scripts from the project root in numerical order:

```r
source("code/01_import_clean.R")
source("code/02_eda.R")
source("code/03_analysis.R")
source("code/04_migration_analysis.R")
source("code/05_household_analysis.R")
source("code/06_age_migration_comparison.R")
```

Render the website with:

```r
quarto::quarto_render()
```

or from a terminal with:

```powershell
quarto render
```
