# Demographic Change in Munich

This repository contains the group project for **StatProg II - Advanced Statistical Programming using R**.

The project analyses demographic change in Munich using the *Monatszahlen Bevölkerung* dataset from Munich Open Data. We focus on age structure, migration dynamics, international population composition and household-size composition.

## Project Website

Rendered project website: **TODO: add GitHub Pages link**

Main project pages:

- [Home](index.qmd)
- [Project Proposal](proposal.qmd)
- [Final Report](report.qmd)
- [Group Reflection](group-reflection.qmd)

## Project Purpose

The purpose of this project is to apply statistical programming tools to a real-world open dataset. The project combines data cleaning, exploratory analysis, visualisation, reproducible reporting and collaborative GitHub workflow.

The final output is a Quarto website containing the project proposal, final report, group reflection and supporting project materials.

## Research Questions

This report asks how Munich's demographic structure changed between 2000 and 2024.

More specifically, we focus on three descriptive questions:

1. How did Munich's age structure change during this period?
2. How did migration dynamics and international population composition change?
3. Did household-size composition change alongside Munich's age structure and migration dynamics, or did it remain relatively stable?

The analysis is descriptive and exploratory. It does not aim to identify causal effects.

## Dataset

The project uses the *Monatszahlen Bevölkerung* dataset from the Open Data Portal of the City of Munich.

Dataset source: [Monatszahlen Bevölkerung, Munich Open Data](https://opendata.muenchen.de/dataset/monatszahlen-bevoelkerung)

The dataset contains monthly demographic indicators for Munich. Each row represents one demographic topic, subcategory and time period. Most rows refer to a specific month within a year, while rows with `MONAT == "Summe"` represent annual summary values.

Key variables include:

- `JAHR`: year
- `MONAT`: month or annual summary marker
- `MONATSZAHL`: demographic topic, such as age groups, migration indicators or household categories
- `AUSPRAEGUNG`: subcategory within the demographic topic
- `WERT`: reported value, such as population count or migration value

For age structure and migration, the analysis covers **2000-2024**. For household-size composition, the analysis covers **2012-2024**, because this is the consistent period available for household-size categories.

## Data Scope and Limitations

Rows where `MONAT == "Summe"` are excluded from analyses based on monthly observations because they are annual summary rows. The analysis excludes 2025 because it is incomplete.

Migration indicators are summarised as yearly averages of monthly observations. They should therefore not be interpreted as annual totals.

The project uses aggregate municipal statistics. It can describe demographic change, but it cannot explain individual migration decisions, household formation processes or causal relationships between migration, age structure and household composition.

## Repository Structure

```text
.
├── README.md
├── _quarto.yml
├── index.qmd
├── proposal.qmd
├── report.qmd
├── group-reflection.qmd
├── code/
├── data/
├── docs/
├── figures/
└── notes/
```

Main files and folders:

- `README.md`: overview of the project repository
- `_quarto.yml`: Quarto website configuration
- `index.qmd`: website homepage
- `proposal.qmd`: project proposal and updated proposal material
- `report.qmd`: final report
- `group-reflection.qmd`: group reflection page for the final submission
- `code/`: R scripts for cleaning, exploration, analysis and figure generation
- `data/`: raw and processed data used in the project
- `docs/`: rendered Quarto website output for GitHub Pages
- `figures/`: exported figures used in the proposal and report
- `notes/`: meeting notes and project notes

## Analysis Workflow

The project follows a reproducible workflow:

1. Import and clean the raw Munich Open Data file.
2. Create processed datasets for age structure, migration indicators, international population composition and household-size composition.
3. Generate figures and summary tables.
4. Use Quarto to render the proposal, final report and group reflection as a website.

To reproduce the main outputs, run:

```bash
Rscript code/01_import_clean.R
Rscript code/02_eda.R
Rscript code/04_migration_analysis.R
Rscript code/05_household_analysis.R
Rscript code/06_age_migration_comparison.R
quarto render
```

## Main Outputs

The final report presents four connected findings:

- Munich grew across age groups, but not evenly.
- Migration dynamics became more volatile after the mid-2010s.
- Internationalisation increased, especially among non-EU groups.
- Household-size composition remained relatively stable from 2012 to 2024.


## Group Members

- **Yang Xi**
- **Youpeng Jiang**
- **Xiaohan Liu**

## Project Milestones

- Initial project proposal: `proposal.qmd`
- Updated project proposal: `proposal.qmd`
- Final report: `report.qmd`
- Group reflection: `group-reflection.qmd`
- Rendered website output: `docs/`

## Contribution Statement and AI Tools Disclosure

Contribution statements and AI tools disclosure are documented separately in the final contribution materials.

AI tools were used as supportive tools during the project workflow. All AI-supported outputs were reviewed, adapted and verified by group members. The group remains responsible for the final code, analysis, interpretation, writing and submitted materials.

Individual contribution statements are also submitted separately on Moodle according to the course instructions.

## Licence

Dataset licence: [Datenlizenz Deutschland Namensnennung 2.0](https://www.govdata.de/dl-de/by-2-0)
