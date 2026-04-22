# Anatomy Education: Dissection vs Simulation Review

Welcome to the repository for our methodological and bibliometric analysis comparing traditional anatomical dissection and modern simulation techniques in medical education.

## Overview

This repository contains the supplementary materials and the complete reproducible R code used to perform the data analysis. The analysis evaluates scientific literature, categorizing studies based on modality (dissection, simulation, hybrid), experimental design, and reported outcomes (subjective, objective, and clinical).

## Repository Structure

- `src/code.R`: The main R script containing all data processing steps, including:
  - Lexical rule application and text classification to categorize abstracts.
  - Descriptive statistics and distributions of the sample.
  - Generation of global and modality-specific visualizations.
  - Inferential statistics (Chi-square tests and proportion analyses).
  - Logistic regression models to predict the reporting of objective and clinical outcomes.
  - Bibliometric modeling to assess citation visibility.

- `Supplementary Material #1.docx`: Supplementary document detailing additional methodology or extended results.
- `Supplementary Material #2.csv`: The main dataset retrieved from Scopus.
- `Supplementary Material #3.csv`: Results from sensitivity analyses (Original Articles Only logistic regressions).

## How to use

1. Clone this repository to your local machine.
2. Open the `src/code.R` script.
3. Update the `DATA_PATH` variable under the "Configuration & File Paths" section to point to your local copy of `Supplementary Material #2.csv`.
4. Run the code in RStudio (requires `tidyverse`, `scales`, `patchwork`, and `broom` packages).

## Contact

For any questions or collaborations, please reach out to the authors of the study.
