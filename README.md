# Data Management and Analysis Workshop
Patrick Lavallee Delgado \
University of Pennsylvania \
January 2025

This repository contains the Data Management and Analysis Workshop, given to the Education Policy Research Practicum at the University of Pennsylvania Graduate School of Education. The workshop slides introduce principles of data privacy, repository management, code documentation, and quality assurance. The Quarto notebooks walk through common data tasks for an example project.

## Contents
- `workshop.pdf` slides from workshop
- `src/`
  - `01-clean-data.qmd` example dataset assembly notebook
  - `02-run-analysis.qmd` example data analysis notebook
  - `03-draw-plots.qmd` example data visualization notebook
  - Rendered PDF versions of notebooks

## Requirements
- R version 4.2.3
- Jupyter version 1.0.0
- Quarto version 1.4
- tidyverse version 2.0.0 or later
- openxlsx version 4.2.5 or later

## How to run
1. Download CCD school district data for SY 2021-22 [here](https://nces.ed.gov/ccd/files.asp):
    - Fiscal file: `sdf21_1a.txt`
    - Membership file: `ccd_lea_052_2021_l_1a_080621.csv`
2. Save data in `in/`.
3. Satisfy software requirements above.
4. Open R.
5. Set the working directory to this repository.
6. Run each Quarto notebook in `src/` sequentially.
7. Find tables and figures in `out/`.
