# Independent statistical approaches address overcrediting problems in REDD+: Code for analysis

This repository contains the code used for the analyses in *Independent statistical approaches address overcrediting problems in REDD+* (Swinfield et al). The code will generate all of the figures and statistical outputs included in both the main text and the supplementary information.

The analyses detailed in the results can be divided into 4 sections:

1.  Independent methods suggest consistently less avoided deforestation
2.  Less varied avoided deforestation from independent approaches
3.  Self-reported reference areas are not well-matched to projects
4.  Differences in counterfactual deforestation rates explain overcrediting

Analyses 1 and 2 were carried out as one group, and analyses 3 and 4 as another.

### Respository structure

-   Analyses 1-2 are contained within `/R/analysis_1_2.Rmd`

-   Analyses 3-4 are contained within `/R/analysis_3_4.Rmd`

-   Scripts called in the above R notebooks are contained within `/scripts`

-   Data is stored in an external repository (see 'Inputs')

### Inputs

The below inputs are available to download at the following link: https://www.cl.cam.ac.uk/~avsm2/consensus-data-20240618.zip

`analysis_1_2.Rmd` uses the following inputs:

-   `Project_country_method.csv`, containing information about the projects to evaluate, including country, start date and the method used to evaluate them

-   `project_summaries.parquet`, containing a summary of the outputs of the PACT evaluation method (in terms of avoided deforestation) for each project.

-   `AG_2022.csv`and `AG_2024.csv`, data from Guizar-Coutino et al (2022) and (2024)

-   `TW_2020.csv`, `TW_2023.csv`, data from West et al (2020) and (2023).

-   `TW_2024_SC.csv`, synthetic control data from West et al (2024).

-   `TW_2024_1.csv`, `TW_2024_2.csv`, and `TW_2024_3.csv`, data from West et al (2024)

-   `certified.csv`, containing the self-reported avoided deforestation rates.

`analysis_3_4.Rmd` uses the following inputs:

-   `project_info.csv`, a config file containing the VCS IDs and start years for each project analysed

-   `/data/tidy_data`, the repository containing the cleaned outputs of our pipeline for each set of project points and its corresponding counterfactual. Each row of the `.csv` file represents a separate point. The identity of the row ('Project' or 'Counterfactual') is contained within the `type` column.

-   `/data/tidy_data_reference`, as above but also including a set of points extracted from the self-reported reference areas during the same period (labelled 'Reference' in the `type` column).

-   `/data/tidy_data_historical`, containing only the set of points extracted from the self-reported reference area during the historical period.

-   `/data/project_shapefiles`, containing the shapefiles delineating the boundaries of the projects.

-   `/data/reference_shapefiles`, containing the shapefiles delineating the boundaries of the self-reported reference areas. These were digitised by hand or using colour thresholding in R.

-   `/data/CF_VCS_avoided_deforestation`, an output of `analysis_1_2.Rmd` containing additionality information about each project.

-   `/data/historical_reference_periods.csv`, containing the years of the self-reported historical reference periods for each project, as reported in project design documents.

-   `/data/jrc_evaluation_periods.csv`, containing the years of the JRC-ACC evaluation period chosen to match most closely with the self-reported evaluation period.

-   `/data/countries.csv`, containing the countries in which each of the projects (with reference areas) is located

### Usage

This project requires R studio. Each `.Rmd` file is designed to be run as a self-contained entity from start to finish, either by knitting the file or by running each code chunk individually. Note that analysis_3\_4.Rmd deals with larger datasets and so takes significantly longer to run than analysis_1\_2.Rmd.
