# Understanding the reasons for over-crediting in REDD+

This repository contains the code used for the analyses in *Understanding the reasons for over-crediting in REDD+* (Swinfield et al). The code will generate all of the figures and statistical outputs included in both the main text and the supplementary information.

This code is structured as separate R scripts relating to each figure or supplement. For scripts fig3, and fig4, compound rates extracted from the PACT parquet files will be written as .csvs to be read by fig.5. 

### Inputs

The data can be downloaded at the following DOI: (10.5281/zenodo.15745712)[https://zenodo.org/records/15745712]


Different data types are organised into folders relating to each file format. These folders should be placed in the same directory as the scripts.

#### CSVs

Used in fig2, s3, and s8:

- ag_2022_avoided_amounts
- ag_2024_avoided_amounts
- tw_2020_avoided_amounts 
- tw_2023_avoided_amounts
- tw_2024_avoided_amounts
- pact_2025_avoided_amounts
- certified_avoided_amounts
- project_metadata

Used in fig3, s4: 
- evaluation_end_years
- certified_project_amounts

Used in fig 4, s7:
- evaluation_end_years

Used in fig5:
- acc_pact_control_rates
- acc_pact_project_rates
- certified_control_rates
- certified_project_rates
- acc_certified_control_rates

#### Parquets

Used in fig3, s4: 
- acc_project_area_parquets (folder of parquets)

Used in fig4, s7: 
- acc_certified_control_parquets (folder of parquets)
- acc_pact_matching_parquets (folder of parquets)


#### Geojsons

Used in fig3, s4: 
- project_area_geojsons (folder of geojsons)

Used in fig4:
- certified_control_area_geojsons (folder of geojsons)
- project_area_geojsons (folder of geojsons)


### Usage

These scripts require R and several dependent libraries specified at the start of each script. It is important to note that fig4.R and s7.R may take a while to run due to calculating compound deforestation rates across multiple parquets.
