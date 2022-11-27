# Rent burden explorer

This is a simple shiny app for plotting rental costs (based on census data). It uses tidycensus to download data, then loads it and plots in a shiny app.

## Usage

Run rentburden/rent_burden_app.R to start the shiny app.

This distribution includes 2020 and 2021 census data for California, which is loaded in the shiny app. To make your own dataset, use the included get_acs_data.R script. Note that you will need your own Census API key (available here: <https://api.census.gov/data/key_signup.html>) to run it.

## Requirements

Requirements can be handled with renv using included renv.lock, or installed yourself.

### R packages:

-   shiny
-   tidycensus
-   tidyverse
-   ggplot2
-   sf

## Acknowledgements

The (academic) workers of the world
