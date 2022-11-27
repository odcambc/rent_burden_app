library(tidycensus)
library(tidyverse)

# Sign up for a census API key here: http://api.census.gov/data/key_signup.html
# Don't include it in repos.

census_api_key("You need your own census API key")

acs_5_2020_all_data <- get_acs(
  state = "CA",
  geography = "tract",
  variables = c("B25064_001", "B25031_002", "B25031_003", "B25031_004",
                "B25031_005", "B25031_006", "B25058_001"),
  geometry = TRUE,
  survey = "acs5",
  year = 2020
)

st_write(acs_5_2020_all_data, "acs5.shp")

acs_1_2021_all_puma_data <- get_acs(
  state = "CA",
  geography = "public use microdata area",
  variables = c("B25064_001", "B25031_002", "B25031_003", "B25031_004",
                "B25031_005", "B25031_006", "B25058_001"),
  geometry = TRUE,
  survey = "acs1",
  year = 2021
)

st_write(acs_1_2021_all_puma_data, "acs1_puma.shp")
