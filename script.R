# install and load packages
pacman::p_load(
  rio,            # for importing/exporting data
  here,           # for relative file paths
  skimr,          # for reviewing the data
  janitor,        # for cleaning and tabulating data
  epikit,         # for creating age categories
  kableExtra,     # Build and manipulate complex tables
  tidyverse)      # for data management and visualization


# Import dataset ----
life_exp <- import(here("data", "life_exp.csv"))
health_xp <- import(here("data", "health_xp.csv"))

# data cleaning ----
# life_exp
# add proper column names
life_exp_columns <- c("country", as.character(1800:2100))

# Pivoting data
life_exp_longer <- life_exp %>%
  # Add column names
  setNames(column_names) %>%
  # Pivot longer all columns except country
  pivot_longer(
    cols = -country,
    names_to = "year",
    values_to = "value",
    names_transform = list(year = as.numeric)
  ) %>%
  # Filter for years between 1995 and 2010
  filter(year >= 1995 & year <= 2010) %>%
  # Remove any rows with NA values
  drop_na() %>%
  # Arrange by country and year
  arrange(country, year)

# health_xp
# add column names 
health_xp_columns <- c("country", as.character(1995:2010))

# Pivoting data
health_xp_longer <- health_xp %>%
  # Add column names
  setNames(health_xp_columns) %>%
  # Pivot longer all columns except country
  pivot_longer(
    cols = -country,
    names_to = "year",
    values_to = "value",
    names_transform = list(year = as.numeric)
  ) %>%
  # Remove any rows with NA values
  drop_na() %>%
  # Arrange by country and year
  arrange(country, year)



# export cleaned dataset
#write_csv(linelist_cleaned, here("data", "linelist_cleaned.csv"))

