# Install and load packages using pacman
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,      # Load tidyverse first as it's the primary package
  rio,            # for importing/exporting data
  here,           # for relative file paths
  skimr,          # for reviewing the data
  janitor,        # for cleaning and tabulating data
  epikit,         # for creating age categories
  kableExtra      # for complex table manipulation
)

# Import datasets with no headers
life_exp <- read_csv(here("data", "life_exp.csv"), col_names = FALSE)
health_xp <- read_csv(here("data", "health_xp.csv"), col_names = FALSE)

# Data cleaning ----

# Clean life expectancy data
life_exp_longer <- life_exp %>%
  # Set column names
  set_names(c("country", as.character(1800:2100))) %>%
  # Remove the first row (which contains "country")
  slice(-1) %>%  # This removes the first row that was incorrectly read as data
  # Pivot longer
  pivot_longer(
    cols = -country,
    names_to = "year",
    values_to = "life_expectancy",
    names_transform = list(year = as.numeric)
  ) %>%
  # Filter years
  filter(between(year, 1995, 2010)) %>%
  # Remove NA values
  drop_na() %>%
  # Arrange data
  arrange(country, year)

# Clean health expenditure data
health_xp_longer <- health_xp %>%
  # Set column names
  set_names(c("country", as.character(1995:2010))) %>%
  # Remove the first row (which contains "country")
  slice(-1) %>%  # This removes the first row that was incorrectly read as data
  # Pivot longer
  pivot_longer(
    cols = -country,
    names_to = "year",
    values_to = "health_expenditure",
    names_transform = list(year = as.numeric)
  ) %>%
  # Remove NA values
  drop_na() %>%
  # Arrange data
  arrange(country, year)

# Join the datasets
combined_data <- life_exp_longer %>%
  left_join(health_xp_longer, by = c("country", "year"))