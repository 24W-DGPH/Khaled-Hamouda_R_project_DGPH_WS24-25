# Install required packages if not already installed
install.packages(c("tidyverse",
                   "shiny", 
                   "shinydashboard", 
                   "lubridate",
                   "janitor",
                   "here",
                   "DT"))

# Load the libraries
library(tidyverse)
library(shiny)
library(shinydashboard)
library(lubridate)
library(janitor)
library(here)
library(readr)

# Import dataset ----
healthcare_dataset_raw <- read_csv("healthcare_dataset_raw.csv")

# data cleaning ----

# check for missing data
sum(is.na(linelist_reordered_final))  # total number of NAs in dataset
colSums(is.na(linelist_reordered_final))  # number of NAs in each column

# standardize column name syntax  
linelist <- healthcare_dataset_raw %>% 
  janitor::clean_names()

# removing duplicate rows
linelist <- linelist %>% 
  distinct()

# printing columns names for columns reorder
names(linelist)

# reordered linelist
linelist_reordered <- linelist %>% 
  select(name, gender, age, blood_type, medical_condition,
         date_of_admission, discharge_date, doctor, hospital, 
         insurance_provider, billing_amount, room_number,
         admission_type, medication, test_results)
  
# adding a new column of hospitalization duration
linelist_reordered_final <- linelist_reordered %>% 
  mutate(hopitalization_duration = discharge_date - date_of_admission,
         .after = discharge_date)

# create cleaned 
healthcare_dataset_cleaned <- linelist_reordered_final
  healthcare_dataset_cleaned

# export cleaned dataset
write_csv(healthcare_dataset_cleaned, "healthcare_dataset_cleaned.csv")

