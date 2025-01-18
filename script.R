# install and load packages
pacman::p_load(
  rio,            # for importing/exporting data
  here,           # for relative file paths
  skimr,          # for reviewing the data
  janitor,        # for cleaning and tabulating data
  epikit,         # for creating age categories
  tidyverse)      # for data management and visualization


# Import dataset ----
linelist_raw <- import(here("data", "healthcare_dataset.csv"))

# data cleaning ----

linelist_cleaned <- linelist_raw %>%    # create "clean" dataset, starting with the raw
  clean_names() %>%                                        # clean the column names
  distinct() %>%                                           # de-duplicate rows
  select(
    name,      gender,   age,   blood_type, 
         medical_condition,   date_of_admission,     
            discharge_date,    doctor, hospital, 
        insurance_provider,      billing_amount,    
               room_number,      admission_type,        
                medication,        test_results
    ) %>%                                                  #re-order columns
  mutate(
    discharge_date = as.Date(discharge_date),
    date_of_admission = as.Date(date_of_admission),
    hospitalization_duration = as.numeric(discharge_date - date_of_admission)
    , .after = discharge_date              
  )                                                        #add column






# export cleaned dataset
write_csv(linelist_cleaned, here("data", "linelist_cleaned.csv"))

