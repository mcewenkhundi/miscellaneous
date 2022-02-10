#R club book club, https://epirhandbook.com/en/iteration-loops-and-lists.html
#Chapter16, iteration, loops and lists
#Focussed on importing and exporting datas
#Date: 28Jan2022

pacman::p_load(
  rio,            # import/export
  here,           # relative filepaths
  tidyverse,      # data mgmt and viz
  writexl,        # write Excel file with multiple sheets
  readxl,          # import Excel with multiple sheets
  epirhandbook
)

#Get the excel data
#Only download data once
#get_data(file = "hospital_linelists.xlsx")


#Get the excel sheet names
sheet_names <- readxl::excel_sheets("hospital_linelists.xlsx")

#Import the data using map

combined_list <- sheet_names %>% 
  purrr::set_names() %>% 
  map(.f = ~import("hospital_linelists.xlsx", which = .x))

#Straight into a data.frame format

combined_df <- sheet_names %>% 
  purrr::set_names() %>% 
  map_df(.f = ~import("hospital_linelists.xlsx", which = .x), .id = "origin")

#Split the dataset and export
linelist <- readRDS(here::here("linelist_cleaned.rds"))

linelist_split <- linelist %>% 
  group_split(hospital)

#The sheet names
linelist %>% 
  count(hospital)

names_hosp <- linelist_split %>%   # Assign to names of listed data frames 
  # Extract the names by doing the following to each data frame: 
  map(.f = ~pull(.x, hospital)) %>%        # Pull out hospital column
  map(.f = ~as.character(.x)) %>%          # Convert to character, just in case
  map(.f = ~unique(.x))


names(linelist_split) <- names_hosp 
#could also use function set names

#To save the linelist_split to excel with each hosp in its own sheet
linelist_split %>% 
  writexl::write_xlsx(path = here("data", "hospital_linelists.xlsx"))





