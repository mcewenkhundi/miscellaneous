#Project: Epir book club
#Chapter 26: Survey analysis
#Author: McEwen Khundi

## load packages from CRAN
pacman::p_load(rio,          # File import
               here,         # File locator
               tidyverse,    # data management + ggplot2 graphics
               tsibble,      # handle time series datasets
               survey,       # for survey functions
               srvyr,        # dplyr wrapper for survey package
               gtsummary,    # wrapper for survey package to produce tables
               apyramid,     # a package dedicated to creating age pyramids
               patchwork,    # for combining ggplots
               ggforce       # for alluvial/sankey plots
) 

## load packages from github
pacman::p_load_gh(
  "R4EPI/sitrep"          # for observation time / weighting functions
)

#The dataset is imported using the import() function from the rio package. See 
#the page on Import and export for various ways to import data.

# import the survey data
survey_data <- rio::import("survey_data.xlsx")

# import the dictionary into R
survey_dict <- rio::import("survey_dict.xlsx") 

#Sampling population weights
# import the population data
population <- rio::import("population.xlsx")

## define the number of households in each cluster
cluster_counts <- tibble(cluster = c("village_1", "village_2", "village_3", "village_4", 
                                     "village_5", "village_6", "village_7", "village_8",
                                     "village_9", "village_10"), 
                         households = c(700, 400, 600, 500, 300, 
                                        800, 700, 400, 500, 500))

#Cleaning and formating data
## select the date variable names from the dictionary 
DATEVARS <- survey_dict %>% 
  filter(type == "date") %>% 
  filter(name %in% names(survey_data)) %>% 
  ## filter to match the column names of your data
  pull(name) # select date vars

## change to dates 
survey_data <- survey_data %>%
  mutate(across(all_of(DATEVARS), as.Date))


## add those with only age in months to the year variable (divide by twelve)
survey_data <- survey_data %>% 
  mutate(age_years = if_else(is.na(age_years), 
                             age_months / 12, 
                             age_years))

## define age group variable
survey_data <- survey_data %>% 
  mutate(age_group = age_categories(age_years, 
                                    breakers = c(0, 3, 15, 30, 45)
  ))


## create a character variable based off groups of a different variable 
survey_data <- survey_data %>% 
  mutate(health_district = case_when(
    cluster_number %in% c(1:5) ~ "district_a", 
    TRUE ~ "district_b"
  ))


## select the yes/no variable names from the dictionary 
YNVARS <- survey_dict %>% 
  filter(type == "yn") %>% 
  filter(name %in% names(survey_data)) %>% 
  ## filter to match the column names of your data
  pull(name) # select yn vars

## change to dates 
survey_data <- survey_data %>%
  mutate(across(all_of(YNVARS), 
                str_detect, 
                pattern = "yes"))

#26.3 Survey data

## join the individual and household data to form a complete data set
survey_data <- left_join(survey_data_hh, 
                         survey_data_indiv,
                         by = c("_index" = "_parent_index"))


## create a unique identifier by combining indeces of the two levels 
survey_data <- survey_data %>% 
  mutate(uid = str_glue("{index}_{index_y}"))

#26.4 Observation time
## set the start/end of recall period
## can be changed to date variables from dataset 
## (e.g. arrival date & date questionnaire)
survey_data <- survey_data %>% 
  mutate(recall_start = as.Date("2018-01-01"), 
         recall_end   = as.Date("2018-05-01")
  )

survey_data %>%
  select(contains("date"),contains("recall"),contains("start")) %>%
  View()


# set inappropriate dates to NA based on rules 
## e.g. arrivals before start, departures departures after end
survey_data <- survey_data %>%
  mutate(
    arrived_date = if_else(arrived_date < recall_start, 
                           as.Date(NA),
                           arrived_date),
    birthday_date = if_else(birthday_date < recall_start,
                            as.Date(NA),
                            birthday_date),
    left_date = if_else(left_date > recall_end,
                        as.Date(NA),
                        left_date),
    death_date = if_else(death_date > recall_end,
                         as.Date(NA),
                         death_date)
  )

# We can then use our date variables to define start and end dates for each individual. 
#We can use the find_start_date() function from sitrep to fine the causes for the 
#dates and then use that to calculate the difference between days (person-time).
#start date: Earliest appropriate arrival event within your recall period Either 
#the beginning of your recall period (which you define in advance), or a date 
#after the start of recall if applicable (e.g. arrivals or births)
#end date: Earliest appropriate departure event within your recall period Either 
#the end of your recall period, or a date before the end of recall if applicable 

#(e.g. departures, deaths)
survey_data %>%
  select(contains("date"),contains("recall"),contains("start")) %>%
  View()

## create new variables for start and end dates/causes
survey_data <- survey_data %>% 
  ## choose earliest date entered in survey
  ## from births, household arrivals, and camp arrivals 
  find_start_date("birthday_date",
                  "arrived_date",
                  period_start = "recall_start",
                  period_end   = "recall_end",
                  datecol      = "startdate",
                  datereason   = "startcause" 
  ) %>%
  ## choose earliest date entered in survey
  ## from camp departures, death and end of the study
  find_end_date("left_date",
                "death_date",
                period_start = "recall_start",
                period_end   = "recall_end",
                datecol      = "enddate",
                datereason   = "endcause" 
  )


## label those that were present at the start/end (except births/deaths)
survey_data <- survey_data %>% 
  mutate(
    ## fill in start date to be the beginning of recall period (for those empty) 
    startdate = if_else(is.na(startdate), recall_start, startdate), 
    ## set the start cause to present at start if equal to recall period 
    ## unless it is equal to the birth date 
    startcause = if_else(startdate == recall_start & startcause != "birthday_date",
                         "Present at start", startcause), 
    ## fill in end date to be end of recall period (for those empty) 
    enddate = if_else(is.na(enddate), recall_end, enddate), 
    ## set the end cause to present at end if equall to recall end 
    ## unless it is equal to the death date
    endcause = if_else(enddate == recall_end & endcause != "death_date", 
                       "Present at end", endcause))


## Define observation time in days
survey_data <- survey_data %>% 
  mutate(obstime = as.numeric(enddate - startdate))

survey_data %>%
  select(contains("date"),contains("recall"),
         contains("start"),contains("end"), obstime) %>%
  View()

#26.5 Weighting
## store the cases that you drop so you can describe them (e.g. non-consenting 
## or wrong village/cluster)
dropped <- survey_data %>% 
  filter(!consent | is.na(startdate) | is.na(enddate) | village_name == "other")

## use the dropped cases to remove the unused rows from the survey data set  
survey_data <- anti_join(survey_data, dropped, by = names(dropped))

# stratified ------------------------------------------------------------------
# create a variable called "surv_weight_strata"
# contains weights for each individual - by age group, sex and health district
survey_data <- add_weights_strata(x = survey_data,
                                  p = population,
                                  surv_weight = "surv_weight_strata",
                                  surv_weight_ID = "surv_weight_ID_strata",
                                  age_group, sex, health_district)

## cluster ---------------------------------------------------------------------

# get the number of people of individuals interviewed per household
# adds a variable with counts of the household (parent) index variable
survey_data <- survey_data %>%
  add_count(index, name = "interviewed")


## create cluster weights
survey_data <- add_weights_cluster(x = survey_data,
                                   cl = cluster_counts,
                                   eligible = member_number,
                                   interviewed = interviewed,
                                   cluster_x = village_name,
                                   cluster_cl = cluster,
                                   household_x = index,
                                   household_cl = households,
                                   surv_weight = "surv_weight_cluster",
                                   surv_weight_ID = "surv_weight_ID_cluster",
                                   ignore_cluster = FALSE,
                                   ignore_household = FALSE)


# stratified and cluster ------------------------------------------------------
# create a survey weight for cluster and strata
survey_data <- survey_data %>%
  mutate(surv_weight_cluster_strata = surv_weight_strata * surv_weight_cluster)



base_survey_design_cluster <- svydesign(ids = ~village_name, # cluster ids
                                        weights = ~surv_weight_cluster, # weight variable created above
                                        strata = NULL,                 # sampling was simple (no strata)
                                        data = survey_data              # have to specify the dataset
)
