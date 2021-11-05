#Project: R for epidemiologists book review
#Task: Chapter 13 grouping data
#Author: McEwen Khundi

#Loading packages
pacman::p_load(
  rio,       # to import data
  here,      # to locate files
  tidyverse, # to clean, handle, and plot the data (includes dplyr)
  janitor)   # adding total rows and columns

#Import data
linelist <- import("linelist_cleaned.rds")

#Grouping by function
#takes an existing tibble/data.frame and converts it into a grouped tbl/data.frame
#where operations are performed by group

#group by outcome
ll_by_outcome <- linelist %>% 
  group_by(outcome)

#a grouped tibble has groups information it's print info
ll_by_outcome

#can also formarly test if tibble/data.frame is grouped
is_grouped_df(ll_by_outcome)

#Using tally with grouped data
linelist %>% 
  tally()

linelist %>% 
  group_by(outcome) %>% 
  tally()

##Can count in more than one group
linelist %>% 
  group_by(outcome, gender) %>% 
  tally()

#Summary functions in general 

#On the whole dataset
linelist %>% 
  summarise(
    n_cases  = n(),
    mean_age = mean(age_years, na.rm=T),
    max_age  = max(age_years, na.rm=T),
    min_age  = min(age_years, na.rm=T),
    n_males  = sum(gender == "m", na.rm=T))

linelist %>% 
  group_by(outcome) %>% 
  summarise(
    n_cases  = n(),
    mean_age = mean(age_years, na.rm=T),
    max_age  = max(age_years, na.rm=T),
    min_age  = min(age_years, na.rm=T),
    n_males    = sum(gender == "m", na.rm=T))

#Function tally is a shortcut

linelist %>%
  summarise(n = n())

linelist %>%
  tally()

linelist %>%
  group_by(outcome) %>%
  tally()

#Also check function count()
#does groupby and counts
linelist %>%
  count(outcome)

##Version for mutate, to keep variable in dataset
linelist %>% 
  as_tibble() %>%                   # convert to tibble for nicer printing 
  add_count(hospital) %>%           # add column n with counts by hospital
  select(hospital, n, everything()) 

#Grouped summaries work with dates
daily_counts <- linelist %>% 
  drop_na(date_onset) %>%        # remove that were missing date_onset
  count(date_onset)  

daily_counts

#Mutate on grouped data
linelist %>% 
  # group data by hospital (no change to linelist yet)
  group_by(hospital) %>% 
  
  # new columns
  mutate(
    # mean days to admission per hospital (rounded to 1 decimal)
    group_delay_admit = round(mean(days_onset_hosp, na.rm=T), 1),
    
    # difference between row's delay and mean delay at their hospital (rounded to 1 decimal)
    diff_to_group     = round(days_onset_hosp - group_delay_admit, 1)) %>%
  
  # select certain rows only - for demonstration/viewing purposes
  select(case_id, hospital, days_onset_hosp, group_delay_admit, diff_to_group)

#Shortcuts in repeating operations using accross function

linelist %>%
  group_by(outcome) %>%
  summarise(across(where(is.numeric), mean))

#if you want several summary functions pass them in a list
#Notice the missing values
linelist %>%
  group_by(outcome) %>%
  summarise(across(where(is.numeric), list(mean=mean, sd=sd,min=min,max=max))) %>%
  View()

linelist %>%
  group_by(outcome) %>%
  summarise(across(where(is.numeric), list(mean=~mean(.x,na.rm =TRUE),
                                           sd=~sd(.x,na.rm =TRUE),
                                           min=~min(.x,na.rm =TRUE),
                                           max=~max(.x,na.rm =TRUE)))) %>%
  View()

#Across also works on mutate
#Change the factor levels of all variables with yes/no response
glimpse(linelist)

linelist %>%
  mutate(across(.cols = c(fever, chills, cough, aches, vomit),
                .fns = ~fct_recode(.x,  Yes =  "yes", No = "no"))) %>%
select(fever, chills, cough, aches, vomit)  

##Extra based on latest updates to dplyr
#What if you want to summarise rowwise
#rowise grouping
df <- tibble(x = 1:2, y = 3:4, z = 5:6)
df

#it gives a summary of all the values, not something we want
df %>% mutate(m = mean(c(x, y, z)))

#check
mean(c(1,3,5,2,4,6))

df %>% rowwise() %>% mutate(m = mean(c(x, y, z)))

#shorcut for multiple operations
df %>% rowwise() %>% mutate(m = mean(c_across(x:z)))

df %>% rowwise() %>% mutate(total = sum(c_across(x:z)))

rf <- df %>%
      rowwise()
#compine the colwise operations and the rowwise operations
rf %>% 
  mutate(total = sum(c_across(x:z))) %>% 
  ungroup() %>% 
  mutate(across(x:z, ~ . / total))

#Using other function on a rowise grouped dataset
###Exact bimial test
df_tb <- tribble(
  ~colA, ~colB,
  2,   15,
  3,   27,
  3,   36
)

df_tb <- df_tb %>%
  rowwise() %>%
  mutate(bino_exact = list(binom.test(colA,colB)),
         bino_exact_rs = list(broom::tidy(bino_exact)))

df_tb 

#References
#Check the articles on 
#https://dplyr.tidyverse.org/index.html
#Chapter 5 R for data science 









