#R club book club, https://epirhandbook.com/en/iteration-loops-and-lists.html
#Chapter16, iteration, loops and lists
#Date: 28Jan2022

#Iteration/loops
#Repeating analysis on subgroups such as countries, agegroups, clusters
#Two options
#The for loop, base R approach
#mapping functions from purrr

#Demo
#Creating epicurves
#Running T-tests

pacman::p_load(
  rio,         # import/export
  here,        # file locator
  purrr,       # iteration
  tidyverse    # data management and visualization
)

linelist <- import(here("linelist_cleaned.rds"))

#Simplified for loop
#Parts of a loop
#Sequence, operation and container


for( i in 1:10){
  
  print(paste("index number:", i, "value of compuation:", i*2))
}

map(1:10, ~paste("index number:", .x, "value of compuation:", .x*2))

#Sequence can also be a character
hospital_names <- unique(linelist$hospital)

for(hosp in hospital_names){
  
  print(paste("hospital name:", hosp))

  }

#Find the number of missing values in each column
names(linelist)

for (col in names(linelist)){        # loop runs for each column in linelist; column name represented by "col" 
  
  # Example operations code - print number of missing values in column
  print(sum(is.na(linelist[[col]])))  # linelist is indexed by current value of "col"
  
}

#alternative way of doing above operation in tidyverse

linelist %>%
  summarise(across(.cols = everything(), .fns = ~sum(is.na(.x))))

#Find the class of each variable

linelist %>%
  summarise(across(.cols = everything(), .fns = ~class(.x)))

#Only numeric variables, caculate the mean and min and max
linelist %>%
  summarise(across(.cols = where(is.numeric), 
                   .fns = list(mean=~mean(.x, na.rm = T), min=~min(.x, na.rm = T), max=~max(.x, na.rm = T))))

##Containers in for loops
# create container to store results - a character vector
cases_demographics <- vector(mode = "character", length = nrow(linelist))

linelist %>%
  select(gender, age_years) %>%
  head()

# the for loop
for (i in 1:nrow(linelist)){
  
  # OPERATIONS
  # extract values from linelist for row i, using brackets for indexing
  row_gender  <- linelist$gender[[i]]
  row_age     <- linelist$age_years[[i]]    # don't forget to index!
  
  # combine gender-age and store in container vector at indexed location
  cases_demographics[[i]] <- str_c(row_gender, row_age, sep = ",") 
  
} 

bind_cols(linelist, cases_demographics) %>%
  dplyr::select(age_years, gender, cases_demographics)


#But the above operation is unnecessary because str_c, is vectorised by default
#that's a term used for functions that operate row-wise without need for a loop
#what are some examples
#rowSums()
#rowMeans()

linelist %>%
  mutate(gender_age = str_c(gender, age_years, sep = ","),
         .keep = "used" )  %>%#Only keep vars used in operation
  head()
  
##Number of observations/cases in each hospital

for (hosp in hospital_names){ 
  hospital_cases <- linelist %>% filter(hospital == hosp)
  
  print(paste("Number of cases, obs in clinic", hosp,nrow(hospital_cases)))
}

##Using tidyvers approach
linelist %>%
  count(hospital)


##Looping through plots
#Create an epi curve for each hospital


#We plot for the overall data
#What do epi curves do?
outbreak <- incidence2::incidence(   
  x = linelist,                   # dataframe - complete linelist
  date_index = date_onset,        # date column
  interval = "week",              # aggregate counts weekly
  groups = gender,                # group values by gender
  na_as_group = TRUE)             # missing gender is own group

# plot epi curve
plot(outbreak,                       # name of incidence object
     fill = "gender",                # color bars by gender
     color = "black",                # outline color of bars
     title = "Outbreak of ALL cases" # title
)

# make vector of the hospital names
hospital_names <- unique(linelist$hospital)

##lets add a way to store the plots?

# for each name ("hosp") in hospital_names, create and print the epi curve
for (hosp in hospital_names) {
  
  # create incidence object specific to the current hospital
  outbreak_hosp <- incidence2::incidence(
    x = linelist %>% filter(hospital == hosp),   # linelist is filtered to the current hospital
    date_index = date_onset,
    interval = "week", 
    groups = gender,
    na_as_group = TRUE
  )
  
  # Create and save the plot. Title automatically adjusts to the current hospital
  plot_hosp <- plot(
    outbreak_hosp,
    fill = "gender",
    color = "black",
    title = stringr::str_glue("Epidemic of cases admitted to {hosp}")
  )
  
  # print the plot for the current hospital
  print(plot_hosp)
  
} 


# Lets clean the above code by creating functions

epicurveDataFormat <- function(hospital_name){

incidence2::incidence(
  x = linelist %>% filter(hospital == hospital_name),   # linelist is filtered to the current hospital
  date_index = date_onset,
  interval = "week", 
  groups = gender,
  na_as_group = TRUE
)

}

epicurvePlot <- function(epidataHospital){
plot_hosp <- plot(
  epidataHospital,
  fill = "gender",
  color = "black",
  title = stringr::str_glue("Epidemic of cases admitted to {hosp}")
)
}


for (hosp in hospital_names) {
  outbreak_hosp <- epicurveDataFormat(hospital_name = hosp)
  
  plot_hosp <- epicurvePlot(epidataHospital = outbreak_hosp)

  # print the plot for the current hospital
  print(plot_hosp)
  
} 


##Using the purrr map functions
#Syntax map(.x = SEQUENCE, .f = FUNCTION, OTHER ARGUMENTS)

epidata_list_all_hospitals <- map(hospital_names, ~epicurveDataFormat(hospital_name = .x))


map(epidata_list_all_hospitals, ~epicurvePlot(epidataHospital = .x))

##We could chain those operations
map(hospital_names, ~epicurveDataFormat(hospital_name = .x)) %>%
  map(., ~epicurvePlot(epidataHospital = .x))

#Save above and you patchwork package to combine plots
#patchwork

##T test

# Results are saved as a list

t.test(linelist$age~linelist$gender)

broom::tidy(t.test(linelist$age~linelist$gender))
######

t.test_results <- linelist %>% 
  select(age, wt_kg, ht_cm, ct_blood, temp) %>%  # keep only some numeric columns to map across
  map(.f = ~t.test(.x ~ linelist$gender))  


t.test_results_tidy <- t.test_results %>% {
  tibble(
    variables = names(.),
    tidy_results = map(., ~broom::tidy(.x)))
} 


t.test_results_tidy_wider <- t.test_results_tidy %>%
  unnest_wider(tidy_results)

#References
#https://epirhandbook.com/en/iteration-loops-and-lists.html
#https://r4ds.had.co.nz/iteration.html
#

