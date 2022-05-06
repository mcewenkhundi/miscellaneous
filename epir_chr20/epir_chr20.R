#Rbook club: The Epidemiologist R Handbook
#Chapter20: Missing data
#McEwen Khundi

# Assess missingness
# Filter out rows by missingness
# Plot missingness over time
# Handle how NA is displayed in plots
# Perform missing value imputation: MCAR, MAR, MNAR

pacman::p_load(
  rio,           # import/export
  tidyverse,     # data mgmt and viz
  naniar,        # assess and visualize missingness
  mice           # missing data imputation
)

# import the linelist
linelist <- import("linelist_cleaned.rds")

#Missing data in R

df <- tibble::tribble(
  ~wind, ~temp, ~district,
  -99,    45,   "Lilongwe",
  68,    NA,    "Blantyre", 
  72,    25,     NA,
  33,    NA,     "Mwanza",
  -99,  -99,     NA
)

#Useful function when working with missing values
df %>%
  filter(temp == NA)

df %>%
  filter(is.na(temp))

#Remove all missing temp observations
df %>%
  filter(!is.na(temp))

#Remove all rows with missing values from a dataset
df %>%
  drop_na()

#Can specify columns
df %>%
  drop_na(district)

#if you know the value of missing value
#can replace it
df %>%
  mutate(district = replace_na(district, "Mchinji"))

df %>% replace_na(list(temp = 0, district = "Mchinji"))

#Change value to missing
df %>%
  mutate(wind = na_if(wind, -99))

#Change to missing (NA) all -99
df %>%
  mutate(across(where(is.numeric),~na_if(., -99)))


#Missing values in mathematical functions
mean(df$temp)
mean(df$temp, na.rm = T)

##Using functin from the naniar r package
# percent of ALL data frame values that are missing
pct_miss(df)
pct_miss(linelist)

# Percent of rows with any value missing
pct_miss_case(linelist)   # use n_complete() for counts


# Percent of rows that are complete (no values missing)  
pct_complete_case(linelist) # use n_complete() for counts

#The gg_miss_var() function will show you the number (or %) of missing values in each column. A few nuances:
gg_miss_var(linelist, show_pct = TRUE)

#Here the data are piped %>% into the function. The facet = argument 
#is also used to split the data.
linelist %>% 
  gg_miss_var(show_pct = TRUE, facet = outcome)

# Heatplot of missingness across the entire data frame  
vis_miss(linelist)

#Explore and visualize missingness relationships
#geom miss_point

ggplot(
  data = linelist,
  mapping = aes(x = age_years, y = temp)) +     
  geom_miss_point()


#Shadow columns
# creates a binary NA/not NA column for every existing column, and binds all 
# these new columns to the original dataset with the appendix "_NA". This doubles the number o

shadowed_linelist <- linelist %>% 
  bind_shadow()

View(shadowed_linelist)

df %>% 
  bind_shadow

ggplot(data = shadowed_linelist,          # data frame with shadow columns
       mapping = aes(x = date_hospitalisation)) + # shadow column of interest
  geom_density()  

ggplot(data = shadowed_linelist,          # data frame with shadow columns
       mapping = aes(x = date_hospitalisation, # numeric or date column
                     colour = age_years_NA)) + # shadow column of interest
  geom_density()   

#
  linelist %>%
  bind_shadow() %>%                # create the shows cols
  group_by(date_outcome_NA) %>%    # shadow col for stratifying
  summarise(across(
    .cols = age_years,             # variable of interest for calculations
    .fns = list("mean" = mean,     # stats to calculate
                "sd" = sd,
                "var" = var,
                "min" = min,
                "max" = max),  
    na.rm = TRUE))  
  
  #Imputation
  #Assumptions for missing data
  #Missing completely at Random (MCAR)
   #Can ignore the missing observations
  #Missing at Random (MAR)
   #Use imputation
  #Missing not at random
   #get additional information and impute
  
#For simple analysis
  #Can use mean imputation
  #Can bias the data since it does not take into consideration 
  #the information that is available in the other variables

  linelist <- linelist %>%
    mutate(temp_replace_na_with_mean = replace_na(temp, mean(temp, na.rm = T)))
  
  #For our dataset, imagine you knew that all observations with a missing 
  #value for their outcome (which can be “Death” or “Recover”) were actually 
  #people that died (note: this is not actually true for this dataset):
  
  linelist <- linelist %>%
    mutate(outcome_replace_na_with_death = replace_na(outcome, "Death"))
  
  
#Regression imputation
  simple_temperature_model_fit <- lm(temp ~ fever + age_years, data = linelist)
  
  #using our simple temperature model to predict values just for the observations where temp is missing
  predictions_for_missing_temps <- predict(simple_temperature_model_fit,
                                           newdata = linelist %>% filter(is.na(temp))) 
  
 #Same approach through the mice package
 model_dataset <- linelist %>%
    select(temp, fever, age_years)  
  
  temp_imputed <- mice(model_dataset,
                       method = "norm.predict",
                       seed = 1,
                       m = 1,
                       print = F)
temp_imputed_values <- temp_imputed$imp$temp 

#Multiple imputation
# imputing missing values for all variables in our model_dataset, and creating 10 new imputed datasets
multiple_imputation = mice(
  model_dataset,
  seed = 1,
  m = 10,
  print = FALSE) 


model_fit <- with(multiple_imputation, lm(temp ~ age_years + fever))

base::summary(mice::pool(model_fit))
  