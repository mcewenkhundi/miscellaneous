library(tidyverse)

who %>%
  View()

who1 <- who %>% 
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = TRUE
  )
who1 %>% 
  View()

# We can get some hint of the structure of the 
# values in the new key column by counting them:
  
  who1 %>% 
  count(key)
  
# You might be able to parse this out by yourself with a little thought and some
# experimentation, but luckily we have the data dictionary handy. It tells us:
#   
# The first three letters of each column denote whether the column contains new
# or old cases of TB. In this dataset, each column contains new cases.
#   
# The next two letters describe the type of TB:
#     
# rel stands for cases of relapse ep stands for cases of extrapulmonary TB sn 
# stands for cases of pulmonary TB that could not be diagnosed by a pulmonary 
# smear (smear negative)
# sp stands for cases of pulmonary TB that could be diagnosed by a pulmonary 
# smear (smear positive)
# The sixth letter gives the sex of TB patients. The dataset groups cases 
# by males (m) and females (f).
#   
# The remaining numbers gives the age group. The dataset groups cases into 
# seven age groups:  
# The remaining numbers gives the age group. The dataset groups cases into
# seven age groups:
#   
#   014 = 0 – 14 years old
# 1524 = 15 – 24 years old
# 2534 = 25 – 34 years old
# 3544 = 35 – 44 years old
# 4554 = 45 – 54 years old
# 5564 = 55 – 64 years old
# 65 = 65 or older

#make the change below for concistency 

  who2 <- who1 %>% 
    mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
  who2

  
# We can separate the values in each code with two passes of separate(). 
# The first pass will split the codes at each underscore.
  
who3 <- who2 %>% 
    separate(key, c("new", "type", "sexage"), sep = "_")
  who3

# Then we might as well drop the new column because it’s constant in this dataset.
# While we’re dropping columns, let’s also drop iso2 and iso3 since they’re 
# redundant.  

  who3 %>% 
    count(new)

  who4 <- who3 %>% 
    select(-new, -iso2, -iso3)

# Next we’ll separate sexage into sex and age by splitting after the first 
# character:
    
    who5 <- who4 %>% 
    separate(sexage, into = c("sex", "age"), sep = 1)
  who5

# The who dataset is now tidy!
# 
# I’ve shown you the code a piece at a time, assigning each interim result to a 
# new variable. This typically isn’t how you’d work interactively. Instead, 
# you’d gradually build up a complex pipe:
    
    who %>%
    pivot_longer(
      cols = new_sp_m014:newrel_f65, 
      names_to = "key", 
      values_to = "cases", 
      values_drop_na = TRUE
    ) %>% 
    mutate(
      key = stringr::str_replace(key, "newrel", "new_rel")
    ) %>%
    separate(key, c("new", "var", "sexage")) %>% 
    select(-new, -iso2, -iso3) %>% 
    separate(sexage, c("sex", "age"), sep = 1)  
    
#Compute the rate for table2, and table4a + table4b. You will 
# need to perform four operations:
    
# Extract the number of TB cases per country per year.
# Extract the matching population per country per year.
# Divide cases by population, and multiply by 10000.
# Store back in the appropriate place.
# Which representation is easiest to work with? Which is hardest? Why?

    t2_cases <- filter(table2, type == "cases") %>%
      rename(cases = count) %>%
      arrange(country, year)
    
    t2_population <- filter(table2, type == "population") %>%
      rename(population = count) %>%
      arrange(country, year)
    
#Then create a new data frame with the population and cases columns, and 
# calculate the cases per capita in a new column.
    
    
    t2_cases_per_cap <- tibble(
      year = t2_cases$year,
      country = t2_cases$country,
      cases = t2_cases$cases,
      population = t2_population$population
    ) %>%
      mutate(cases_per_cap = (cases / population) * 10000) %>%
      select(country, year, cases_per_cap)

# To store this new variable in the appropriate location, we will add new rows 
# to table2.
    
    t2_cases_per_cap <- t2_cases_per_cap %>%
      mutate(type = "cases_per_cap") %>%
      rename(count = cases_per_cap)

    bind_rows(table2, t2_cases_per_cap) %>%
      arrange(country, year, type, count) %>% View()
    
# Note that after adding the cases_per_cap rows, the type of count is coerced 
# to numeric (double) because cases_per_cap is not an integer.
# For table4a and table4b, create a new table for cases per capita, which 
# we’ll name table4c, with country rows and year columns.
    
    
table4c <- tibble(
        country = table4a$country,
        `1999` = table4a[["1999"]] / table4b[["1999"]] * 10000,
        `2000` = table4a[["2000"]] / table4b[["2000"]] * 10000
      )
table4c

# Neither table is particularly easy to work with. Since table2 has separate 
# rows for cases and population we needed to generate a table with columns for
# cases and population where we could calculate cases per capita. table4a and 
# table4b split the cases and population variables into different tables which
# made it easy to divide cases by population. However, we had to repeat this 
# calculation for each row.

# Recreate the plot showing change in cases over time using table2 instead of
# table1. What do you need to do first?
#   
# Before creating the plot with change in cases over time, we need to filter 
# table to only include rows representing cases of TB.

table2 %>%
  filter(type == "cases") %>%
  ggplot(aes(year, count)) +
  geom_line(aes(group = country), colour = "grey50") +
  geom_point(aes(colour = country)) +
  scale_x_continuous(breaks = unique(table2$year)) +
  ylab("cases")

# # The ideal format of a data frame to answer this question is one with columns
# country, year, cases, and population. Then problem could be answered with a 
# single mutate() call.  

# Pivoting
# This code is reproduced from the chapter because it is needed by the exercises.


tidy4a <- table4a %>% 
  pivot_longer(c(`1999`, `2000`), 
               names_to = "year", 
               values_to = "cases")
tidy4b <- table4b %>% 
  pivot_longer(c(`1999`, `2000`), 
               names_to = "year", 
               values_to = "population")

# Why are pivot_longer() and pivot_wider() not perfectly symmetrical? Carefully
# consider the following example:
# Carefully consider the following example:
  
  
stocks <- tibble(
    year   = c(2015, 2015, 2016, 2016),
    half  = c(   1,    2,     1,    2),
    return = c(1.88, 0.59, 0.92, 0.17)
  )
stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return")

  