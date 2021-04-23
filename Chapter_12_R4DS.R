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

# What would happen if you widen this table? Why? How could you add a new column
# to uniquely identify each value?
  
  people <- tribble(
    ~name, ~key, ~value,
    #-----------------|--------|------
    "Phillip Woods",  "age", 45,
    "Phillip Woods", "height", 186,
    "Phillip Woods", "age", 50,
    "Jessica Cordero", "age", 37,
    "Jessica Cordero", "height", 156
  )
glimpse(people)

# Widening this data frame using pivot_wider() produces columns that are lists of 
# numeric vectors because the name and key columns do not uniquely identify rows. 
# In particular, there are two rows with values for the age of “Phillip Woods”.

pivot_wider(people, names_from="name", values_from = "value")

# We could solve the problem by adding a row with a distinct observation count 
# for each combination of name and key.

people2 <- people %>%
  group_by(name, key) %>%
  mutate(obs = row_number())
people2

# We can make people2 wider because the combination of name and obs will uniquely 
# identify the rows in the wide data frame.

pivot_wider(people2, names_from="name", values_from = "value")

# Another way to solve this problem is by keeping only distinct rows of the 
# name and key values, and dropping duplicate rows.

# However, before doing this understand why there are duplicates in the data. 
# The duplicate values may not be just a nuisance, but may indicate deeper 
# problems with the data.

people %>%
  distinct(name, key, .keep_all = TRUE) %>%
  pivot_wider(names_from="name", values_from = "value")

# Tidy the simple tibble below. Do you need to make it wider or longer?
# What are the variables?
  
  preg <- tribble(
    ~pregnant, ~male, ~female,
    "yes", NA, 10,
    "no", 20, 12
  )
# To tidy the preg table use pivot_longer() to create a long table. 
# The variables in this data are:
#   
#   sex (“female”, “male”)
# pregnant (“yes”, “no”)
# count, which is a non-negative integer representing the number of observations.
# The observations in this data are unique combinations of sex and pregnancy status.
# 

preg_tidy <- preg %>%
  pivot_longer(c(male, female), names_to = "sex", values_to = "count")
preg_tidy

# Remove the (male, pregnant) row with a missing value to simplify the tidied
# data frame.

preg_tidy2 <- preg %>%
  pivot_longer(c(male, female), names_to = "sex", 
               values_to = "count", values_drop_na = TRUE)
preg_tidy2

# his an example of turning an explicit missing value into an implicit missing 
# value, which is discussed in the upcoming section, Missing Values section.
# The missing (male, pregnant) row represents an implicit missing value
# because the value of count can be inferred from its absence. In the tidy
# data, we can represent rows with missing values of count either explicitly
# with an NA (as in preg_tidy) or implicitly by the absence of a row
# (as in preg_tidy2). But in the wide data, the missing values can
# only be represented explicitly.

# Though we have already done enough to make the data tidy, there are some other 
# transformations that can clean the data further. If a variable takes two values,
# like pregnant and sex, it is often preferable to store them as logical vectors.


preg_tidy3 <- preg_tidy2 %>%
  mutate(
    female = sex == "female",
    pregnant = pregnant == "yes"
  ) %>%
  select(female, pregnant, count)
preg_tidy3

# In the previous data frame, I named the logical variable representing the sex
# female, not sex. This makes the meaning of the variable self-documenting. 
# If the variable were named sex with values TRUE and FALSE, without reading
# the documentation, we wouldn’t know whether TRUE means male or female.

# Apart from some minor memory savings, representing these variables as logical
# vectors results in more clear and concise code. Compare the filter() calls to
# select non-pregnant females from preg_tidy2 and preg_tidy.  

filter(preg_tidy2, sex == "female", pregnant == "no")

filter(preg_tidy3, female, !pregnant)

# What do the extra and fill arguments do in separate()? Experiment with the 
# various options for the following two toy datasets.

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"))

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
  separate(x, c("one", "two", "three"))

# The extra argument tells separate() what to do if there are too many pieces, 
# and the fill argument tells it what to do if there aren’t enough. By default, 
# separate() drops extra values with a warning.

# Adding the argument, extra = "drop", produces the same result as above 
# but without the warning.

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"), extra = "drop")

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"))

# Setting extra = "merge", then the extra values are not split, so "f,g" 
# appears in column three.

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"), extra = "merge")

# In this example, one of the values, "d,e", has too few elements. 
# The default for fill is similar to those in separate(); it fills 
# columns with missing values but emits a warning. In this example, 
# the 2nd row of column three is NA.
tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
  separate(x, c("one", "two", "three"))

# Alternative options for the fill are "right", to fill with missing values
# from the right, but without a warning

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
  separate(x, c("one", "two", "three"), fill = "right")

# The option fill = "left" also fills with missing values without 
# emitting a warning, but this time from the left side. Now, the 
# 2nd row of column one will be missing, and the other values in
# that row are shifted right.

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
  separate(x, c("one", "two", "three"), fill = "left")

#Both unite() and separate() have a remove argument. What does it do? Why would
# you set it to FALSE?
#   
# The remove argument discards input columns in the result data frame. You 
# would set it to FALSE if you want to create a new variable, but keep the
# old one.
# 
# Exercise 12.4.3
# Compare and contrast separate() and extract(), Why are there three variations 
# of separation (by position, by separator, and with groups), but only one unite?
#   
#The function separate(), splits a column into multiple columns by separator,
# if the sep argument is a character vector, or by character positions, 
# if sep is numeric.


# example with separators
tibble(x = c("X_1", "X_2", "AA_1", "AA_2")) %>%
  separate(x, c("variable", "into"), sep = "_")

# example with position
tibble(x = c("X1", "X2", "Y1", "Y2")) %>%
  separate(x, c("variable", "into"), sep = c(1))

# The function extract() uses a regular expression to specify groups in
# character vector and split that single character vector into multiple 
# columns. This is more flexible than separate() because it does not require 
# a common separator or specific column positions.
# 

# example with separators
tibble(x = c("X_1", "X_2", "AA_1", "AA_2")) %>%
  extract(x, c("variable", "id"), regex = "([A-Z])_([0-9])")

# example with position
tibble(x = c("X1", "X2", "Y1", "Y2")) %>%
  extract(x, c("variable", "id"), regex = "([A-Z])([0-9])")

# example that separate could not parse
tibble(x = c("X1", "X20", "AA11", "AA2")) %>%
  extract(x, c("variable", "id"), regex = "([A-Z]+)([0-9]+)")

# Both separate() and extract() convert a single column to many columns. 
# However, unite() converts many columns to one, with a choice of a 
# separator to include between column values.

tibble(variable = c("X", "X", "Y", "Y"), id = c(1, 2, 1, 2)) %>%
  unite(x, variable, id, sep = "_")

# In other words, with extract() and separate() only one column can be chosen,
# but there are many choices how to split that single column into different
# columns. With unite(), there are many choices as to which columns to include,
# but only one choice as to how to combine their contents into a single vector.

# 12.5 Missing values
# Exercise 12.5.1
# Compare and contrast the fill arguments to pivot_wider() and complete().
# 
# The values_fill argument in pivot_wider() and the fill argument to complete() 
# both set vales to replace NA. Both arguments accept named lists to set values 
# for each column. Additionally, the values_fill argument of pivot_wider() 
# accepts a single value. In complete(), the fill argument also sets a 
# value to replace NAs but it is named list, allowing for different values
# for different variables. Also, both cases replace both implicit and
# explicit missing values.
# 
# For example, this will fill in the missing values of the long data frame 
# with 0 complete():
  
stocks <- tibble(
    year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
    qtr    = c(   1,    2,    3,    4,    2,    3,    4),
    return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
  )

stocks %>% 
  pivot_wider(names_from = year, values_from = return,
              values_fill = 0)

stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)

stocks %>% 
  pivot_wider(names_from = year, values_from = return,
              values_fill = 0)

# For example, this will fill in the missing values of the long data 
# frame with 0 complete():
 
stocks %>% 
  complete(year, qtr) 
stocks %>% 
  complete(year, qtr, fill=list(return=0))
# Exercise 12.5.2
# What does the direction argument to fill() do?
#   
# With fill, the direction determines whether NA values should be replaced by 
# the previous non-missing value ("down") or the next non-missing value ("up").
  
# 12.6 Case Study
# This code is repeated from the chapter because it is needed by the exercises.
  
who1 <- who %>% 
    pivot_longer(
      cols = new_sp_m014:newrel_f65, 
      names_to = "key", 
      values_to = "cases", 
      values_drop_na = TRUE
    )
  
  who1
  who2 <- who1 %>% 
    mutate(names_from = stringr::str_replace(key, "newrel", "new_rel"))
  who2
  
  who3 <- who2 %>% 
    separate(key, c("new", "type", "sexage"), sep = "_")
  
  who3 %>%
    count(new)
  
  who4 <- who3 %>%
    select(-new, -iso2, -iso3)
  
  who5 <- who4 %>%
    separate(sexage, c("sex", "age"), sep = 1)
  
  # Exercise 12.6.1
  # In this case study, I set na.rm = TRUE just to make it easier to check 
  # that we had the correct values. Is this reasonable? Think about how missing
  # values are represented in this dataset. Are there implicit missing values? 
  #   What’s the difference between an NA and zero?
  #   
  # The reasonableness of using na.rm = TRUE depends on how missing values are 
  # represented in this dataset. The main concern is whether a missing value 
  # means that there were no cases of TB or whether it means that the WHO does
  # not have data on the number of TB cases. Here are some things we should 
  # look for to help distinguish between these cases.
  # 
  # If there are no 0 values in the data, then missing values may be used to 
  # indicate no cases.
  # 
  # If there are both explicit and implicit missing values, then it suggests
  # that missing values are being used differently. In that case, it is likely 
  # that explicit missing values would mean no cases, and implicit missing 
  # values would mean no data on the number of cases.
  # 
  # First, I’ll check for the presence of zeros in the data.
  
  who1 %>%
    filter(cases == 0) %>%
    nrow()
  #> [1] 11080
  # There are zeros in the data, so it appears that cases of zero TB are 
  # explicitly indicated, and the value of NA is used to indicate missing data.
  # 
  # Second, I should check whether all values for a (country, year) are missing 
  # or whether it is possible for only some columns to be missing.
  
  pivot_longer(who, c(new_sp_m014:newrel_f65),
               names_to = "key", values_to = "cases") %>%
    group_by(country, year) %>%
    mutate(prop_missing = sum(is.na(cases)) / n()) %>%
    filter(prop_missing > 0, prop_missing < 1)
  # From the results above, it looks like it is possible for a (country, year) 
  # row to contain some, but not all, missing values in its columns.
  # 
  # Finally, I will check for implicit missing values. Implicit missing values 
  # are (year, country) combinations that do not appear in the data.
  
  nrow(who)
  #> [1] 7240
  
  who %>% 
    complete(country, year) %>%
    nrow()
  #> [1] 7446
  # Since the number of complete cases of (country, year) is greater than the 
  # number of rows in who, there are some implicit values. But that doesn’t 
  # tell us what those implicit missing values are. To do this, I will use
  # the anti_join() function introduced in the later Relational Data chapter.
  # 
  anti_join(complete(who, country, year), who, by = c("country", "year")) %>% 
    select(country, year) %>% 
    group_by(country) %>% 
    # so I can make better sense of the years
    summarise(min_year = min(year), max_year = max(year))

    # All of these refer to (country, year) combinations for years prior to the
  # existence of the country. For example, Timor-Leste achieved independence 
  # in 2002, so years prior to that are not included in the data.
  # To summarize:
    
  # 0 is used to represent no cases of TB.
  # Explicit missing values (NAs) are used to represent missing data for 
  # (country, year) combinations in which the country existed in that year.
  # Implicit missing values are used to represent missing data because a country
  # did not exist in that year.
    
  # Exercise 12.6.2
  # What happens if you neglect the mutate() step? 
  # (mutate(key = str_replace(key, "newrel", "new_rel"))
  # The separate() function emits the warning “too few values”. If we check 
  # the rows for keys beginning with "newrel_", we see that sexage is 
  # missing, and type = m014.
  
  who3a <- who1 %>%
    separate(key, c("new", "type", "sexage"), sep = "_")
  
  
  filter(who3a, new == "newrel") %>% head()
  
# Exercise 12.6.3
# I claimed that iso2 and iso3 were redundant with country. Confirm this claim.
# If iso2 and iso3 are redundant with country, then, within each country, 
# there should only be one distinct combination of iso2 and iso3 values, 
# which is the case.
  
select(who3, country, iso2, iso3) %>%
    distinct() %>%
    group_by(country) %>%
    filter(n() > 1)

# This makes sense, since iso2 and iso3 contain the 2- and 3-letter country 
# abbreviations for the country. The iso2 variable contains each country’s 
# ISO 3166 alpha-2, and the iso3 variable contains each country’s ISO 3166 
# alpha-3 abbreviation. You may recognize the ISO 3166-2 abbreviations, 
# since they are almost identical to internet country-code top level 
# domains, such as .uk (United Kingdom), .ly (Libya), .tv (Tuvalu), 
# and .io (British Indian Ocean Territory).
  
# Exercise 12.6.4
# For each country, year, and sex compute the total number of cases of TB. 
# Make an informative visualization of the data.
  
  who5 %>%
    group_by(country, year, sex) %>%
    filter(year > 1995) %>%
    summarise(cases = sum(cases)) %>%
    unite(country_sex, country, sex, remove = FALSE) %>%
    ggplot(aes(x = year, y = cases, group = country_sex, colour = sex)) +
    geom_line()
  #> `summarise()` regrouping output by 'country', 'year' (override with `.groups` argument)
  
  
  # A small multiples plot faceting by country is difficult given the number
  # of countries. Focusing on those countries with the largest changes or 
  # absolute magnitudes after providing the context above is another option.
  
  