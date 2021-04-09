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
  
  