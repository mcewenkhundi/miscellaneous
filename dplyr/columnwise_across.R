library(tidyverse)

# Here are a couple of examples of across() in conjunction with its favourite 
# verb, summarise(). But you can use across() with any dplyr verb, as you’ll 
# see a little later.

starwars %>% 
  summarise(across(where(is.character), ~ length(unique(.x))))

starwars %>% 
  group_by(species) %>% 
  filter(n() > 1) %>% 
  summarise(across(c(sex, gender, homeworld), ~ length(unique(.x))))

starwars %>% 
  group_by(homeworld) %>% 
  filter(n() > 1) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

# Because across() is usually used in combination with summarise() and mutate(),
# it doesn’t select grouping variables in order to avoid accidentally modifying them:
df <- data.frame(g = c(1, 1, 2), x = c(-1, 1, 3), y = c(-1, -4, -9))
df %>% 
  group_by(g) %>% 
  summarise(across(where(is.numeric), sum))

# Multiple functions
# You can transform each variable with more than one function by supplying a 
# named list of functions or lambda functions in the second argument:

min_max <- list(
    min = ~min(.x, na.rm = TRUE), 
    max = ~max(.x, na.rm = TRUE)
  )
starwars %>% summarise(across(where(is.numeric), min_max))

starwars %>% summarise(across(c(height, mass, birth_year), min_max))

# Control how the names are created with the .names argument which takes a glue spec:
  
starwars %>% summarise(across(where(is.numeric),
                              min_max, .names = "{.fn}.{.col}"))

starwars %>% summarise(across(c(height, mass, birth_year), min_max, .names = "{.fn}.{.col}"))

#If you’d prefer all summaries with the same function to be grouped together, 
#you’ll have to expand the calls yourself:
  
  starwars %>% summarise(
    across(c(height, mass, birth_year), ~min(.x, na.rm = TRUE), .names = "min_{.col}"),
    across(c(height, mass, birth_year), ~max(.x, na.rm = TRUE), .names = "max_{.col}")
    )
  
#(One day this might become an argument to across() but we’re not yet sure
#how it would work.)
  
# We cannot however use where(is.numeric) in that last case because the second 
# across() would pick up the variables that were newly created (“min_height”, 
#                                                                “min_mass” and 
#                                                                “min_birth_year”).
# We can work around this by combining both calls to across() into a 
# single expression that returns a tibble:
    
starwars %>% summarise(
      tibble(
        across(where(is.numeric), ~min(.x, na.rm = TRUE), .names = "min_{.col}"),
        across(where(is.numeric), ~max(.x, na.rm = TRUE), .names = "max_{.col}")  
      )
    )

#Alternatively we could reorganize results with relocate():
    
starwars %>% 
summarise(across(where(is.numeric), min_max, .names = "{.fn}.{.col}")) %>% 
relocate(starts_with("min"))

# Current column
# If you need to, you can access the name of the “current” column inside by
# calling cur_column(). This can be useful if you want to perform some sort of 
# context dependent transformation that’s already encoded in a vector:
#   
df <- tibble(x = 1:3, y = 3:5, z = 5:7)
mult <- list(x = 1, y = 10, z = 100)

df %>% mutate(across(all_of(names(mult)), ~ .x * mult[[cur_column()]]))

# Gotchas
# Be careful when combining numeric summaries with where(is.numeric):
  
df <- data.frame(x = c(1, 2, 3), y = c(1, 4, 9))

df %>% 
  summarise(n = n(), across(where(is.numeric), sd))

#Here n becomes NA because n is numeric, so the across() computes its standard 
# deviation, and the standard deviation of 3 (a constant) is NA. You probably
# want to compute n() last to avoid this problem:
#   
  df %>% 
  summarise(across(where(is.numeric), sd), n = n())

#Alternatively, you could explicitly exclude n from the columns to operate on:
  
df %>% 
  summarise(n = n(), across(where(is.numeric) & !n, sd))

#Another approach is to combine both the call to n() and across() in a 
#single expression that returns a tibble:
  
df %>% 
  summarise(
    tibble(n = n(), across(where(is.numeric), sd))
  )

# Other verbs
# So far we’ve focused on the use of across() with summarise(), but it works with any other dplyr verb that uses data masking:
#   
# Rescale all numeric variables to range 0-1:
  
  rescale01 <- function(x) {
    rng <- range(x, na.rm = TRUE)
    (x - rng[1]) / (rng[2] - rng[1])
  }
df <- tibble(x = 1:4, y = rnorm(4))
df %>% mutate(across(where(is.numeric), rescale01))

# For some verbs, like group_by(), count() and distinct(), you can omit
#the summary functions:
# Find all distinct

starwars %>% distinct(across(contains("color")))

#Count all combinations of variables with a given pattern:
  
starwars %>% count(across(contains("color")), sort = TRUE)

# across() doesn’t work with select() or rename() because they already use tidy 
# select syntax; if you want to transform column names with a function, you can 
# use rename_with().

# filter()
# We cannot directly use across() in filter() because we need an extra step to 
# combine the results. To that end, filter() has two special purpose 
#companion functions:
   
# if_any() keeps the rows where the predicate is true for at least one
#selected column:
starwars %>% 
filter(if_any(everything(), ~ !is.na(.x)))

#if_all() keeps the rows where the predicate is true for all selected columns:
  starwars %>% 
  filter(if_all(everything(), ~ !is.na(.x)))

  
#Find all rows where no variable has missing values:
  
starwars %>% filter(across(everything(), ~ !is.na(.x)))

# _if, _at, _all
# Prior versions of dplyr allowed you to apply a function to multiple columns in a different way: using functions with _if, _at, and _all() suffixes. These functions solved a pressing need and are used by many people, but are now superseded. That means that they’ll stay around, but won’t receive any new features and will only get critical bug fixes.
# 
# Why do we like across()?
#   Why did we decide to move away from these functions in favour of across()?
#   
#   across() makes it possible to express useful summaries that were previously impossible:
  
df %>%
  group_by(g) %>% 
  summarise(
    across(where(is.numeric), mean), 
    across(where(is.factor), nlevels),
    n = n(), 
  )    
