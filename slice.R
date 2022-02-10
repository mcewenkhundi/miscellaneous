#Subset rows using their positions
#https://dplyr.tidyverse.org/reference/slice.html
library(tidyverse)
mtcars <- mtcars
head(mtcars)

mtcars %>% slice(1L)

tail(mtcars)
mtcars %>% slice(n())

mtcars %>%
  slice(5:n())

#Rows can be dropped with negative indices
slice(mtcars, -(1:4))

# First and last rows based on existing order
mtcars %>% slice_head(n = 5)

mtcars %>% slice_tail(n = 5)

# Rows with minimum and maximum values of a variable
mtcars %>% slice_min(mpg, n = 5)

mtcars %>% slice_max(mpg, n = 5)

# slice_min() and slice_max() may return more rows than requested
# in the presence of ties. Use with_ties = FALSE to suppress
mtcars %>% slice_min(cyl, n = 1)
mtcars %>% slice_min(cyl, n = 1, with_ties = FALSE)

# slice_sample() allows you to random select with or without replacement
mtcars %>% slice_sample(n = 5)

mtcars %>% slice_sample(n = 5, replace = TRUE)

# Group wise operation ----------------------------------------
df <- tibble(
  group = rep(c("a", "b", "c"), c(1, 2, 4)),
  x = runif(7)
)

# All slice helpers operate per group, silently truncating to the group
# size, so the following code works without error
df %>% group_by(group) %>% slice_head(n = 2)

# When specifying the proportion of rows to include non-integer sizes
# are rounded down, so group a gets 0 rows
df %>% group_by(group) %>% slice_head(prop = 0.5)

# Filter equivalents --------------------------------------------
# slice() expressions can often be written to use `filter()` and
# `row_number()`, which can also be translated to SQL. For many databases,
# you'll need to supply an explicit variable to use to compute the row number.
filter(mtcars, row_number() == 1L)

filter(mtcars, row_number() == n())

filter(mtcars, between(row_number(), 5, n()))




