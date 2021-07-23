
f <- function(y) function() y

lf <- vector("list", 5)
for (i in seq_along(lf)) lf[[i]] <- f(i)
lf[[1]]()  # returns 5

g <- function(y) { force(y); function() y }
lg <- vector("list", 5)
for (i in seq_along(lg)) lg[[i]] <- g(i)
lg[[1]]()  # returns 1

## This is identical to
g <- function(y) { y; function() y }

penguins <- palmerpenguins::penguins

library(tidyverse)
peng <- penguins %>%
  rowwise() %>%
  mutate(typeVect = class(c_across(where(is.numeric))),
         length =  length(c_across(where(is.numeric))),
         ht = n(),
         data_d = cur_data())
