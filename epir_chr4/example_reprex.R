# Author: McEwen Khundi
# Date: 27Aug2021
# Purpose: Demo how to make a reproducible example

#References resources
#https://reprex.tidyverse.org/
#https://www.youtube.com/watch?v=5gqksthQ0cM
#https://community.rstudio.com/
#How to read r help https://twitter.com/dataandme/status/982664813443715072/photo/1

#Steps taken when you have an R related question
# Try r help ?function name i.e ?mean
# Ask a close friend
# Check similar questions on online forums
# Ask a reprex quesion on an online forum such as https://community.rstudio.com/

#install.packages("reprex")

#How can I add new variables to a dataset/I can't see my newly created variables

library(tidyverse)
#Create a reprex
# Create three vectors 
name <- c("al", "bea", "carol")
age <- c(6, 7, 4)
hair <- c("brown", "green", "blond")

# Create data frame 
children <- data.frame(name, age, hair)
children

children %>%
  mutate(age2 = age*2)

#Now i cant see the new var above
children


