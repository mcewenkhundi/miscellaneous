library(tidyverse)

# Create three vectors 
name <- c("al", "bea", "carol")
age <- c(6, 7, 4)
hair <- c("brown", "green", "blond")

# Create data frame 
children <- data.frame(name, age, hair)

children %>%
  mutate(age2 = age*2 )

# I cant see my newly created var

children