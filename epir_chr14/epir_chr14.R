#Project: R book club
#Task: Reviewing chapter 14: Joining data
#Author: McEwen Khundi
#Date: 14-1-2022

#It's rare for data analysis to only involve one table of data, usually
# you combine several tables/datasets. 

#The variables that are used to connect pairs of
#tables are called keys/pid's

#Join demonstration

library(tidyverse)
library(tidylog) #Get feedback from dplyr functions

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

x %>%
  select(val_x)
#Inner join
#Includes observations that are in both tables
#unmatched rows not included
inner_join(x,y, by = "key")

#same as
x %>% 
  inner_join(y, by = "key")

#Outer joins
#Left join keeps all observations in x
#Right join keeps all obs in y
#Full join keeps all obs in x and y

x %>% 
  left_join(y, by = "key")

x %>% 
  right_join(y, by = "key")

#What happens when you have duplicate keys/pid

m <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
n <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3"
)

left_join(m, n, by = "key")

#Filtering joins
#Semi join: keeps all obs in x that a match in y
#Anti join: drops all obs in x that have a match in y

x %>% 
  semi_join(y, by = "key")
x %>% 
  anti_join(y, by = "key")

#Merging on several variables
x_z <- tribble(
  ~key,~pl, ~val_x,
  1, 3, "x1",
  2, 4, "x2",
  3, 3, "x3"
)
y_z <- tribble(
  ~key,~pl, ~val_y,
  1, 3, "y1",
  2, 4, "y2",
  4, 3,"y3"
)

inner_join(x_z, y_z)

#What happens when you have same var names in the two datasets
x <- tribble(
  ~key, ~name,
  1, "x1",
  2, "x2",
  3, "x3",
  NA, "x7",
  NA, "x8"
)
y <- tribble(
  ~key, ~name,
  1, "y1",
  2, "y2",
  4, "y3",
  NA, "y4"
)

inner_join(x, y, by = "key")

#When the names are different you need to specify
#x %>%
#full_join(y, by = c("key_x" = "key_y"), keep = TRUE)

#Other joins
#Set operation joins
#Fuzzy joins

#References
#https://r4ds.had.co.nz/relational-data.html
#https://epirhandbook.com/en/joining-data.html

