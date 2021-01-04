#Return the unique records and the last duplicated row

library(dplyr)
df <- data.frame(id = c(1,2,3,1,3,4,3), ch = c(1:7))

df

#This returns the unique records and the first duplicated row 

df %>%
  group_by(id) %>%
  filter(row_number() == 1)
 
#This returns the unique records and the last duplicated row
df %>%
  group_by(id) %>%
  filter(row_number() == max(row_number()))

#This shows all the duplicates in the dataset
set.seed(199037)
tibble(x = sample(LETTERS, 50, replace = TRUE)) %>% add_count(x) %>% filter(n > 1)
tibble(x = sample(LETTERS, 50, replace = TRUE)) %>% count(x) %>% filter(n > 1)

         
         