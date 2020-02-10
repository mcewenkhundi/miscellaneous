#introduction of the cur_ fucntions in dplyr 1.0
library(dplyr)

df <- tibble(
  g = sample(rep(letters[1:3], 1:3)),
  x = runif(6),
  y = runif(6)
)

gf <- df %>% group_by(g)

gf %>% summarise(n = n())

gf %>% mutate(id = cur_group_id())
gf %>% summarise(row = cur_group_rows())
gf %>% summarise(data = list(cur_group()))
gf %>% summarise(data = list(cur_data()))

gf %>% mutate(across(everything(), ~ paste(cur_column(), round(.x, 2))))
