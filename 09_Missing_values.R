library(tidyverse)

na_if(1:5, c(5:1))


y <- c("abc", "def", "", "ghi")
na_if(y, "")
#> [1] "abc" "def" NA    "ghi"

# na_if is particularly useful inside mutate,
# and is meant for use with vectors rather than entire data frames
starwars %>%
  select(name, eye_color) %>%
  mutate(eye_color = na_if(eye_color, "unknown"))

starwars %>%
  mutate_if(is.character, list(~na_if(., "unknown")))

mtcars %>%
  mutate_if(is.numeric, function(x){x[x %in% c(1,4)]=NA_real_
  x })

#The opposite of na_if is replace_na

df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"), z = list(1:5, NULL, 10:20))
df %>% replace_na(list(x = 0, y = "unknown"))

df %>% mutate(x = replace_na(x, 0))


# NULL are the list-col equivalent of NAs
df %>% replace_na(list(z = list(5)))
#> # A tibble: 3 x 3
#>       x y     z
#>   <dbl> <chr> <list>
#> 1     1 a     <int [5]>
#> 2     2 NA    <dbl [1]>
#> 3    NA b     <int [11]>

df$x %>% replace_na(0)
#> [1] 1 2 0
df$y %>% replace_na("unknown")
#> [1] "a"       "unknown" "b"

##https://cran.r-project.org/web/packages/naniar/vignettes/replace-with-na.html
