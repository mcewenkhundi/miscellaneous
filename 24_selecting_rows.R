library(tidyverse)
df <- tibble(x = c("a", "b"), y = c(1, 1), z = c(-1, 1))

# Find all rows where EVERY numeric variable is greater than zero
s <- df %>% filter(across(where(is.numeric), ~ .x > 0))


# Find all rows where ANY numeric variable is greater than zero
rowAny <- function(x) rowSums(x) > 0
df %>%
  filter(rowAny(across(where(is.numeric), ~ .x > 0)))

library(dplyr)
#>
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#>
#>     filter, lag
#> The following objects are masked from 'package:base':
#>
#>     intersect, setdiff, setequal, union

#Notice I added a row with two -1 values
df <- tibble(x = c("a", "b", "c"), y = c(1, 1, -1), z = c(-1, 1, -1))
rowAny <- function(x) rowSums(x) > 0

#Look at the result of the across() function with mutate. It is TRUE or
#FALSE for each value of y and z
l=df %>% mutate(New = across(where(is.numeric), ~ .x > 0))
#> # A tibble: 3 x 4
#>   x         y     z New$y $z
#>   <chr> <dbl> <dbl> <lgl> <lgl>
#> 1 a         1    -1 TRUE  FALSE
#> 2 b         1     1 TRUE  TRUE
#> 3 c        -1    -1 FALSE FALSE

#rowAny() sums TRUE and False with TRUE = 1 and FALSE = 0. If there is any
#TRUE value, the sum will be greater than 0 and that is taken as TRUE
df %>% mutate(New = rowAny(across(where(is.numeric), ~ .x > 0)))
#> # A tibble: 3 x 4
#>   x         y     z New
#>   <chr> <dbl> <dbl> <lgl>
#> 1 a         1    -1 TRUE
#> 2 b         1     1 TRUE
#> 3 c        -1    -1 FALSE

#filtering on those TRUE and FALSE values keeps the TRUE rows.
df %>%
  filter(rowAny(across(where(is.numeric), ~ .x > 0)))
#> # A tibble: 2 x 3
#>   x         y     z
#>   <chr> <dbl> <dbl>
#> 1 a         1    -1
#> 2 b         1     1

df <- data.frame(x = c("a", "b", "c"), y = c(1, 1, -1), z = c(-1, 1, -1))
df[rowSums(x = Filter(f = is.numeric, x = df) > 0) > 0,]


df %>%
  dplyr::mutate(if_any(where(is.numeric), ~ .x > 0))

df %>%
  dplyr::mutate(if_all(where(is.numeric), ~ .x > 0))



































