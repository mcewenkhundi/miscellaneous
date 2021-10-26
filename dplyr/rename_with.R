library(tidyverse)

iris <- as_tibble(iris) # so it prints a little nicer
rename(iris, petal_length = Petal.Length)


rename_with(iris, toupper)



rename_with(iris, toupper, starts_with("Petal"))



rename_with(iris, ~ tolower(gsub(".", "_", .x, fixed = TRUE)))

