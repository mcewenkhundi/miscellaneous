library(tidyverse)
fish_encounters
fish_encounters %>%
  pivot_wider(names_from = station, values_from = seen)

# Fill in missing values
fish_encounters %>%
  pivot_wider(
    names_from = station,
    values_from = seen,
    values_fill = list(seen = 0)
  )

# Generate column names from multiple variables
us_rent<- us_rent_income %>% 
  pivot_wider(names_from = variable, values_from = c(estimate, moe))

us_rent %>%
  pivot_longer(c(-GEOID,-NAME),names_to = c("estimate","variable"), values_to = "values", names_sep = "_")

# Can perform aggregation with values_fn
warpbreaks <- as_tibble(warpbreaks[c("wool", "tension", "breaks")])
warpbreaks
warpbreaks %>%
  pivot_wider(
    names_from = wool,
    values_from = breaks,
    values_fn = list(breaks = length)
  )
