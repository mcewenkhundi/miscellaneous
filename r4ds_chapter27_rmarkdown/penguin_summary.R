library(ggplot2)
library(dplyr)
library(palmerpenguins)

penguins_species <- penguins %>% filter(species == "Adelie")


# Summary data for `r params$my_species`s

# Graph of bill_length vs body_mass

ggplot(penguins_species, aes(bill_length_mm,  body_mass_g)) + 
  geom_point() + 
  geom_smooth(se = FALSE) 


gt::gt(head(penguins_species))

head(penguins_species)
