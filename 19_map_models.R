# Modelling ------------------------------------------------
library(tidyverse)
library(broom)
mods <- mtcars %>%
  group_by(cyl) %>%
  condense(mod = lm(mpg ~ disp, data = across()))
mods

mods %>%
  mutate(rsq = summary(mod)$r.squared)

if (requireNamespace("broom")) {
  mods %>% summarise(broom::glance(mod))
  mods %>% summarise(broom::tidy(mod))
  mods %>% summarise(broom::augment(mod))
}

iris %>%
  group_nest(Species) %>%
  mutate(fit = map(data, ~lm(Petal.Length~Sepal.Length, data = .x))) %>%
  mutate(fit_summary = map(fit, tidy)) %>%
  unnest(fit_summary)
#Using row wise and 

iris %>%
  group_nest(Species) %>%
  rowwise() %>%
  mutate(fit =  list(lm(Petal.Length~Sepal.Length, data = cur_data()))) %>%
  mutate(fit_summary = list(tidy(fit))) %>%
  unnest(fit_summary)
  


