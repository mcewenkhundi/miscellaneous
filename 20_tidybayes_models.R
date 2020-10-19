#https://cran.r-project.org/web/packages/tidybayes/vignettes/tidy-brms.html

library(magrittr)
library(dplyr)
library(purrr)
library(forcats)
library(tidyr)
library(modelr)
library(ggdist)
library(tidybayes)
library(ggplot2)
library(cowplot)
library(rstan)
library(brms)
library(ggrepel)
library(RColorBrewer)
library(gganimate)

theme_set(theme_tidybayes() + panel_border())

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

set.seed(5)
n = 10
n_condition = 5
ABC =
  tibble(
    condition = rep(c("A","B","C","D","E"), n),
    response = rnorm(n * 5, c(0,1,2,1,-1), 0.5)
  )

head(ABC, n = 10)

m = brm(
  response ~ (1|condition), 
  data = ABC, 
  prior = c(
    prior(normal(0, 1), class = Intercept),
    prior(student_t(3, 0, 1), class = sd),
    prior(student_t(3, 0, 1), class = sigma)
  ),
  control = list(adapt_delta = .99),
)

get_variables(m)

m %>%
  spread_draws(r_condition[condition,term]) %>%
  head(10)
m %>%
  spread_draws(r_condition[condition,]) %>%
  head(10)

m %>%
  spread_draws(b_Intercept, sigma) %>%
  head(10)
m %>%
  spread_draws(b_Intercept, sigma) %>%
  median_qi(b_Intercept, sigma)

m %>%
  spread_draws(r_condition[condition,]) %>%
  median_qi()

#Posterior model fits
ABC %>%
  data_grid(condition) %>%
  add_fitted_draws(m) %>%
  head(10)

m_mpg = brm(
  mpg ~ hp * cyl, 
  data = mtcars, 
  
)

mtcars %>%
  group_by(cyl) %>%
  data_grid(hp = seq_range(hp, n = 51)) %>%
  add_fitted_draws(m_mpg) %>%
  median_qi()
  ggplot(aes(x = hp, y = mpg, color = ordered(cyl))) +
  stat_lineribbon(aes(y = .value)) +
  geom_point(data = mtcars) +
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2")








