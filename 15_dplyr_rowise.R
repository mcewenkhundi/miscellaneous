#rowise

library(dplyr)


df <- tibble(x = runif(6), y = runif(6))
# Compute the mean of x and y in each row
df %>% rowwise() %>% mutate(z = mean(c(x, y)))

# Compute the minimum of x and y in each row
df %>% rowwise() %>% mutate(z = min(c(x, y)))
# In this case you can use an existing vectorised function:
df %>% mutate(z = pmin(x, y))
# Where these functions exist they'll be much faster than rowwise
# so be on the lookout for them.

# rowwise() is also useful when doing simulations
params <- tribble(
  ~sim, ~n, ~mean, ~sd,
  1,  1,     1,   1,
  2,  2,     2,   4,
  3,  3,    -1,   2
)
# Here I supply variables to preserve after the summary
params %>%
  rowwise(sim) %>%
  summarise(z = rnorm(n, mean, sd))

# If you want one row per simulation, put the results in a list()
params %>%
  rowwise(sim) %>%
  summarise(z = list(rnorm(n, mean, sd)))

# Or use do() which do this automatically:
params %>%
  rowwise(sim) %>%
  condense(z = rnorm(n, mean, sd))