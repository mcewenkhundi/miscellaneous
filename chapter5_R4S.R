library(tidyverse)
library(nycflights13)

View(flights)

#Selecting variables
vars <- c("dep_time", "dep_delay", "arr_time", "arr_delay")
select(flights, any_of(vars))
select(flights, all_of(vars))

#using regular expression
select(flights, matches("^(dep|arr)_(time|delay)$"))

select(flights, matches("^(dep|arr)_(time|delay)$"))

#Using tidy eval bang bang
select(flights, !!vars)

#If a list use bang bang bang
select(flights, !!!list(vars))

var_syms <- syms(vars)

select(flights, !!!var_syms)

select(flights, vars)

#mutate
time2mins <- function(x) {
  (x %/% 100 * 60 + x %% 100) %% 1440
}

flights_times <- mutate(flights,
                        dep_time_mins = time2mins(dep_time),
                        sched_dep_time_mins = time2mins(sched_dep_time)
)
# show only the relevant columns
select(
  flights_times, dep_time, dep_time_mins, sched_dep_time,
  sched_dep_time_mins
)

#Ranking functions
rankme <- tibble(
  x = c(10, 5, 1, 5, 5)
)
rankme <- mutate(rankme,
                 x_row_number = row_number(x),
                 x_rank = rank(x),
                 x_min_rank = min_rank(x),
                 x_dense_rank = dense_rank(x)
)
#The group_by and the summary functions

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )
#> `summarise()` ungrouping output (override with `.groups` argument)

ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )
#> `summarise()` ungrouping output (override with `.groups` argument)

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

#postive mean
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

#same as the above
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = length(unique(carrier))) %>% 
  arrange(desc(carriers))

not_cancelled %>% 
  count(dest)

not_cancelled %>% 
  count(tailnum, wt = distance)

# How many flights left before 5am? (these usually indicate delayed
# flights from the previous day)
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))

# What proportion of flights are delayed by more than an hour?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_prop = mean(arr_delay > 60))

#altenative ways of count functions

not_cancelled %>% 
  count(dest)

not_cancelled %>%
  group_by(dest) %>%
  summarise(n = length(dest))

df_small <- tribble(
            ~colA, ~colB,
            "a",   1,
            "b",   2,
            NA_character_,   3
          )

#Length in summary is equivalent to count and not using the sum function
df_small %>%
  group_by(colA) %>%
  summarise(n = length(colA))

df_small %>%
  group_by(colA) %>%
  summarise(n = sum(!is.na(colA)))

df_small %>%
  count(colA)

df_small %>%
  group_by(colA) %>%
  tally()

not_cancelled %>%
  group_by(dest) %>%
  summarise(n = sum(!is.na(dest)))


not_cancelled %>%
  group_by(dest) %>%
  summarise(n = n())

not_cancelled %>%
  group_by(tailnum) %>%
  tally()

not_cancelled %>% 
  count(tailnum, wt = distance)

not_cancelled %>%
  group_by(tailnum) %>%
  summarise(n = sum(distance))

not_cancelled %>%
  group_by(tailnum) %>%
  tally(distance)

#Gruoped mutates and filters
#Find the worst members of each group:
  
not_cancelled %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

#mutate can be used to make transparent group level filtering
not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(rank_delay = dense_rank(desc(arr_delay))) %>%
  select(contains("delay")) %>%
  ungroup() %>%
  filter(rank_delay < 10)

not_cancelled %>%
  group_by(year, month, day) %>%
  filter(dense_rank(desc(arr_delay)) < 10)%>%
  select(contains("delay"))


flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time)

#Find all groups bigger than a threshold:
  
  popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)
  
flights %>% 
  group_by(dest) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n > 365)

#popular_dests
  
#Standardise to compute per group metrics:
    
  popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)
  
  # A grouped filter is a grouped mutate followed by an ungrouped filter.
  # I generally avoid them except for quick and dirty manipulations: 
  # otherwise it's hard to check that you've done the manipulation correctly.
  
  # Functions that work most naturally in grouped mutates and filters
  # are known as window functions (vs. the summary functions used for
  # summaries). You can learn more about useful window functions in
  # the corresponding vignette: vignette("window-functions")
  

















