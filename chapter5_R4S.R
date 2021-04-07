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

df_small <- tribble(~a,~b)

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

#From the exercise section
# Describe how each operation changes when you combine it with grouping.
# 
# Summary functions (mean()), offset functions (lead(), lag()), ranking 
# functions (min_rank(), row_number()), operate within each group when 
# used with group_by() in mutate() or filter(). Arithmetic operators (+, -), 
# logical operators (<, ==), modular arithmetic operators (%%, %/%), 
# logarithmic functions (log) are not affected by group_by.
# 
# Summary functions like mean(), median(), sum(), std() and others covered 
#in the section Useful Summary Functions calculate their values within each
#group when used with mutate() or filter() and group_by().
tibble(x = 1:9,
       group = rep(c("a", "b", "c"), each = 3)) %>%
  mutate(x_mean = mean(x)) %>%
  group_by(group) %>%
  mutate(x_mean_2 = mean(x))

# Arithmetic operators +, -, *, /, ^ are not affected by group_by()
tibble(x = 1:9,
       group = rep(c("a", "b", "c"), each = 3)) %>%
  mutate(y = x + 2) %>%
  group_by(group) %>%
  mutate(z = x + 2)

# The modular arithmetic operators %/% and %% are not affected by group_by()

tibble(x = 1:9,
       group = rep(c("a", "b", "c"), each = 3)) %>%
  mutate(y = x %% 2) %>%
  group_by(group) %>%
  mutate(z = x %% 2)

#The logarithmic functions log(), log2(), and log10() are not affected by group_by().

tibble(x = 1:9,
       group = rep(c("a", "b", "c"), each = 3)) %>%
  mutate(y = log(x)) %>%
  group_by(group) %>%
  mutate(z = log(x))

# The offset functions lead() and lag() respect the groupings in group_by(). 
# The functions lag() and lead() will only return values within each group.

tibble(x = 1:9,
       group = rep(c("a", "b", "c"), each = 3)) %>%
  group_by(group) %>%
  mutate(lag_x = lag(x),
         lead_x = lead(x))

# The cumulative and rolling aggregate functions cumsum(), cumprod(), cummin(),
# cummax(), and cummean() calculate values within each group.

tibble(x = 1:9,
       group = rep(c("a", "b", "c"), each = 3)) %>%
  mutate(x_cumsum = cumsum(x)) %>%
  group_by(group) %>%
  mutate(x_cumsum_2 = cumsum(x))

#Logical comparisons, <, <=, >, >=, !=, and == are not affected by group_by().
tibble(x = 1:9,
       y = 9:1,
       group = rep(c("a", "b", "c"), each = 3)) %>%
  mutate(x_lte_y = x <= y) %>%
  group_by(group) %>%
  mutate(x_lte_y_2 = x <= y)

# Ranking functions like min_rank() work within each group when
# used with group_by().

tibble(x = 1:9,
       group = rep(c("a", "b", "c"), each = 3)) %>%
  mutate(rnk = min_rank(x)) %>%
  group_by(group) %>%
  mutate(rnk2 = min_rank(x))

#Though not asked in the question, note that arrange() 
#ignores groups when sorting values.

tibble(x = runif(9),
       group = rep(c("a", "b", "c"), each = 3)) %>%
  group_by(group) %>%
  arrange(x)

# Which plane (tailnum) has the worst on-time record?
#   
# The question does not define a way to measure on-time record, so I will 
#consider two metrics:
#   
# proportion of flights not delayed or cancelled, and
# mean arrival delay.
# The first metric is the proportion of not-cancelled and on-time flights.
#I use the presence of an arrival time as an indicator that a flight was 
# not cancelled. However, there are many planes that have never 
# flown an on-time flight. Additionally, many of the planes that 
# have the lowest proportion of on-time flights have only flown a 
# small number of flights.

flights %>%
  filter(!is.na(tailnum)) %>%
  mutate(on_time = !is.na(arr_time) & (arr_delay <= 0)) %>%
  group_by(tailnum) %>%
  summarise(on_time = mean(on_time), n = n()) %>%
  filter(min_rank(on_time) == 1)
# So, I will remove planes that flew at least 20 flights. The choice of 20 was 
# chosen because it round number near the first quartile of the number 
# of flights by plane.56

quantile(count(flights, tailnum)$n)

# The plane with the worst on time record that flew at least 20 flights is:
  
  flights %>%
  filter(!is.na(tailnum), is.na(arr_time) | !is.na(arr_delay)) %>%
  mutate(on_time = !is.na(arr_time) & (arr_delay <= 0)) %>%
  group_by(tailnum) %>%
  summarise(on_time = mean(on_time), n = n()) %>%
  filter(n >= 20) %>%
  filter(min_rank(on_time) == 1)

# There are cases where arr_delay is missing but arr_time is not missing. 
# I have not debugged the cause of this bad data, so these rows are dropped
# for the purposes of this exercise.
# 
# The second metric is the mean minutes delayed. As with the previous metric,
# I will only consider planes which flew least 20 flights. 
# A different plane has the worst on-time record when measured 
# as average minutes delayed.

flights %>%
  filter(!is.na(arr_delay)) %>%
  group_by(tailnum) %>%
  summarise(arr_delay = mean(arr_delay), n = n()) %>%
  filter(n >= 20) %>%
  filter(min_rank(desc(arr_delay)) == 1)

# What time of day should you fly if you want to avoid delays as much as possible?
#   
# Let’s group by the hour of the flight. The earlier the flight is scheduled, 
# the lower its expected delay. This is intuitive as delays will affect later 
# flights. Morning flights have fewer (if any) previous flights 
# that can delay them.

flights %>%
  group_by(hour) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(arr_delay)

# For each destination, compute the total minutes of delay. For each flight, 
# compute the proportion of the total delay for its destination.
# 
# The key to answering this question is to only include delayed flights when 
# calculating the total delay and proportion of delay.


flights %>%
  filter(arr_delay > 0) %>%
  group_by(dest) %>%
  mutate(
    arr_delay_total = sum(arr_delay),
    arr_delay_prop = arr_delay / arr_delay_total
  ) %>%
  select(dest, month, day, dep_time, carrier, flight,
         arr_delay, arr_delay_prop) %>%
  arrange(dest, desc(arr_delay_prop))

# There is some ambiguity in the meaning of the term flights in the question. 
# The first example defined a flight as a row in the flights table, which is a 
# trip by an aircraft from an airport at a particular date and time. However,
# flight could also refer to the flight number, which is the code a carrier uses 
# for an airline service of a route. For example, AA1 is the flight number
# of the 09:00 American Airlines flight between JFK and LAX. The flight number
# is contained in the flights$flight column, though what is called a “flight”
# is a combination of the flights$carrier and flights$flight columns.

flights %>%
  filter(arr_delay > 0) %>%
  group_by(dest, origin, carrier, flight) %>%
  summarise(arr_delay = sum(arr_delay)) %>%
  group_by(dest) %>%
  mutate(
    arr_delay_prop = arr_delay / sum(arr_delay)
  ) %>%
  arrange(dest, desc(arr_delay_prop)) %>%
  select(carrier, flight, origin, dest, arr_delay_prop)

#Delays are typically temporally correlated: even once the problem that 
# caused the initial delay has been resolved, later flights are delayed to allow 
# earlier flights to leave. Using lag() explore how the delay of a flight is 
# related to the delay of the immediately preceding flight.
# 
# This calculates the departure delay of the preceding flight 
# from the same airport.

lagged_delays <- flights %>%
  arrange(origin, month, day, dep_time) %>%
  group_by(origin) %>%
  mutate(dep_delay_lag = lag(dep_delay)) %>%
  filter(!is.na(dep_delay), !is.na(dep_delay_lag))


# This plots the relationship between the mean delay of a flight for all values 
# of the previous flight. For delays less than two hours, the relationship 
# between the delay of the preceding flight and the current flight is nearly a
# line. After that the relationship becomes more variable, as long-delayed
# flights are interspersed with flights leaving on-time. After about 8-hours, 
# a delayed flight is likely to be followed by a flight leaving on time.

lagged_delays %>%
  group_by(dep_delay_lag) %>%
  summarise(dep_delay_mean = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay_mean, x = dep_delay_lag)) +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 1500, by = 120)) +
  labs(y = "Departure Delay", x = "Previous Departure Delay")

#The overall relationship looks similar in all three origin airports.

lagged_delays %>%
  group_by(origin, dep_delay_lag) %>%
  summarise(dep_delay_mean = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay_mean, x = dep_delay_lag)) +
  geom_point() +
  facet_wrap(~ origin, ncol=1) +
  labs(y = "Departure Delay", x = "Previous Departure Delay")

# Look at each destination. Can you find flights that are suspiciously fast? (i.e. flights that represent a potential data entry error). Compute the air time of a flight relative to the shortest flight to that destination. Which flights were most delayed in the air?
#   
# When calculating this answer we should only compare flights within the same (origin, destination) pair.
# 
# To find unusual observations, we need to first put them on the same scale. I will standardize values by subtracting the mean from each and then dividing each by the standard deviation.
# 
# A standardized variable is often called a  
# z
# -score. The units of the standardized variable are standard deviations from the mean. This will put the flight times from different routes on the same scale. The larger the magnitude of the standardized variable for an observation, the more unusual the observation is. Flights with negative values of the standardized variable are faster than the mean flight for that route, while those with positive values are slower than the mean flight for that route.

standardized_flights <- flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest, origin) %>%
  mutate(
    air_time_mean = mean(air_time),
    air_time_sd = sd(air_time),
    n = n()
  ) %>%
  ungroup() %>%
  mutate(air_time_standard = (air_time - air_time_mean) / (air_time_sd + 1))

# I add 1 to the denominator and numerator to avoid dividing by zero. 
# Note that the ungroup() here is not necessary. However, I will be 
# using this data frame later. Through experience, I have found that 
# I have fewer bugs when I keep a data frame grouped for only those 
# verbs that need it. If I did not ungroup() this data frame, the 
# arrange() used later would not work as expected. It is better 
# to err on the side of using ungroup() when unnecessary.
# The distribution of the standardized air flights has long right tail.
# 
ggplot(standardized_flights, aes(x = air_time_standard)) +
  geom_density()
#> Warning: Removed 4 rows containing non-finite values (stat_density).


# Unusually fast flights are those flights with the smallest standardized values.

standardized_flights %>%
  arrange(air_time_standard) %>%
  select(
    carrier, flight, origin, dest, month, day,
    air_time, air_time_mean, air_time_standard
  ) %>%
  head(10) %>%
  print(width = Inf)

# I used width = Inf to ensure that all columns will be printed.
# 
# The fastest flight is DL1499 from LGA to ATL which departed on 2013-05-25 
# at 17:09. It has an air time of 65 minutes, compared to an average flight
# time of 114 minutes for its route. This is 4.6 standard deviations below
# the average flight on its route.
# # 
# # It is important to note that this does not necessarily imply that there
# was a data entry error. We should check these flights to see whether there 
# was some reason for the difference. It may be that we are missing some 
# piece of information that explains these unusual times.
# # 
# # A potential issue with the way that we standardized the flights is 
# that the mean and standard deviation used to calculate are sensitive 
# to outliers and outliers is what we are looking for. Instead of 
# standardizing variables with the mean and variance, we could use 
# the median as a measure of central tendency and the interquartile
# range (IQR) as a measure of spread. The median and IQR are more 
# resistant to outliers than the mean and standard deviation. The 
# following method uses the median and inter-quartile range, which are
# less sensitive to outliers.

standardized_flights2 <- flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest, origin) %>%
  mutate(
    air_time_median = median(air_time),
    air_time_iqr = IQR(air_time),
    n = n(),
    air_time_standard = (air_time - air_time_median) / air_time_iqr)

# The distribution of the standardized air flights using this new definition
# also has long right tail of slow flights.

ggplot(standardized_flights2, aes(x = air_time_standard)) +
  geom_density()
#> Warning: Removed 4 rows containing non-finite values (stat_density).


# Unusually fast flights are those flights with the smallest standardized values.

standardized_flights2 %>%
  arrange(air_time_standard) %>%
  select(
    carrier, flight, origin, dest, month, day, air_time,
    air_time_median, air_time_standard
  ) %>%
  head(10) %>%
  print(width = Inf)

# All of these answers have relied only on using a distribution of comparable 
# observations to find unusual observations. In this case, the comparable 
# observations were flights from the same origin to the same destination. 
# Apart from our knowledge that flights from the same origin to the same 
# destination should have similar air times, we have not used any other 
# domain-specific knowledge. But we know much more about this problem. 
# The most obvious piece of knowledge we have is that we know that flights 
# cannot travel back in time, so there should never be a flight with a negative 
# airtime. But we also know that aircraft have maximum speeds. While different 
# aircraft have different cruising speeds, commercial airliners typically cruise
# at air speeds around 547–575 mph. Calculating the ground speed of aircraft is 
# complicated by the way in which winds, especially the influence of wind, 
# especially jet streams, on the ground-speed of flights. A strong tailwind 
# can increase ground-speed of the aircraft by 200 mph. Apart from the 
# retired Concorde. For example, in 2018, a transatlantic flight traveled 
# at 770 mph due to a strong jet stream tailwind. This means that any flight
# traveling at speeds greater than 800 mph is implausible, and it may be worth
# checking flights traveling at greater than 600 or 700 mph. Ground speed 
# could also be used to identify aircraft flying implausibly slow. Joining
# flights data with the air craft type in the planes table and getting information
# about typical or top speeds of those aircraft could provide a more detailed way
# to identify implausibly fast or slow flights. Additional data on high altitude
# wind speeds at the time of the flight would further help.
# 
# Knowing the substance of the data analysis at hand is one of the most
# important tools of a data scientist. The tools of statistics are a complement,
# not a substitute, for that knowledge.
# 
# With that in mind, Let’s plot the distribution of the ground speed of flights.
# The modal flight in this data has a ground speed of between 400 and 500 mph. 
# The distribution of ground speeds has a large left tail of slower flights below 
# 400 mph constituting the majority. There are very few flights with a ground 
# speed over 500 mph.

flights %>%
  mutate(mph = distance / (air_time / 60)) %>%
  ggplot(aes(x = mph)) +
  geom_histogram(binwidth = 10)
#> Warning: Removed 9430 rows containing non-finite values (stat_bin).


# The fastest flight is the same one identified as the largest outlier earlier. 
# Its ground speed was 703 mph. This is fast for a commercial jet, but not impossible.

flights %>%
  mutate(mph = distance / (air_time / 60)) %>%
  arrange(desc(mph)) %>%
  select(mph, flight, carrier, flight, month, day, dep_time) %>%
  head(5)
# One explanation for unusually fast flights is that they are “making up time” 
# in the air by flying faster. Commercial aircraft do not fly at their top 
# speed since the airlines are also concerned about fuel consumption. But, 
# if a flight is delayed on the ground, it may fly faster than usual in 
# order to avoid a late arrival. So, I would expect that some of the unusually
# fast flights were delayed on departure.

flights %>%
  mutate(mph = distance / (air_time / 60)) %>%
  arrange(desc(mph)) %>%
  select(
    origin, dest, mph, year, month, day, dep_time, flight, carrier,
    dep_delay, arr_delay
  )
#
head(5)
#> [1] 5
# Five of the top ten flights had departure delays, and three of those were
# able to make up that time in the air and arrive ahead of schedule.
# 
# Overall, there were a few flights that seemed unusually fast, but they 
# all fall into the realm of plausibility and likely are not data entry 
# problems. [Ed. Please correct me if I am missing something]
# 
# The second part of the question asks us to compare flights to the fastest 
# flight on a route to find the flights most delayed in the air. I will calculate
# the amount a flight is delayed in air in two ways. The first is the absolute 
# delay, defined as the number of minutes longer than the fastest flight on 
# that route,air_time - min(air_time). The second is the relative delay,
# which is the percentage increase in air time relative to the time of the 
# fastest flight along that route, (air_time - min(air_time)) / min(air_time) * 100.

air_time_delayed <-
  flights %>%
  group_by(origin, dest) %>%
  mutate(
    air_time_min = min(air_time, na.rm = TRUE),
    air_time_delay = air_time - air_time_min,
    air_time_delay_pct = air_time_delay / air_time_min * 100
  )
#> Warning in min(air_time, na.rm = TRUE): no non-missing arguments to min;
#> returning Inf
# The most delayed flight in air in minutes was DL841 from JFK to SFO which
# departed on 2013-07-28 at 17:27. It took 189 minutes longer than the 
# flight with the shortest air time on its route.

air_time_delayed %>%
  arrange(desc(air_time_delay)) %>%
  select(
    air_time_delay, carrier, flight,
    origin, dest, year, month, day, dep_time,
    air_time, air_time_min
  ) %>%
  head() %>%
  print(width = Inf)
#> # A tibble: 6 x 11

# The most delayed flight in air as a percentage of the fastest flight 
# long that route was US2136 from LGA to BOS departing on 2013-06-17 at 16:52.
# It took 410% longer than the flight with the shortest air time on its route.

air_time_delayed %>%
  arrange(desc(air_time_delay)) %>%
  select(
    air_time_delay_pct, carrier, flight,
    origin, dest, year, month, day, dep_time,
    air_time, air_time_min
  ) %>%
  head() %>%
  print(width = Inf)
# Exercise 5.7.7
# Find all destinations that are flown by at least two carriers. Use that 
# information to rank the carriers.
# 
# To restate this question, we are asked to rank airlines by the number of 
# destinations that they fly to, considering only those airports that are 
# flown to by two or more airlines. There are two steps to calculating this
# ranking. First, find all airports serviced by two or more carriers. Then, 
# rank carriers by the number of those destinations that they service.

flights %>%
  # find all airports with > 1 carrier
  group_by(dest) %>%
  mutate(n_carriers = n_distinct(carrier)) %>%
  filter(n_carriers > 1) %>%
  # rank carriers by numer of destinations
  group_by(carrier) %>%
  summarize(n_dest = n_distinct(dest)) %>%
  arrange(desc(n_dest))

# The carrier "EV" flies to the most destinations, considering only airports 
# flown to by two or more carriers. What airline does the "EV" 
# carrier code correspond to?
#   
  filter(airlines, carrier == "EV")

# Unless you know the airplane industry, it is likely that you don’t recognize
# ExpressJet; I certainly didn’t. It is a regional airline that partners with 
# major airlines to fly from hubs (larger airports) to smaller airports. 
# This means that many of the shorter flights of major carriers are 
# operated by ExpressJet. This business model explains why ExpressJet
# services the most destinations.
# 
# Among the airlines that fly to only one destination from New York
# are Alaska Airlines and Hawaiian Airlines.

filter(airlines, carrier %in% c("AS", "F9", "HA"))

# Exercise 5.7.8
# For each plane, count the number of flights before the 
# first delay of greater than 1 hour.
# 
# The question does not specify arrival or departure delay. 
# I consider dep_delay in this answer, though similar code 
# could be used for arr_delay.

flights %>%
  # sort in increasing order
  select(tailnum, year, month,day, dep_delay) %>%
  filter(!is.na(dep_delay)) %>%
  arrange(tailnum, year, month, day) %>%
  group_by(tailnum) %>%
  # cumulative number of flights delayed over one hour
  mutate(cumulative_hr_delays = cumsum(dep_delay > 60)) %>%
  # count the number of flights == 0
  summarise(total_flights = sum(cumulative_hr_delays < 1)) %>%
  arrange(total_flights)

# The exception is flights on the days on which daylight savings started 
# (March 10) or ended (November 3). Since in the US, daylight savings goes
# into effect at 2 a.m., and generally flights are not scheduled to depart
# between midnight and 2 a.m., the only flights which would be scheduled to 
# depart in Eastern Daylight Savings Time (Eastern Standard Time) time but 
# departed in Eastern Standard Time (Eastern Daylight Savings Time), would 
# have been scheduled before midnight, meaning they were delayed across days. 
# If time zones seem annoying, it is not your imagination. They are.
# I recommend this video, The Problem with Time & Timezones - Computerphile.↩

# Yes, technically, base::pi is an approximation of  
# π
# to seven digits of precision. Don’t @ me.↩
# 
# We could address this issue using a statistical model, but that is outside the
# scope of this text.↩
# 
# The count() function is introduced in Chapter 5.6. It returns the count
# of rows by group. In this case, the number of rows in flights for each tailnum. 
# The data frame that count() returns has columns for the groups, and a column n,
# which contains that count.↩




























