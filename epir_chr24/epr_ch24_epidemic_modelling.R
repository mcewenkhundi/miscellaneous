# Chapter 24: Epidemic modelling
# Mphatso
# Date: 27 May 2022

# chapter outputs --------- 
# 1. estimate the effective reproduction number Rt and related statistics such as the doubling time
# 2. produce short-term projections of future incidence

# load packages --------
pacman::p_load(
  rio,          # File import
  here,         # File locator
  tidyverse,    # Data management + ggplot2 graphics
  epicontacts,  # Analysing transmission networks
  EpiNow2,      # Rt estimation
  EpiEstim,     # Rt estimation
  projections,  # Incidence projections
  incidence2,   # Handling incidence data
  epitrix,      # Useful epi functions
  distcrete     # Discrete delay distributions
)

# import the cleaned linelist -----
linelist <- import(here("linelist_cleaned.rds"))

# linelist <- linelist %>%
#               filter(!is.na(date_onset), !is.na(date_infection))

# Estimating Rt - defined as the expected number of secondary cases per infected case at a given time t ----

## EpiNow2 package ----
## requires delay distributions, e.g. incubation period, generation time, etc

# 1) defining incubation period from literature
incubation_period_lit <- list(
  mean = log(9.1),
  mean_sd = log(0.1),
  sd = log(7.3),
  sd_sd = log(0.1),
  max = 30
)

# 2) estimate the incubation period distribution from the observed case data using bootstrapped_dist_fit function,
# based on observed delay between infection and symptom onset
incubation_period <- bootstrapped_dist_fit(
  linelist$date_onset - linelist$date_infection,
  dist = "lognormal",
  max_value = 100,
  bootstraps = 1
)

## generate contacts
contacts <- linelist %>%
  transmute(
    from = infector,
    to = case_id
  ) %>%
  drop_na() # 2088 cases without infector dropped

class(contacts)
head(contacts)

## generate epicontacts object using epicontacts package
#epicontacts()
#?transmute

epic <- make_epicontacts(
  linelist = linelist,
  contacts = contacts, 
  directed = TRUE
)
# total=3800 contacts

## estimate gamma generation time from transmission pairs
generation_time <- bootstrapped_dist_fit(
  get_pairwise(epic, "date_infection"),
  dist = "gamma",   # why gamma distribution?
  max_value = 20,
  bootstraps = 1
)

## get incidence from onset dates
cases <- linelist %>%
  #filter(!is.na(date_onset)) %>%
  group_by(date = date_onset) %>%
  summarise(confirm = n())

## run epinow to calculate Rt
epinow_res <- epinow(
  reported_cases = cases,
  generation_time = generation_time,
  delays = delay_opts(incubation_period),
  return_output = TRUE,
  verbose = TRUE,
  horizon = 21,
  stan = stan_opts(samples = 750, chains = 4)
)

#saveRDS(epinow_res, file = here("epinow_res.rds"))
epinow_res <- readRDS(file = here("epinow_res.rds"))
## plot epinow results
plot(epinow_res)

## summary table
epinow_res$summary

## extract summary and convert to tibble
estimates <- as_tibble(epinow_res$estimates$summarised)
estimates

## make wide df for median plotting
df_wide <- estimates %>%
  filter(
    variable %in% c("growth_rate", "R"),
    date < as.Date("2014-09-01")
  ) %>%
  ## convert growth rates to doubling times
  mutate(
    across(
      c(median, lower_90:upper_90),
      ~ case_when(
        variable == "growth_rate" ~ log(2)/.x,
        TRUE ~ .x
      )
    ),
    ## rename variable to reflect transformation
    variable = replace(variable, variable == "growth_rate", "doubling_time")
  )

## make long df for quantile plotting
df_long <- df_wide %>%
  ## here we match matching quantiles (e.g. lower_90 to upper_90)
  pivot_longer(
    lower_90:upper_90,
    names_to = c(".value", "quantile"),
    names_pattern = "(.+)_(.+)"
  )

## make plot
ggplot() +
  geom_ribbon(
    data = df_long,
    aes(x = date, ymin = lower, ymax = upper, alpha = quantile),
    color = NA
  ) +
  geom_line(
    data = df_wide,
    aes(x = date, y = median)
  ) +
  ## use label_parsed to allow subscript label
  facet_wrap(
    ~ variable,
    ncol = 1,
    scales = "free_y",
    labeller = as_labeller(c(R = "R[t]", doubling_time = "Doubling~time"), label_parsed),
    strip.position = 'left'
  ) +
  ## manually define quantile transparency
  scale_alpha_manual(
    values = c(`20` = 0.7, `50` = 0.4, `90` = 0.2),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    x = NULL,
    y = NULL,
    alpha = "Credibel\ninterval"
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %d\n%Y"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.background = element_blank(),
    strip.placement = 'outside'
  )

## EpiEstim package ----
# requires incidence data and serial interval distribution

# create incidence input using incidence2 package, but additional steps needed to align output with 
# what estimateR() expected input
## 1) complete() to include all dates, including where no cases
## 2) rename() variable names

## get incidence from onset date
cases <- incidence2::incidence(linelist, date_index = date_onset) %>% # get case counts by day
  tidyr::complete(date_index = seq.Date(                              # ensure all dates are represented
    from = min(date_index, na.rm = T),
    to = max(date_index, na.rm=T),
    by = "day"),
    fill = list(count = 0)) %>%                                       # convert NA counts to 0
  rename(I = count,                                                   # rename to names expected by estimateR
         dates = date_index)

## 256 missing observations were removed.

# define serial interval
# 1) based on literature

## make config
config_lit <- make_config(
  mean_si = 12.0,
  std_si = 5.2
)

## estimate Rt with the estimate_R function

epiestim_res_lit <- estimate_R(
incid = cases,
method = "parametric_si",
config = config_lit
)

epiestim_res_lit <- estimate_R(
  incid = cases,
  method = "parametric_si",
  config = config_lit
)

## default config will estimate R on weekly sliding windows.
## to change this change the t_start and t_end arguments.

# plot summary outputs
plot(epiestim_res_lit)

## 2) based on data - using get_pairwise fn from epicontacts package

## generate contacts object
contacts <- linelist %>%
  transmute(
    from = infector,
    to = case_id
  ) %>%
  drop_na()

## generate epicontacts object
epic <- make_epicontacts(
  linelist = linelist,
  contacts = contacts, 
  directed = TRUE
)

## estimate gamma serial interval
## fit differences in onset times using gamma distribution

## use fit_disc_gamma from epitrix fn as requires "discretised" distribution !!!

serial_interval <- fit_disc_gamma(get_pairwise(epic, "date_onset"))

## make config
config_emp <- make_config(
  mean_si = serial_interval$mu,
  std_si = serial_interval$sd
)

## run epiestim
epiestim_res_emp <- estimate_R(
  incid = cases,
  method = "parametric_si",
  config = config_emp
)

## default config will estimate R on weekly sliding windows
## to change this change the t_start and t_end arguments

## plot outputs
plot(epiestim_res_emp)

# specifying estimation time windows
# default is weekly window - warning if estimating Rt too quickly, e.g. start of epidemic --> poor precision

## define a vector of dates starting on June 1st
start_dates <- seq.Date(
  as.Date("2014-06-01"),
  max(cases$dates) - 7,
  by = 1
) %>%
  ## subtract the starting date to convert to numeric
  `-`(min(cases$dates)) %>%
  ## convert to integer
  as.integer()

## add six days for a one week sliding window
end_dates <- start_dates + 6

## make config
config_partial <- make_config(
  mean_si = 12.0,
  std_si = 5.2,
  t_start = start_dates,
  t_end = end_dates
)


## run epiestim
epiestim_res_partial <- estimate_R(
  incid = cases,
  method = "parametric_si",
  config = config_partial
)

## plot outputs
plot(epiestim_res_partial)

# analysing outputs

## make wide dataframe for median
df_wide <- epiestim_res_lit$R %>%
  rename_all(clean_labels) %>%
  rename(
    lower_95_r = quantile_0_025_r,
    lower_90_r = quantile_0_05_r,
    lower_50_r = quantile_0_25_r,
    upper_50_r = quantile_0_75_r,
    upper_90_r = quantile_0_95_r,
    upper_95_r = quantile_0_975_r,
  ) %>%
  mutate(
    ## extract the median date from t_start and t_end
    dates = epiestim_res_emp$dates[round(map2_dbl(t_start, t_end, median))],
    var = "R[t]"
  ) %>%
  ## merge in daily incidence data
  left_join(cases, "dates") %>%
  ## calculate risk across all r estimates
  mutate(
    across(
      lower_95_r:upper_95_r,
      ~ .x*I,
      .names = "{str_replace(.col, '_r', '_risk')}"
    )
  ) %>%
  ## seperate r estimates and risk estimates
  pivot_longer(
    contains("median"),
    names_to = c(".value", "variable"),
    names_pattern = "(.+)_(.+)"
  ) %>%
  ## assign factor levels
  mutate(variable = factor(variable, c("risk", "r")))

## make long dataframe from quantiles
df_long <- df_wide %>%
  select(-variable, -median) %>%
  ## seperate r/risk estimates and quantile levels
  pivot_longer(
    contains(c("lower", "upper")),
    names_to = c(".value", "quantile", "variable"),
    names_pattern = "(.+)_(.+)_(.+)"
  ) %>%
  mutate(variable = factor(variable, c("risk", "r")))

## make plot
ggplot() +
  geom_ribbon(
    data = df_long,
    aes(x = dates, ymin = lower, ymax = upper, alpha = quantile),
    color = NA
  ) +
  geom_line(
    data = df_wide,
    aes(x = dates, y = median),
    alpha = 0.2
  ) +
  ## use label_parsed to allow subscript label
  facet_wrap(
    ~ variable,
    ncol = 1,
    scales = "free_y",
    labeller = as_labeller(c(r = "R[t]", risk = "Transmission~potential"), label_parsed),
    strip.position = 'left'
  ) +
  ## manually define quantile transparency
  scale_alpha_manual(
    values = c(`50` = 0.7, `90` = 0.4, `95` = 0.2),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    x = NULL,
    y = NULL,
    alpha = "Credible\ninterval"
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %d\n%Y"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.background = element_blank(),
    strip.placement = 'outside'
  )

