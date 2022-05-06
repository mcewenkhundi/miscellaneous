

## only keep the current fold being fit 
mini_data <- filter(case_roll, .id == 53) %>% 
  as_tibble()

i = 53
## create an empty data set for forecasting on 
forecast_data <- tibble(
  epiweek = seq(max(mini_data$epiweek) + 1,
                max(mini_data$epiweek) + weeks_ahead,
                by = 1),
  case_int = rep.int(NA, weeks_ahead),
  .id = rep.int(i, weeks_ahead)
)

## add the forecast data to the original 
mini_data <- bind_rows(mini_data, forecast_data)

## define the cut off based on latest non missing count data 
cv_cut_off <- mini_data %>% 
  ## only keep non-missing rows
  drop_na(case_int) %>% 
  ## get the latest week
  summarise(max(epiweek)) %>% 
  ## extract so is not in a dataframe
  pull()

## make mini_data back in to a tsibble
mini_data <- tsibble(mini_data, index = epiweek)

## define fourier terms (sincos) 
mini_data <- mini_data %>% 
  mutate(
    ## combine fourier terms for weeks prior to  and after cut-off date
    fourier = rbind(
      ## get fourier terms for previous years
      forecast::fourier(
        ## only keep the rows before cut-off
        filter(mini_data, 
               epiweek <= cv_cut_off), 
        ## include one set of sin cos terms 
        K = 1
      ), 
      ## predict the fourier terms for following year (using baseline data)
      fourier(
        ## only keep the rows before cut-off
        filter(mini_data, 
               epiweek <= cv_cut_off),
        ## include one set of sin cos terms 
        K = 1, 
        ## predict 52 weeks ahead
        h = weeks_ahead
      )
    )
  )


# split data for fitting and prediction
dat1 <- mini_data %>% 
  filter(epiweek <= cv_cut_off)

dat2 <- mini_data %>% 
  filter(epiweek > cv_cut_off)

## define the model you want to fit (negative binomial) 
model <- glm_nb_model(
  ## set number of cases as outcome of interest
  case_int ~
    ## use epiweek to account for the trend
    epiweek +
    ## use the furier terms to account for seasonality
    fourier
)

# define which data to use for fitting and which for predicting
fitting_data <- dat1
pred_data <- dat2

# fit model 
fitted_model <- trending::fit(model, fitting_data)
fitted_model <- fitted_model[[1]][[1]]

# forecast with data want to predict with 
forecasts <- pred_data %>%
  mutate(estimate = fitted_model %>% 
  predict(pred_data, simulate_pi = FALSE)) %>% 
  ## only keep the week and the forecast estimate
  select(epiweek, estimate)

}

fitted_model %>% 
  predict(pred_data, simulate_pi = FALSE) %>% 
  ## only keep the week and the forecast estimate
  select(epiweek, estimate)
