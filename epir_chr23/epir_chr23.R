#epir_chr23
#time series analysis

pacman::p_load(rio,          # File import
               here,         # File locator
               tidyverse,    # data management + ggplot2 graphics
               tsibble,      # handle time series datasets
               slider,       # for calculating moving averages
               imputeTS,     # for filling in missing values
               feasts,       # for time series decomposition and autocorrelation
               forecast,     # fit sin and cosin terms to data (note: must load after feasts)
               trending,     # fit and assess models 
               tmaptools,    # for getting geocoordinates (lon/lat) based on place names
               ecmwfr,       # for interacting with copernicus sateliate CDS API
               stars,        # for reading in .nc (climate data) files
               units,        # for defining units of measurement (climate data)
               yardstick,    # for looking at model accuracy
               surveillance  # for aberration detection
)

# import the counts into R
counts <- rio::import("campylobacter_germany.xlsx")

## ensure the date column is in the appropriate format
counts$date <- as.Date(counts$date)

## create a calendar week variable 
## fitting ISO definitons of weeks starting on a monday
counts <- counts %>% 
  mutate(epiweek = yearweek(date, week_start = 1))

#load the climate data
## define path to weather folder 
file_paths <- list.files(
  here::here("weather"), # replace with your own file path 
  full.names = TRUE)

## only keep those with the current name of interest 
file_paths <- file_paths[str_detect(file_paths, "germany")]

## read in all the files as a stars object 
data <- stars::read_stars(file_paths)

## change to a data frame 
temp_data <- as_tibble(data) %>% 
  ## add in variables and correct units
  mutate(
    ## create an calendar week variable 
    epiweek = tsibble::yearweek(time), 
    ## create a date variable (start of calendar week)
    date = as.Date(epiweek),
    ## change temperature from kelvin to celsius
    t2m = set_units(t2m, celsius), 
    ## change precipitation from metres to millimetres 
    tp  = set_units(tp, mm)) %>% 
  ## group by week (keep the date too though)
  group_by(epiweek, date) %>% 
  ## get the average per week
  summarise(t2m = as.numeric(mean(t2m)), 
            tp = as.numeric(mean(tp)))

#from the tidyverts set of packages
## define time series object 
counts <- tsibble(counts, index = epiweek)
epiweek <- tsibble(counts, index = epiweek)
#You can take a quick look at your data by using ggplot2.
#We see from the plot that there is a clear seasonal pattern, 
#and that there are no missings. However, there seems to be
#an issue with reporting at the beginning of each year; 
#cases drop in the last week of the year and then increase
#for the first week of the next year.

## plot a line graph of cases by week
ggplot(counts, aes(x = epiweek, y = case)) + 
  geom_line()

#tsibble does not allow duplicate obs
## get a vector of TRUE/FALSE whether rows are duplicates
are_duplicated(counts, index = epiweek) 

## get a data frame of any duplicated rows 
duplicates(counts, index = epiweek) 

#Missings
#We saw from our brief inspection above that there are no missings, 
#but we also saw there seems to be a problem with reporting delay around 
#new year. One way to address this problem could be to set these values to 
#missing and then to impute values. The simplest form of time series 
#imputation is to draw a straight line between the last non-missing and 
#the next non-missing value. To do this we will use the imputeTS package 
#function na_interpolation().
#See the Missing data page for other options for imputation.
#Another alternative would be to calculate a moving average, to try 
#and smooth over these apparent reporting issues (see next section, and
#the page on Moving averages).

## create a variable with missings instead of weeks with reporting issues
counts <- counts %>% 
  mutate(case_miss = if_else(
    ## if epiweek contains 52, 53, 1 or 2
    str_detect(epiweek, "W51|W52|W53|W01|W02"), 
    ## then set to missing 
    NA_real_, 
    ## otherwise keep the value in case
    case
  ))

## alternatively interpolate missings by linear trend 
## between two nearest adjacent points
counts <- counts %>% 
  mutate(case_int = imputeTS::na_interpolation(case_miss)
  )

## to check what values have been imputed compared to the original
ggplot_na_imputations(counts$case_miss, counts$case_int) + 
  ## make a traditional plot (with black axes and white background)
  theme_classic()

#Moving averages
#if data is very noisy (counts jumping up and down) then it can be helpful to 
#calculate a moving average. In the example below, for each week we calculate 
#the average number of cases from the four previous weeks. This smooths the data,
#to make it more interpretable. In our case this does not really add much, so we
#will stick to the interpolated data for further analysis

## create a moving average variable (deals with missings)
counts <- counts %>% 
  ## create the ma_4w variable 
  ## slide over each row of the case variable
  mutate(ma_4wk = slider::slide_dbl(case, 
                                    ## for each row calculate the name
                                    ~ mean(.x, na.rm = TRUE),
                                    ## use the four previous weeks
                                    .before = 4))

## make a quick visualisation of the difference 
ggplot(counts, aes(x = epiweek)) + 
  geom_line(aes(y = case)) + 
  geom_line(aes(y = ma_4wk), colour = "red")

#Periodicity 
#Define a function
## Function arguments
#####################
## x is a dataset
## counts is variable with count data or rates within x 
## start_week is the first week in your dataset
## period is how many units in a year 
## output is whether you want return spectral periodogram or the peak weeks
## "periodogram" or "weeks"

# Define function
periodogram <- function(x, 
                        counts, 
                        start_week = c(2002, 1), 
                        period = 52, 
                        output = "weeks") {
  
  
  ## make sure is not a tsibble, filter to project and only keep columns of interest
  prepare_data <- dplyr::as_tibble(x)
  
  # prepare_data <- prepare_data[prepare_data[[strata]] == j, ]
  prepare_data <- dplyr::select(prepare_data, {{counts}})
  
  ## create an intermediate "zoo" time series to be able to use with spec.pgram
  zoo_cases <- zoo::zooreg(prepare_data, 
                           start = start_week, frequency = period)
  
  ## get a spectral periodogram not using fast fourier transform 
  periodo <- spec.pgram(zoo_cases, fast = FALSE, plot = FALSE)
  
  ## return the peak weeks 
  periodo_weeks <- 1 / periodo$freq[order(-periodo$spec)] * period
  
  if (output == "weeks") {
    periodo_weeks
  } else {
    periodo
  }
  
}

## get spectral periodogram for extracting weeks with the highest frequencies 
## (checking of seasonality) 
periodo <- periodogram(counts, 
                       case_int, 
                       start_week = c(2002, 1),
                       output = "periodogram")

## pull spectrum and frequence in to a dataframe for plotting
periodo <- data.frame(periodo$freq, periodo$spec)

## plot a periodogram showing the most frequently occuring periodicity 
ggplot(data = periodo, 
       aes(x = 1/(periodo.freq/52),  y = log(periodo.spec))) + 
  geom_line() + 
  labs(x = "Period (Weeks)", y = "Log(density)")

## get a vector weeks in ascending order 
peak_weeks <- periodogram(counts, 
                          case_int, 
                          start_week = c(2002, 1), 
                          output = "weeks")

#Decomposition
#Classical decomposition is used to break a time series down several parts, 
#which when taken together make up for the pattern you see. These different parts are:
#The trend-cycle (the long-term direction of the data)
#The seasonality (repeating patterns)

## decompose the counts dataset 
counts %>% 
  # using an additive classical decomposition model
  model(classical_decomposition(case_int, type = "additive")) %>% 
  ## extract the important information from the model
  components() %>% 
  ## generate a plot 
  autoplot()
#The random (what is left after removing trend and season)

#Autocorrelation
#Autocorrelation tells you about the relation between the counts of each week 
#and the weeks before it (called lags).
## using the counts dataset
counts %>% 
  ## calculate autocorrelation using a full years worth of lags
  ACF(case_int, lag_max = 52) %>% 
  ## show a plot
  autoplot()

## using the counts data set 
counts %>% 
  ## calculate the partial autocorrelation using a full years worth of lags
  PACF(case_int, lag_max = 52) %>% 
  ## show a plot
  autoplot()

## test for independance 
Box.test(counts$case_int, type = "Ljung-Box")

## add in fourier terms using the epiweek and case_int variables
counts$fourier <- select(counts, epiweek, case_int) %>% 
  fourier(K = 1)

#From the trending package
## define the model you want to fit (negative binomial) 
model <- glm_nb_model(
  ## set number of cases as outcome of interest
  case_int ~
    ## use epiweek to account for the trend
    epiweek +
    ## use the fourier terms to account for seasonality
    fourier)

## fit your model using the counts dataset
#source of error in the book
class(counts)
#remove the tsible class

fitted_model <- fit(model, counts)


#fitted_model <- fitted_model[[1]][[1]]

## calculate confidence intervals and prediction intervals 
#works despite the error message
observed <- predict(fitted_model, simulate_pi = FALSE)
observed <- observed[[1]][[1]] 
View(observed)


observed <- observed[[1]][[1]] # thats what is in the object
## plot your regression 
ggplot(data = observed, aes(x = epiweek)) + 
  ## add in a line for the model estimate
  geom_line(aes(y = estimate),
            col = "Red") + 
  ## add in a band for the prediction intervals 
    geom_ribbon(aes(ymin = lower_pi, 
                  ymax = upper_pi), fill = "blue", 
              alpha = 0.25) + 
  geom_ribbon(aes(ymin = lower_ci, 
                  ymax = upper_ci), 
              alpha = 0.30) + 
  ## add in a line for your observed case counts
  geom_line(aes(y = case_int), 
            col = "black") + 
  ## make a traditional plot (with black axes and white background)
  theme_classic()

# Residuals
# To see how well our model fits the observed data we need to look at the residuals.
# The residuals are the difference between the observed counts and the counts 
# estimated from the model. We could calculate this simply by using case_int - 
#   estimate, but the residuals() function extracts this directly from the regression for us.
# 
# What we see from the below, is that we are not explaining all of the variation 
#that we could with the model. It might be that we should fit more fourier terms,
#and address the amplitude. However for this example we will leave it as is. The 
#plots show that our model does worse in the peaks and troughs (when counts are 
#at their highest and lowest) and that it might be more likely to underestimate 
#the observed counts.

## calculate the residuals 
fitted_model <- fitted_model[[1]][[1]]
observed <- observed %>% 
  mutate(resid = residuals(fitted_model, type = "response"))

## are the residuals fairly constant over time (if not: outbreaks? change in practice?)
observed %>%
  ggplot(aes(x = epiweek, y = resid)) +
  geom_line() +
  geom_point() + 
  labs(x = "epiweek", y = "Residuals")

## is there autocorelation in the residuals (is there a pattern to the error?)  
observed %>% 
  as_tsibble(index = epiweek) %>% 
  ACF(resid, lag_max = 52) %>% 
  autoplot()

## are residuals normally distributed (are under or over estimating?)  
observed %>%
  ggplot(aes(x = resid)) +
  geom_histogram(binwidth = 100) +
  geom_rug() +
  labs(y = "count") 

## compare observed counts to their residuals 
## should also be no pattern 
observed %>%
  ggplot(aes(x = estimate, y = resid)) +
  geom_point() +
  labs(x = "Fitted", y = "Residuals")

## formally test autocorrelation of the residuals
## H0 is that residuals are from a white-noise series (i.e. random)
## test for independence 
## if p value significant then non-random
Box.test(observed$resid, type = "Ljung-Box")

#Relation of two time series
#Here we look at using weather data (specifically the temperature) to explain
#campylobacter case counts.

## left join so that we only have the rows already existing in counts
## drop the date variable from temp_data (otherwise is duplicated)
counts <- left_join(counts, 
                    select(temp_data, -date),
                    by = "epiweek")

# First plot your data to see if there is any obvious relation.
# The plot below shows that there is a clear relation in 
# the seasonality of the two variables, and that temperature might peak a few weeks before the case number. For more on pivoting data, see the handbook section on pivoting 
# data.
counts %>% 
  ## keep the variables we are interested 
  select(epiweek, case_int, t2m) %>% 
  ## change your data in to long format
  pivot_longer(
    ## use epiweek as your key
    !epiweek,
    ## move column names to the new "measure" column
    names_to = "measure", 
    ## move cell values to the new "values" column
    values_to = "value") %>% 
  ## create a plot with the dataset above
  ## plot epiweek on the x axis and values (counts/celsius) on the y 
  ggplot(aes(x = epiweek, y = value)) + 
  ## create a separate plot for temperate and case counts 
  ## let them set their own y-axes
  facet_grid(measure ~ ., scales = "free_y") +
  ## plot both as a line
  geom_line()

# Lags and cross-correlation
# To formally test which weeks are most highly related between
# cases and temperature. We can use the cross-correlation 
# function (CCF()) from the feasts package. You could also
# visualise (rather than using arrange) using the autoplot()
# function.

counts %>% 
  ## calculate cross-correlation between interpolated counts and temperature
  CCF(case_int, t2m,
      ## set the maximum lag to be 52 weeks
      lag_max = 52, 
      ## return the correlation coefficient 
      type = "correlation") %>% 
  autoplot()
  ## arange in decending order of the correlation coefficient 
  ## show the most associated lags
  as_tibble() %>% #not available in the book
  arrange(-ccf) %>% 
  ## only show the top ten 
  slice_head(n = 10)

#We see from this that a lag of 4 weeks is most highly correlated, so we make a
#lagged temperature variable to include in our regression.
counts <- counts %>% 
  ## create a new variable for temperature lagged by four weeks
  mutate(t2m_lag4 = lag(t2m, n = 4))

#Negative binomial with two variables
#We fit a negative binomial regression as done previously. This time we add the
#temperature variable lagged by four weeks.
## define the model you want to fit (negative binomial) 
model <- glm_nb_model(
  ## set number of cases as outcome of interest
  case_int ~
    ## use epiweek to account for the trend
    epiweek +
    ## use the fourier terms to account for seasonality
    fourier + 
    ## use the temperature lagged by four weeks 
    t2m_lag4
)

## fit your model using the counts dataset
fitted_model <- trending::fit(model, counts)

## calculate confidence intervals and prediction intervals 
observed <- predict(fitted_model, simulate_pi = FALSE)

observed <- observed[[1]][[1]]

# To investigate the individual terms, we can pull the original negative binomial 
# regression out of the trending format using get_model() and pass this to the broom
# package tidy() function to retrieve exponentiated estimates and associated confidence intervals.
# 
# What this shows us is that lagged temperature, after controlling for trend and 
#seasonality, is similar to the case counts (estimate ~ 1) and significantly associated. 
#This suggests that it might be a good variable for use in predicting future case 
#numbers (as climate forecasts are readily available).

fitted_model[[1]][[1]] %>% 
  ## extract original negative binomial regression
  #get_model() %>% 
  ## get a tidy dataframe of results
  tidy(exponentiate = TRUE, 
       conf.int = TRUE)

#A quick visual inspection of the model shows that it might do a better 
#job of estimating the observed case counts.

## plot your regression 
ggplot(data = observed, aes(x = epiweek)) + 
  ## add in a line for the model estimate
  geom_line(aes(y = estimate),
            col = "Red") + 
  ## add in a band for the prediction intervals 
  geom_ribbon(aes(ymin = lower_pi, 
                  ymax = upper_pi), 
              alpha = 0.2) + 
  geom_ribbon(aes(ymin = lower_ci, 
                  ymax = upper_ci),fill = "blue", 
              alpha = 0.25) +
  ## add in a line for your observed case counts
  geom_line(aes(y = case_int), 
            col = "black") + 
  ## make a traditional plot (with black axes and white background)
  theme_classic()

#Residuals
#We investigate the residuals again to see how well our model fits the observed data.
#The results and interpretation here are similar to those of the previous regression,
#so it may be more feasible to stick with the simpler model without temperature.

## calculate the residuals 
observed <- observed %>% 
  mutate(resid = case_int - estimate)

## are the residuals fairly constant over time (if not: outbreaks? change in practice?)
observed %>%
  ggplot(aes(x = epiweek, y = resid)) +
  geom_line() +
  geom_point() + 
  labs(x = "epiweek", y = "Residuals")

## is there autocorelation in the residuals (is there a pattern to the error?)  
observed %>% 
  as_tsibble(index = epiweek) %>% 
  ACF(resid, lag_max = 52) %>% 
  autoplot()

## are residuals normally distributed (are under or over estimating?)  
observed %>%
  ggplot(aes(x = resid)) +
  geom_histogram(binwidth = 100) +
  geom_rug() +
  labs(y = "count") 

## compare observed counts to their residuals 
## should also be no pattern 
observed %>%
  ggplot(aes(x = estimate, y = resid)) +
  geom_point() +
  labs(x = "Fitted", y = "Residuals")

## formally test autocorrelation of the residuals
## H0 is that residuals are from a white-noise series (i.e. random)
## test for independence 
## if p value significant then non-random
Box.test(observed$resid, type = "Ljung-Box")

# Outbreak detection
# We will demonstrate two (similar) methods of detecting outbreaks here. The first
# builds on the sections above. We use the trending package to fit regressions to 
# previous years, and then predict what we expect to see in the following year. If
# observed counts are above what we expect, then it could suggest there is an outbreak.
# The second method is based on similar principles but uses the surveillance package, 
# which has a number of different algorithms for aberration detection.
# CAUTION: Normally, you are interested in the current year (where you only know 
# counts up to the present week). So in this example we are pretending to be in 
# week 39 of 2011.

# trending package
# For this method we define a baseline (which should usually be about 5 years of data). We fit a regression to the baseline data, and then use that to predict the estimates for the next year.
# Cut-off date
# It is easier to define your dates in one place and then use these throughout the rest of your code.
# Here we define a start date (when our observations started) and a cut-off date 
#(the end of our baseline period - and when the period we want to predict for starts).
#~We also define how many weeks are in our year of interest (the one we are going to be predicting)~. 
#We also define how many weeks are between our baseline cut-off and the end date that we are interested in predicting for.
#NOTE: In this example we pretend to currently be at the end of September 2011 (“2011 W39”).

## define start date (when observations began)
start_date <- min(counts$epiweek)

## define a cut-off week (end of baseline, start of prediction period)
cut_off <- yearweek("2010-12-31")

## define the last date interested in (i.e. end of prediction)
end_date <- yearweek("2011-12-31")

## find how many weeks in period (year) of interest
num_weeks <- as.numeric(end_date - cut_off)

#Add rows
#To be able to forecast in a tidyverse format, we need to have the right number of rows in our dataset, i.e. one row for each week up to the end_datedefined above. The code below allows you to add these rows for by a grouping variable - for example if we had multiple countries in one dataset, we could group by country and then add rows appropriately for each. The group_by_key() function from tsibble allows us to do this grouping and then pass the grouped data to dplyr functions, group_modify() and add_row(). Then we specify the sequence of weeks between one after the maximum week currently available in the data and the end week.

## add in missing weeks till end of year 
counts <- counts %>%
  ## group by the region
  group_by_key() %>%
  ## for each group add rows from the highest epiweek to the end of year
  group_modify(~add_row(.,
                        epiweek = seq(max(.$epiweek) + 1, 
                                      end_date,
                                      by = 1)))

#Fourier terms
#We need to redefine our fourier terms - as we want to fit them to the baseline 
#date only and then predict (extrapolate) those terms for the next year. To do 
#this we need to combine two output lists from the fourier() function together; the first one is for the baseline data, and the second one predicts for the year of interest (by defining the h argument).

#N.b. to bind rows we have to use rbind() (rather than tidyverse bind_rows) as the fourier columns are a list (so not named individually).
## define fourier terms (sincos) 
counts <- counts %>% 
  mutate(
    ## combine fourier terms for weeks prior to  and after 2010 cut-off date
    ## (nb. 2011 fourier terms are predicted)
    fourier = rbind(
      ## get fourier terms for previous years
      fourier(
        ## only keep the rows before 2011
        filter(counts, 
               epiweek <= cut_off), 
        ## include one set of sin cos terms 
        K = 1
      ), 
      ## predict the fourier terms for 2011 (using baseline data)
      fourier(
        ## only keep the rows before 2011
        filter(counts, 
               epiweek <= cut_off),
        ## include one set of sin cos terms 
        K = 1, 
        ## predict 52 weeks ahead
        h = num_weeks
      )
    )
  )

# Split data and fit regression
# We now have to split our dataset in to the baseline period and the prediction period. 
#This is done using the dplyr group_split() function after group_by(), and will create a list with two data frames, one for before your cut-off and one for after.
# 
# We then use the purrr package pluck() function to pull the datasets out of the 
#list (equivalent of using square brackets, e.g. dat[[1]]), and can then fit our
#model to the baseline data, and then use the predict() function for our data of interest after the cut-off.
# 
# See the page on Iteration, loops, and lists to learn more about purrr.
# 
# CAUTION: Note the use of simulate_pi = FALSE within the predict() argument. This is because the default behaviour of trending is to use the ciTools package to estimate a prediction interval. This does not work if there are NA counts, and also produces more granular intervals. See ?trending::predict.trending_model_fit for details.
# split data for fitting and prediction
# dat <- counts %>%
#   group_by(epiweek <= cut_off) %>%
#   group_split()

dat1 <- counts %>%
  group_by(epiweek <= cut_off) 

dat2 <- counts %>%
  group_by(epiweek > cut_off)

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
# fitting_data <- pluck(dat, 2)
# pred_data <- pluck(dat, 1) %>% 
#   select(case_int, epiweek, fourier)

# fit model 
# fitted_model <- trending::fit(model, fitting_data)
fitted_model <- trending::fit(model, dat1)
# get confint and estimates for fitted data
observed <- fitted_model %>% 
  predict(simulate_pi = FALSE)
observed <- observed[[1]][[1]]

# forecast with data want to predict with 
# forecasts <- fitted_model %>% 
#   predict(pred_data, simulate_pi = FALSE)
 forecasts <- fitted_model %>% 
  predict(dat2, simulate_pi = FALSE)

 forecasts <- forecasts[[1]][[1]]
 
## combine baseline and predicted datasets
observed <- bind_rows(observed, forecasts)

#As previously, we can visualise our model with ggplot. We highlight alerts with red dots for observed counts above the 95% prediction interval. This time we also add a vertical line to label when the forecast starts.

## plot your regression 
ggplot(data = observed, aes(x = epiweek)) + 
  ## add in a line for the model estimate
  geom_line(aes(y = estimate),
            col = "grey") + 
  ## add in a band for the prediction intervals 
  geom_ribbon(aes(ymin = lower_pi, 
                  ymax = upper_pi), 
              alpha = 0.25) + 
  ## add in a line for your observed case counts
  geom_line(aes(y = case_int), 
            col = "black") + 
  ## plot in points for the observed counts above expected
  geom_point(
    data = filter(observed, case_int > upper_pi), 
    aes(y = case_int), 
    colour = "red", 
    size = 2) + 
  ## add vertical line and label to show where forecasting started
  geom_vline(
    xintercept = as.Date(cut_off), 
    linetype = "dashed") + 
  annotate(geom = "text", 
           label = "Forecast", 
           x = cut_off, 
           y = max(observed$upper_pi) - 250, 
           angle = 90, 
           vjust = 1
  ) + 
  ## make a traditional plot (with black axes and white background)
  theme_classic()

#Prediction validation
#An alternative is to use a method called cross-validation. In this scenario you
#roll over all of the data available to fit multiple models to predict one year ahead. 
#You use more and more data in each model, as seen in the figure below from the same 
#[Hyndman et al text]((https://otexts.com/fpp3/). For example, the first model uses
#2002 to predict 2003, the second uses 2002 and 2003 to predict 2004, and so on.
## Cross validation: predicting week(s) ahead based on sliding window

## expand your data by rolling over in 52 week windows (before + after) 
## to predict 52 week ahead
## (creates longer and longer chains of observations - keeps older data)

## define window want to roll over
roll_window <- 52

## define weeks ahead want to predict 
weeks_ahead <- 52

## create a data set of repeating, increasingly long data
## label each data set with a unique id
## only use cases before year of interest (i.e. 2011)
case_roll <- counts %>% 
  filter(epiweek < cut_off) %>% 
  ## only keep the week and case counts variables
  as_tibble() %>% #do this line for it to work
  select(epiweek, case_int) %>% 
  slice(1:(n() - weeks_ahead)) %>%
  ## drop the last x observations 
  ## depending on how many weeks ahead forecasting 
  ## (otherwise will be an actual forecast to "unknown")
  slice(1:(n() - weeks_ahead)) %>%
  as_tsibble(index = epiweek) %>% 
  ## roll over each week in x after windows to create grouping ID 
  ## depending on what rolling window specify
  stretch_tsibble(.init = roll_window, .step = 1) %>% 
  ## drop the first couple - as have no "before" cases
  filter(.id > roll_window)


## for each of the unique data sets run the code below
forecasts <- purrr::map(unique(case_roll$.id), 
                        function(i) {
                          
                          ## only keep the current fold being fit 
                          mini_data <- filter(case_roll, .id == i) %>% 
                            as_tibble()
                          
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
                            select(epiweek, estimate) %>%
                            as_tibble()
                          
                        }
)

## make the list in to a data frame with all the forecasts
forecasts <- bind_rows(forecasts)

## join the forecasts with the observed
forecasts <- left_join(forecasts, 
                       select(counts, epiweek, case_int),
                       by = "epiweek")

## using {yardstick} compute metrics
## RMSE: Root mean squared error
## MAE:  Mean absolute error  
## MASE: Mean absolute scaled error
## MAPE: Mean absolute percent error
model_metrics <- bind_rows(
  ## in your forcasted dataset compare the observed to the predicted
  rmse(forecasts, case_int, estimate), 
  mae( forecasts, case_int, estimate),
  mase(forecasts, case_int, estimate),
  mape(forecasts, case_int, estimate),
) %>% 
  ## only keep the metric type and its output
  select(Metric  = .metric, 
         Measure = .estimate) %>% 
  ## make in to wide format so can bind rows after
  pivot_wider(names_from = Metric, values_from = Measure)

## return model metrics 
#results very different from the one in the book
#Something did not go well with the model
model_metrics


