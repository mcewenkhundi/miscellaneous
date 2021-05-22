library(tidyverse)
library(modelr)
options(na.action = na.warn)

library(nycflights13)
library(lubridate)

#Why are low quality diamonds more expensive?
#low quality diamonds (poor cuts, bad colours, and inferior clarity) have higher prices.
#Note that the worst diamond color is J (slightly yellow), and the worst clarity is I1

View(diamonds)

ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()

#Price and carat
# The weight of the diamond is the single most important factor for determining 
# the price of the diamond, and lower quality diamonds tend to be larger

ggplot(diamonds, aes(carat, price)) + 
  geom_hex(bins = 50)

# We can make it easier to see how the other attributes of a diamond affect its
# relative price by fitting a model to separate out the effect of carat.
# But first, lets make a couple of tweaks to the diamonds dataset to make
# it easier to work with:
#   
# Focus on diamonds smaller than 2.5 carats (99.7% of the data)
# Log-transform the carat and price variables.

diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))

#Together, these changes make it easier to see the relationship between carat and price:
  
ggplot(diamonds2, aes(lcarat, lprice)) + 
geom_hex(bins = 50)

# The log-transformation is particularly useful here because it makes the pattern 
# linear, and linear patterns are the easiest to work with. Let’s take the next 
# step and remove that strong linear pattern. We first make the pattern explicit
# by fitting a model:

mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)

# Then we look at what the model tells us about the data. Note that I back 
# transform the predictions, undoing the log transformation, so I can
# overlay the predictions on the raw data:

grid <- diamonds2 %>% 
  data_grid(carat = seq_range(carat, 20)) %>% 
  mutate(lcarat = log2(carat)) %>% 
  add_predictions(mod_diamond, "lprice") %>% 
  mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, price)) + 
  geom_hex(bins = 50) + 
  geom_line(data = grid, colour = "red", size = 1)

# That tells us something interesting about our data. If we believe our model, 
# then the large diamonds are much cheaper than expected. This is probably 
# because no diamond in this dataset costs more than $19,000.
# 
# Now we can look at the residuals, which verifies that we’ve successfully
# removed the strong linear pattern:
  
diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond, "lresid")

ggplot(diamonds2, aes(lcarat, lresid)) + 
  geom_hex(bins = 50)

#Importantly, we can now re-do our motivating plots using those 
#residuals instead of price.

ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()

# A more complicated model
# If we wanted to, we could continue to build up our model, moving the effects 
# we’ve observed into the model to make them explicit. For example, we could 
# include color, cut, and clarity into the model so that we also make explicit
# the effect of these three categorical variables:
   
mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)

# This model now includes four predictors, so it’s getting harder to visualise. 
# Fortunately, they’re currently all independent which means that we can plot 
# them individually in four plots. To make the process a little easier, 
# we’re going to use the .model argument to data_grid:
  
grid <- diamonds2 %>% 
  data_grid(cut, .model = mod_diamond2) %>% 
  add_predictions(mod_diamond2)
grid

ggplot(grid, aes(cut, pred)) + 
  geom_point()

# If the model needs variables that you haven’t explicitly supplied, data_grid() 
# will automatically fill them in with “typical” value. For continuous variables, 
# it uses the median, and categorical variables it uses the most common value 
# (or values, if there’s a tie)
diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond2, "lresid2")

ggplot(diamonds2, aes(lcarat, lresid2)) + 
  geom_hex(bins = 50)

# This plot indicates that there are some diamonds with quite large 
# residuals - remember a residual of 2 indicates that the diamond is 4x 
# the price that we expected. It’s often useful to look at unusual 
# values individually:
  
diamonds2 %>% 
  filter(abs(lresid2) > 1) %>% 
  add_predictions(mod_diamond2) %>% 
  mutate(pred = round(2 ^ pred)) %>% 
  select(price, pred, carat:table, x:z) %>% 
  arrange(price)

# Nothing really jumps out at me here, but it’s probably worth spending time 
# considering if this indicates a problem with our model, or if there are 
# errors in the data. If there are mistakes in the data, this could be an
# opportunity to buy diamonds that have been priced low incorrectly.

#24.3 What affects the number of daily flights?
# Let’s work through a similar process for a dataset that seems even simpler
# at first glance: the number of flights that leave NYC per day. 
# This is a really small dataset — only 365 rows and 2 columns — 
# and we’re not going to end up with a fully realised model, but
# as you’ll see, the steps along the way will help us better
# understand the data. Let’s get started by counting the number
# of flights per day and visualising it with ggplot2.


daily <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(n = n())

ggplot(daily, aes(date, n)) + 
  geom_line()

# 24.3.1 Day of week
# Understanding the long-term trend is challenging because there’s a very strong
# day-of-week effect that dominates the subtler patterns. Let’s start by looking
# at the distribution of flight numbers by day-of-week:
   
daily <- daily %>% 
  mutate(wday = wday(date, label = TRUE))

ggplot(daily, aes(wday, n)) + 
  geom_boxplot()

# There are fewer flights on weekends because most travel is for business.
# The effect is particularly pronounced on Saturday: you might sometimes
# leave on Sunday for a Monday morning meeting, but it’s very rare that 
# you’d leave on Saturday as you’d much rather be at home with your family.
# 
# One way to remove this strong pattern is to use a model. First, we
# fit the model, and display its predictions overlaid on the original data:
mod <- lm(n ~ wday, data = daily)

grid <- daily %>% 
  data_grid(wday) %>% 
  add_predictions(mod, "n")

ggplot(daily, aes(wday, n)) + 
  geom_boxplot() +
  geom_point(data = grid, colour = "red", size = 4)

#Next we compute and visualise the residuals:
  
daily <- daily %>% 
  add_residuals(mod)

daily %>% 
  ggplot(aes(date, resid)) + 
  geom_ref_line(h = 0) + 
  geom_line()

# Note the change in the y-axis: now we are seeing the deviation from the expected
# number of flights, given the day of week. This plot is useful because now that
# we’ve removed much of the large day-of-week effect, we can see some of the 
# subtler patterns that remain:
# Our model seems to fail starting in June: you can still see a strong regular
# pattern that our model hasn’t captured. Drawing a plot with one line for 
# each day of the week makes the cause easier to see:
 
  ggplot(daily, aes(date, resid, colour = wday)) + 
  geom_ref_line(h = 0) + 
  geom_line()
  
# Our model fails to accurately predict the number of flights on Saturday: 
#   during summer there are more flights than we expect, and during Fall 
# there are fewer. We’ll see how we can do better to capture this 
# pattern in the next section.
#   
# There are some days with far fewer flights than expected:
    
daily %>% 
  filter(resid < -100)

# If you’re familiar with American public holidays, you might spot New Year’s 
# day, July 4th, Thanksgiving and Christmas. There are some others that don’t 
# seem to correspond to public holidays. You’ll work on those in one of the 
# exercises.
# 
# There seems to be some smoother long term trend over the course of a year.
# We can highlight that trend with geom_smooth():

daily %>% 
  ggplot(aes(date, resid)) + 
  geom_ref_line(h = 0) + 
  geom_line(colour = "grey50") + 
  geom_smooth(se = FALSE, span = 0.20)

# There are fewer flights in January (and December), and more in summer (May-Sep).
# We can’t do much with this pattern quantitatively, because we only have a single
# year of data. But we can use our domain knowledge to brainstorm potential
# explanations.

# 24.3.2 Seasonal Saturday effect
# Let’s first tackle our failure to accurately predict the number of flights
# on Saturday. A good place to start is to go back to the raw numbers, 
# focussing on Saturdays:
  
daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n)) + 
  geom_point() + 
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

# (I’ve used both points and lines to make it more clear what is data 
#   and what is interpolation.)
# I suspect this pattern is caused by summer holidays: many people go on holiday 
# in the summer, and people don’t mind travelling on Saturdays for vacation.
# Looking at this plot, we might guess that summer holidays are from early 
# June to late August. That seems to line up fairly well with the state’s 
# school terms: summer break in 2013 was Jun 26–Sep 9.
# 
# Why are there more Saturday flights in the Spring than the Fall? I 
# asked some American friends and they suggested that it’s less common to plan
# family vacations during the Fall because of the big Thanksgiving and Christmas 
# holidays. We don’t have the data to know for sure, but it seems like a plausible
# working hypothesis.
# 
# Lets create a “term” variable that roughly captures the three school terms, and 
# check our work with a plot:
  
  term <- function(date) {
    cut(date, 
        breaks = ymd(20130101, 20130605, 20130825, 20140101),
        labels = c("spring", "summer", "fall") 
    )
  }

daily <- daily %>% 
  mutate(term = term(date)) 

daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n, colour = term)) +
  geom_point(alpha = 1/3) + 
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

# (I manually tweaked the dates to get nice breaks in the plot. Using a
#   visualisation to help you understand what your function is doing is 
#   a really powerful and general technique.)
# It’s useful to see how this new variable affects the other days of the week:
  
  daily %>% 
  ggplot(aes(wday, n, colour = term)) +
  geom_boxplot()

# It looks like there is significant variation across the terms, so fitting a
# separate day of week effect for each term is reasonable. This improves our 
# model, but not as much as we might hope:
    
mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)
  
daily %>% 
  gather_residuals(without_term = mod1, with_term = mod2) %>% 
    ggplot(aes(date, resid, colour = model)) +
    geom_line(alpha = 0.75)

# We can see the problem by overlaying the predictions from the model on to the raw data:
  
grid <- daily %>% 
  data_grid(wday, term) %>% 
  add_predictions(mod2, "n")

ggplot(daily, aes(wday, n)) +
  geom_boxplot() + 
  geom_point(data = grid, colour = "red") + 
  facet_wrap(~ term)

# Our model is finding the mean effect, but we have a lot of big outliers, so
# mean tends to be far away from the typical value. We can alleviate this 
# problem by using a model that is robust to the effect of outliers: MASS::rlm().
# This greatly reduces the impact of the outliers on our estimates, and gives 
# a model that does a good job of removing the day of week pattern:
  
mod3 <- MASS::rlm(n ~ wday * term, data = daily)

daily %>% 
  add_residuals(mod3, "resid") %>% 
  ggplot(aes(date, resid)) + 
  geom_hline(yintercept = 0, size = 2, colour = "white") + 
  geom_line()

# It’s now much easier to see the long-term trend, and the positive and 
# negative outliers.

# 24.3.3 Computed variables
# If you’re experimenting with many models and many visualisations, it’s a 
# good idea to bundle the creation of variables up into a function so there’s
# no chance of accidentally applying a different transformation in different 
# places. For example, we could write:
  
  compute_vars <- function(data) {
    data %>% 
      mutate(
        term = term(date), 
        wday = wday(date, label = TRUE)
      )
  }
#Another option is to put the transformations directly in the model formula:
  
wday2 <- function(x) wday(x, label = TRUE)
mod3 <- lm(n ~ wday2(date) * term(date), data = daily)

# Either approach is reasonable. Making the transformed variable explicit is 
# useful if you want to check your work, or use them in a visualisation. 
# But you can’t easily use transformations (like splines) that return multiple
# columns. Including the transformations in the model function makes life a 
# little easier when you’re working with many different datasets because the
# model is self contained.

# 24.3.4 Time of year: an alternative approach
# In the previous section we used our domain knowledge (how the US school term 
#                                                       affects travel) 
# to improve the model. An alternative to using our knowledge explicitly 
# in the model is to give the data more room to speak. We could use a
# more flexible model and allow that to capture the pattern we’re interested in.
# A simple linear trend isn’t adequate, so we could try using a natural spline 
# to fit a smooth curve across the year:
  
library(splines)
mod <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)

daily %>% 
  data_grid(wday, date = seq_range(date, n = 13)) %>% 
  add_predictions(mod) %>% 
  ggplot(aes(date, pred, 
             colour = wday)) + 
  geom_line() +
  geom_point()


# We see a strong pattern in the numbers of Saturday flights. This is 
# reassuring, because we also saw that pattern in the raw data. It’s a good 
# sign when you get the same signal from different approaches.



















































































































































