library(tidyverse)
library(modelr)
options(na.action = na.warn)

#a simple model
#Lets look at the sim1 dataset from the modelr package
sim1

ggplot(sim1, aes(x, y)) +
  geom_point()

models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point() 

#Need to find a model that has the least distance between the observed y and
#the predicted y

#function for predicted y
model1 <- function(a, data) {
  a[1] + data$x*a[2]
}

model1(c(7, 1.5), sim1)

#root mean squared deviation
measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff^2))
}

measure_distance(c(7, 1.5), sim1)

#Now can use purrr to compute the distance for all the models
# a helper function
sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>%
          mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

#Lets overlay the 10 best models on the data
ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, colour = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist),
    data = filter(models, rank(dist) <= 10)
  )

#Visualising the models

ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))

#Another way to check models is by doing grid search
grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist)) 

#Taking the 10 best models on the original data
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(grid, rank(dist) <= 10)
  )

#You can also use the Newton-Raphson search to find the best model
# If you have a function that defines the distance between a model and a 
# dataset, an algorithm that can minimise that distance by modifying the 
# parameters of the model, you can find the best model. The neat thing 
# about this approach is that it will work for any family of models 
# that you can write an equation for.
best <- optim(c(0, 0), measure_distance, data = sim1)
best$par


ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2])

#The last option is the lm
sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)
#> (Intercept)           x 
#>    4.220822    2.051533

#Prediction
#Generate an evenly spaced grid of values that cover the region 
#where out data lies.
#The easiest way modelr::data_grid()
#Finds the unique values then generates all combinations

grid <- sim1 %>% 
        data_grid(x)

grid

#Takes the data and the model and adds a new column of predictions to the dataframe
grid <- grid %>% 
  add_predictions(sim1_mod) 
grid

ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)

#Residuals
sim1 <- sim1 %>% 
  add_residuals(sim1_mod)

#Can draw a freq poly to see how the residuals are
ggplot(sim1, aes(resid)) + 
  geom_freqpoly(binwidth = 0.5) 

#Looks like random noise suggesting the model has done a good job

ggplot(sim1, aes(x, resid)) + 
  geom_ref_line(h = 0) +
  geom_point() 

df <- tribble(
  ~y, ~x1, ~x2,
  4, 2, 5,
  5, 1, 6
)
df
model_matrix(df, y ~ x1)

#if you dont want the intercep
model_matrix(df, y ~ x1 - 1)
model_matrix(df, y ~ x1 + x2)

#Categorical data
mod2 <- lm(y ~ x, data = sim2)

grid <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod2)
grid

# predict the mean value for each category. (Why? Because the mean
# minimises the root-mean-squared distance.) That’s easy to see if we overlay 
# the predictions on top of the original data:
ggplot(sim2, aes(x)) + 
  geom_point(aes(y = y)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)

#cant make predictions of levels you did not observe
tibble(x = "e") %>% 
  add_predictions(mod2)

#Interactions (continuous and categorical)


ggplot(sim3, aes(x1, y)) + 
  geom_point(aes(colour = x2))


#There are two possible models you could fit to this data:
  
mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)


grid <- sim3 %>% 
  data_grid(x1, x2) %>% 
  gather_predictions(mod1, mod2)
grid


ggplot(sim3, aes(x1, y, colour = x2)) + 
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) + 
  facet_wrap(~ model)

#Which is the best models
sim3 <- sim3 %>% 
  gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, colour = x2)) + 
  geom_point() + 
  facet_grid(model ~ x2)

# Interactions (two continuous)
# Let’s take a look at the equivalent model for two continuous variables. 
#Initially things proceed almost identically to the previous example:
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

# Note my use of seq_range() inside data_grid(). Instead of using every unique
# value of x, I’m going to use a regularly spaced grid of five values between
# the minimum and maximum numbers.

grid <- sim4 %>% 
  data_grid(
    x1 = seq_range(x1, 5), 
    x2 = seq_range(x2, 5) 
  ) %>% 
  gather_predictions(mod1, mod2)
grid

# Next let’s try and visualise that model. We have two continuous predictors, 
# so you can imagine the model like a 3d surface. We could display that using 
# geom_tile():
  
  ggplot(grid, aes(x1, x2)) + 
  geom_tile(aes(fill = pred)) + 
  facet_wrap(~ model)

# That doesn’t suggest that the models are very different! But that’s partly an
# illusion: our eyes and brains are not very good at accurately comparing shades 
# of colour. Instead of looking at the surface from the top, we could look at it
# from either side, showing multiple slices:
    
    ggplot(grid, aes(x1, pred, colour = x2, group = x2)) + 
    geom_line() +
    facet_wrap(~ model)
    
  ggplot(grid, aes(x2, pred, colour = x1, group = x1)) + 
    geom_line() +
    facet_wrap(~ model)
  
  