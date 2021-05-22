library(modelr)
library(tidyverse)

# 25 Many models
# 25.1 Introduction
# In this chapter you’re going to learn three powerful ideas that help you to
# work with large numbers of models with ease:

# The gapminder data summarises the progression of countries over time, 
# looking at statistics like life expectancy and GDP. The data is easy to access 
# in R, thanks to Jenny Bryan who created the gapminder package:
  
library(gapminder)
View(gapminder)

# In this case study, we’re going to focus on just three variables to answer the 
# question “How does life expectancy (lifeExp) change over time (year) for each 
# country (country)?”. A good place to start is with a plot:

# This is a small dataset: it only has ~1,700 observations and 3 variables. But 
# it’s still hard to see what’s going on! Overall, it looks like life expectancy
# has been steadily improving. However, if you look closely, you might notice some
# countries that don’t follow this pattern. How can we make those countries easier 
# to see?
  
gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)

# One way is to use the same approach as in the last chapter: there’s a strong 
# signal (overall linear growth) that makes it hard to see subtler trends. We’ll
# tease these factors apart by fitting a model with a linear trend. The model 
# captures steady growth over time, and the residuals will show what’s left.
# 
# You already know how to do that if we had a single country:
   
nz <- filter(gapminder, country == "New Zealand")
nz %>% 
  ggplot(aes(year, lifeExp)) + 
  geom_line() + 
  ggtitle("Full data = ")

nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>% 
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) + 
  geom_line() + 
  ggtitle("Linear trend + ")

nz %>% 
  add_residuals(nz_mod) %>% 
  ggplot(aes(year, resid)) + 
  geom_hline(yintercept = 0, colour = "white", size = 3) + 
  geom_line() + 
  ggtitle("Remaining pattern")

#How can we easily fit that model to every country?
# 25.2.1 Nested data
# (I’m cheating a little by grouping on both continent and country. Given country,
#   continent is fixed, so this doesn’t add any more groups, but it’s an easy way 
#   to carry an extra variable along for the ride.)
by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()

by_country

# Unfortunately using str() is not recommended as it will often produce very long 
# output. But if you pluck out a single element from the data column you’ll see 
# that it contains all the data for that country (in this case, Afghanistan).

by_country$data[[1]]

# 25.2.2 List-columns
# Now that we have our nested data frame, we’re in a good position to fit some
# models. We have a model-fitting function:
  
country_model <- function(df) {
    lm(lifeExp ~ year, data = df)
}

# And we want to apply it to every data frame. The data frames are in a list, 
# so we can use purrr::map() to apply country_model to each element:
  
models <- map(by_country$data, country_model)

# In other words, instead of creating a new object in the global environment, 
# we’re going to create a new variable in the by_country data frame. That’s a 
# job for dplyr::mutate():
  
by_country <- by_country %>% 
  mutate(model = map(data, country_model))
by_country

# This has a big advantage: because all the related objects are stored together, 
# you don’t need to manually keep them in sync when you filter or arrange. The 
# semantics of the data frame takes care of that for you:
  
  by_country %>% 
  filter(continent == "Europe")

  by_country %>% 
    arrange(continent, country)
  
  by_country %>%
    ungroup() %>%
    count(continent)

# 25.2.3 Unnesting
# Previously we computed the residuals of a single model with a single dataset. 
# Now we have 142 data frames and 142 models. To compute the residuals, we need 
# to call add_residuals() with each model-data pair:
    
by_country <- by_country %>% 
    mutate(
      resids = map2(data, model, add_residuals)
    )
by_country

# Previously we used nest() to turn a regular data frame into an nested data 
# frame, and now we do the opposite with unnest():
  
resids <- unnest(by_country, resids)
resids

# Note that each regular column is repeated once for each row of the nested tibble.
# Now we have regular data frame, we can plot the residuals:
  
  resids %>% 
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)
  
#Facetting by continent is particularly revealing:
  # It looks like we’ve missed some mild patterns. There’s also something 
  # interesting going on in Africa: we see some very large residuals which 
  # suggests our model isn’t fitting so well there. We’ll explore that more 
  # in the next section, attacking it from a slightly different angle.
    
    resids %>% 
    ggplot(aes(year, resid, group = country)) +
    geom_line(alpha = 1 / 3) + 
    facet_wrap(~continent)

# 25.2.4 Model quality
# Instead of looking at the residuals from the model, we could look at some
# general measurements of model quality. You learned how to compute some 
# specific measures in the previous chapter. Here we’ll show a different 
# approach using the broom package. The broom package provides a general set of
# functions to turn models into tidy data. Here we’ll use broom::glance() to 
# extract some model quality metrics. If we apply it to a model, we get a data 
# frame with a single row:
#       
broom::glance(nz_mod)    

#We can use mutate() and unnest() to create a data frame with a row for each country:
  
  by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance)

# This isn’t quite the output we want, because it still includes all the list
# columns. This is default behaviour when unnest() works on single row data 
# frames. To suppress these columns we use .drop = TRUE:
    
glance <- by_country %>% 
    mutate(glance = map(model, broom::glance)) %>% 
    unnest(glance, .drop = TRUE)
    glance

# (Pay attention to the variables that aren’t printed: there’s a lot of useful
# stuff there.)
# With this data frame in hand, we can start to look for models that don’t fit well:
      
glance %>% 
  arrange(r.squared)

# The worst models all appear to be in Africa. Let’s double check that with a
# plot. Here we have a relatively small number of observations and a discrete 
# variable, so geom_jitter() is effective:
  
glance %>% 
  ggplot(aes(continent, r.squared)) + 
  geom_jitter(width = 0.5)

#We could pull out the countries with particularly bad R2 and plot the data:
  
bad_fit <- filter(glance, r.squared < 0.25)

# We see two main effects here: the tragedies of the HIV/AIDS epidemic and 
# the Rwandan genocide.

gapminder %>% 
  semi_join(bad_fit, by = "country") %>% 
  ggplot(aes(year, lifeExp, colour = country)) +
  geom_line()

#25.3 List-columns
tibble(
  x = list(1:3, 3:5), 
  y = c("1, 2", "3, 4, 5")
)

tribble(
  ~x, ~y,
  1:3, "1, 2",
  3:5, "3, 4, 5"
)

# Generally there are three parts of an effective list-column pipeline:
#   
# You create the list-column using one of nest(), summarise() + list(), or mutate() + 
#   a map function, as described in Creating list-columns.
# 
# You create other intermediate list-columns by transforming existing list columns 
# with map(), map2() or pmap(). For example, in the case study above, we created a 
# list-column of models by transforming a list-column of data frames.
# 
# You simplify the list-column back down to a data frame or atomic vector, as
# described in Simplifying list-columns.


#25.4 Creating list-columns

# 25.4.1 With nesting
# nest() creates a nested data frame, which is a data frame with a list-column
# of data frames. In a nested data frame each row is a meta-observation: the other
# columns give variables that define the observation (like country and continent 
#                                                     above), and the list-column 
# of data frames gives the individual observations that make up the 
# meta-observation.
gapminder %>% 
  group_by(country, continent) %>% 
  nest()

# You can also use it on an ungrouped data frame, specifying which columns you 
# want to nest:
  
gapminder %>% 
  nest(data = c(year:gdpPercap))

# 25.4.2 From vectorised functions
# Some useful functions take an atomic vector and return a list. For example, in 
# strings you learned about stringr::str_split() which takes a character vector 
# and returns a list of character vectors. If you use that inside mutate, you’ll
# get a list-column:
  
df <- tribble(
    ~x1,
    "a,b,c", 
    "d,e,f,g"
  ) 

df %>% 
  mutate(x2 = stringr::str_split(x1, ","))

# # unnest() knows how to handle these lists of vectors:
# (If you find yourself using this pattern a lot, make sure to check out
#   tidyr::separate_rows() which is a wrapper around this common pattern).

# Another example of this pattern is using the map(), map2(), pmap() from purrr. 
# For example, we could take the final example from Invoking different functions
# and rewrite it to use mutate():
  
sim <- tribble(
    ~f,      ~params,
    "runif", list(min = -1, max = 1),
    "rnorm", list(sd = 5),
    "rpois", list(lambda = 10)
  )

sim %>%
  mutate(sims = invoke_map(f, params, n = 10))

# 25.4.3 From multivalued summaries
# One restriction of summarise() is that it only works with summary functions 
# that return a single value. That means that you can’t use it with functions 
# like quantile() that return a vector of arbitrary length:
mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = quantile(mpg))

# You can however, wrap the result in a list! This obeys the contract of 
# summarise(), because each summary is now a list (a vector) of length 1.
mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = list(quantile(mpg)))

# To make useful results with unnest, you’ll also need to capture the probabilities:
  
probs <- c(0.01, 0.25, 0.5, 0.75, 0.99)
mtcars %>% 
  group_by(cyl) %>% 
  summarise(p = list(probs), q = list(quantile(mpg, probs))) %>% 
  unnest(c(p, q))

# 25.4.4 From a named list
# Data frames with list-columns provide a solution to a common problem: what do 
# you do if you want to iterate over both the contents of a list and its elements?
#   Instead of trying to jam everything into one object, it’s often easier to 
# make a data frame: one column can contain the elements, and one column can
# contain the list. An easy way to create such a data frame from a list is
# tibble::enframe().

x <- list(
  a = 1:5,
  b = 3:4, 
  c = 5:6
) 

df <- enframe(x)
df

#Now if you want to iterate over names and values in parallel, you can use map2():
  
df %>% 
  mutate(
    smry = map2_chr(name, value, ~ stringr::str_c(.x, ": ", .y[1]))
  )

# 25.5 Simplifying list-columns
# To apply the techniques of data manipulation and visualisation you’ve learned
# in this book, you’ll need to simplify the list-column back to a regular column
# (an atomic vector), or set of columns. The technique you’ll use to collapse
# back down to a simpler structure depends on whether you want a single value
# per element, or multiple values:
#   
#   If you want a single value, use mutate() with map_lgl(), map_int(), map_dbl(),
# and map_chr() to create an atomic vector.
# 
# If you want many values, use unnest() to convert list-columns back to regular 
# columns, repeating the rows as many times as necessary.

# 25.5.1 List to vector
# If you can reduce your list column to an atomic vector then it will be a 
# regular column. For example, you can always summarise an object with its 
# type and length, so this code will work regardless of what sort of \
# list-column you have:
  
  df <- tribble(
    ~x,
    letters[1:5],
    1:3,
    runif(5)
  )

df %>% mutate(
  type = map_chr(x, typeof),
  length = map_int(x, length)
)

# Don’t forget about the map_*() shortcuts - you can use map_chr(x, "apple") to 
# extract the string stored in apple for each element of x. This is useful for
# pulling apart nested lists into regular columns. Use the .null argument to 
# provide a value to use if the element is missing (instead of returning NULL):
  
df <- tribble(
    ~x,
    list(a = 1, b = 2),
    list(a = 2, c = 4)
  )
df %>% mutate(
  a = map_dbl(x, "a"),
  b = map_dbl(x, "b", .null = NA_real_)
)

# 25.5.2 Unnesting
# unnest() works by repeating the regular columns once for each element of the 
# list-column. For example, in the following very simple example we repeat the 
# first row 4 times (because there the first element of y has length four), and
# the second row once:
  
tibble(x = 1:2, y = list(1:4, 1)) %>% unnest(y)

# This means that you can’t simultaneously unnest two columns that contain
# different number of elements:
  
# Ok, because y and z have the same number of elements in
# every row
  df1 <- tribble(
    ~x, ~y,           ~z,
    1, c("a", "b"), 1:2,
    2, "c",           3
  )
df1

df1 %>% unnest(c(y, z))

# Doesn't work because y and z have different number of elements
df2 <- tribble(
  ~x, ~y,           ~z,
  1, "a",         1:2,  
  2, c("b", "c"),   3
)
df2

df2 %>% unnest(c(y, z))

# The same principle applies when unnesting list-columns of data frames. You can
# unnest multiple list-cols as long as all the data frames in each row have the 
# same number of rows.

# 25.6 Making tidy data with broom
# The broom package provides three general tools for turning models 
# into tidy data frames:
#   
# broom::glance(model) returns a row for each model. Each column 
# gives a model summary: either a measure of model quality, or complexity, or a 
# combination of the two.
# 
# broom::tidy(model) returns a row for each coefficient in the model. Each 
# column gives information about the estimate or its variability.
# 
# broom::augment(model, data) returns a row for each row in data, adding 
# extra values like residuals, and influence statistics.



































































