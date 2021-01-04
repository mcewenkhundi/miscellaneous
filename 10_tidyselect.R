#Exploring the new features of the tidyselect
library(dplyr)
library(tidyselect)

# NOT RUN {
nms <- names(iris)
vars_select(nms, starts_with("Petal"))
vars_select(nms, ends_with("Width"))
vars_select(nms, contains("etal"))
vars_select(nms, matches(".t."))
vars_select(nms, Petal.Length, Petal.Width)
vars_select(nms, everything())
vars_select(nms, last_col())
vars_select(nms, last_col(offset = 2))

# With multiple matchers, the union of the matches is selected:
vars_select(nms, starts_with(c("Petal", "Sepal")))

# `!` negates a selection:
vars_select(nms, !ends_with("Width"))

# `&` and `|` take the intersection or the union of two selections:
vars_select(nms, starts_with("Petal") & ends_with("Width"))
vars_select(nms, starts_with("Petal") | ends_with("Width"))

# `/` takes the difference of two selections
vars_select(nms, starts_with("Petal") / ends_with("Width"))

# `all_of()` selects the variables in a character vector:
vars <- c("Petal.Length", "Petal.Width")
vars_select(nms, all_of(vars))

# Whereas `all_of()` is strict, `any_of()` allows missing
# variables.
try(vars_select(nms, all_of(c("Species", "Genres"))))
vars_select(nms, any_of(c("Species", "Genres")))

# The lax variant is especially useful to make sure a variable is
# selected out:
vars_select(nms, -any_of(c("Species", "Genres")))

# The order of selected columns is determined from the inputs
vars_select(names(mtcars), starts_with("c"), starts_with("d"))
vars_select(names(mtcars), one_of(c("carb", "mpg")))
# }



