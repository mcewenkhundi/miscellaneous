new_counter <- function() {
  i <- 0
  
  function() {
    i <<- i + 1
    i
  }
}

counter_one <- new_counter()
counter_two <- new_counter()
counter_two()

library(tidyverse)
library(scales)

y <- c(12345, 123456, 1234567)
comma_format()(y)

j = number_format(scale = 1e-3, suffix = " K")
j
