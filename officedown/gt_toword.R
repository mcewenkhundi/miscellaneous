library(gt)
library(flextable)
library(tidyverse)
library(gtsummary)

tab_rtf <- mtcars %>%
  gt() %>%
  tab_style(style = cell_fill(color = "lightblue"),
            locations = cells_body(columns = "disp",rows = disp > 250)) %>%
  as_rtf()

my_conn <- file("table.RTF", "w")
writeLines(tab_rtf, my_conn)
close(my_conn)
