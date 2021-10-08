#How to extract confidence intervals from binom.exact tests

library(tidyverse)

df_tb <- tribble(
  ~colA, ~colB,
  2,   15,
  3,   27,
  3,   36
)

df_tb <- df_tb %>%
  rowwise() %>%
  mutate(bino_exact = list(binom.test(colA,colB)),
         bino_exact_rs = broom::tidy(bino_exact))

df_tb$bino_exact[[1]]
