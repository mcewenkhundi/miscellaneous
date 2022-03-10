library(gtsummary)

# \donttest{
# Example 1 ----------------------------------
tbl_strata_ex1 <-
  trial %>%
  select(age, grade, stage, trt) %>%
  mutate(grade = paste("Grade", grade)) %>%
  tbl_strata(
    strata = grade,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = trt, missing = "no") %>%
      add_n(),
    .header = "**{strata}**, N = {n}"
  )

# Example 2 ----------------------------------
tbl_strata_ex2 <-
  trial %>%
  select(grade, response) %>%
  mutate(grade = paste("Grade", grade)) %>%
  tbl_strata2(
    strata = grade,
    .tbl_fun =
      ~.x %>%
      tbl_summary(
        label = list(response = .y),
        missing = "no",
        statistic = response ~ "{p}%"
      ) %>%
      add_ci(pattern = "{stat} ({ci})") %>%
      modify_header(stat_0 = "**Rate (95% CI)**") %>%
      modify_footnote(stat_0 = NA),
    .combine_with = "tbl_stack",
    .combine_args = list(group_header = NULL),
    .quiet = TRUE
  ) %>%
  modify_caption("**Response Rate by Grade**")
# }
