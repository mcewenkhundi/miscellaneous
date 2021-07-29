reports <- tibble(
  species = unique(penguins$species),
  filename = stringr::str_c("penguin-summary-", species, ".html"),
  params = purrr::map(species, ~ list(my_species = .))
)
reports

reports %>% 
  select(output_file = filename, params) %>% 
  purrr::pwalk(rmarkdown::render, input = "penguin_summary.Rmd")
