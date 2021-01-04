?mpg
library(tidyverse)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))

ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()

library(tidyverse)
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

diamonds %>%
  count(cut)
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

diamonds %>%
  count(cut_width(carat, width = 0.5))

smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 30))

diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point(na.rm = TRUE)

ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

ggplot(data = diamonds, mapping = aes(x = price,
                                      y = ..density..)) +
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median),y=hwy))

ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

diamonds %>% 
  count(color, cut)

diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 100)
ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

# install.packages("hexbin")
ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

library(modelr)

mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid))

diamonds2 <- diamonds %>%
  add_residuals(mod) %>%
  mutate(resid = exp(resid))

diamonds %>%
  arrange(desc(y)) %>%
  head()

diamonds %>%
  arrange(desc(z)) %>%
  head()
diamonds %>%
  count(color, cut) %>%
  group_by(color) %>%
  mutate(prop = n / sum(n))

double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'"

name <- "Hadley"
time_of_day <- "morning"
birthday <- TRUE

str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
)
#> [1] "Good morning Hadley."

library(tidyverse)

x <- c("apple", "banana", "pear")
str_detect(x, "e")

# How many common words start with t?
sum(str_detect(words, "^t"))
#> [1] 65
# What proportion of common words end with a vowel?
mean(str_detect(words, "[aeiou]$"))
#> [1] 0.2765306

# Find all words containing at least one vowel, and negate
no_vowels_1 <- !str_detect(words, "[aeiou]")
# Find all words consisting only of consonants (non-vowels)
no_vowels_2 <- str_detect(words, "^[^aeiou]+$")
identical(no_vowels_1, no_vowels_2)
#> [1] TRUE

words[str_detect(words, "x$")]
#> [1] "box" "sex" "six" "tax"
str_subset(words, "x$")
#> [1] "box" "sex" "six" "tax"

df <- tibble(
  word = words, 
  i = seq_along(word)
)

df %>% 
  filter(str_detect(word, "x$"))

x <- c("apple", "banana", "pear")
str_count(x, "a")
#> [1] 1 3 1

# On average, how many vowels per word?
mean(str_count(words, "[aeiou]"))
#> [1] 1.991837

df %>% 
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )
#> # A tibble: 980 x 4
#>   word         i vowels consonants
#>   <chr>    <int>  <int>      <int>
#> 1 a            1      1          0
#> 2 able         2      2          2
#> 3 about        3      3          2
#> 4 absolute     4      4          4
#> 5 accept       5      2          4
#> 6 account      6      3          4
#> # … with 974 more rows


str_count("abababa", "aba")
#> [1] 2
str_view_all("abababa", "aba")

length(sentences)
#> [1] 720
head(sentences)
#> [1] "The birch canoe slid on the smooth planks." 
#> [2] "Glue the sheet to the dark blue background."
#> [3] "It's easy to tell the depth of a well."     
#> [4] "These days a chicken leg is a rare dish."   
#> [5] "Rice is often served in round bowls."       
#> [6] "The juice of lemons makes fine punch."

colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colour_match <- str_c(colours, collapse = "|")
colour_match
#> [1] "red|orange|yellow|green|blue|purple"

has_colour <- str_subset(sentences, colour_match)
matches <- str_extract(has_colour, colour_match)
head(matches)
#> [1] "blue" "blue" "red"  "red"  "red"  "blue"

more <- sentences[str_count(sentences, colour_match) > 1]
str_view_all(more, colour_match)

str_extract_all(more, colour_match)

noun <- "(a|the) ([^ ]+)"

has_noun <- sentences %>%
  str_subset(noun) %>%
  head(10)
has_noun %>% 
  str_extract(noun)
#>  [1] "the smooth" "the sheet"  "the depth"  "a chicken"  "the parked"
#>  [6] "the sun"    "the huge"   "the ball"   "the woman"  "a helps"

has_noun %>% 
  str_match(noun)

tibble(sentence = sentences) %>% 
  tidyr::extract(
    sentence, c("article", "noun"), "(a|the) ([^ ]+)", 
    remove = FALSE
  )

x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")
#> [1] "-pple"  "p-ar"   "b-nana"
str_replace_all(x, "[aeiou]", "-")
#> [1] "-ppl-"  "p--r"   "b-n-n-"

x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))


sentences %>% 
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>% 
  head(5)
  
sentences %>%
  head(5) %>% 
  str_split(" ")

"a|b|c|d" %>% 
  str_split("\\|") %>% 
  .[[1]]
#> [1] "a" "b" "c" "d"

sentences %>%
  head(5) %>% 
  str_split(" ", simplify = TRUE)

fields <- c("Name: Hadley", "Country: NZ", "Age: 35")
fields %>% str_split(": ", n = 2, simplify = TRUE)

x <- "This is a sentence.  This is another sentence."
str_view_all(x, boundary("word"))

str_split(x, " ")

str_split(x, boundary("word"))

str_view(fruit, "nana")

str_view(fruit, regex("nana"))

bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")

str_view(bananas, regex("banana", ignore_case = TRUE))

x <- "Line 1\nLine 2\nLine 3"
str_extract_all(x, "^Line")[[1]]
str_extract_all(x, regex("^Line", multiline = TRUE))[[1]]

phone <- regex("
  \\(?     # optional opening parens
  (\\d{3}) # area code
  [) -]?   # optional closing parens, space, or dash
  (\\d{3}) # another three numbers
  [ -]?    # optional space or dash
  (\\d{3}) # three more numbers
  ", comments = TRUE)

str_match("514-791-8141", phone)
#>      [,1]          [,2]  [,3]  [,4] 
#> [1,] "514-791-814" "514" "791" "814"

month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

y1 <- factor(x1, levels = month_levels)
y1
sort(y1)

x2 <- c("Dec", "Apr", "Jam", "Mar")
x2

y2 <- factor(x2, levels = month_levels)
y2

y2 <- parse_factor(x2, levels = month_levels)

f1 <- factor(x1, levels = unique(x1))
f1
fct_inorder(x1)

gss_cat

gss_cat %>%
  count(race)

ggplot(gss_cat, aes(race)) +
  geom_bar()

ggplot(gss_cat, aes(race)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

relig_summary <- gss_cat %>%
  group_by(relig) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )
#> `summarise()` ungrouping output (override with `.groups` argument)

ggplot(relig_summary, aes(tvhours, relig)) + geom_point()

ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()

ggplot(relig_summary, 
       aes(tvhours, fct_reorder(relig, tvhours))) +
       geom_point()

relig_summary %>%
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) +
  geom_point()

rincome_summary <- gss_cat %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )
#> `summarise()` ungrouping output (override with `.groups` argument)

ggplot(rincome_summary, 
       aes(age, fct_reorder(rincome, age))) + 
  geom_point()

ggplot(rincome_summary, aes(age, fct_relevel(rincome, "Not applicable"))) +
  geom_point()

by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  count(age, marital) %>%
  group_by(age) %>%
  mutate(prop = n / sum(n))

ggplot(by_age, aes(age, prop, colour = marital)) +
  geom_line(na.rm = TRUE)

ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
  geom_line() +
  labs(colour = "marital")

gss_cat %>% 
  count(marital) %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital)) +
  geom_bar()

gss_cat %>% count(partyid)

gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat"
  )) %>%
  count(partyid)
gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat",
                              "Other"                 = "No answer",
                              "Other"                 = "Don't know",
                              "Other"                 = "Other party"
  )) %>%
  count(partyid)

gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat")
  )) %>%
  count(partyid)

gss_cat %>%
  mutate(relig = fct_lump(relig)) %>%
  count(relig)
gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>%
  count(relig, sort = TRUE) %>%
  print(n = Inf)

library(tidyverse)
# Look for a literal character that normally has special meaning in a regex
str_view(c("abc", "a.c", "a*c", "a c"), "a[.]c")

str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c")

str_view(c("abc", "a.c", "a*c", "a c"), "a[ ]")

str_view(c("grey", "gray"), "gr(e|a)y")

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "(CC?)L")

str_view(x, 'C[XL]+')

str_view(fruit, "(..)\\1", match = TRUE)

x <- c("apple", "banana", "pear")
str_detect(x, "e")
#> [1]  TRUE FALSE  TRUE

# How many common words start with t?
sum(str_detect(words, "^t"))
#> [1] 65
# What proportion of common words end with a vowel?
mean(str_detect(words, "[aeiou]$"))
#> [1] 0.2765306

words[str_detect(words, "x$")]
#> [1] "box" "sex" "six" "tax"
str_subset(words, "x$")
#> [1] "box" "sex" "six" "tax"

df <- tibble(
  word = words, 
  i = seq_along(word)
)
df %>% 
  filter(str_detect(word, "x$"))


x <- c("apple", "banana", "pear")

str_count(x, "a")
#> [1] 1 3 1

# On average, how many vowels per word?
mean(str_count(words, "[aeiou]"))
#> [1] 1.991837

df %>% 
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )

colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colour_match <- str_c(colours, collapse = "|")
colour_match
#> [1] "red|orange|yellow|green|blue|purple"

has_colour <- str_subset(sentences, colour_match)
matches <- str_extract(has_colour, colour_match)
head(matches)
#> [1] "blue" "blue" "red"  "red"  "red"  "blue"

more <- sentences[str_count(sentences, colour_match) > 1]
str_view_all(more, colour_match)

str_extract_all(more, colour_match)

str_extract_all(more, colour_match, simplify = TRUE)
#>      [,1]     [,2] 
#> [1,] "blue"   "red"
#> [2,] "green"  "red"
#> [3,] "orange" "red"

x <- c("a", "a b", "a b c")
str_extract_all(x, "[a-z]", simplify = TRUE)


noun <- "(a|the) ([^ ]+)"

has_noun <- sentences %>%
  str_subset(noun) %>%
  head(10)
has_noun %>% 
  str_extract(noun)

tibble(sentence = sentences) %>% 
  tidyr::extract(
    sentence, c("article", "noun"), "(a|the) ([^ ]+)", 
    remove = FALSE
  )

x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")
#> [1] "-pple"  "p-ar"   "b-nana"
str_replace_all(x, "[aeiou]", "-")
#> [1] "-ppl-"  "p--r"   "b-n-n-"

x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")
#> [1] "-pple"  "p-ar"   "b-nana"
str_replace_all(x, "[aeiou]", "-")
#> [1] "-ppl-"  "p--r"   "b-n-n-"

x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))

sentences %>% 
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>% 
  head(5)

sentences %>%
  head(5) %>% 
  str_split(" ")

bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")

str_view(bananas, regex("banana", ignore_case = TRUE))

phone <- regex("
  \\(?     # optional opening parens
  (\\d{3}) # area code
  [) -]?   # optional closing parens, space, or dash
  (\\d{3}) # another three numbers
  [ -]?    # optional space or dash
  (\\d{3}) # three more numbers
  ", comments = TRUE)

str_match("514-791-8141", phone)
#>      [,1]          [,2]  [,3]  [,4] 
#> [1,] "514-791-814" "514" "791" "814"

str_extract_all(x, boundary("word"))

apropos("replace")

head(dir(pattern = "\\.Rmd$"))
#> [1] "communicate-plots.Rmd" "communicate.Rmd"       "datetimes.Rmd"        
#> [4] "EDA.Rmd"               "explore.Rmd"           "factors.Rmd"

pryr::object_size(diamonds)

assign("x", 10)
x

"x" %>% assign(100)
x

env <- environment()
"z" %>% assign(100, envir = env)
z

#Function definitions in r

df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

df$a <- (df$a - min(df$a, na.rm = TRUE)) / 
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) / 
  (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE)) / 
  (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) / 
  (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

# Load data ------------------------------------------------

has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}

b <- list(a = 1, 2)

x = "d"
switch(x, 
       a = ,
       b = "ab",
       c = ,
       d = "cd"
)

has_name(b)

commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters[1:10])
#> [1] "a, b, c, d, e, f, g, h, i, j"

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule("Important output")
#> Important output -----------------------------------------------------------
pad = "-"
title <- paste0("Important output")
width <- getOption("width") - nchar(title) - 5
cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")

x <- c(1, 2)
sum(x, na.rm  = TRUE)

NA
NA_

tibble(x = 1:4, y = 1:2)

tibble(x = 1:4, y = rep(1:2,2))

tibble(x = 1:4, y = rep(1:2, 2))


tibble(x = 1:4, y = rep(1:2, each = 2))

c(x = 1, y = 2, z = 4)

set_names(1:3, c("a", "b", "c"))

x <- 1:20
x[-which(x > 10)]
x[x<=10]

x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
typeof(x)

attributes(x)

x <- as.Date("1971-01-01")
unclass(x)
typeof(x)
attributes(x)

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

output <- vector("double", ncol(df))

for(i in seq_along(df)) {
  output[[i]] <- median(df[[i]])
}

output

y <- vector("double", 0)
seq_along(y)
#> integer(0)
1:length(y)

#Known output
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

for (i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}

for (i in seq_along(x)) {
  name <- names(x)[[i]]
  value <- x[[i]]
}

means <- c(0, 1, 2)

output <- double()
for (i in seq_along(means)) {
  n <- sample(100, 1)
  output <- c(output, rnorm(n, means[[i]]))
}
str(output)

out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
}

#Unknown sequential length

flip <- function() sample(c("T", "H"), 1)

flips <- 0
nheads <- 0

while (nheads < 3) {
  if (flip() == "H") {
    nheads <- nheads + 1
  } else {
    nheads <- 0
  }
  flips <- flips + 1
}
flips

# for loops to functionals
output <- vector("double", length(df))
for (i in seq_along(df)) {
  output[[i]] <- mean(df[[i]])
}
output
#> [1] -0.3260369  0.1356639  0.4291403 -0.2498034

col_mean <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- mean(df[[i]])
  }
  output
}

# Extending the same to median and sd
col_median <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- median(df[[i]])
  }
  output
}
col_sd <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- sd(df[[i]])
  }
  output
}


col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  }
  out
}
col_summary(df, median)
#> [1] -0.51850298  0.02779864  0.17295591 -0.61163819
col_summary(df, mean)
#> [1] -0.3260369  0.1356639  0.4291403 -0.2498034

# functional programming in purrr maps
map_dbl(df, mean)
#>          a          b          c          d 
#> -0.3260369  0.1356639  0.4291403 -0.2498034
map_dbl(df, median)
#>           a           b           c           d 
#> -0.51850298  0.02779864  0.17295591 -0.61163819
map_dbl(df, sd)
#>         a         b         c         d 
#> 0.9214834 0.4848945 0.9816016 1.1563324

df %>% map_dbl(mean)
#>          a          b          c          d 
#> -0.3260369  0.1356639  0.4291403 -0.2498034
df %>% map_dbl(median)
#>           a           b           c           d 
#> -0.51850298  0.02779864  0.17295591 -0.61163819
df %>% map_dbl(sd)
#>         a         b         c         d 
#> 0.9214834 0.4848945 0.9816016 1.1563324

df %>% map_dbl(1)

models <- mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df))
models


models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))
models

models %>% 
  map(summary) 

models %>% 
  map(summary) %>% 
  map_dbl(~.$r.squared)


models %>% 
  map(summary) %>% 
  map_dbl("r.squared")
#>         4         6         8 
#> 0.5086326 0.4645102 0.4229655

#U can also use integers to select by position
x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x %>% map_dbl(2)
#> [1] 2 5 8

#base R functional functions
#maps and lapply
x1 <- list(
  c(0.27, 0.37, 0.57, 0.91, 0.20),
  c(0.90, 0.94, 0.66, 0.63, 0.06), 
  c(0.21, 0.18, 0.69, 0.38, 0.77)
)
x2 <- list(
  c(0.50, 0.72, 0.99, 0.38, 0.78), 
  c(0.93, 0.21, 0.65, 0.13, 0.27), 
  c(0.39, 0.01, 0.38, 0.87, 0.34)
)

threshold <- function(x, cutoff = 0.8) x[x > cutoff]
x1 %>% sapply(threshold) %>% str()
#> List of 3
#>  $ : num 0.91
#>  $ : num [1:2] 0.9 0.94
#>  $ : num(0)
x2 %>% sapply(threshold) %>% str()
#>  num [1:3] 0.99 0.93 0.87

#vapply is the safe altenative
#Dealing with errors in R

safe_log <- safely(log)
str(safe_log(10))

str(safe_log("a"))

x <- list(1, 10, "a")
y <- x %>% map(safely(log))
str(y)

y <- y %>% transpose()
str(y)

is_ok <- y$error %>% map_lgl(is_null)

y$result[is_ok] %>% flatten_dbl()

x <- list(1, 10, "a")
x %>% map_chr(possibly(~log(x=.), "error"))

x <- list(1, -1)
x %>% map(quietly(log)) %>% str()

mu <- list(5, 10, -3)
mu %>% 
  map(rnorm, n = 5) %>% 
  str()

sigma <- list(1, 5, 10)
seq_along(mu) %>% 
  map(~rnorm(5, mu[[.]], sigma[[.]])) %>% 
  str()
map2(mu, sigma, rnorm, n = 5) %>% str()

#Like map(), map2() is just a wrapper around a for loop:
  
  map2 <- function(x, y, f, ...) {
    out <- vector("list", length(x))
    for (i in seq_along(x)) {
      out[[i]] <- f(x[[i]], y[[i]], ...)
    }
    out
  }
  
  #Many arguments pmap
  n <- list(1, 3, 5)
  args1 <- list(n, mu, sigma)
  args1 %>%
    pmap(rnorm) %>% 
    str()

  #Better to name the arguments
  args2 <- list(mean = mu, sd = sigma, n = n)
  args2 %>% 
    pmap(rnorm) %>% 
    str()

  #can keep the args in a data.frame
  params <- tribble(
    ~mean, ~sd, ~n,
    5,     1,  1,
    10,     5,  3,
    -3,    10,  5
  )
  params %>% 
    pmap(rnorm)

#involking different functions

  f <- c("runif", "rnorm", "rpois")
  param <- list(
    list(min = -1, max = 1), 
    list(sd = 5), 
    list(lambda = 10)
  )  

  invoke_map(f, param, n = 5) %>% str()
  
  sim <- tribble(
    ~f,      ~params,
    "runif", list(min = -1, max = 1),
    "rnorm", list(sd = 5),
    "rpois", list(lambda = 10)
  )
  sim %>% 
    mutate(sim = invoke_map(f, params, n = 10))
  
  #for side effects use walk, walk2 and pwalk
  library(ggplot2)
  plots <- mtcars %>% 
    split(.$cyl) %>% 
    map(~ggplot(., aes(mpg, wt)) + geom_point())
  paths <- stringr::str_c(names(plots), ".pdf")
  
  pwalk(list(paths, plots), ggsave, path = here::here())
  
  #Predicate functions
  iris %>% 
    keep(is.factor) %>% 
    str()
  
  iris %>% 
    discard(is.factor) %>% 
    str()
  x <- list(1:5, letters, list(10))
  
  x %>% 
    some(is_character)
  #> [1] TRUE
  
  x %>% 
    every(is_vector)  
  
  x <- sample(10)
  x
  #>  [1] 10  6  1  3  2  4  5  8  9  7
  
  x %>% 
    detect(~ . > 5)
  #> [1] 10
  
  x %>% 
    detect_index(~ . > 5)
  
#Modelling in R chapter 
library(tidyverse)
  
library(modelr)
options(na.action = na.warn)
  
ggplot(sim1, aes(x, y)) + 
  geom_point()  
  
#Trying different models to see the one that fit the data
models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), 
              data = models, alpha = 1/4) +
  geom_point() 

#The model function
model1 <- function(a, data) {
  a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)

#finding the best model
measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)

#helper function for measuring distance
sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models

#10 smallest distances

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
  )

ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))

#Using grid search
grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist)) 

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(grid, rank(dist) <= 10)
  )

best <- optim(c(0, 0), measure_distance, data = sim1)
best$par
#> [1] 4.222248 2.051204

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2])

#general linear model in r
sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

grid <- sim1 %>% 
  data_grid(x) 
grid

#Next we add predictions.
grid <- grid %>% 
  add_predictions(sim1_mod) 
grid

#ideas on visualising 
#complex models  http://vita.had.co.nz/papers/model-vis.html.
ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)

#We use the original dataset becouse we need to the original y
#values
sim1 <- sim1 %>% 
  add_residuals(sim1_mod)
sim1

ggplot(sim1, aes(resid)) + 
  geom_freqpoly(binwidth = 0.5)

ggplot(sim1, aes(x, resid)) + 
  geom_ref_line(h = 0) +
  geom_point() 

#How the formulas work in R functions
df <- tribble(
  ~y, ~x1, ~x2,
  4, 2, 5,
  5, 1, 6
)
model_matrix(df, y ~ x1)

#By default R adds an intercept
model_matrix(df, y ~ x1 - 1)

model_matrix(df, y ~ x1 + x2)

#For categorical variables
df <- tribble(
  ~ sex, ~ response,
  "male", 1,
  "female", 2,
  "male", 1
)
model_matrix(df, response ~ sex)

ggplot(sim2) + 
  geom_point(aes(x, y))

#We can fit a model to it and generate predictions
mod2 <- lm(y ~ x, data = sim2)

grid <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod2)
grid

ggplot(sim2, aes(x)) + 
  geom_point(aes(y = y)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)

#interaction of categorical and a continuous variable
library(tidyverse)
library(modelr)
mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

grid <- sim3 %>% 
  data_grid(x1, x2) %>% 
  gather_predictions(mod1, mod2)
grid


ggplot(sim3, aes(x1, y, colour = x2)) + 
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) + 
  facet_grid(x2~ model)

#Which model is better
sim3 <- sim3 %>% 
  gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, colour = x2)) + 
  geom_point() + 
  facet_grid(model ~ x2)

#Interaction between countinous variables
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)


grid <- sim4 %>% 
  data_grid(
    x1 = seq_range(x1, 5), 
    x2 = seq_range(x2, 5) 
  ) %>% 
  gather_predictions(mod1, mod2)
grid

#Visualise the models
ggplot(grid, aes(x1, x2)) + 
  geom_tile(aes(fill = pred)) + 
  facet_wrap(~ model)


ggplot(grid, aes(x1, pred, colour = x2, group = x2)) + 
  geom_line() +
  facet_wrap(~ model)
ggplot(grid, aes(x2, pred, colour = x1, group = x1)) + 
  geom_line() +
  facet_wrap(~ model)

sim4 <- sim4 %>% 
  gather_residuals(mod1, mod2)

ggplot(sim4, aes(x1, resid, colour = x2)) + 
  geom_point() + 
  facet_grid(model ~ x2)

ggplot(sim4, aes(x2, resid, colour = x1)) + 
  geom_point() + 
  facet_grid(model ~ x1)
#use model matrix to make sure of the model that you are working 
#on
df <- tribble(
  ~y, ~x,
  1,  1,
  2,  2, 
  3,  3
)
model_matrix(df, y ~ x^2 + x)
model_matrix(df, y ~ I(x^2) + x)

#Non linear data

sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
)

ggplot(sim5, aes(x, y)) +
  geom_point()

#Models to explore

mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)

grid <- sim5 %>% 
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")

ggplot(sim5, aes(x, y)) + 
  geom_point() +
  geom_line(data = grid, colour = "red") +
  facet_wrap(~ model)

#To get warnings when R drops missing values
options(na.action = na.warn)

        df <- tribble(
          ~x, ~y,
          1, 2.2,
          2, NA,
          3, 3.5,
          4, 8.3,
          NA, 10
        )
        
        mod <- lm(y ~ x, data = df)
        #> Warning: Dropping 2 rows with missing values

        nobs(mod)
#Chapter24
library(tidyverse)
library(modelr)
options(na.action = na.warn)
        
library(nycflights13)
library(lubridate)

#Why are the lower quality diamonds have higer prices
ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()

#Price and weght which is carat
ggplot(diamonds, aes(carat, price)) + 
  geom_hex(bins = 50)

#Focus on diamonds smaller than 2.5 carats (99.7% of the data)
#Log-transform the carat and price variables.
diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))

ggplot(diamonds2, aes(lcarat, lprice)) + 
  geom_hex(bins = 50)

#We first make the pattern explicit by fitting a model:
  
mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)

#Then we look at what the model tells us about the data. 
#Note that I back transform the predictions, 
#undoing the log transformation, so I can overlay the 
#predictions on the raw data:
  
  grid <- diamonds2 %>% 
  data_grid(carat = seq_range(carat, 20)) %>% 
  mutate(lcarat = log2(carat)) %>% 
  add_predictions(mod_diamond, "lprice") %>% 
  mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, price)) + 
  geom_hex(bins = 50) + 
  geom_line(data = grid, colour = "red", size = 1)

#Now we can look at the residuals, which verifies that we’ve 
#successfully removed the strong linear pattern:
  
  diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond, "lresid")

ggplot(diamonds2, aes(lcarat, lresid)) + 
  geom_hex(bins = 50)

#Importantly, we can now re-do our motivating plots using those 
#residuals instead of price.

ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()

#A more complicated example
mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)

grid <- diamonds2 %>% 
  data_grid(cut, .model = mod_diamond2) %>% 
  add_predictions(mod_diamond2)
grid

ggplot(grid, aes(cut, pred)) + 
  geom_point()

diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond2, "lresid2")

ggplot(diamonds2, aes(lcarat, lresid2)) + 
  geom_hex(bins = 50)

#Look at values with big residuals
diamonds2 %>% 
  filter(abs(lresid2) > 1) %>% 
  add_predictions(mod_diamond2) %>% 
  mutate(pred = round(2 ^ pred)) %>% 
  select(price, pred, carat:table, x:z) %>% 
  arrange(price)

#What affects the number of daily flights?
library(tidyverse)
library(modelr)

daily <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(n = n())
daily

ggplot(daily, aes(date, n)) + 
  geom_line()

#Day of the week has a strong effect
daily <- daily %>% 
  mutate(wday = wday(date, label = TRUE))
ggplot(daily, aes(wday, n)) + 
  geom_boxplot()

#One way to remove this strong pattern is to use a model. 
#First, we fit the model, and display its predictions 
#overlaid on the original data:
  
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

#Our model seems to fail starting in June: you can still 
#see a strong regular pattern that our model hasn’t captured.
#Drawing a plot with one line for each day of the week makes 
#the cause easier to see:
  
ggplot(daily, aes(date, resid, colour = wday)) + 
geom_ref_line(h = 0) + 
geom_line()

#There seems to be some smoother long term trend over the 
#course of a year. We can 
#highlight that trend with geom_smooth():
  
daily %>% 
ggplot(aes(date, resid)) + 
geom_ref_line(h = 0) + 
geom_line(colour = "grey50") + 
geom_smooth(se = FALSE, span = 0.20)
#> `geom_smooth()` using method = 'loess' and formula 'y ~ x'

daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n)) + 
  geom_point() + 
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", 
               date_labels = "%b")

#term to capture the school terms
term <- function(date) {
  cut(date, 
      breaks = ymd(20130101, 20130605, 
                   20130825, 20140101),
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

#look at the saturdays again
daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n, colour = term)) +
  geom_point(alpha = 1/3) + 
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

#It’s useful to see how this new variable affects the other days of the week:
  
daily %>% 
  ggplot(aes(wday, n, colour = term)) +
  geom_boxplot()

#It looks like there is significant variation across the terms,
#so fitting a separate day of week effect for each term is reasonable.
#This improves our model, but not as much as we might hope:
  
mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)

#Looking at the residuals

daily %>% 
  gather_residuals(without_term = mod1, with_term = mod2) %>% 
  ggplot(aes(date, resid, colour = model)) +
  geom_line(alpha = 0.75)

grid <- daily %>% 
  data_grid(wday, term) %>% 
  add_predictions(mod2, "n")

ggplot(daily, aes(wday, n)) +
  geom_boxplot() + 
  geom_point(data = grid, colour = "red") + 
  facet_wrap(~ term)

#A model thats not affected by outliers
mod3 <- MASS::rlm(n ~ wday * term, data = daily)

daily %>% 
  add_residuals(mod3, "resid") %>% 
  ggplot(aes(date, resid)) + 
  geom_hline(yintercept = 0, size = 2, colour = "white") + 
  geom_line()

#Time of the year approach
library(splines)
mod <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)

daily %>% 
  data_grid(wday, date = seq_range(date, n = 13)) %>% 
  add_predictions(mod) %>% 
  ggplot(aes(date, pred, colour = wday)) + 
  geom_line() +
  geom_point()

#Chapter25 many models in R
library(modelr)
library(tidyverse)

library(gapminder)
gapminder

#In this case study, we’re going to focus on just three variables
#to answer the question “How does life expectancy (lifeExp) 
#change over time (year) for each country (country)?”.
#A good place to start is with a plot:
  
gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)

  #Fit models to tease out the pattern
  #Do this first for a single country
  #You already know how to do that if we had a single country:
    
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

  #Do the above for every other country
  by_country <- gapminder %>% 
    group_by(country, continent) %>% 
    nest()
  
  by_country
  
  by_country$data[[1]]

  #Modelling function to use in purrr map
  country_model <- function(df) {
    lm(lifeExp ~ year, data = df)
  }
  
#Use the pipe instead
models <- map(by_country$data, country_model)

by_country <- by_country %>% 
  mutate(model = map(data, country_model))
  by_country
  
#All related fields are stored together
  by_country %>% 
    filter(continent == "Europe")

  by_country %>% 
    arrange(continent, country)

  #Adding residuals for all the models
  by_country <- by_country %>% 
    mutate(
      resids = map2(data, model, add_residuals)
    )
  by_country

#Using unnest to get back the residuals
  resids <- unnest(by_country, resids)
  resids

  #We can now plot the residuals
  resids %>% 
    ggplot(aes(year, resid)) +
    geom_line(aes(group = country), alpha = 1 / 3) + 
    geom_smooth(se = FALSE)
  #> `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
  
  #Facet by continent to check those that are not fitting
  #we see some very large residuals which suggests our model 
  #isn’t fitting so well there. We’ll explore 
  #that more in the next section, attacking it from a slightly different angle.
  resids %>% 
    ggplot(aes(year, resid, group = country)) +
    geom_line(alpha = 1 / 3) + 
    facet_wrap(~continent)
  
  #Can get model quality parameters from the broom::glance()
  broom::glance(nz_mod)
  
#We can use mutate() and unnest() to create a data frame 
#with a row for each country:
by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance)  %>% View()
  
glance <- by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)

#With this data frame in hand, we can start to look for models 
#that don’t fit well:

glance %>% 
  arrange(r.squared)  

#The worst models all appear to be in Africa. 
#Let’s double check that with a plot. 
#Here we have a relatively small number of observations and a 
#discrete variable, so geom_jitter() is effective:

glance %>% 
  ggplot(aes(continent, r.squared, color = continent)) + 
  geom_jitter(width = 0.5)  

#We could pull out the countries with particularly bad R2
#and plot the data:
  
bad_fit <- filter(glance, r.squared < 0.25)

gapminder %>% 
  semi_join(bad_fit, by = "country") %>% 
  ggplot(aes(year, lifeExp, colour = country)) +
  geom_line()
    
#List columns
#Not natively supported in base R
data.frame(x = list(1:3, 3:5))

#Can improve by using I
#But doesnt print well
data.frame(
  x = I(list(1:3, 3:5)), 
  y = c("1, 2", "3, 4, 5")
)

#Tibble alleviates this problem by being lazier (tibble() 
#doesn’t modify its inputs) and by providing a better print method:
  
tibble(
    x = list(1:3, 3:5), 
    y = c("1, 2", "3, 4, 5")
  )

#It’s even easier with tribble() as it can automatically
#work out that you need a list:
    
tribble(
  ~x, ~y,
  1:3, "1, 2",
  3:5, "3, 4, 5"
  )
#With nesting
gapminder %>% 
  group_by(country, continent) %>% 
  nest()
gapminder %>% 
  nest(data = c(year:gdpPercap))

#From vectorarised functions

df <- tribble(
  ~x1,
  "a,b,c", 
  "d,e,f,g"
) 

df %>% 
  mutate(x2 = stringr::str_split(x1, ","))


#unnest() knows how to handle these lists of vectors:
#(If you find yourself using this pattern a lot,
#make sure to check out tidyr::separate_rows() 

df %>% 
  mutate(x2 = stringr::str_split(x1, ",")) %>% 
  unnest(x2)

sim <- tribble(
  ~f,      ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)

sim %>%
  mutate(sims = invoke_map(f, params, n = 10))
#From multivalued summaries
mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = quantile(mpg))

#use a list
mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = list(quantile(mpg)))

#To make useful results with unnest, you’ll also need 
#to capture the probabilities:
probs <- c(0.01, 0.25, 0.5, 0.75, 0.99)
mtcars %>% 
  group_by(cyl) %>% 
  summarise(p = list(probs), q = list(quantile(mpg, probs))) %>% 
  unnest(c(p, q))

#From a named list
x <- list(
  a = 1:5,
  b = 3:4, 
  c = 5:6
) 

df <- enframe(x)
df
#If you want to use names
df %>% 
  mutate(
    smry = map2_chr(name, value, ~ stringr::str_c(.x, ": ", .y[1]))
  )

mtcars %>% 
  group_by(cyl) %>% 
  summarise_all(list(list))

#If you want a single value, use mutate() with map_lgl(), map_int(), 
#map_dbl(), and map_chr() to create an atomic vector.

#If you want many values, use unnest() to convert list-columns back 
#to regular columns, repeating the rows as many times as necessary.

#List to vector
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

#Pulling parts
df <- tribble(
  ~x,
  list(a = 1, b = 2),
  list(a = 2, c = 4)
)
df %>% mutate(
  a = map_dbl(x, "a"),
  b = map_dbl(x, "b", .null = NA_real_)
)
#Unnesting
tibble(x = 1:2, y = list(1:4, 1)) %>% unnest(y)

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







































  
  
  
  
  
  
  
  
  
  
  
  
  
  

































































  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  