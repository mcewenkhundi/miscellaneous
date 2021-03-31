library(tidyverse)

#Include a literal single or double quote
double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'"
#Beware that the printed represantation of the string is not the string
#itself. use writeLines()
x <- c("\"", "\\")
x
writeLines(x)

#Other special characters exist for newline "\n" for "\t" for tab
#to see others ?'"'

#String length
str_length(c("a", "R for data science", NA))
nchar(c("a", "R for data science", NA))

#Combining strings
#base r is paste()
str_c("x", "y")

str_c("x", "y", "z")

str_c("x", "y", sep = ", ")

#Missing values are contegious in R
x <- c("abc", NA)
str_c("|-", x, "-|")

str_c("|-", str_replace_na(x), "-|")

#R is vectolarised and always automatically recycles a shorter vector
#to the longest
str_c("prefix-", c("a", "b", "c"), "-suffix")

#objects of length zero are silently dropped
name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE

str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
)

#Subsetting strings
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)

str_sub(x, -3, -1)

#Notice str_sub wont fail if string is short
str_sub("a", 1, 5)

#Can also use substring subsetting to assign values
str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))

#Sorting
x <- c("apple", "eggplant", "banana")

str_sort(x, locale = "en")  # English
#> [1] "apple"    "banana"   "eggplant"

str_sort(x, locale = "haw") # Hawaiian
#> [1] "apple"    "eggplant" "banana"

#depends on the computer locale, so be careful
str_sort(x)

#Excercise
paste("a","b")
paste0("a","b")

#str_c does not separate strings just like paste0
str_c("a", "b")

#handling of strings differ between the paste functions and the
#str_c function one propagates missing various the others convert
#NA to "NA"
str_c("foo", NA)

paste("foo", NA)

paste0("foo", NA)

#Explain the difference between the collapse and sep argu in str_c
#collapse turns string into length one
str_c("a","b", sep = ",")
str_c("a","b", collapse = "_")

str_c(letters,"j", sep = ",")
str_c(letters,"j", collapse = ", ")
#selecting the middle character
x <- c("a", "abc", "abcd", "abcde", "abcdef")
L <- str_length(x)
m <- ceiling(L / 2)
str_sub(x, m, m)

#hte function str_trim() trims the whitespace from a string.

str_trim(" abc ")
#> [1] "abc"
str_trim(" abc ", side = "left")
#> [1] "abc "
str_trim(" abc ", side = "right")
#> [1] " abc"

#Opposite of str_trim is str_pad
#The opposite of str_trim() is str_pad() which adds characters to each side.

str_pad("abc", 5, side = "both")
#> [1] " abc "
str_pad("abc", 4, side = "right")
#> [1] "abc "
str_pad("abc", 4, side = "left")
#> [1] " abc"

# Write a function that turns (e.g.) a vector c("a", "b", "c") into 
# the string "a, b, and c". Think carefully about what it should do 
# if given a vector of length 0, 1, or 2.
# 
# See the Chapter [Functions] for more details on writing R functions.
# 
# This function needs to handle four cases.
# 
# n == 0: an empty string, e.g. "".
# n == 1: the original vector, e.g. "a".
# n == 2: return the two elements separated by “and”, e.g. "a and b".
# n > 2: return the first n - 1 elements separated by 
# commas, and the last element separated by a comma and “and”, e.g. "a, b, and c".
str_commasep <- function(x, delim = ",") {
  n <- length(x)
  if (n == 0) {
    ""
  } else if (n == 1) {
    x
  } else if (n == 2) {
    # no comma before and when n == 2
    str_c(x[[1]], "and", x[[2]], sep = " ")
  } else {
    # commas after all n - 1 elements
    not_last <- str_c(x[seq_len(n - 1)], delim)
    # prepend "and" to the last element
    last <- str_c("and", x[[n]], sep = " ")
    # combine parts with spaces
    str_c(c(not_last, last), collapse = " ")
  }
}
str_commasep("")
#> [1] ""
str_commasep("a")
#> [1] "a"
str_commasep(c("a", "b"))
#> [1] "a and b"
str_commasep(c("a", "b", "c"))
#> [1] "a, b, and c"
str_commasep(c("a", "b", "c", "d"))
#> [1] "a, b, c, and d"

#Matching patterns with regular expressions

#Basic matches with exact matches
x <- c("apple", "banana", "pear")
str_view(x, "an")

#The special character "." matches any character except a newline
str_view(x, ".a.")
# To use . in a regular expression we escape it
# To create the regular expression, we need \\
dot <- "\\."

# But the expression itself only contains one:
writeLines(dot)
#> \.

# And this tells R to look for an explicit .
str_view(c("abc", "a.c", "bef"), "a\\.c")

#To match \
x <- "a\\b"
writeLines(x)
#> a\b

str_view(x, "\\\\")

#Anchors tag the start or end of an expression
x <- c("apple", "banana", "pear")
str_view(x, "^a")
str_view(x, "a$")

#to match a full word
x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")
str_view(x, "^apple$")

#Word boundary \b

str_view(c("summarise","summary","rowsum","sum"), "\\bsum\\b")

str_view(stringr::words, "^yx$")

#Character classes and altenatives
# \d any digit, \s any white space and [abc]
#[^abc] matches anything except
# Look for a literal character that normally has special meaning in a regex
str_view(c("abc", "a.c", "a*c", "a c"), "a[.]c")
str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c")
str_view(c("abc", "a.c", "a*c", "a c"), "a[ ]")

#using or in regular expression
str_view(c("grey", "gray"), "gr(e|a)y")

#Excercise
#Words starting with vowels

str_subset(stringr::words, "[aeiou]")

#Words that contain only consonants: Use the negate argument of str_subset.

str_subset(stringr::words, "[aeiou]", negate=TRUE)

str_subset(stringr::words, "[^e]ed$")

str_subset(c("ed", stringr::words), "(^|[^e])ed$")

#Repetition
# ?: 0 or 1
# +: 1 or more
# *: 0 or more
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x, "CC+")
str_view(x, 'C[LX]+')

str_view(c("color","colour"), "colou?r")

str_view(c("bana","banana"), "bana(na)?")
str_view(c("bana","banana"), "bana(na)+")

# You can also specify the number of matches precisely:
#   
# {n}: exactly n
# {n,}: n or more
# {,m}: at most m
# {n,m}: between n and m

str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}")

#To match the shortest string
str_view(x, 'C{2,3}?')
str_view(x, 'C[LX]+?')

library(tidyverse)
#Str replace, allow you to replace matches with new strings
x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")

str_replace_all(x, "[aeiou]", "-")

#Replace multiple values
x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))

#You can use back references to replace
sentences %>% 
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>% 
  head(5)

