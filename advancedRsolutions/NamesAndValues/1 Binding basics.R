#Chapter1: Names and values
#1.1 Binding basics

library(lobstr)

#q1:Q1: Explain the relationship between a, b, c, and d in the following code:
a <- 1:10
b <- a
c <- b
d <- 1:10

list_of_names <- list(a, b, c, d)
obj_addrs(list_of_names)

# a, b, and c point to the same object (with the same address in memory). 
# This object has the value 1:10. d points to a different object with the
# same value.

# Q2: The following code accesses the mean function in multiple ways. Do they
# all point to the same underlying function object? Verify this with 
# lobstr::obj_addr().
mean
base::mean
get("mean")
evalq(mean)
match.fun("mean")

mean_functions <- list(
  mean,
  base::mean,
  get("mean"),
  evalq(mean),
  match.fun("mean")
)

unique(obj_addrs(mean_functions))











