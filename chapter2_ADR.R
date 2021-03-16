#Quiz on the first page
df <- data.frame(runif(3), runif(3))
names(df) <- c(1,2)
df
df$`3` <- df$`1` + df$`2`
df

x <- runif(1e6)
y <- list(x, x, x)
library(lobstr)
obj_size(x)
obj_size(y)

a <- c(1, 5, 3, 2)
b <- a
b[[1]] <- 10

#end of quiz
#Bininding basics
x <- c(1,2,3)
y <- x
obj_addr(x)
obj_addr(y)

#Non syntactic names
#Names cant start with _ or a digit
?Reserved
#can use backticks to overide this behaviour
`_abc` <- 1
`_abc`

#Excercises
#Describe the relationship between a,b,c
a <- 1:10
b <- a
c <- b
d <- 1:10
list_temp <- list(a,b,c)
obj_addrs(list_temp)

#Different ways of calling the mean function, do they
#all point to the same function mean
mean
base::mean
get("mean")
match.fun("mean")

#could have used add_addrs and a list of the objects
obj_addr(mean)
obj_addr(base::mean)
obj_addr(evalq(mean))
obj_addr(match.fun("mean"))

#Copy on modify
#You can see when an object has been copied with the help
#of base::tracemem()
x <- c(1,2,3)
cat(tracemem(x), "\n")
obj_addr(x)
y <- x
y[[3]] <- 4L
obj_addr(y)

y[[3]] <- 5L
untracemem(y)
#Function calls
f <- function(a){
  a
}

x <- c(1, 2, 3)
cat(tracemem(x), "\n")
z <- f(x)

obj_addr(z)

#Lists
l1 <- list(1, 2, 3)
l2 <- l1
l2[[3]] <- 4
ref(l1,l2)

#Data frames
#Modifying columns has shalow copying
#Row modification has deep copying

#Character vectors
x <- c("a", "a", "abc", "d")
ref(x, character = TRUE)

#Excercise
x <- c(1L, 2L, 3L)
tracemem(x)

x[[1]] <- 1L

a <- 1:10
b <- list(a, a)
c <- list(b, a, 1:10)
b
a[[1]] <- 100L
b
ref(b)
ref(a)
b

#Object size 
base_pkgs <- c(
  "package:stats", "package:graphics", "package:grDevices",
  "package:utils", "package:datasets", "package:methods",
  "package:base"
)
library(dplyr)
base_objs <- base_pkgs %>%
  lapply(as.environment) %>%
  lapply(function(x) mget(ls(x, all.names = TRUE), x)) %>%
  setNames(base_pkgs)

sum(lengths(base_objs))

vapply(base_objs, obj_size, double(1)) / 1024^2
#getting elements from the global environment
lapply(ls(envir = .GlobalEnv), 
       function(x) {mget(x, envir = .GlobalEnv)})

as.numeric(obj_size(!!!base_objs)) / 1024^2

#predict the output
a <- runif(1e6)
obj_size(a)

b <- list(a, a)
obj_size(b)
obj_size(a, b)

b[[1]][[1]] <- 10
obj_size(b)
obj_size(a, b)

b[[2]][[1]] <- 10
obj_size(b)
obj_size(a, b)

#Environments
#are modified in place
e1 <- rlang::env(a = 1, b = 2, c = 3)
e1
e2 <- e1
e1$c <- 4
e2$c
#environments can contain themselves
e <- rlang::env()
e$self <- e
ref(e)

#Excercise 
x <- list()  # creates initial object
obj_addr(x)
#> [1] "0x55862f23ab80"

tracemem(x)
#> [1] "<0x55862f23ab80>"
x[[1]] <- x  # Copy-on-modify triggers new copy
#> tracemem[0x55862f23ab80 -> 0x55862e8ce028]:

obj_addr(x)       # copied object has new memory address
#> [1] "0x55862e8ce028"
obj_addr(x[[1]])  # list element contains old memory address
#> [1] "0x55862f23ab80"

#Bench marking 
create_random_df <- function(nrow, ncol) {
  random_matrix <- matrix(runif(nrow * ncol), nrow = nrow)
  as.data.frame(random_matrix)
}

create_random_df(2, 2)
#>      V1     V2
#> 1 0.972 0.0116
#> 2 0.849 0.4339
subtract_df <- function(x, medians) {
  for (i in seq_along(medians)) {
    x[[i]] <- x[[i]] - medians[[i]]
  }
  x
}

subtract_list <- function(x, medians) {
  x <- as.list(x)
  x <- subtract_df(x, medians)
  list2DF(x)
}

benchmark_medians <- function(ncol) {
  df <- create_random_df(nrow = 1e4, ncol = ncol)
  medians <- vapply(df, median, numeric(1), USE.NAMES = FALSE)
  
  bench::mark(
    "data frame" = subtract_df(df, medians),
    "list" = subtract_list(df, medians),
    time_unit = "ms"
  )
}

benchmark_medians(1)
#> # A tibble: 2 x 6
#>   expression    min median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>  <dbl>  <dbl>     <dbl> <bch:byt>    <dbl>
#> 1 data frame 0.0423 0.0774    12031.     344KB     11.5
#> 2 list       0.0567 0.127      7548.     156KB     16.5



