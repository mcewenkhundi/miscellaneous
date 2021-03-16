library(rlang)

e1 <- env(
  a = FALSE,
  b = "a",
  c = 2.3,
  d = 1:3,
)
e1

env_print(e1)
env_names(e1)

#To compare enviroments
identical(global_env(), current_env())
#> [1] TRUE

global_env() == current_env()
#> Error in global_env() == current_env(): comparison (1) is possible only for
#> atomic and list types

#Parent
e2a <- env(d = 4, e = 5)
e2b <- env(e2a, a = 1, b = 2, c = 3)

e2a
env_parent(e2a)
env_parent(e2b)
identical(e2a, env_parent(e2b))

#Only one environment does not have a parent the empty 
e2c <- env(empty_env(), d = 4, e = 5)
e2d <- env(e2c, a = 1, b = 2, c = 3)

#to see all the parents and acestors use env_parents
env_parents(e2a)
env_parents(e2b)
env_parents(e2b, last = empty_env())

#<<- creates a variable and modifies in the parrent env
x <- 0
f <- function() {
  x <<- 1
}
f()
x

#Getting and setting values
e3 <- env(x = 1, y = 2)
e3$x
#> [1] 1
e3$z <- 3
e3[["z"]]
#> [1] 3

#Numeric subscripts dont work
e3[[1]]

e3[c("x", "y")]

#adding values two ways
env_poke(e3, "a", 100)
e3$a
#> [1] 100

env_bind(e3, a = 10, b = 20)
env_names(e3)
#> [1] "x" "y" "z" "a" "b"

#To check if exists
env_has(e3, "a")
#>    a 
#> TRUE

#Null does not remove variables
e3$a <- NULL
env_has(e3, "a")
#>    a 
#> TRUE

env_unbind(e3, "a")
env_has(e3, "a")
#>     a 
#> FALSE


#Two variations of env_bind()

env_bind_lazy(current_env(), b = {sample(20,2)})

print(b)
#> [1] 1
#>    user  system elapsed 
#>    0.00    0.00    1.09
print(b)
#> [1] 1
#>    user  system elapsed 
#>       0       0       0

env_bind_active(current_env(), b = function() {sample(20,2)})

print(b)
#> [1] 1
#>    user  system elapsed 
#>    0.00    0.00    1.09
print(b)

#Recursing over enviroments to find a named object
where <- function(name, env = caller_env()) {
  if (identical(env, empty_env())) {
    # Base case
    stop("Can't find ", name, call. = FALSE)
  } else if (env_has(env, name)) {
    # Success case
    env
  } else {
    # Recursive case
    where(name, env_parent(env))
  }
}
where("yyy")
#> Error: Can't find yyy

x <- 5
where("x")
#> <environment: R_GlobalEnv>

where("mean")

#The same function above using iteration
f <- function(..., env = caller_env()) {
  if (identical(env, empty_env())) {
    # base case
  } else if (success) {
    # success case
  } else {
    # recursive case
    f(..., env = env_parent(env))
  }
}

f2 <- function(..., env = caller_env()) {
  while (!identical(env, empty_env())) {
    if (success) {
      # success case
      return()
    }
    # inspect parent
    env <- env_parent(env)
  }
  
  # base case
}

#Special environments 
base::search()
rlang::search_envs()

# The function environment
#A function binds to the environment in which it was created in
y <- 1
f <- function(x) x + y
fn_env(f)
#> <environment: R_GlobalEnv>

#The above is not always the case
e <- env()
e$g <- function() 1
env_print(g)
fn_env(e$g)

#Execution environment that is refresh everytime
g <- function(x) {
  if (!env_has(current_env(), "a")) {
    message("Defining a")
    a <- 1
  } else {
    a <- a + 1
  }
  a
}
g()

#Capturing the execution environment 
h2 <- function(x) {
  a <- x * 2
  current_env()
}

e <- h2(x = 10)
env_print(e)
#> <environment: 0x7fe6c8502738>
#> parent: <environment: global>
#> bindings:
#>  * a: <dbl>
#>  * x: <dbl>
fn_env(h2)
#> <environment: R_GlobalEnv>

#Another way to capture the envirment is to return an object
#that has a binding to it
plus <- function(x) {
  function(y) x + y
}

plus_one <- plus(1)
plus_one
#> function(y) x + y
#> <environment: 0x7fe6c6cd3ad8>

#Simple call stacks
f <- function(x) {
  g(x = 2)
}
g <- function(x) {
  h(x = 3)
}
h <- function(x) {
  stop()
}

f(x = 1)
#> Error:
traceback()
#> 4: stop()
#> 3: h(x = 3) 
#> 2: g(x = 2)
#> 1: f(x = 1)

#Instead of stop() + traceback() to 
#understand the call stack, we’re going to use lobstr::cst() to print out the call stack tree:
  
  h <- function(x) {
    lobstr::cst()
  }
f(x = 1)

#Revisions 

library(rlang)
e <- env()
e$g <- function() {1}
fn_env(e$g)

y <- env(x = function(){3})
y$x
fn_env(y$x)
