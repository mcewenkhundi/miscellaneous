#Chapter1: Names and values
#Copy-on-modify

#Q1: Why is tracemem(1:10) not useful?

# A: When 1:10 is called an object with an address in memory is created, but it
# is not bound to a name. Therefore, the object cannot be called or manipulated 
# from R. As no copies will be made, it is not useful to track the object for 
# copying.

obj_addr(1:10)  # the object exists, but has no name

# Q2: Explain why tracemem() shows two copies when you run this code. Hint: 
#   carefully look at the difference between this code and the code shown 
# earlier in the section.
x <- c(1L, 2L, 3L)
tracemem(x)

x[[3]] <- 4

#Can be avoided copy by sub-assignin an integer instead of a double

x <- c(1L, 2L, 3L)
tracemem(x)
#> <0x55eec6940ae0>

x[[3]] <- 4L

#Please be aware that running this code in RStudio will result in additional 
#copies because of the reference from the environment pane.

#Q3: Sketch out the relationship between the following objects:
a <- 1:10
b <- list(a, a)
c <- list(b, a, 1:10)

# A: a contains a reference to an address with the value 1:10. b contains a 
# list of two references to the same address as a. c contains a list of b 
# (containing two references to a), a (containing the same reference again)
# and a reference pointing to a different address containing the same value (1:10).
ref(c)

#Q4: What happens when you run this code:

x <- list(1:10)

ref(x)

tracemem(x)
x[[2]] <- x

ref(x)















