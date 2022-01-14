#Chapter3: Names and values
#Object size

# Q1: In the following example, why are object.size(y) and obj_size(y) so
# radically different? Consult the documentation of object.size().
y <- rep(list(runif(1e4)), 100)

object.size(y)

obj_size(y)

# A: object.size() doesn’t account for shared elements within lists. 
# Therefore, the results differ by a factor of ~ 100.

#Q2: Take the following list. Why is its size somewhat misleading?
#Long response refer to the book for the answer
funs <- list(mean, sd, var)
obj_size(funs)

#Q3: Predict the output of the following code:
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
