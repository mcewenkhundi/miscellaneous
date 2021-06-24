
library(palmerpenguins)
penguins <- penguins

#https://stats.stackexchange.com/questions/3212/mode-class-and-type-of-r-objects
#Vector types in R
typeof()
class()

#Testing functions

#Connection between a list and a data.frame/tibble in R

#Selecting variables base R vs dplyr


penguins_numeric <- dplyr::select(penguins, where(is.numeric))

out <- vector("double", length = ncol(penguins_numeric))

#Using an index 

for(i in seq_along(penguins_numeric)){
  
  out[[i]] <- mean(penguins_numeric[[i]], na.rm = TRUE)
}

#Using the value

for(i in penguins_numeric){
 print(i[1:6])
}

#Both value and name
#generic

for(i in seq_along(penguins_numeric)){
  name <- names(penguins_numeric)[i]
  value <- penguins_numeric[[i]]
  print("-------------------------")
  print(name)
  print(value[1:6])
}

#Unknown output length value
means <- c(0,1,3)

out_list <- vector("list", length = length(means))

for(i in seq_along(means)){
 n <- sample(100, 1) 
 
 out_list[[i]] <- rnorm(n, mean = means[[i]])
}
out_list


#Unknown input length
flips <- function(){sample(c("H","T"), 1)}

nheads <- 0
nflips <- 0

while(nheads < 3){
  
  if(flips()=="H"){
    nheads <- nheads + 1
  } else {
    nheads <- 0
  }
  
  nflips <- nflips + 1
  
}

simple_mean <- function(df){
  
  out <- vector("double", length = ncol(df))
  
  for(i in seq_along(df)){
    out[[i]] <- mean(df[[i]], na.rm = TRUE)
  }
  out
}

simple_mean(penguins_numeric)

simple_median <- function(df){
  
  out <- vector("double", length = ncol(df))
  
  for(i in seq_along(df)){
    out[[i]] <- mean(df[[i]], na.rm = TRUE)
  }
  out
}

simple_median(penguins_numeric)

simple_map <- function(df, fn,...){
  
  out <- vector("double", length = ncol(df))
  
  for(i in seq_along(df)){
    out[[i]] <- fn(df[[i]],...)
  }
  out
}

simple_map(penguins_numeric, fn = mean, na.rm=TRUE)

