#Sometimes you want to create some calculation based on other
#values that you are not interested to keep
#https://adv-r.hadley.nz/evaluation.html chapter 20.1

foo <- local({
  x <- 10
  y <- 200
  x + y
})

foo
#x not found in memory
x
#y not found in memory
y
