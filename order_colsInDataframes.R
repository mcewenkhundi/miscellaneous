
#https://stackoverflow.com/questions/29873293/dply-order-columns-alphabetically-in-r/46950262

#https://community.rstudio.com/t/re-arranging-columns-in-numerical-order/55207

#using nume range 
#https://tidyselect.r-lib.org/reference/starts_with.html

#https://stackoverflow.com/questions/7334644/sort-columns-of-a-dataframe-by-column-name

test = data.frame(C = c(0, 2, 4, 7, 8), A = c(4, 2, 4, 7, 8), B = c(1, 3, 8, 3, 2))

test

test[, order(names(test))]
test[, sort(names(test))]

test %>% 
  dplyr::select(sort(names(.)))

#So to have a specific column come first, then the rest alphabetically, I'd propose this solution:

test[, c("B", sort(setdiff(names(test), "B")))]

test %>% 
  dplyr::select(A,sort(names(.)))



