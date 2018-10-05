
#How to calculate row-wise percentages in a using the arsenal package
#The code below produces col-wise percentages, but i would like to produce row-wise percentages. 

library(arsenal )

tab1 <- tableby(arm ~ sex + race, data=mockstudy)

summary(tab1, text=TRUE)
