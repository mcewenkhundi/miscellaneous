
#How to calculate row-wise percentages in a using the arsenal package
#The code below produces col-wise percentages, but i would like to produce row-wise percentages. 

library(arsenal)

 df <-  data.frame(arm = sample(c("Int","Contr"),size=10, replace = T), 
                  sex = sample(c("Male", "Female"),size=10, replace = T),
                 agegp = sample(c("<15","15-49",">50"),size=10, replace = T))

tab1 <- tableby(arm ~ sex + agegp, data=df)

summary(tab1, text=TRUE)

#Solution
summary(tableby(arm ~ sex + agegp, data = df, cat.stats = "countrowpct"), text = TRUE)

