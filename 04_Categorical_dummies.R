#Creating dummy variables from categorical variables

library(fastDummies)

crime <- data.frame(city = c("SF", "SF", "NYC"),
                    year = c(1990, 2000, 1990),
                    crime = 1:3)
crime

dummy_cols(crime)
