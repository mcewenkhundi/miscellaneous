library(brms)

zinb <- read.csv("http://stats.idre.ucla.edu/stat/data/fish.csv")
zinb$camper <- factor(zinb$camper, labels = c("no", "yes"))
head(zinb)

fit_zinb1 <- brm(count ~ persons + child + camper, data = zinb,
                 family = zero_inflated_poisson("log"))

summary(fit_zinb1)
report::report(fit_zinb1)
report::report_priors(fit_zinb1)
