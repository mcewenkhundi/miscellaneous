#Project: Epir chapter 27
#Task: Chapter review

pacman::p_load(
  tidyverse,
  janitor,
  survival,
  survminer
)

# import linelist
linelist_case_data <- rio::import("linelist_cleaned.rds")

# Data management and transformation
# In short, survival data can be described as having the following three characteristics:
#   
#   the dependent variable or response is the waiting time until the occurrence of a well-defined event,
# observations are censored, in the sense that for some units the event of interest has not occurred at the time the data are analyzed, and
# there are predictors or explanatory variables whose effect on the waiting time we wish to assess or control.
# Thus, we will create different variables needed to respect that structure and run the survival analysis.
# 
# We define:
#   
#   a new data frame linelist_surv for this analysis
# our event of interest as being “death” (hence our survival probability will be the probability of being alive after a certain time after the time of origin),
# the follow-up time (futime) as the time between the time of onset and the time of outcome in days,
# censored patients as those who recovered or for whom the final outcome is not known ie the event “death” was not observed (event=0).

# CAUTION: Since in a real cohort study, the information on the time of origin and 
# the end of the follow-up is known given individuals are observed, we will remove
# observations where the date of onset or the date of outcome is unknown. Also the 
# cases where the date of onset is later than the date of outcome will be removed 
# since they are considered as wrong.

# TIP: Given that filtering to greater than (>) or less than (<) a date can remove
# rows with missing values, applying the filter on the wrong dates will also remove 
# the rows with missing dates.

#create a new data called linelist_surv from the linelist_case_data
#We then use case_when() to create a column age_cat_small in which there are only 3 age categories.

linelist_surv <-  linelist_case_data %>% 
  
  dplyr::filter(
    # remove observations with wrong or missing dates of onset or date of outcome
    date_outcome > date_onset) %>% 
  
  dplyr::mutate(
    # create the event var which is 1 if the patient died and 0 if he was right censored
    event = ifelse(is.na(outcome) | outcome == "Recover", 0, 1), 
    
    # create the var on the follow-up time in days
    futime = as.double(date_outcome - date_onset), 
    
    # create a new age category variable with only 3 strata levels
    age_cat_small = dplyr::case_when( 
      age_years < 5  ~ "0-4",
      age_years >= 5 & age_years < 20 ~ "5-19",
      age_years >= 20   ~ "20+"),
    
    # previous step created age_cat_small var as character.
    # now convert it to factor and specify the levels.
    # Note that the NA values remain NA's and are not put in a level "unknown" for example,
    # since in the next analyses they have to be removed.
    age_cat_small = fct_relevel(age_cat_small, "0-4", "5-19", "20+")
  )

# TIP: We can verify the new columns we have created by doing a summary on the 
# futime and a cross-tabulation between event and outcome from which it was created.
# Besides this verification it is a good habit to communicate the median follow-up
# time when interpreting survival analysis results.

summary(linelist_surv$futime)

# cross tabulate the new event var and the outcome var from which it was created
# to make sure the code did what it was intended to
linelist_surv %>% 
  tabyl(outcome, event)

#Now we cross-tabulate the new age_cat_small var and the old age_cat col to ensure correct assingments

linelist_surv %>% 
  tabyl(age_cat_small, age_cat)

#Now we review the 10 first observations of the linelist_surv data looking at 
#specific variables (including those newly created).

linelist_surv %>% 
  select(case_id, age_cat_small, date_onset, date_outcome, outcome, event, futime) %>% 
  head(10)

#We can also cross-tabulate the columns age_cat_small and gender to have more 
#details on the distribution of this new column by gender. We use tabyl() and 
#the adorn functions from janitor as described in the Descriptive tables page.

linelist_surv %>% 
  tabyl(gender, age_cat_small, show_na = F) %>% 
  adorn_totals(where = "both") %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front")

#27.3 Basics of survival analysis
# Use Suv() syntax for right-censored data
survobj <- Surv(time = linelist_surv$futime,
                event = linelist_surv$event)

#To review, here are the first 10 rows of the linelist_surv data, viewing only some important columns.

linelist_surv %>% 
  select(case_id, date_onset, date_outcome, futime, outcome, event) %>% 
  head(10)

#And here are the first 10 elements of survobj. It prints as essentially a vector of follow-up time, with “+” to represent if an observation was right-censored. See how the numbers align above and below.

#print the 50 first elements of the vector to see how it presents
head(survobj, 50)

#Running initial analyses
# The summary of this survfit object will give what is called a life table. For 
# each time step of the follow-up (time) where an event happened (in ascending order):
#   
# the number of people who were at risk of developing the event (people who did 
# not have the event yet nor were censored: n.risk)
# those who did develop the event (n.event)
# and from the above: the probability of not developing the event (probability of 
# not dying, or of surviving past that specific time)
# finally, the standard error and the confidence interval for that probability are derived and displayed

# We fit the KM estimates using the formula where the previously Surv object “survobj” is the response variable. “~ 1” precises we run the model for the overall survival.
# fit the KM estimates using a formula where the Surv object "survobj" is the response variable.
# "~ 1" signifies that we run the model for the overall survival  
linelistsurv_fit <-  survival::survfit(survobj ~ 1)

#print its summary for more details
summary(linelistsurv_fit)

#While using summary() we can add the option times and specify certain times at
#which we want to see the survival information

#print its summary at specific times
summary(linelistsurv_fit, times = c(5,10,20,30,60))

# We can also use the print() function. The print.rmean = TRUE argument is used to obtain the mean survival time and its standard error (se).
# 
# NOTE: The restricted mean survival time (RMST) is a specific survival measure more and more used in cancer survival analysis and which is often defined as the area under the survival curve, given we observe patients up to restricted time T (more details in Resources section).

# print linelistsurv_fit object with mean survival time and its se. 
print(linelistsurv_fit, print.rmean = TRUE)

#TIP: We can create the surv object directly in the survfit() function and save a
#line of code. This will then look like: 
#linelistsurv_quick <- survfit(Surv(futime, event) ~ 1, data=linelist_surv).

# Cumulative hazard
# Besides the summary() function, we can also use the str() function that gives 
#more details on the structure of the survfit() object. It is a list of 16 elements.
#Among these elements is an important one: cumhaz, which is a numeric vector. This could be plotted to allow show the cumulative hazard, with the hazard being the instantaneous rate of event occurrence (see references).
# 
str(linelistsurv_fit)

plot(linelistsurv_fit, 
     xlab = "Days of follow-up",    # x-axis label
     ylab="Survival Probability",   # y-axis label
     main= "Overall survival curve" # figure title
)

#The compliment of survival plot is mortality plot
# original plot
plot(
  linelistsurv_fit,
  xlab = "Days of follow-up",       
  ylab = "Survival Probability",       
  mark.time = TRUE,              # mark events on the curve: a "+" is printed at every event
  conf.int = FALSE,              # do not plot the confidence interval
  main = "Overall survival curve and cumulative mortality"
)

# draw an additional curve to the previous plot
lines(
  linelistsurv_fit,
  lty = 3,             # use different line type for clarity
  fun = "event",       # draw the cumulative events instead of the survival 
  mark.time = FALSE,
  conf.int = FALSE
)

# add a legend to the plot
legend(
  "topright",                               # position of legend
  legend = c("Survival", "Cum. Mortality"), # legend text 
  lty = c(1, 3),                            # line types to use in the legend
  cex = .85,                                # parametes that defines size of legend text
  bty = "n"                                 # no box type to be drawn for the legend
)

#27.4 Comparison of survival curves
#Log rank test

# create the new survfit object based on gender
linelistsurv_fit_sex <-  survfit(Surv(futime, event) ~ gender, data = linelist_surv)

#Now we can plot the survival curves by gender. Have a look at the order of the strata levels in the gender column before defining your colors and legend.

# set colors
col_sex <- c("lightgreen", "darkgreen")

# create plot
plot(
  linelistsurv_fit_sex,
  col = col_sex,
  xlab = "Days of follow-up",
  ylab = "Survival Probability")

# add legend
legend(
  "topright",
  legend = c("Female","Male"),
  col = col_sex,
  lty = 1,
  cex = .9,
  bty = "n")

#And now we can compute the test of the difference between the survival curves using survdiff()

#compute the test of the difference between the survival curves
survival::survdiff(
  Surv(futime, event) ~ gender, 
  data = linelist_surv
)

#Better plot from the survminer package
survminer::ggsurvplot(
  linelistsurv_fit_sex, 
  data = linelist_surv,          # again specify the data used to fit linelistsurv_fit_sex 
  conf.int = FALSE,              # do not show confidence interval of KM estimates
  surv.scale = "percent",        # present probabilities in the y axis in %
  break.time.by = 10,            # present the time axis with an increment of 10 days
  xlab = "Follow-up days",
  ylab = "Survival Probability",
  pval = T,                      # print p-value of Log-rank test 
  pval.coord = c(40,.91),        # print p-value at these plot coordinates
  risk.table = T,                # print the risk table at bottom 
  legend.title = "Gender",       # legend characteristics
  legend.labs = c("Female","Male"),
  font.legend = 10, 
  palette = "Dark2",             # specify color palette 
  surv.median.line = "hv",       # draw horizontal and vertical lines to the median survivals
  ggtheme = theme_light()        # simplify plot background
)

#We may also want to test for differences in survival by the source of infection (source of contamination).
#In this case, the Log rank test gives enough evidence of a difference in the survival probabilities at alpha= 0.005

linelistsurv_fit_source <-  survfit(
  Surv(futime, event) ~ source,
  data = linelist_surv
)

# plot
ggsurvplot( 
  linelistsurv_fit_source,
  data = linelist_surv,
  size = 1, linetype = "strata",   # line types
  conf.int = T,
  surv.scale = "percent",  
  break.time.by = 10, 
  xlab = "Follow-up days",
  ylab= "Survival Probability",
  pval = T,
  pval.coord = c(40,.91),
  risk.table = T,
  legend.title = "Source of \ninfection",
  legend.labs = c("Funeral", "Other"),
  font.legend = 10,
  palette = c("#E7B800","#3E606F"),
  surv.median.line = "hv", 
  ggtheme = theme_light()
)

#27.5 Cox regression analysis
#fitting the cox model
linelistsurv_cox_sexage <-  survival::coxph(
  Surv(futime, event) ~ gender + age_cat_small, 
  data = linelist_surv
)


#printing the model fitted
linelistsurv_cox_sexage

#summary of the model
summary(linelistsurv_cox_sexage)

#It was interesting to run the model and look at the results but a first look to verify whether the proportional hazards assumptions is respected could help saving time.
test_ph_sexage <- survival::cox.zph(linelistsurv_cox_sexage)
test_ph_sexage

# In another model we add more risk factors such as the source of infection and the number of days between date of onset and admission. This time, we first verify the proportional hazards assumption before going forward.
# 
# In this model, we have included a continuous predictor (days_onset_hosp). In this case we interpret the parameter estimates as the increase in the expected log of the relative hazard for each one unit increase in the predictor, holding other predictors constant. We first verify the proportional hazards assumption.

#fit the model
linelistsurv_cox <-  coxph(
  Surv(futime, event) ~ gender + age_years+ source + days_onset_hosp,
  data = linelist_surv
)


#test the proportional hazard model
linelistsurv_ph_test <- cox.zph(linelistsurv_cox)
linelistsurv_ph_test

#The graphical verification of this assumption may be performed with the function ggcoxzph() from the survminer package.
#Interpretation https://stats.stackexchange.com/questions/560975/how-to-interpret-schoenfield-residual-plot
# survminer::ggcoxzph(linelistsurv_ph_test)
# 
# The model results indicate there is a negative association between onset to admission duration and all-cause mortality. The expected hazard is 0.9 times lower in a person who who is one day later admitted than another, holding gender constant. Or in a more straightforward explanation, a one unit increase in the duration of onset to admission is associated with a 10.7% (coef *100) decrease in the risk of death.
# 
# Results show also a positive association between the source of infection and the all-cause mortality. Which is to say there is an increased risk of death (1.21x) for patients that got a source of infection other than funerals.
# 
#print the summary of the model
summary(linelistsurv_cox)

#We can verify this relationship with a table:
  
  linelist_case_data %>% 
  tabyl(days_onset_hosp, outcome) %>% 
  adorn_percentages() %>%  
  adorn_pct_formatting()
  
  # We would need to consider and investigate why this association exists in the data.
  # One possible explanation could be that patients who live long enough to be admitted 
  #later had less severe disease to begin with. Another perhaps more likely explanation is that since we used a simulated fake dataset, this pattern does not reflect reality!
  ggforest(linelistsurv_cox, data = linelist_surv)
  