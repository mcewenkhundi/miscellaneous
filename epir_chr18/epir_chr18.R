#R book club: The epidemiologist R handbook
#Chapter 18: Simple statistical tests
#Author: McEwen Khundi
#Date 25Feb2022

#Preparation
pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics, 
  gtsummary,    # summary statistics and tests
  rstatix,      # statistics
  corrr,        # correlation analayis for numeric variables
  janitor,      # adding totals and percents to tables
  flextable     # converting tables to HTML
)

#Simple tests to be explored
# T-test
# Shapiro-Wilk test
# Wilcoxon rank sum test
# Kruskal-Wallis test
# Chi-squared test

linelist <- import("linelist_cleaned.rds")

#Using base R: t.test
#Output hard to interpret, usually lists

## compare mean age by outcome group with a t-test
?t.test
t.test(age_years ~ gender, data = linelist)

# You can also use a t-test to determine whether a sample mean is 
# significantly different from some specific value. Here we conduct a one-sample t-test with the known/hypothesized population mean as mu =:
t.test(linelist$age_years, mu = 45)

#A shapiro  test: a test of normality
#Only for 3 to 5000 obs, otherwith use q-q plots
shapiro.test(linelist$age_years)

#Wilcoxon rank sum test
#When two numeric samples are not normally distributed or have unequal variance
## compare age distribution by outcome group with a wilcox test
wilcox.test(age_years ~ outcome, data = linelist)

# Chi-squared test
# Pearson’s Chi-squared test is used in testing for significant differences 
# between categorical croups.
## compare the proportions in each group with a chi-squared test
chisq.test(linelist$gender, linelist$outcome)

#rstatix package
#offers stats tests in a pipe-friendly approach.
#results in a data frame that can easly be used
#Works with grouped data

#Summary stats
summary(linelist$age_years)

summary(select(linelist, age, temp))

linelist %>%
  rstatix::get_summary_stats(age, temp)

#Can specify a range of stats
# “full”, “common”, “robust”, “five_number”, “mean_sd”,
# “mean_se”, “mean_ci”, “median_iqr”, 
# “median_mad”, “quantile”, “mean”, “median”, “min”, “max”.
linelist %>%
  group_by(hospital) %>%
  rstatix::get_summary_stats(age, temp, type = "common")

#T-tests
linelist %>% 
  t_test(age_years ~ gender)

#One sample test
linelist %>% 
  t_test(age_years ~ 1, mu = 30)

#Can do the test within groups
linelist %>% 
  group_by(gender) %>% 
  t_test(age_years ~ 1, mu = 18)

#Chi-squared test
#The chi-square test function accepts a table, so first we 
#create a cross-tabulation. 
#There are many ways to create a cross-tabulation (see Descriptive tables) 
#but here we use tabyl() from janitor and remove the left-most column of value labels before passing to chisq_test().

linelist %>% 
  tabyl(gender, outcome) %>% 
  select(-1) %>% 
  chisq_test()

#Cross tabs from gtsummary
linelist %>%
  tbl_cross(
    row = gender,
    col = outcome,
    percent = "col" #row, cell
  ) %>%
  add_p()



#gtsummary package
#Produce pretty descriptive table for reports
linelist %>% 
  select(gender,cough, fever, chills, age_years, outcome) %>%    # keep variables of interest
  tbl_summary(by = outcome) %>%  # produce summary table and specify grouping variable
  add_p()   

#Can customise 
#Age years, mean and sd, and t_test
linelist %>% 
  select(gender,cough, fever, chills, age_years, outcome) %>%             # keep variables of interest
  tbl_summary(                               # produce summary table
    statistic = age_years ~ "{mean} ({sd})", # specify what statistics to show
    by = outcome) %>%                        # specify the grouping variable
  add_p(age_years ~ "t.test")     

#Other customisations
#Change variable names
#Table title
#Characteristics to variable
#Introduce the outcome title
linelist %>% 
  select(gender,cough, fever, chills, age_years, outcome) %>%             # keep variables of interest
  tbl_summary(                               # produce summary table
    statistic = age_years ~ "{mean} ({sd})", # specify what statistics to show
    by = outcome,                            # specify the grouping variable
    label = list(age_years ~ "Age years", # labels
                 gender ~ "Gender",
                 chills ~ "Chills",
                 fever ~ "Fever",
                 cough ~ "Cough"),
    missing_text= "Missing") %>%          #text for missing values              
  add_p(age_years ~ "t.test") %>% 
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Treatment Outcome**") %>%
  modify_caption("**Table 1. Patient Characteristics**") 


#Other customisations
#Using selecters, labels, captions
#https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html

linelist %>% 
  mutate(gender = factor(gender, levels = c("f","m"),
                         labels = c("Female", "Male"))) %>%
  select(gender,cough, fever, chills, age_years, wt_kg, outcome) %>%             # keep variables of interest
  tbl_summary( # produce summary table
    #type = list(c(chills, fever, cough) ~ "categorical"), #if you want to show yes and no's
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / {N} ({p}%)"), # specify what statistics to show
        percent = "col", #change the row, col or cell percentage calculation here
    by = outcome,                            # specify the grouping variable
    label = list(age_years ~ "Age years", # labels
                 gender ~ "Gender",
                 chills ~ "Chills",
                 fever ~ "Fever",
                 cough ~ "Cough",
                 wt_kg ~ "Weight Kg"),
    missing_text= "Missing") %>%          #text for missing values              
  add_p(all_continuous() ~ "t.test") %>% 
  add_overall(last = TRUE) %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "Treatment Outcome") %>%
  modify_caption("Table 1. Patient Characteristics") %>%
  bold_labels() %>%
  as_flex_table() %>%  #convert to flextable and export to word
  theme_vanilla() %>% #style of lines  
  save_as_docx(path = "Overall baselline.docx")

#Produce report for each clinic
#Include clinic name in title and file name 
clinic_names <- unique(linelist$hospital)

for(clinic in clinic_names){
  
  linelist %>% 
    filter(hospital ==  clinic) %>%
    mutate(gender = factor(gender, levels = c("f","m"), labels = c("Female", "Male"))) %>%
    select(gender,cough, fever, chills, age_years, wt_kg, outcome) %>%             # keep variables of interest
    tbl_summary(                               # produce summary table
      statistic = list(all_continuous() ~ "{mean} ({sd})",
                       all_categorical() ~ "{n} / {N} ({p}%)"), # specify what statistics to show
      percent = "col", #change the row, col or cell percentage calculation here
      by = outcome,                            # specify the grouping variable
      label = list(age_years ~ "Age years", # labels
                   gender ~ "Gender",
                   chills ~ "Chills",
                   fever ~ "Fever",
                   cough ~ "Cough",
                   wt_kg ~ "Weight Kg"),
      missing_text= "Missing") %>%          #text for missing values              
    add_p(all_continuous() ~ "t.test") %>% 
    add_overall(last = TRUE) %>% 
    modify_header(label ~ "**Variable**") %>%
    modify_spanning_header(c("stat_1", "stat_2") ~ "Treatment Outcome") %>%
    modify_caption("Table 1. Patient Characteristics: {clinic}") %>% #include title name in the title of each table
    bold_labels() %>%
    as_flex_table() %>% #convert to flextable and export to word, https://community.rstudio.com/t/how-to-export-summary-table-into-word/99952/4
    theme_vanilla() %>% #style of lines #https://ardata-fr.github.io/flextable-book/index.html
    save_as_docx(path = paste0("Overall baselline ",clinic,".docx"))
  
}
