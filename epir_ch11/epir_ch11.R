#Project: R book club epidemiologist R handbook
#Task: chapter 11
#Author: McEwen Khundi

pacman::p_load(
  rio,           # import/export
  here,          # filepaths
  lubridate,     # working with dates
  forcats,       # factors
  aweek,         # create epiweeks with automatic factor levels
  janitor,       # tables
  tidyverse      # data mgmt and viz
)

# import your dataset
linelist <- readRDS(here("linelist_cleaned.rds"))

#Creating a factor from a numerical variable
#Categories of delay, days from infection onset to hospitalisation
linelist <- linelist %>% 
  mutate(delay_cat = case_when(
    # criteria                                   # new value if TRUE
    days_onset_hosp < 2                        ~ "<2 days",
    days_onset_hosp >= 2 & days_onset_hosp < 5 ~ "2-5 days",
    days_onset_hosp >= 5                       ~ ">5 days",
    is.na(days_onset_hosp)                     ~ NA_character_,
    TRUE                                       ~ "Check me"))  

#delay cat is a character not yet a factor
#some checks
class(linelist$delay_cat)

table(linelist$delay_cat, useNA = "always")

##display in a bar graph
#check the order of the bars
ggplot(data = linelist)+
  geom_bar(mapping = aes(x = delay_cat))

#create a factor and also specify factor category order
linelist <- linelist %>%
  mutate(delay_cat = fct_relevel(delay_cat, "<2 days", "2-5 days", ">5 days"))

levels(linelist$delay_cat)

ggplot(data = linelist)+
  geom_bar(mapping = aes(x = delay_cat))

#Adjusting level order
#reversing the order of levels
linelist <- linelist %>%
  mutate(delay_cat_r = fct_rev(delay_cat))

ggplot(data = linelist)+
  geom_bar(mapping = aes(x = delay_cat_r))

#Order fct levels by frequency 
linelist <- linelist %>%
  mutate(delay_cat_infreq = fct_infreq(delay_cat))

ggplot(data = linelist, aes(x = delay_cat_infreq))+
  geom_bar()+
  labs(x = "Delay onset to admission (days)",
       title = "Ordered by frequency")

# reversed frequency
ggplot(data = linelist, aes(x = fct_rev(fct_infreq(delay_cat))))+
  geom_bar()+
  labs(x = "Delay onset to admission (days)",
       title = "Reverse of order by frequency")

#Order factor by the summary value of another column
# boxplots ordered by original factor levels
ggplot(data = linelist)+
  geom_boxplot(
    aes(x = delay_cat,
        y = ct_blood, 
        fill = delay_cat))+
  labs(x = "Delay onset to admission (days)",
       title = "Ordered by original alpha-numeric levels")+
  theme_classic()+
  theme(legend.position = "none") #remove legend


# boxplots ordered by median CT value
# Not appropriate here because the factor has an inherent order
ggplot(data = linelist)+
  geom_boxplot(
    aes(x = fct_reorder(delay_cat, ct_blood, "median"),
        y = ct_blood,
        fill = delay_cat))+
  labs(x = "Delay onset to admission (days)",
       title = "Ordered by median CT value in group")+
  theme_classic()+
  theme(legend.position = "none")

#Order by the end value of the lines
epidemic_data <- linelist %>%         # begin with the linelist   
  filter(date_onset < as.Date("2014-09-21")) %>%    # cut-off date, for visual clarity
  count(                                            # get case counts per week and by hospital
    epiweek = lubridate::floor_date(date_onset, "week"),  
    hospital                                            
  ) 

ggplot(data = epidemic_data)+                       # start plot
  geom_line(                                        # make lines
    aes(
      x = epiweek,                                  # x-axis epiweek
      y = n,                                        # height is number of cases per week
      color = fct_reorder2(hospital, epiweek, n)))+ # data grouped and colored by hospital, with factor order by height at end of plot
  labs(title = "Factor levels (and legend display) by line height at end of plot",
       color = "Hospital")                          # change legend title

#Combine levels
levels(linelist$delay_cat)

linelist %>% 
  mutate(delay_cat = fct_recode(
    delay_cat,
    "Less than 2 days" = "<2 days",
    "2 to 5 days"      = "2-5 days",
    "More than 5 days" = ">5 days")) %>% 
  tabyl(delay_cat)

linelist %>% 
  mutate(delay_cat = fct_recode(
    delay_cat,
    "Less than 5 days" = "<2 days",
    "Less than 5 days" = "2-5 days",
    "More than 5 days" = ">5 days")) %>% 
  tabyl(delay_cat)

#Recode by frequency
linelist %>%    
  mutate(hospital = fct_lump(                      # adjust levels
    hospital,
    n = 2,                                          # keep top 2 levels
    other_level = "Other Hospital")) %>%            # all others as "Other Hospital"
  tabyl(hospital)  

#Plotting epi weeks and options of x-axis tick labels
#On the display formats refer to section 9.8 of https://epirhandbook.com/en/working-with-dates.html#working-with-dates-1
linelist %>% 
  mutate(epiweek_date = floor_date(date_onset, "week")) %>%  # create week column
  ggplot()+                                                  # begin ggplot
  geom_histogram(mapping = aes(x = epiweek_date))+           # histogram of date of onset
  scale_x_date(date_labels = "%Y-W%W")                       # adjust disply of dates to be YYYY-WWw
