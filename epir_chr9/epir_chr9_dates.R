#Project: R book club (Epir handbook)
#Task: Data mananagement of dates (epir book chapter 9)
#Author: McEwen.

# Checks if package is installed, installs if necessary, and loads package
#for current session
pacman::p_load(
  lubridate,  # general package for handling and converting dates  
  linelist,   # has function to "guess" messy dates
  aweek,      # another option for converting dates to weeks, and weeks to dates
  zoo,        # additional date/time functions
  tidyverse,  # data management and visualization  
  rio)        # data import/export

#Use the option below to install linelist if it fails to install above then
#re run the above script
#devtools::install_github("reconhub/linelist")

#Download the example dataset
#Refer to the Download handbook and data chapter 2
#pacman::p_install_gh("appliedepi/epirhandbook")

epirhandbook::get_data(file = "linelist_cleaned.xlsx")
#save file as csv to lose excel pre-formating

#Import data into R
linelist <- import("linelist_cleaned.csv")


#Current date
Sys.Date()
Sys.time()

#Using function from lubridate
today()
now()
date()

#Importing dates in R, lubridate package is very friendly than the base R
#Specify order of day, month and year
#Seperators automatically identified
ymd("2020-10-11")
ymd("20201011")
mdy("10/11/2020")
mdy("Oct 11 20")

#Lets check the linelist dataset
#An ebora outbreak simualated dataset
View(linelist)

#check the classes of variables, date vars only
linelist %>%
  select(contains("date"))%>%
  glimpse()

# Use lubridate function to convert to dates
linelist <- linelist %>%
  mutate(date_onset = lubridate::dmy(date_onset))

linelist %>%
  select(contains("date"))%>%
  glimpse()

#Since all have the same formating lets convert them at once
#This is only possible if all the date vars have the same format
linelist <- linelist %>%
  mutate(across(.cols = contains("date"),.fns = dmy))

linelist %>%
  select(contains("date"))%>%
  glimpse()

#If you have parts of a date in separate columns
make_date(year = "2021", month = "11", day = "03")

#Sometimes the same column might have different formated dates, dmy, mdy etc
#Use with caution
linelist::guess_dates(c("03 Jan 2018",
                        "07/03/1982",
                        "08/20/85"))

#Working with times alone use 
time1 <- "13:45" 
time2 <- "15:20"

# Times converted to a datetime class
#Opted to use hm() from lubridate instead of the strptime function used in the book
#simpler
time1_clean <- hm(time1)
time2_clean <- hm(time2)

#Extract hour(), minute(), and second
hour(time1_clean)
minute(time2_clean)

#Use above to categorise the time of admission ("Morning, Afternoon, Evening, Night")
linelist <- linelist %>%
  mutate(hour_admit = hour(hm(time_admission))) %>%
  mutate(time_period = case_when(
    hour_admit > 06 & hour_admit < 12 ~ "Morning",
    hour_admit >= 12 & hour_admit < 17 ~ "Afternoon",
    hour_admit >= 17 & hour_admit < 21 ~ "Evening",
    hour_admit >=21 | hour_admit <= 6 ~ "Night"))

#why are some missing
linelist %>%
  count(time_period)

#Check if no information provided
sum((linelist$time_admission==""))

#Extracting date components

example_date <- ymd("2020-03-01")
month(example_date, label = TRUE)

day(example_date) 

wday(example_date)

# Filtering dates 
# Use a specific cut out
linelist_oct14_15_y2015 <- linelist %>%
  filter(between(date_hospitalisation,dmy("14Oct2014"),dmy("30Oct2014"))) 

nrow(linelist) - nrow(linelist_oct14_15_y2015)

##all cases in january hospitalisations
linelist_all_janhospitalisations <- linelist %>%
  filter(month(date_hospitalisation) == 1) 

nrow(linelist_all_janhospitalisations)

#Same with extracting time from a datetime variable

example_datetime <- ymd_hm("2020-03-01 14:45")

hour(example_datetime)     # extract hour
minute(example_datetime)   # extract minute
second(example_datetime)   # extract second

#Date arithmatic

example_date - ymd("2020-02-20")

#Find the difference between the date of hospitalisation and date of onset
linelist <- linelist %>%
            mutate(days_onset_hosp_new = date_hospitalisation - date_onset)

#summary stats by fever etc
linelist %>%
  group_by(fever) %>%
  summarise(mean_delay = mean(days_onset_hosp_new, na.rm = TRUE))

linelist %>%
  group_by(cough) %>%
  summarise(mean_delay = mean(days_onset_hosp_new, na.rm = TRUE))

#Epidemiological weeks
#rounding the date to the start of the week 
#floor_date(, unit = "week")
weekly_counts <- linelist %>% 
  drop_na(date_onset) %>%             # remove cases missing onset date
  mutate(weekly_cases = floor_date(   # make new column, week of onset
    date_onset,
    unit = "week"))

weekly_counts <- weekly_counts  %>%            
                 count(weekly_cases, gender) 

#Plot the data of onset of infection
weekly_counts %>%
  ggplot(aes(x=weekly_cases, y=n, group = gender, color = gender)) +
  geom_line() +
  labs(x = "Week of onset", y = "Count") +
  theme_bw() #remove the default black and white theme

#if you wanted the year to be first
weekly_counts %>%
  ggplot(aes(x=weekly_cases, y=n, group = gender, color = gender)) +
  geom_line() +
  scale_x_date(date_labels = "%Y %b") +
  labs(x = "Week of onset", y = "Count") +
  theme_bw() #remove the default black and white theme

#graph of hospitalisations
#By day
Byday_counts_hospitalizations <- linelist  %>%            
  count(date_hospitalisation, gender) 

Byday_counts_hospitalizations %>%
  ggplot(aes(x=date_hospitalisation, y=n, group = gender, color = gender)) +
  geom_line() +
  labs(x = "Week of onset", y = "Count") +
  theme_bw() #remove the default black and white theme

#By week
Byweek_counts_hospitalizations <- linelist  %>%     
   mutate(date_hospitalisation = floor_date(date_hospitalisation, unit = "week")) %>%
  count(date_hospitalisation, gender) 

Byweek_counts_hospitalizations %>%
  ggplot(aes(x=date_hospitalisation, y=n, group = gender, color = gender)) +
  geom_line() +
  labs(x = "Week of hospitalisation", y = "Count") +
  theme_bw() #remove the default black and white theme


##By day of the week with most hospitalisations
Byweek_counts_hospitalizations <- linelist  %>%     
  mutate(date_hospitalisation_wday = wday(date_hospitalisation, label = TRUE)) %>%
  count(date_hospitalisation_wday, gender) 

Byweek_counts_hospitalizations %>%
  ggplot(aes(x = date_hospitalisation_wday, y = n)) +
  geom_col()

linelist %>%
  filter(date_hospitalisation >= dmy("1Jan2015"))