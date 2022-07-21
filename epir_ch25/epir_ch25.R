# Author: McEwen Khundi
# Project: R club
# Task: Chapter 25, Contact tracing

pacman::p_load(
  rio, # importing data
  here, # relative file pathways
  janitor, # data cleaning and tables
  lubridate, # working with dates
  epikit, # age_categories() function
  apyramid, # age pyramids
  tidyverse, # data manipulation and visualization
  RColorBrewer, # color palettes
  formattable, # fancy tables
  kableExtra # table formatting
)

# datasets downloaded from links on the book page
# Case data
# These data are a table of the cases, and information about them.

cases <- import(here("cases_clean.rds")) %>%
  select(
    case_id, firstName, lastName, gender, age, age_class,
    occupation, classification, was_contact, hospitalization_typeid
  )

# Contacts data
# These data are a table of all the contacts and information about them. Again,
# provide your own file path. After importing we perform a
# few preliminary data cleaning steps including:

# Set age_class as a factor and reverse the level order so that younger ages are first
# Select only certain column, while re-naming a one of them
# Artificially assign rows with missing admin level 2 to “Djembe”,
# to improve clarity of some example visualisations

contacts <- import(here("contacts_clean.rds")) %>%
  mutate(age_class = forcats::fct_rev(age_class)) %>%
  select(contact_id, contact_status, firstName, lastName, gender, age,
    age_class, occupation, date_of_reporting, date_of_data_entry,
    date_of_last_exposure = date_of_last_contact,
    date_of_followup_start, date_of_followup_end, risk_level, was_case, admin_2_name
  ) %>%
  mutate(admin_2_name = replace_na(admin_2_name, "Djembe"))

# Follow-up data
# These data are records of the “follow-up” interactions with the contacts.
# Each contact is supposed to have an encounter each day for 14 days after their exposure.

followups <- rio::import(here::here("followups_clean.rds")) %>%
  select(
    contact_id, followup_status, followup_number,
    date_of_followup, admin_2_name, admin_1_name
  ) %>%
  mutate(followup_status = str_to_lower(followup_status))

# Relationships data
# Here we import data showing the relationship between cases and contacts.
# We select certain column to show.
relationships <- rio::import(here::here("relationships_clean.rds")) %>%
  select(
    source_visualid, source_gender, source_age, date_of_last_contact,
    date_of_data_entry, target_visualid, target_gender,
    target_age, exposure_type
  )

# Descriptive analyses
# Demographics

# Age and Gender of contacts
# The pyramid below compares the age distribution of contacts, by gender. Note
# that contacts missing age are included in their own bar at the top. You can
# change this default behavior, but then consider listing the number missing
# in a caption.

apyramid::age_pyramid(
  data = contacts, # use contacts dataset
  age_group = "age_class", # categorical age column
  split_by = "gender"
) + # gender for halfs of pyramid
  labs(
    fill = "Gender", # title of legend
    title = "Age/Sex Pyramid of COVID-19 contacts"
  ) + # title of the plot
  theme_minimal()

# the relationships dataset contains the ages of both cases and contacts,
# so you could use that dataset and create an age pyramid showing the differences
# between these two groups of people. The relationships data frame will be mutated
# to transform the numberic age columns into categories (see the Cleaning data and
# core functions page). We also pivot the dataframe longer to facilitate easy
# plotting with ggplot2 (see Pivoting data).

relation_age <- relationships %>%
  select(source_age, target_age) %>%
  transmute( # transmute is like mutate() but removes all other columns not mentioned
    source_age_class = epikit::age_categories(source_age, breakers = seq(0, 80, 5)),
    target_age_class = epikit::age_categories(target_age, breakers = seq(0, 80, 5)),
  ) %>%
  pivot_longer(cols = contains("class"), names_to = "category", values_to = "age_class") # pivot longer


relation_age


# Now we can plot this transformed dataset with age_pyramid() as before, but
# replacing gender with category (contact, or case).

apyramid::age_pyramid(
  data = relation_age, # use modified relationship dataset
  age_group = "age_class", # categorical age column
  split_by = "category"
) + # by cases and contacts
  scale_fill_manual(
    values = c("orange", "purple"), # to specify colors AND labels
    labels = c("Case", "Contact")
  ) +
  labs(
    fill = "Legend", # title of legend
    title = "Age/Sex Pyramid of COVID-19 contacts and cases"
  ) + # title of the plot
  theme_minimal() # simple background

# We can also view other characteristics such as occupational
# breakdown (e.g. in form of a pie chart).
# Clean dataset and get counts by occupation
occ_plot_data <- cases %>%
  mutate(
    occupation = forcats::fct_explicit_na(occupation), # make NA missing values a category
    occupation = forcats::fct_infreq(occupation)
  ) %>% # order factor levels in order of frequency
  count(occupation) # get counts by occupation

# Make pie chart
ggplot(data = occ_plot_data, mapping = aes(x = "", y = n, fill = occupation)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(
    fill = "Occupation",
    title = "Known occupations of COVID-19 cases"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )

# Contacts per case
# The number of contacts per case can be an important metric to assess quality of
# contact enumeration and the compliance of the population toward public health response.
# Depending on your data structure, this can be assessed with a dataset that
# contains all cases and contacts. In the Go.Data datasets, the links between cases
# (“sources”) and contacts (“targets”) is stored in the relationships dataset.
# In this dataset, each row is a contact, and the source case is listed in the row.
# There are no contacts who have relationships with multiple cases, but if this
# exists you may need to account for those before plotting (and explore them too!).
# We begin by counting the number of rows (contacts) per source case. This
# is saved as a data frame.

contacts_per_case <- relationships %>%
  count(source_visualid)

contacts_per_case

# We use geom_histogram() to plot these data as a histogram.

ggplot(data = contacts_per_case) + # begin with count data frame created above
  geom_histogram(mapping = aes(x = n)) + # print histogram of number of contacts per case
  scale_y_continuous(expand = c(0, 0)) + # remove excess space below 0 on y-axis
  theme_light() + # simplify background
  labs(
    title = "Number of contacts per case",
    y = "Cases",
    x = "Contacts per case"
  )


# Contact Follow Up
# Contact tracing data often contain “follow-up” data, which record outcomes of
# daily symptom checks of persons in quarantine. Analysis of this data can inform
# response strategy, identify contacts at-risk of loss-to-follow-up or at-risk of
# developing disease.

# Let’s see how many instances of “duplicate” rows we have:

followups %>%
  count(contact_id, date_of_followup) %>% # get unique contact_days
  filter(n > 1)


followups_clean <- followups %>%
  
  # De-duplicate
  group_by(contact_id, date_of_followup) %>%        # group rows per contact-day
  arrange(contact_id, desc(date_of_followup)) %>%   # arrange rows, per contact-day, by date of follow-up (most recent at top)
  slice_head() %>%                                  # keep only the first row per unique contact id  
  ungroup() %>% 
  
  # Other cleaning
  mutate(followup_number = replace(followup_number, followup_number > 14, NA)) %>% # clean erroneous data
  drop_na(contact_id)    

# In this dataset, “seen_not_ok” means “seen with symptoms”, and “seen_ok” 
# means “seen without symptoms”.
followups_clean %>% 
  tabyl(followup_status)

# Plot over time
# As the dates data are continuous, we will use a histogram to plot them with
# date_of_followup assigned to the x-axis. We can achieve a “stacked” histogram 
# by specifying a fill = argument within aes(), which we assign to the column 
# followup_status. Consequently, you can set the legend title using the fill = argument of labs().

#We can see that the contacts were identified in waves (presumably corresponding
#with epidemic waves of cases), and that follow-up completion did not seemingly
#improve over the course of the epidemic

ggplot(data = followups_clean)+
  geom_histogram(mapping = aes(x = date_of_followup, fill = followup_status)) +
  scale_fill_discrete(drop = FALSE)+   # show all factor levels (followup_status) in the legend, even those not used
  theme_classic() +
  labs(
    x = "",
    y = "Number of contacts",
    title = "Daily Contact Followup Status",
    fill = "Followup Status",
    subtitle = str_glue("Data as of {max(followups$date_of_followup, na.rm=T)}"))   # dynamic subtitle

#Daily individual tracking
#A convenient visualisation mechanism (if the number of cases is not too large) 
#can be a heat plot, made with geom_tile(). See more details in the [heat plot] page.

ggplot(data = followups_clean)+
  geom_tile(mapping = aes(x = followup_number, y = contact_id, fill = followup_status),
            color = "grey")+       # grey gridlines
  scale_fill_manual( values = c("yellow", "grey", "orange", "darkred", "darkgreen"))+
  theme_minimal()+
  scale_x_continuous(breaks = seq(from = 1, to = 14, by = 1))

#Analyse by group
#Perhaps these follow-up data are being viewed on a daily or weekly basis for 
#operational decision-making. You may want more meaningful disaggregations by 
#geographic area or by contact-tracing team. We can do this by adjusting
#the columns provided to group_by().
plot_by_region <- followups_clean %>%                                        # begin with follow-up dataset
  count(admin_1_name, admin_2_name, followup_status) %>%   # get counts by unique region-status (creates column 'n' with counts)
  
  # begin ggplot()
  ggplot(                                         # begin ggplot
    mapping = aes(x = reorder(admin_2_name, n),     # reorder admin factor levels by the numeric values in column 'n'
                  y = n,                            # heights of bar from column 'n'
                  fill = followup_status,           # color stacked bars by their status
                  label = n))+                      # to pass to geom_label()              
  geom_col()+                                     # stacked bars, mapping inherited from above 
  geom_text(                                      # add text, mapping inherited from above
    size = 3,                                         
    position = position_stack(vjust = 0.5), 
    color = "white",           
    check_overlap = TRUE,
    fontface = "bold")+
  coord_flip()+
  labs(
    x = "",
    y = "Number of contacts",
    title = "Contact Followup Status, by Region",
    fill = "Followup Status",
    subtitle = str_glue("Data as of {max(followups_clean$date_of_followup, na.rm=T)}")) +
  theme_classic()+                                                                      # Simplify background
  facet_wrap(~admin_1_name, strip.position = "right", scales = "free_y", ncol = 1)      # introduce facets 

plot_by_region


#KPI Tables
# Set "Report date" to simulate running the report with data "as of" this date
report_date <- as.Date("2020-06-10")

# Create follow-up data to reflect the report date.
table_data <- followups_clean %>% 
  filter(date_of_followup <= report_date)


followup_info <- table_data %>% 
  group_by(contact_id) %>% 
  summarise(
    date_last_record   = max(date_of_followup, na.rm=T),
    date_last_seen     = max(date_of_followup[followup_status %in% c("seen_ok", "seen_not_ok")], na.rm=T),
    status_last_record = followup_status[which(date_of_followup == date_last_record)]) %>% 
  ungroup()

#Now we will add this information to the contacts dataset, and calculate some additional columns.

contacts_info <- followup_info %>% 
  right_join(contacts, by = "contact_id") %>% 
  mutate(
    database_date       = max(date_last_record, na.rm=T),
    days_since_seen     = database_date - date_last_seen,
    days_since_exposure = database_date - date_of_last_exposure
  )

#Next we summarise the contacts data by region, to achieve a concise data frame
# of summary statistic columns.
contacts_table <- contacts_info %>% 
  
  group_by(`Admin 2` = admin_2_name) %>%
  
  summarise(
    `Registered contacts` = n(),
    `Active contacts`     = sum(contact_status == "UNDER_FOLLOW_UP", na.rm=T),
    `In first week`       = sum(days_since_exposure < 8, na.rm=T),
    `In second week`      = sum(days_since_exposure >= 8 & days_since_exposure < 15, na.rm=T),
    `Became case`         = sum(contact_status == "BECAME_CASE", na.rm=T),
    `Lost to follow up`   = sum(days_since_seen >= 3, na.rm=T),
    `Never seen`          = sum(is.na(date_last_seen)),
    `Followed up - signs` = sum(status_last_record == "Seen_not_ok" & date_last_record == database_date, na.rm=T),
    `Followed up - no signs` = sum(status_last_record == "Seen_ok" & date_last_record == database_date, na.rm=T),
    `Not Followed up`     = sum(
      (status_last_record == "NOT_ATTEMPTED" | status_last_record == "NOT_PERFORMED") &
        date_last_record == database_date, na.rm=T)) %>% 
  
  arrange(desc(`Registered contacts`))

#And now we apply styling from the formattable and knitr packages, including a
#footnote that shows the “as of” date.
contacts_table %>%
  mutate(
    `Admin 2` = formatter("span", style = ~ formattable::style(
      color = ifelse(`Admin 2` == NA, "red", "grey"),
      font.weight = "bold",font.style = "italic"))(`Admin 2`),
    `Followed up - signs`= color_tile("white", "orange")(`Followed up - signs`),
    `Followed up - no signs`= color_tile("white", "#A0E2BD")(`Followed up - no signs`),
    `Became case`= color_tile("white", "grey")(`Became case`),
    `Lost to follow up`= color_tile("white", "grey")(`Lost to follow up`), 
    `Never seen`= color_tile("white", "red")(`Never seen`),
    `Active contacts` = color_tile("white", "#81A4CE")(`Active contacts`)
  ) %>%
  kable("html", escape = F, align =c("l","c","c","c","c","c","c","c","c","c","c")) %>%
  kable_styling("hover", full_width = FALSE) %>%
  add_header_above(c(" " = 3, 
                     "Of contacts currently under follow up" = 5,
                     "Status of last visit" = 3)) %>% 
  kableExtra::footnote(general = str_glue("Data are current to {format(report_date, '%b %d %Y')}"))

#Transmission Matrices
heatmap_ages <- relationships %>% 
  select(source_age, target_age) %>% 
  mutate(                              # transmute is like mutate() but removes all other columns
    source_age_class = epikit::age_categories(source_age, breakers = seq(0, 80, 5)),
    target_age_class = epikit::age_categories(target_age, breakers = seq(0, 80, 5))) 
cross_tab <- table(
  source_cases = heatmap_ages$source_age_class,
  target_cases = heatmap_ages$target_age_class)

cross_tab
### stopped
long_prop <- data.frame(prop.table(cross_tab))

ggplot(data = long_prop)+       # use long data, with proportions as Freq
  geom_tile(                    # visualize it in tiles
    aes(
      x = target_cases,         # x-axis is case age
      y = source_cases,     # y-axis is infector age
      fill = Freq))+            # color of the tile is the Freq column in the data
  scale_fill_gradient(          # adjust the fill color of the tiles
    low = "blue",
    high = "orange")+
  theme(axis.text.x = element_text(angle = 90))+
  labs(                         # labels
    x = "Target case age",
    y = "Source case age",
    title = "Who infected whom",
    subtitle = "Frequency matrix of transmission events",
    fill = "Proportion of all\ntranmsission events"     # legend title
  )














