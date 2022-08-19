#Project: Epir chapter 30
#Title: ggplot2 basics
#Task: Chapter review

pacman::p_load(
  tidyverse,      # includes ggplot2 and other data management tools
  rio,            # import/export
  here,           # file locator
  stringr,         # working with characters
  janitor         # quick tabulations and cleaning of col names
)

#import linelist
#this is a dataset of cases from a simulated Ebola epidemic.
linelist <- rio::import("linelist_cleaned.rds")

#The variables of focus will be the 
#continuous variables age, wt_kg (weight in kilos), 
#ct_blood (CT values), and 
#days_onset_hosp (difference between onset date and hospitalisation)
View(linelist)

#Usually some data transformation is done before producing a graph
#That might be changing values of a factor/character
#Reshapig data from wide form to long form

# make display version of columns with more friendly names
linelist %>%
  tabyl(gender) # Need better labels for gender and missing values

#done below
linelist <- linelist %>%
  mutate(
    gender_disp = case_when(gender == "m" ~ "Male",        # m to Male 
                            gender == "f" ~ "Female",      # f to Female,
                            is.na(gender) ~ "Unknown"),    # NA to Unknown
    
    outcome_disp = replace_na(outcome, "Unknown")          # replace NA outcome with "unknown"
  )

linelist %>%
  tabyl(gender_disp)

#Longer data format plays well with graphs
symptoms_data <- linelist %>% 
  select(c(case_id, fever, chills, cough, aches, vomit))

#Reshape this to a wide format
View(symptoms_data)

symptoms_data_long <- symptoms_data %>%    # begin with "mini" linelist called symptoms_data
  
  pivot_longer(
    cols = -case_id,                       # pivot all columns except case_id (all the symptoms columns)
    names_to = "symptom_name",             # assign name for new column that holds the symptoms
    values_to = "symptom_is_present") %>%  # assign name for new column that holds the values (yes/no)
  
  mutate(symptom_is_present = replace_na(symptom_is_present, "unknown")) # convert NA to "unknown"

View(symptoms_data_long)

#Basics of gglot graph
#ggplot: grammer of graphics
#Producing graphs via layers on top of each other

# plot data from my_data columns as red points
ggplot(data = my_data)+                   # use the dataset "my_data"
  geom_point(                             # add a layer of points (dots)
    mapping = aes(x = col1, y = col2),    # "map" data column to axes
    color = "red")+                       # other specification for the geom
  labs()+                                 # here you add titles, axes labels, etc.
  theme()                                 # here you adjust color, font, size etc of non-data plot elements (axes, title, etc.) 

##First command ggplot
# This will create plot that is a blank canvas
ggplot(data = linelist)

#To create visuals from the data you add geoms,
#geoms need to be supplied with mappings via the aes() function
#The type of geom depends on what you want to display
#Histograms - geom_histogram()
#Bar charts - geom_bar() or geom_col() (see “Bar plot” section)
#Box plots - geom_boxplot()
#Points (e.g. scatter plots) - geom_point()
#Line graphs - geom_line() or geom_path()
#Trend lines - geom_smooth()

#scatterplot, use geom_point
ggplot(data = linelist, mapping = aes(x = age, y = wt_kg))+
  geom_point()

#histogram, use geom hist
ggplot(data = linelist, mapping = aes(x = age))+
  geom_histogram()

#aesthetics in the aes(), refer to the visual property of the plotted data
#depends on the geom
#shape = Display a point with geom_point() as a dot, star, triangle, or square…
# fill = The interior color (e.g. of a bar or boxplot)
# color = The exterior line of a bar, boxplot, etc., or the point color if using geom_point()
# size = Size (e.g. line thickness, point size)
# alpha = Transparency (1 = opaque, 0 = invisible)
# binwidth = Width of histogram bins
# width = Width of “bar plot” columns
# linetype = Line type (e.g. solid, dashed, dotted)

#Static values of aesthetics should be asigned out of the aes() funcion
# scatterplot
ggplot(data = linelist, mapping = aes(x = age, y = wt_kg))+  # set data and axes mapping
  geom_point(color = "darkgreen", size = 0.5, alpha = 0.2)         # set static point aesthetics

# histogram
ggplot(data = linelist, mapping = aes(x = age))+       # set data and axes
  geom_histogram(              # display histogram
    binwidth = 7,                # width of bins
    color = "red",               # bin line color
    fill = "blue",               # bin interior color
    alpha = 0.1)                 # bin transparency

#When based on column values, should be based in the aes() function
# scatterplot
ggplot(data = linelist,   # set data
       mapping = aes(     # map aesthetics to column values
         x = age,           # map x-axis to age            
         y = wt_kg,         # map y-axis to weight
         color = age)
)+     # map color to age
  geom_point()         # display data as points 

# scatterplot
ggplot(data = linelist,   # set data
       mapping = aes(     # map aesthetics to column values
         x = age,           # map x-axis to age            
         y = wt_kg,         # map y-axis to weight
         color = age,       # map color to age
         size = age))+      # map size to age
  geom_point(             # display data as points
    shape = "diamond",      # points display as diamonds
    alpha = 0.3)            # point transparency at 30%

#Groups
#when, discrete col var asigned to an aesthetic
ggplot(data = linelist,
       mapping = aes(x = age, y = wt_kg, color = gender))+
  geom_point(alpha = 0.5)

#Faceting plots, small multiples
#Can use either facet_wrap() or facet_grid()

#We are using malaria count data for this section
# These data are daily counts of malaria cases, by facility-day
malaria_data <- import("malaria_facility_count_data.rds") %>%  # import
  select(-submitted_date, -Province, -newid)                                 # remove unneeded columns

View(malaria_data)

# A plot with facets by district
ggplot(malaria_data, aes(x = data_date, y = malaria_tot)) +
  geom_col(width = 1, fill = "darkred") +       # plot the count data as columns
  theme_minimal()+                              # simplify the background panels
  labs(                                         # add plot labels, title, etc.
    x = "Date of report",
    y = "Malaria cases",
    title = "Malaria cases by district") +
  facet_wrap(~District)                       # the facets are created

#To allow two variables we can use facet_grid
malaria_age <- malaria_data %>%
  select(-malaria_tot) %>% 
  pivot_longer(
    cols = c(starts_with("malaria_rdt_")),  # choose columns to pivot longer
    names_to = "age_group",      # column names become age group
    values_to = "num_cases"      # values to a single column (num_cases)
  ) %>%
  mutate(
    age_group = str_replace(age_group, "malaria_rdt_", ""),
    age_group = forcats::fct_relevel(age_group, "5-14", after = 1))

#faceted by two variables cols and rows
malaria_age_gg <- ggplot(malaria_age, aes(x = data_date, y = num_cases)) +
  geom_col(fill = "darkred", width = 1) +
  theme_minimal()+
  labs(
    x = "Date of report",
    y = "Malaria cases",
    title = "Malaria cases by district and age group"
  ) +
  facet_grid(District ~ age_group)

malaria_age_gg
#Can also free the axis scales
#check options in ?facet_grid

#Saving plots
#Use the ggsave() function
#Allows a variety of export files .pdf, .png

ggsave(plot = malaria_age_gg,file= "malaria_age_plot.pdf"
       , width = 10, height = 10)

#Labels
age_by_wt <- ggplot(
  data = linelist,   # set data
  mapping = aes(     # map aesthetics to column values
    x = age,           # map x-axis to age            
    y = wt_kg,         # map y-axis to weight
    color = age))+     # map color to age
  geom_point()+           # display data as points
  labs(
    title = "Age and weight distribution",
    subtitle = "Fictional Ebola outbreak, 2014",
    x = "Age in years",
    y = "Weight in kilos",
    color = "Age",
    caption = stringr::str_glue("Data as of {max(linelist$date_hospitalisation, na.rm=T)}"))

age_by_wt

#To change other elements use themes
#Custom themes are available
age_by_wt_green <- ggplot(data = linelist, mapping = aes(x = age, y = wt_kg))+  
  geom_point(color = "darkgreen", size = 0.5, alpha = 0.2)+
  labs(title = "Theme classic")

age_by_wt_green +
  theme_classic()

age_by_wt_green +
  theme_bw()

age_by_wt_green +
  theme_minimal()

age_by_wt_green +
  theme_gray()

#To modify theme elements on your own
age_by_wt + 
  theme_classic()+                                 # pre-defined theme adjustments
  theme(
    legend.position = "bottom",                    # move legend to bottom
    
    plot.title = element_text(size = 30),          # size of title to 30
    plot.caption = element_text(hjust = 0),        # left-align caption
    plot.subtitle = element_text(face = "italic"), # italicize subtitle
    
    axis.text.x = element_text(color = "red", size = 15, angle = 90), # adjusts only x-axis text
    axis.text.y = element_text(size = 15),         # adjusts only y-axis text
    
    axis.title = element_text(size = 20)           # adjusts both axes titles
  )     
