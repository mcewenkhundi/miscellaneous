#R club book club, https://epirhandbook.com/en/iteration-loops-and-lists.html
#Chapter16, iteration, loops and lists
#Date: 4Feb2022

#Iteration/loops
#Repeating analysis on subgroups such as countries, agegroups, clusters
#Two options
#The for loop, base R approach
#mapping functions from purrr

#Demo
#Creating epicurves


pacman::p_load(
  rio,         # import/export
  here,        # file locator
  purrr,       # iteration
  tidyverse    # data management and visualization
)

linelist <- import(here("linelist_cleaned.rds"))

#Simplified for loop
#Parts of a loop
#Sequence, operation and container


for( i in 1:10){ #sequence
  
  print(paste("index number:", i, "value of compuation:", i*2)) #operation
  #no container needed since output is being printed to the screen
}


#Sequence can also be a character
hospital_names <- unique(linelist$hospital)

for(hosp in hospital_names){
  
  print(paste("hospital name:", hosp))
  #no container needed since output is being printed to the screen
  
}

#Do this graph for each hospital
ggplot(data = linelist) +
  geom_histogram(aes(x = date_onset)) #+
  labs(title = "Date of onset")

for(hosp in hospital_names){
  
  print(  
  ggplot(data = filter(linelist, hospital == hosp )) +
    geom_histogram(aes(x = date_onset)) +
    labs(title = hosp, x = "data of onset")
  )
}

##Store the graphs
graph_container <- vector("list", length = length(hospital_names))
names(graph_container) <- hospital_names

for(hosp in hospital_names){
  
    
  graph_container[[hosp]]  <-  ggplot(data = filter(linelist, hospital == hosp )) +
      geom_histogram(aes(x = date_onset)) +
      labs(title = hosp, x = "data of onset")
}

graph_container

ggpubr::ggarrange(plotlist = graph_container, ncol = 2, nrow = 3)

##Summary stats
#find the mean age of participants in each hospital
hospital_names <- unique(linelist$hospital)

meanage_container <- vector("list", length = length(hospital_names))
names(meanage_container) <- hospital_names

for(hosp in hospital_names){
  
age_variable <- filter(linelist, hospital == hosp) %>%
                       pull(age_years)
  
  meanage_container[[hosp]] <- mean(age_variable, na.rm = TRUE)
}

meanage_container

medianage_container <- vector("list", length = length(hospital_names))
names(medianage_container) <- hospital_names

for(hosp in hospital_names){
  
  medianage_container[[hosp]] <- median(filter(linelist, hospital == hosp) %>%
                                      pull(age_years), na.rm = TRUE)
}

medianage_container

##Follow above approach to find number of cases in each hospital?


## short cuts exits for most loop operation in tidyverse
# How could we do he operation above?
linelist %>%
  group_by(hospital) %>%
  summarise(mean_age = mean(age_years, na.rm = TRUE ), 
            median_age = median(age_years, na.rm = TRUE), n_cases = n())
##
#across()

#should we introduce purrr::map?
#Why use map






