#project: r book club
#task: Demostrating project set up in R in epir book ch6
#author: McEwen Khundi 2021-09-10
#Modified by: name date of modification

##script setup
install.packages("pacman")

##load/install packages for the analysis

pacman::p_load(
  tidyverse, #data manipulation and graphs
  here, # relative paths in R
  rio #importing and exporting data
)

#What is an r project
#An R project enables your work to be bundled in a portable, self-contained folder
#A self-contained working environment with folders for data, scripts, outputs


#Demonstrate creating an r project
#Some important sub folders
#data-raw
#data
#figures
#R rscripts

#Naming of files in a project some best practices
#Some form of version control by appending dates to names

#Importing data and demostrating the use of the here package
linelist <- import(file = here("data", "linelist_cleaned.xlsx"))

#contrast with fixed file path
#if this was moved to a different computer or project it would not work
linelist <- import(file = ("C:/Users/mkhundi/Documents/Projects/miscellaneous/epir_chr6/data/linelist_cleaned.xlsx"))

#exporting files from r
export(linelist, file = here("data", "my_linelist.dta"))

#exporting graphs
ggplot(data = linelist, mapping = aes(y = age, x = gender, fill = gender)) +
  geom_boxplot()+
  theme(legend.position = "none")+   # remove legend (redundant)
  labs(title = "B) Boxplot by gender")

ggsave(here("figures", "boxplotbygender_2021-02-15.png"))


#code styles could be achieved using the styler package
#use the addin
#demo with the code below

linelist %>%
  select(contains("date")) %>%
  names()

#Other project options
#Go to tools menu select options
#some important options
#rstudio appearance themes
#set RStudio to not restore .RData


#Note on commenting code
#The comments should be answering why? and not how?

#other resources
#https://www.rstudio.com/resources/cheatsheets/
#https://ggplot2.tidyverse.org/articles/



