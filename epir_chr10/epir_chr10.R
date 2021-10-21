#Project: R book club
#Task: Working with characters and strings in R
#Date: 08-10-20121

#Were going to use the string r package to demomstrate how to work with dates in 
#R

# install/load packages
pacman::p_load(
  rio, #import datasets
  stringr,    # many functions for handling strings
  tidyverse,  # for optional data manipulation
  tools)   

#create dataset
df <- data.frame(
  case_ID = c(1:6),
  first_names = c("abdul", "fahruk", "janice", "julie","ike","Wes"), 
  last_names  = c("hussein", "akinleye", "okeke","phiri","banda","oguni"),
  address = c("ndirande 205", "kanjedza 401", "chitawila 100", "nkolokosa 105", "mpemba 444", "sochi 55"),
  house_items = c("fridge,bed, phone", 
                  "iron,bed,car,phone",
                  "bank,car, bycycle",
                  "car",
                  "phone",
                  "iron"),
  symptoms  = c("jaundice  , fever  , chills",     # patient 1
                "  chills, aches, pains",        # patient 2 
                "fever ",                       # patient 3
                "vomiting  , diarrhoea",         # patient 4
                "bleeding from gums, fever",   # patient 5
                "rapid pulse  , headache"),      # patient 6
  outcome = c("   Recover", "Death", "   Death", "Recover   ", "Recover   ", "Recover"))

df

#Create a name variable and have it before first_name
# base r has paste()

df <- df %>%
      mutate(name = str_c(first_names, last_names, sep = " "), .before = first_names)

df

#arrange df according to first name alphabetical order
#but descending order
df %>%
  arrange(desc(first_names))

#capitalise all first letter's of all name vars
df %>%
  mutate(name = str_to_upper(name))

#Do it for all names at once
df %>%
  mutate(across(contains("name"), str_to_upper))

#toTitleCase if from the packae tools 
df %>%
  mutate(across(contains("name"), toTitleCase))

#Trim leading and trailing spaces in all character variables 
#usually you are not sure which variables have leading/trailing zeros

df$outcome

df <- df %>%
  mutate(across(where(is.character), str_trim))

#to remove repeated spaces use
str_squish("  Pt requires   IV saline\n") 

#extract by character position for instance you just want a 3 letter outcome var

df %>%
  mutate(outcome3 = str_sub(outcome, 1, 3))

#how untrimmed can cause unexpected results
#use the df with untrimmed char vars
df %>%
  mutate(outcome3 = str_sub(outcome, 1, 3))

#number of characters in a word
df %>%
  mutate(name_len = str_length(name))

#separating a character var
df_split <- separate(df, symptoms, into = c("sym_1", "sym_2", "sym_3"), extra = "merge")

#the opposite of separating, uniting several vars into one var
df_split %>% 
  unite(
    col = "all_symptoms",         # name of the new united column
    c("sym_1", "sym_2", "sym_3"), # columns to unite
    sep = ", ",                   # separator to use in united column
    remove = TRUE,                # if TRUE, removes input cols from the data frame
    na.rm = TRUE                  # if TRUE, missing values are removed before uniting
  )

##extract a specific household item from the household items
df %>%
  mutate(hh_bed = str_extract(house_items, pattern = "bed"), .after = house_items)

##how to extract numbers from a character variable
## this is under regular expressions but we are going to cheat by using functions
#from the readr package

#extract house numbers from the address 

df %>%
  mutate(house_number = readr::parse_number(address), .after = address)


#References
#https://r4ds.had.co.nz/strings.html

