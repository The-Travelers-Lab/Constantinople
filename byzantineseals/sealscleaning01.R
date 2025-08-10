#INTRODUCTION
#Alex Williams Summer 2024 QAC Research, Email: apwilliams@wesleyan.edu
# This code was used in order to clean up a csv file of all of the Dumbarton Oaks seals from Johnathan Shea. This data should be in the folder if you would like to run the code yourself, if you need it, talk to Jesse Torgerson. (Information in readme file)
#The goal of this is to provide an example of data exploration and cleaning up a large, messy dataset, and also to have reproducible code. 
#If you are trying to reproduce the dataset, you can run this file. If you want to understand the choices I made, scroll to the bottom to see the explore section which is commented out

#IMPORTING LIBRARIES
#If you have used R before, but not Tidyverse, it might be helpful to look at some online videos about the dplyr package, which uses many base R functions in different formats in order to make the code more readable
library(dplyr)
library(ggplot2)
library(stringr)

#READING CSV FILE
seals <- read.csv("/Volumes/courses/QAC/Apprenticeship/SUMMER24/apwilliams/project/sealsdata/unmodified_byzantine_seals_data_2024.csv")

#UNDERSTAND THE STRUCTURE OF SEALS 
str(seals)

#CLEANING EMPTY STRINGS INTO MISSING VALUES
#Because the Dataset contains empty strings ("") for missing, this function is to rewrite them as a value that R understands as missing: NA
clean_na <- function(x) {
  x[x == "" | x == " "] <- NA
  return(x)
}
seals <- seals %>% 
  mutate(across(everything(), clean_na))

#CREATING THE CLEANER DATASET
#This dropping some  variables
seals02 <- seals%>%
  select(-Tags,-Storage.Location.Cabinet,-Storage.Location.Tray,-Credit.Line)
names(seals02)

#RENAMING VARIABLES
#We can see in the names command that lots of the variable names are formulated with capital letters and periods
#Even though those names work with R code, I am renaming them using the R tidyverse style guide, as well as the SQL style guide.
#https://style.tidyverse.org/syntax.html
seals02 <- seals02 %>%
  rename_with(~ gsub("\\.", "_", .x) %>% tolower())
seals02 <- seals02 %>%
  rename(owner_name = name_of_owner,
         owner_title = title_of_owner,
         owner_office = office_of_owner,
         institution_name = institutional_name,
         diameter_mm_min = diameter_in_mm__min_,
         diameter_mm_max = diameter_in_mm__max_ ,
         field_diameter_mm = field_diameter_in_mm,
         location = location_of_owner,
         theme = type_of_theme,
         seal_title = title
           )

#UNDERSTANDING CLEANING TASKS & RECODING
noloc <- seals02 %>% #You can see if you open this that all of these are not actual seals but rather dates- they have no information in any categories about acutal seals
  filter(location == "No Location")
seals02 <- seals02 %>% 
  filter(location != "No Location" | is.na(location))

#Recoding these two dummy variables into 1 and 0 instead of "x"
test <- seals02 %>% #Checking to make sure there are no values other then NA or x
  filter(iconographic_seal != "x")
test2 <- seals02 %>% 
  filter(metrical_poem != "x")
seals02 %>% 
  count(iconographic_seal,metrical_poem)

#Code to change the variables
seals02 <- seals02 %>% 
  mutate(
    iconographic_seal= if_else(iconographic_seal == "x",1,NA),
    metrical_poem = if_else(metrical_poem == "x",1,NA)
  )

write.csv(seals02,"/Volumes/courses/QAC/Apprenticeship/SUMMER24/apwilliams/project/sealsdata02.csv")

#CLEANING TASKS FOR NEXT STEPS
#Extract text from HTML code
#Add century when century is in the title but not the spreadsheet
#Fix when name, office, (imperial) title does not appear but appears in the title
#Make century and date columns have start and end dates
#Figure out the number of parallels and/or related items- Might leave it for nodegoat

#I am doing these steps in doing in python due familiarity with beautifulsoup, regex, spacy, and nlp in python. 

#
# #EXPLORATION OF ORIGINAL DATASET
# #UNDERSTANDING HOW LOCATIONS WERE CREATED IN THE ORIGINAL DATASET
# noof <- seals %>%
#   filter(!is.na(Location.of.Owner)) %>%
#   filter(str_detect(Title, "of", negate=TRUE))%>%
#   select(Title,Location.of.Owner)
# #UNDERSTANDING THE "No Location" (We do not want to include this in our analysis because it has no actual observations)
# noloc <- seals %>%
#   filter(Location.of.Owner=="No Location")
# 
# #CREATING A TABLE TO UNDERSTAND THE TYPE OF THEME AND THE RELATION TO LOCATION
# #We want to gather all of seals from Constantinople, even if they do not have a location in the spreadsheet. This was to understand the grouping of seals by location.
# #Collapsing location into 2 variables and NA
# seals <- seals %>%
#   mutate(location_grouped = ifelse(Location.of.Owner == "Constantinople" |
#                                      is.na(Location.of.Owner),
#                                    Location.of.Owner, "Other"))
# #Creating the Table
# table(seals$Type.of.Theme, seals$location_grouped, useNA = "ifany")
# 
# #Used to understand the specific types of Seals in other locations that had these themes
# sealsnoncon <- seals %>%
#   filter(Type.of.Theme %in% c('Imperial', 'Central Administration') & location_grouped == 'Other') %>%
#   select(Title, Location.of.Owner, Type.of.Theme, location_grouped)
# 
# #LOOKING AT A LIST OF DISTINCT LOCATIONS
# dist_locations <- seals %>%
#   select(Location.of.Owner)  %>%
#   distinct(Location.of.Owner)
# 
# #EXPLORATION OF DATASET TO EXPORT
# #EXPLORING SOME BASIC GRAPHS
# #These should give you an idea if something is wrong with your data.
# ggplot(data = seals02) +
#   geom_histogram(mapping = aes(x = diameter_mm_min))
# ggplot(data = seals02) +
#   geom_bar(mapping = aes(x = seal_language))
# ggplot(data = subset(seals02, !is.na(owner_title))) +
#   geom_bar(mapping = aes(x = owner_title))+coord_flip()
# ggplot(data = seals02) +
#   geom_bar(mapping = aes(x = type_of_theme))
# 
# 
# # cut(variable,
# #     breaks=c(vector),
# #     labels=c(vector))

