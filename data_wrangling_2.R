#set-up file

setwd('C:\\Users\\Chegwood\\Documents\\Data Science Stuff\\Springboard - Foundations of Data Science\\Data Wrangling Exercise 2')
library(dplyr)
library(tidyr)
titanic_original <- read.csv('titanic_original.csv')

#create new data frame and clean data

titanic_clean <- titanic_original %>% 
  mutate(embarked = ifelse(embarked == "", 'S', embarked)) %>%
  mutate(age = ifelse(is.na(age), mean(age, na.rm = TRUE), age)) %>% 
  mutate(boat = ifelse(boat == "", 'None', boat)) %>% 
  mutate(has_cabin_number = ifelse(cabin == "", 0, 1))

#print .csv file

write.csv(titanic_clean, 'titanic_clean.csv', row.names = FALSE)


#titanic_clean_2 uses more in-depth filtering to provide missing ages based on pclass, gender and survived variables
#create conditional_age_means lookup

conditional_age_means <- titanic_original %>% 
  group_by(pclass, survived, sex) %>% 
  summarise(age = mean(age, na.rm = TRUE)) %>% 
  mutate(age_join = NA)

#create new data frame and clean / filter based on multiple conditions

titanic_clean_2 <- titanic_original %>%
  mutate(embarked = ifelse(embarked == "", 'S', embarked)) %>% 
  #can we filter this age replacement more? By child vs. adult vs. parent? By passanger class? By price of ticket?
  mutate(age_join = age) %>% 
  left_join(conditional_age_means, by = c('pclass', 'survived', 'sex', 'age_join')) %>% 
  mutate(age = ifelse(is.na(age.x), age.y, age.x)) %>% 
  select(-c(age_join, age.x, age.y)) %>% 
  mutate(boat = ifelse(boat == "", 'None', boat)) %>% 
  mutate(has_cabin_number = ifelse(cabin == "", 0, 1))

#print .csv file

write.csv(titanic_clean_2, 'titanic_clean_2.csv', row.names = FALSE)
