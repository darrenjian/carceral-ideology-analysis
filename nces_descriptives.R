library(tidyverse)
library(tidytext)
library(readxl)
library(stargazer)
library(lme4)
source('help_functions.R')

# school handbook data
schools <- read_csv('../data/school_handbooks.csv')
file_list <- pull(schools, path)
filenames <- pull(schools, filename)

nces <- schools %>% 
  mutate(elem = ifelse(level == 'Elementary', 1, 0),
         middle = ifelse(level == 'Middle', 1, 0),
         high = ifelse(level == 'High', 1, 0),
         program = case_when(charter & magnet ~ 'Charter',
                             charter & !magnet ~ 'Charter',
                             !charter & magnet ~ 'Magnet',
                             !charter & !magnet ~ 'Traditional'),
         high_pov = ifelse(pct_fr_lunch >= 0.5, 1, 0)) %>% 
  select(doc_id, elem, middle, high, charter, magnet, city, suburb, town, rural, 
         pct_fr_lunch, pct_black, pct_hispanic, pct_asian, pct_white)

stats <- nces %>% 
  pivot_longer(-doc_id, names_to = 'variable', values_to = 'value') %>% 
  group_by(variable) %>% 
  summarise(mean = mean(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE)) %>% 
  write_csv('../data/school_data/nces_descriptives.csv')
