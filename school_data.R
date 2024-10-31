library(tidyverse)
library(readxl)
source('help_functions.R')

# entire NCES public school directory (N=102176)
schools <- read_csv('data/school_data/ccd_1819_directory_rev.csv',
                    col_types = cols(.default = 'c'))

# school enrollment data from CCD (via ELSI table 430916)
enr_cols <- c('school_name', 'state_name', 'nces_id', 'website', 'address', 
              'city', 'state', 'zip', 'phone', 'school_type', 'charter', 
              'magnet', 'locale', 'latitude', 'longitude', 'school_id', 'level', 
              'lowest_grade', 'highest_grade', 'free_lunch', 'red_lunch',
              'free_red_lunch', 'male', 'female', 'amind', 'asian', 'hispanic', 
              'black', 'white', 'pacisl', 'twoplus', 'total', 'teacher_fte',
              'st_ratio')
enrollment <- read_csv('data/school_data/ccd_1819_elsi_cleaned.csv',
                       col_names = enr_cols, skip = 1) %>% 
  mutate(charter = ifelse(charter == '1-Yes', 1, 0),
         magnet = ifelse(magnet == '1-Yes', 1, 0),
         rural = ifelse(str_detect(locale, '^4'), 1, 0),
         urban = ifelse(rural, 0, 1),
         urban_rural = ifelse(rural, 'rural', 'urban'),
         bigcity = ifelse(str_detect(locale, '^11-City'), 1, 0),
         city = ifelse(str_detect(locale, '^1'), 1, 0),
         suburb = ifelse(str_detect(locale, '^2'), 1, 0),
         town = ifelse(str_detect(locale, '^3'), 1, 0),
         locale_type = case_when(str_detect(locale, '^1') ~ 'City',
                                 str_detect(locale, '^2') ~ 'Suburb',
                                 str_detect(locale, '^3') ~ 'Town',
                                 str_detect(locale, '^4') ~ 'Rural'),
         pct_fr_lunch = ifelse(is.finite(as.numeric(free_red_lunch)/as.numeric(total)),
                               free_red_lunch/total, NA),
         pct_black = ifelse(is.finite(as.numeric(black)/as.numeric(total)),
                            black/total, NA),
         pct_hispanic = ifelse(is.finite(as.numeric(hispanic)/as.numeric(total)),
                               hispanic/total, NA),
         pct_asian = ifelse(is.finite(as.numeric(asian)/as.numeric(total)),
                               asian/total, NA),
         pct_white = ifelse(is.finite(as.numeric(white)/as.numeric(total)),
                            white/total, NA),
         pct_amind = ifelse(is.finite(as.numeric(amind)/as.numeric(total)),
                               amind/total, NA),
         pct_nonwhite = 1 - pct_white)

write_csv(enrollment, file='data/school_data/sch_enrollment.csv')

# handbooks downloaded (dl) and converted to text
dl <- read_csv('data/doc_data/download_log_2022_04_07-08_04_03_PM.csv')
docs <- read_csv('data/school_data/school_doc_id.csv',
                 col_types = cols(.default = 'c')) %>% 
  select(nces_id=school_id, doc_id, source_url) %>% 
  left_join(dl, on='doc_id') %>% 
  filter(status == 'Complete') %>% 
  distinct(nces_id, doc_id, .keep_all = TRUE) 
doc_lang <- read_csv('data/doc_data/doc_id_language.csv')

languages <- doc_lang %>% 
  group_by(language) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# websites generated for SCH_TYPE_TEXT = 'Regular School'
sites <- read_csv('data/school_data/school_websites.csv',
                  col_types = cols(.default = 'c')) %>% 
  left_join(schools, by = c('STATENAME'='STATENAME', 'SCH_NAME'='SCH_NAME',
                            'NCESSCH'='NCESSCH', 'WEBSITE'='WEBSITE',
                            'LSTREET1'='LSTREET1', 'LZIP'='LZIP',
                            'SCH_TYPE_TEXT'='SCH_TYPE_TEXT')) %>% 
  select(school_id=ST_SCHID, nces_id_ccd=NCESSCH, school_name_ccd=SCH_NAME,
         source_url=final_website) %>% 
  left_join(enrollment, by = 'school_id') %>% 
  distinct(nces_id, source_url, .keep_all = TRUE) %>% 
  left_join(docs, by = c('nces_id_ccd'='nces_id', 'source_url'='source_url'))

text_dir = 'data/docs/'
handbooks <- sites %>% 
  filter(str_detect(doc_id, '^DOC')) %>% 
  mutate(filename = str_glue('{doc_id}.txt'),
         path = str_glue('{text_dir}{filename}')) %>% 
  left_join(doc_lang, by = c('filename'='doc_id')) %>% 
  filter(language=='English') %>% 
  distinct(school_id, .keep_all = TRUE)

hb_locale <- handbooks %>% 
  group_by(locale) %>% 
  summarise(handbooks=n()) %>% 
  drop_na() %>% 
  mutate(hb_total = sum(handbooks),
         hb_pct_total = handbooks/hb_total)

sch_locale <- enrollment %>% 
  group_by(locale) %>% 
  summarise(all_schools=n()) %>% 
  mutate(sch_total = sum(all_schools),
         sch_pct_total = all_schools/sch_total) %>% 
  left_join(hb_locale, by='locale')

handbooks %>% 
  write_csv(path='data/school_handbooks.csv')
