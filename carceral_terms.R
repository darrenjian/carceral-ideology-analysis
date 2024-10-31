library(tidyverse)
library(tidytext)
library(readxl)
library(stargazer)
library(lme4)

# school handbook data
schools <- read_csv('../data/school_handbooks.csv')

# dictionary data
ci_terms <- read_xlsx('../data/handbook_dictionary.xlsx', sheet = 'carceral')
carceral_terms <- pull(ci_terms, word)
carceral_dict <- ci_terms %>% 
  select(Category = category, Word = root) %>% 
  distinct() %>% 
  write_csv('../data/carceral_dictionary.csv')

# handbook data
hb <- read_csv('../data/doc_tf_idf.csv') %>% 
  mutate(carceral = ifelse(word %in% carceral_terms, 1, 0))

ci_doc <- hb %>% 
  group_by(doc_id) %>% 
  summarise(total = length(doc_id),
            ci_n = sum(carceral)) %>% 
  mutate(pct_ci = ci_n/total)

# predict carceral terminology
sch_carceral <- schools %>% 
  left_join(ci_doc, by = 'doc_id') %>% 
  mutate(elem = ifelse(level == 'Elementary', 1, 0),
         middle = ifelse(level == 'Middle', 1, 0),
         high = ifelse(level == 'High', 1, 0),
         pct_ci_100 = pct_ci*100,
         program = case_when(charter & magnet ~ 'Charter',
                             charter & !magnet ~ 'Charter',
                             !charter & magnet ~ 'Magnet',
                             !charter & !magnet ~ 'Traditional'),
         high_pov = ifelse(pct_fr_lunch >= 0.7, 1, 0)) %>% 
  select(doc_id, ci_n, pct_ci, pct_ci_100, longitude, latitude, rural, urban, 
         bigcity, suburb, pct_fr_lunch, high_pov, pct_black,
         pct_hispanic, pct_white, pct_nonwhite, elem, middle, high,
         charter, magnet, program, st_ratio, locale_type, state) %>% 
  filter(pct_ci_100 < 25)
  
fit1 <- lm(pct_ci_100 ~ charter + magnet + pct_fr_lunch + pct_black + 
             charter*pct_black + magnet*pct_black + pct_fr_lunch*pct_black,
           data = sch_carceral)

fit2 <- lm(pct_ci_100 ~ charter + magnet + high + middle + urban + pct_fr_lunch + 
             pct_black + charter*pct_black + magnet*pct_black + urban*pct_black,
           data = sch_carceral)

stargazer(fit1, fit2, type='text')


# ci by program
ci_program <- sch_carceral %>% 
  drop_na(program) %>% 
  group_by(locale_type, program) %>% 
  summarise(avg_pct_ci = mean(pct_ci_100, na.omit=TRUE)) %>% 
  mutate(order = c(1,1,1,4,4,4,2,2,2,3,3,3)) %>% 
  ggplot(aes(x=locale_type, y=avg_pct_ci, fill=program)) + 
  geom_bar(stat='identity', position=position_dodge())
ci_program
