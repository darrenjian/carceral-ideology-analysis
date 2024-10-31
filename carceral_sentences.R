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

# import text files from directory
txt <- file_list %>% 
  map_chr(~ read_file(.)) %>% 
  tibble(name = filenames, text = .) %>% 
  mutate(doc_id = str_remove(name, '.txt')) 

# tokenize documents into sentences
dict <- read_xlsx('../data/handbook_dictionary.xlsx', sheet = 'dictionary') %>% 
  pull(word)
carceral_terms <- read_xlsx('../data/handbook_dictionary.xlsx', sheet = 'carceral') %>% 
  pull(word)

txt_sent <- txt %>% 
  select(-name) %>% 
  unnest_tokens(sent, text, token='sentences') %>% 
  group_by(doc_id) %>% 
  mutate(sent_no = 1:length(doc_id)) %>% 
  ungroup()

txt_proc <- txt_sent %>% 
  unnest_tokens(word, sent) %>% 
  anti_join(get_stopwords()) %>% 
  mutate(word = str_remove(word, "[^\\w]")) %>% 
  filter(!str_detect(word, "[\\d+]"), word %in% dict)

test <- head(txt_proc, 100)
sent_lengths <- txt_proc %>% 
  group_by(doc_id, sent_no) %>% 
  distinct(word, .keep_all=TRUE) %>% 
  mutate(is_carceral = ifelse(word %in% carceral_terms, 1, 0)) %>% 
  group_by(doc_id, sent_no) %>% 
  summarise(sent_n = length(word), ci_sum = sum(is_carceral)) %>% 
  filter(sent_n >= 3)

test2 <- head(sent_lengths, 100)
ci_sent <- sent_lengths %>%
  mutate(any_ci = ifelse(ci_sum > 0, 1, 0)) %>% 
  group_by(doc_id) %>% 
  summarise(ci_sum = sum(any_ci), sent_n = n()) %>% 
  mutate(pct_ci = ci_sum/sent_n)
summary(ci_sent$pct_ci)

sch_carceral <- schools %>% 
  left_join(ci_sent, by = 'doc_id') %>% 
  mutate(elem = ifelse(level == 'Elementary', 1, 0),
         middle = ifelse(level == 'Middle', 1, 0),
         high = ifelse(level == 'High', 1, 0),
         pct_ci_100 = pct_ci*100,
         mean_ci_100 = mean(pct_ci_100, na.rm=TRUE),
         pct_ci_diff = pct_ci_100 - mean_ci_100,
         program = case_when(charter & magnet ~ 'Charter',
                             charter & !magnet ~ 'Charter',
                             !charter & magnet ~ 'Magnet',
                             !charter & !magnet ~ 'Traditional'),
         high_pov = ifelse(pct_fr_lunch >= 0.5, 1, 0),
         minority_serving_blk = as.character(ifelse(pct_black >= 0.5, 1, 0)),
         minority_serving_hisp = as.character(ifelse(pct_hispanic >= 0.5, 1, 0))) %>% 
  select(doc_id, pct_ci, pct_ci_100, pct_ci_diff, longitude, latitude, rural, 
         urban, bigcity, suburb, pct_fr_lunch, high_pov, pct_black,
         pct_hispanic, pct_white, pct_nonwhite, elem, middle, high,
         charter, magnet, program, st_ratio, locale_type, state,
         minority_serving_blk, minority_serving_hisp) %>% 
  filter(pct_ci < 1)

fit1 <- lm(pct_ci_diff ~ charter + magnet + high + middle + urban + pct_fr_lunch + 
             pct_black,# + urban*pct_black + charter*pct_black + magnet*pct_black,
           data = sch_carceral)
fit3 <- lm(pct_ci_diff ~ charter + magnet + high + middle + urban + pct_fr_lunch + 
             pct_black + charter*pct_black + magnet*pct_black,
           data = sch_carceral)

fit2 <- lm(pct_ci_diff ~ charter + magnet + high + middle + urban + pct_fr_lunch + 
             pct_black + urban*pct_black,
           data = sch_carceral)



stargazer(fit1, fit2, fit3, type='text', out = 'regression.txt')

program_order = c(1,1,1,4,4,4,2,2,2,3,3,3)
program_colors = list(c('Charter'=cbPalette[5], 'Magnet'=cbPalette[2], 'Traditional'=cbPalette[7]),
                      c('Charter'='#DDDDDD', 'Magnet'='#DDDDDD', 'Traditional'=cbPalette[7]),
                      c('Charter'='#DDDDDD', 'Magnet'=cbPalette[2], 'Traditional'='#DDDDDD'),
                      c('Charter'=cbPalette[5], 'Magnet'='#DDDDDD', 'Traditional'='#DDDDDD'))
ci_program <- sch_carceral %>% 
  drop_na(program) %>% 
  group_by(locale_type, program) %>% 
  summarise(avg_ci_diff = mean(pct_ci_diff, na.rm=TRUE)) %>% 
  group_by(locale_type, program) %>% 
  ggplot(aes(x=reorder(locale_type, program_order), y=avg_ci_diff, fill=program)) + 
  geom_bar(stat='identity', position=position_dodge(), width=0.8) + coord_flip() +
  xlab('Locale') + ylab('Mean Difference (Percentage Points)') +
  theme_minimal() + theme(legend.title = element_blank()) +
  scale_y_continuous(limits = c(-3.5, 3.5), breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
  ggtitle('Carceral Terminology Prevalence by School Type and Locale')
ci_program
ci_all <- ci_program + scale_fill_manual(values=program_colors[[1]])
ci_trad <- ci_program + scale_fill_manual(values=program_colors[[2]])
ci_magnet <- ci_program + scale_fill_manual(values=program_colors[[3]])
ci_charter <- ci_program + scale_fill_manual(values=program_colors[[4]])

ggsave(filename='figures/ci_all.png', plot=ci_all, width=6, height=4, units='in')
ggsave(filename='figures/ci_trad.png', plot=ci_trad, width=6, height=4, units='in')
ggsave(filename='figures/ci_magnet.png', plot=ci_magnet, width=6, height=4, units='in')
ggsave(filename='figures/ci_charter.png', plot=ci_charter, width=6, height=4, units='in')

program_order = c(1,1,4,4,2,2,3,3)
ci_blk_locale <- sch_carceral %>% 
  group_by(locale_type, minority_serving_blk) %>% 
  summarise(avg_ci_diff = mean(pct_ci_diff, na.omit=TRUE)) %>% 
  drop_na()
ci_blk <- ci_blk_locale %>% 
  ggplot(aes(x=reorder(locale_type, program_order), y=avg_ci_diff, 
             fill=minority_serving_blk)) + 
  geom_bar(stat='identity', position=position_dodge(), width=0.8) + coord_flip() +
  xlab('Locale') + ylab('Mean Difference (Percentage Points)') +
  theme_minimal() + theme(legend.title = element_blank()) +
  scale_y_continuous(limits = c(-2.5, 2.5), breaks = c(-2, -1, 0, 1, 2)) +
  ggtitle('Carceral Terminology Prevalence by Race & Locale') + 
  scale_fill_manual(values=c(cbPalette[5], cbPalette[2]),
                    labels=c('% Black < 50%', '% Black >= 50')) +
  theme(legend.position = 'top') 
ggsave(filename='figures/ci_blk.png', plot=ci_blk, width=5, height=5, units='in')

ci_hisp_locale <- sch_carceral %>% 
  group_by(locale_type, minority_serving_hisp) %>% 
  summarise(avg_ci_diff = mean(pct_ci_diff, na.omit=TRUE)) %>% 
  drop_na()
ci_hisp <- ci_hisp_locale %>% 
  ggplot(aes(x=reorder(locale_type, program_order), y=avg_ci_diff, 
             fill=minority_serving_hisp)) + 
  geom_bar(stat='identity', position=position_dodge(), width=0.8) + coord_flip() +
  xlab('Locale') + ylab('Mean Difference (Percentage Points)') +
  theme_minimal() + theme(legend.title = element_blank()) +
  scale_y_continuous(limits = c(-2.5, 2.5), breaks = c(-2, -1, 0, 1, 2)) +
  ggtitle('Carceral Terminology Prevalence by Race & Locale') + 
  scale_fill_manual(values = c(cbPalette[5], cbPalette[2]), 
                    labels=c('% Hispanic < 50%', '% Hispanic >= 50')) + 
  theme(legend.position = 'top') 
ggsave(filename='figures/ci_hisp.png', plot=ci_hisp, width=5, height=5, units='in')

state_ci <- sch_carceral %>% 
  select(doc_id, state, pct_ci_diff, pct_fr_lunch) %>% 
  drop_na() %>% 
  group_by(state) %>% 
  summarise(avg_ci_diff = mean(pct_ci_diff, na.rm=TRUE),
            avg_fr_lunch = mean(pct_fr_lunch, na.rm=TRUE)*100) %>% 
  drop_na() %>% 
  filter(avg_ci_diff > -10) %>% 
  ggplot(aes(avg_fr_lunch, avg_ci_diff, label=state)) + 
  theme_minimal() + ylab('Mean Difference (Percentage Points)') +
  xlab('Mean Poverty Rate') + geom_hline(yintercept = 0, color=cbPalette[1]) +
  geom_text(color=cbPalette[3], check_overlap=TRUE, size=5) + 
  scale_x_continuous(limits= c(0, 80), breaks=c(0,20,40,60,80)) +
  ggtitle('Carceral Terminology Prevalence by State') 
state_ci
ggsave(filename='figures/state_ci.png', plot=state_ci, width=6, height=4, units='in')

