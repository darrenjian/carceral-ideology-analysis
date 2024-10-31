library(tidyverse)
library(tidytext)
source('help_functions.R')

# school data
schools <- read_csv('../data/school_data/sch_enrollment.csv')

# school handbook data
handbooks <- read_csv('../data/school_handbooks.csv') %>% 
  drop_na(state)

# handbook representation by locale
sch_locale <- schools %>% 
  group_by(locale_type) %>% 
  summarise(count = n_distinct(school_id)) %>% 
  mutate(total = sum(count),
         pct = (count/total)*100,
         order = c(1,4,2,3),
         Sample = 'All Schools')

hb_locale <- handbooks %>% 
  group_by(locale_type) %>% 
  summarise(count = n_distinct(doc_id)) %>% 
  mutate(total = sum(count),
         pct = (count/total)*100,
         order = c(1,4,2,3),
         Sample = 'Handbooks') %>% 
  bind_rows(sch_locale)

sample_colors <- c('All Schools' = cbPalette[1], 'Handbooks' = cbPalette[3])
locale_pct <- hb_locale %>% 
  ggplot(aes(reorder(locale_type, order), pct, fill=Sample)) +
  geom_bar(stat='identity', position = position_dodge()) +
  geom_text(aes(label=round(pct, digits=1)), stat='identity', size=3.5,
            position = position_dodge(0.9), vjust=1.5) + ylim(0, 35) +
  theme_minimal() + scale_fill_manual(values=sample_colors) +
  labs(x='Locale', y='Proportion') + theme(legend.position = 'top',
                                           legend.title = element_blank()) +
  ggtitle('Handbook Distribution versus School Sampling Frame')
locale_pct
ggsave(filename='figures/locale_pct.png', plot=locale_pct,
       width=5, height=3, units='in')

# handbook representation by state see https://ggplot2-book.org/maps.html
state_map <- map_data("state") %>% 
  select(lon = long, lat, group, id = region)

school_coord <- handbooks %>% 
  select(longitude, latitude, state_name, doc_type) %>% 
  mutate(state_name = str_to_lower(state_name)) %>%  
  filter(!state_name %in% c('hawaii', 'alaska'))

hb_map <- ggplot(state_map, aes(lon, lat, group=group)) +
  geom_polygon(fill = "white", colour = "grey50") + 
  geom_point(aes(longitude, latitude, group=state_name), data=school_coord, 
             size=0.5, color=cbPalette[3]) + theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank()) +
  ggtitle('Handbook Distribution by State')
hb_map
ggsave(filename='figures/hb_map.png', plot=hb_map,
       width=6, height=4, units='in')

# handbook representation by level
hb_level <- handbooks %>% 
  mutate(level_category = ifelse(level %in% c('Elementary', 'High', 'Middle'),
                                 level, 'Other')) %>% 
  group_by(level_category) %>% 
  summarise(Total = n()) %>% 
  mutate(order = c(1,3,2,4)) %>% 
  ggplot(aes(reorder(level_category, order), Total)) + 
  geom_bar(stat='identity', fill=cbPalette[3]) + ylab('Total Handbooks') +
  geom_text(aes(label=scales::comma(Total)), stat='identity', size=3.5, vjust=1.5) +
  theme_minimal() + theme(axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::comma) +
  ggtitle('Handbook Distribution by Level')
hb_level
ggsave(filename='figures/hb_level.png', plot=hb_level,
       width=5, height=3, units='in')
