library(tidyverse)
library(tidytext)
library(readxl)

# school handbook data
schools <- read_csv('data/school_handbooks.csv')
file_list <- pull(schools, path)
filenames <- pull(schools, filename)

# import text files from directory
txt <- file_list %>% 
  map_chr(~ read_file(.)) %>% 
  tibble(name = filenames, text = .) %>% 
  mutate(doc_id = str_remove(name, '.txt')) %>% 
  head(100)


# tokenize documents
dict <- read_xlsx('data/handbook_dictionary.xlsx', sheet = 'dictionary') %>% 
  pull(word)

txt_proc <- txt %>% 
  select(-name) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(get_stopwords()) %>% 
  mutate(word = str_remove(word, "[^\\w]")) %>% 
  filter(!str_detect(word, "[\\d+]"), word %in% dict)

doc_word_freq <- txt_proc %>% 
  count(doc_id, word, sort=TRUE)  

doc_tfidf <- doc_word_freq %>% 
  bind_tf_idf(word, doc_id, n)

write_csv(doc_tfidf, 'data/doc_tf_idf.csv')

# most frequent bigrams
bigrams <- txt %>% 
  unnest_tokens(bigram, text, token='ngrams', n=2)

bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)