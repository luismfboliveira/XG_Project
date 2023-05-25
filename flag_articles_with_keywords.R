library(tidyverse)
library(openxlsx)

wd <- getwd()

# path to sample file
path_to_keywords <- 'C:\\Users\\luism\\OneDrive\\XG\\2G_SampleData.xlsx'

#path to collection
path_to_file <- 'C:\\Users\\luism\\OneDrive\\XG\\2G_RawData.xlsx'

keywords <- read.xlsx(path_to_keywords) %>% as_tibble()

all_data <- read.xlsx(path_to_file) %>% as_tibble()

# Cleans sample
keywords_used <-
  keywords %>% 
  select(DE, ID) %>% 
  unite("new_keywords", DE:ID, na.rm = T, sep = ";", remove = F) %>%
  mutate(new_keywords = str_replace_all(new_keywords, c(";;" = "; ")),
         new_keywords = str_squish(new_keywords)) %>% 
  separate_rows(new_keywords, sep = "; ") %>% 
  separate_rows(new_keywords, sep = ";") %>% 
  distinct(new_keywords) %>% 
  filter(str_detect(new_keywords, "\\b")) %>% 
  pull(new_keywords)

  search_words <- str_c("\\b", keywords_used, "\\b", collapse = "|")

flags <- all_data %>%
  rowid_to_column("article_id") %>% 
  unite("new_keywords", DE:ID, na.rm = T, sep = ";", remove = F) %>%
  mutate(new_keywords = str_squish(str_replace_all(new_keywords, ";;", "; "))) %>%
  select(article_id, new_keywords) %>% 
  separate_rows(new_keywords, sep = "; ") %>% 
  separate_rows(new_keywords, sep = ";")

flags %>% 
left_join(keywords_used, by = c("new_keywords" = "new_keywords")) %>% View()

# flags per article
flags <- all_data %>%
  rowid_to_column("article_id") %>% 
  unite("new_keywords", DE:ID, na.rm = T, sep = ";", remove = F) %>%
  mutate(new_keywords = str_squish(str_replace_all(new_keywords, ";;", "; "))) %>%
  select(article_id, new_keywords) %>% 
  separate_rows(new_keywords, sep = "; ") %>% 
  separate_rows(new_keywords, sep = ";") %>% 
  rowwise() %>% 
  mutate(check = any(str_detect(new_keywords, regex(search_words)))) %>% 
  #mutate(check = str_detect(new_keywords, fixed(paste(keywords_used, collapse = "|")))) %>% 
  group_by(article_id) %>% 
  summarise(new_keywords = paste(new_keywords, collapse = "; "),
            has_any = any(check),
            how_many = sum(check),
            pct = sum(check) / n())

# adds flag columns to collection
collection_edit <- 
  all_data %>% 
  rowid_to_column("article_id") %>% 
  left_join(flags, by = "article_id") %>% 
  relocate(new_keywords:pct, .after = "ID")

write.xlsx(collection_edit,
           paste(wd, "collection_flaged_kw.xlsx", sep = "/"),
           row.names = FALSE)