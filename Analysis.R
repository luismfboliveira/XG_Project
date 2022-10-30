##### General Analysis #####

###### Papers and countries per year ######

# Gathers paper production per year and number of countries producing per year
papers_countries_per_year <-
  M_clean %>%
  separate_rows(AU_CO, sep = ";") %>%
  mutate(AU_CO = trimws(AU_CO, which = "both")) %>%
  filter(str_detect(AU_CO, "")) %>%
  group_by(PY) %>%
  summarise(papers_year = n_distinct(article_id),
            countries_year = n_distinct(AU_CO, na.rm = T))

write.xlsx(papers_countries_per_year, paste(directories$dir_general_analysis, "papers_countries_per_year.xlsx", sep = "/"), overwrite = T)

print("Papers and countries per year: done")

# Plots papers per year (we consider 2011 to be take off here)

papers_year <- 
  papers_countries_per_year %>%  
  mutate(Period  = if_else(PY <= 2011, "Before Take off", "After Take off")) %>% 
  ggplot(aes(x = PY, y = papers_year, group = 1)) +
  geom_line(color = "grey") +
  geom_point(aes(colour = Period), shape=20, size=15) +
  geom_text(aes(label=papers_year),color = "white", size = 4) + 
  #scale_x_continuous(limits = seq(min(M$PY), max(M$PY), 1)) +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_blank(), legend.text = element_text(size = 11),
        axis.text.x = element_text(size = 10), panel.border = element_blank()) +
  labs(title = "Research items published on 4G from 2000-2020",
       y = "",
       x = "",
       colour = "")

ggsave("papers_year.pdf", plot = papers_year, device = "pdf", path = directories$dir_general_analysis, units = "in",
       width = 10, height = 6)

ggsave("papers_year.jpeg", plot = papers_year, device = "jpeg", path = directories$dir_general_analysis, units = "in",
       width = 10, height = 6)

# Plots countries producing per year

countries_year <- 
  papers_countries_per_year %>% 
  mutate(Period  = if_else(PY <= 2011, "Before Take off", "After Take off")) %>% 
  ggplot(aes(x = PY, y = countries_year, group = 1)) +
  geom_line(color = "grey", lwd = 1) +
  geom_point(aes(colour = Period), shape=20, size=15) +
  geom_text(aes(label=countries_year),color = "white", size = 4) + 
  #scale_x_discrete(limits = seq(min(M$PY), max(M$PY), 1)) +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_blank(), legend.text = element_text(size = 11),
        axis.text.x = element_text(size = 10), panel.border = element_blank()) +
  labs(title = "Number of countries participating for 4G related articles per year",
       x = "",
       y = "")

ggsave("countries_year.pdf", plot = countries_year, device = "pdf", path = directories$dir_general_analysis, units = "in",
       width = 10, height = 6)

ggsave("countries_year.jpeg", plot = countries_year, device = "jpeg", path = directories$dir_general_analysis, units = "in",
       width = 10, height = 6)


###### Number of authors per articles, countries, only 1 author, only one country, journals ######

countries_clean <-
  M_clean %>% 
  as_tibble() %>% 
  select(article_id, PY, AU, AU1_CO, AU_CO) %>% 
  unite("Unique_countries", c(AU1_CO, AU_CO), sep = ";", na.rm = T, remove = F) %>% 
  separate_rows(Unique_countries, sep = ";") %>% 
  mutate(Unique_countries = trimws(Unique_countries, which = "both")) %>% 
  filter(str_detect(Unique_countries, "")) %>% 
  distinct(article_id, Unique_countries, .keep_all = T) %>% 
  group_by(article_id, PY, AU, AU1_CO, AU_CO) %>% 
  summarise(Unique_countries = paste(Unique_countries, collapse = ";")) %>% 
  ungroup() %>% 
  select(article_id, Unique_countries)

authors_journals_clean <- 
  M_clean %>% 
  as_tibble() %>% 
  select(article_id, PY, AU, SN, eissn) %>%
  filter(!is.na(AU)) %>% 
  separate_rows(AU, sep = ";") %>% 
  mutate(AU = trimws(AU, which = "both"),
         SN = str_replace_all(SN, "-", "")) %>%
  #  EI = str_replace_all(EI, "-", "")) %>% 
  # mutate(SN = coalesce(SN, EI)) %>%
  filter(str_detect(AU, "")) %>% 
  group_by(article_id, PY, SN) %>% 
  summarise(AU = paste(AU, collapse = ";")) %>% 
  ungroup()


year_stats_authors_countries_journals <-
  full_join(authors_journals_clean, countries_clean, by = "article_id") %>% 
  mutate(number_of_authors = str_count(AU, ";") + 1,
         number_of_countries = str_count(Unique_countries, ";") + 1,
         single_author = case_when(str_count(AU, ";") == 0 ~ 1,
                                   TRUE ~ 0),
         single_country = case_when(str_count(Unique_countries, ";") == 0 ~ 1,
                                    TRUE ~ 0)) %>% 
  group_by(PY) %>% 
  summarise(average_number_of_authors = mean(number_of_authors, na.rm = T),
            average_number_of_unique_countries = mean(number_of_countries, na.rm = T),
            number_single_authors_articles = sum(single_author),
            pct_single_authors_articles = mean(single_author, na.rm = T),
            number_single_country_articles = sum(single_country),
            pct_single_country_articles = mean(single_country, na.rm = T),
            unique_journals = n_distinct(SN),
            number_of_articles = n_distinct(article_id)) %>% 
  filter(!is.na(PY))

write.xlsx(year_stats_authors_countries_journals,
           paste(directories$dir_general_analysis, "year_stats_authors_countries_journals.xlsx", sep = "/"), overwrite = T)

print("Number of authors per articles, countries, only 1 author, only one country, journals: done")

##### Herfindahal #####

# Calculates country shares per year and Herfindahal index for each year.
# It takes first author country, therefore, Herfindahal index to see share of leading project.

###### Country shares ######

country_shares <- 
  M_clean %>% 
  filter(!is.na(AU1_CO)) %>% 
  group_by(PY, AU1_CO) %>% 
  summarise(paper_country_year = n())  %>% 
  add_count(PY, wt = paper_country_year) %>% 
  mutate(country_share = paper_country_year / n * 100) %>% 
  ungroup() %>% 
  #expand(PY, AU1_CO)
  complete(PY, AU1_CO, fill = list(paper_country_year = 0, country_share = 0)) %>%
  group_by(PY) %>% 
  fill(n, .direction = "updown") %>% 
  ungroup()

write.xlsx(country_shares, paste(directories$dir_herfindahal, "country_shares.xlsx", sep = "/"), overwrite = T)

###### Calculates Herfindahal index - country ######

Herfindahal_index_country <-
  country_shares %>% 
  pivot_wider(-c(paper_country_year, n), names_from = PY, values_from = country_share) %>% 
  summarise(across(where(is.numeric), ~ sum(.x^2))) %>% 
  pivot_longer(everything()) %>% 
  rename("Year" = name, "Herfindahal_index_country" = value) %>% 
  mutate(Year = as.numeric(Year))

write.xlsx(Herfindahal_index_country, paste(directories$dir_herfindahal, "Herfindahal_index_country.xlsx", sep = "/"), overwrite = T)

print("Calculates Herfindahal index - country: done")

###### Plots herfindahal index - countries ######

herfindahal_index_country <- 
  Herfindahal_index_country %>% 
  ggplot(aes(x = Year, y = round(Herfindahal_index_country,0), group = 1)) +
  geom_line(color = "grey", lwd = 1) +
  geom_point(shape=20, size=20) +
  geom_text(aes(label=round(Herfindahal_index_country,0)),color = "white", size = 4) + 
  #scale_x_discrete(limits = seq(min(M$PY), max(M$PY), 1)) +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_blank(), legend.text = element_text(size = 11),
        axis.text.x = element_text(size = 10), panel.border = element_blank()) +
  labs(title = "Herfindahal index for countries participating in 4G related articles",
       x = "",
       y = "") +
  expand_limits(y = 0)


ggsave("Herfindahal_index_country.pdf", plot = herfindahal_index_country, device = "pdf", path = directories$dir_herfindahal, units = "in",
       width = 10, height = 6)

ggsave("Herfindahal_index_country.jpeg", plot = herfindahal_index_country, device = "jpeg", path = directories$dir_herfindahal, units = "in",
       width = 10, height = 6)


###### Evolution for the 5 most productive countries (by leading articles) ######

country_shares_top_5 <-
  country_shares %>% 
  add_count(AU1_CO, wt = paper_country_year, name = "country_total_papers") %>%
  filter(dense_rank(desc(country_total_papers)) %in% 1:5) %>% 
  ## filter(PY >= 2014) %>% 
  ggplot(aes(x = PY, y = country_share/100, color = AU1_CO)) +
  geom_line(lwd = 2) +
  scale_y_continuous(labels = percent_format()) +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_blank(), legend.text = element_text(size = 11),
        axis.text.x = element_text(size = 10), panel.border = element_blank()) +
  labs(title = "Paper production share for top 5 countries with more first author papers",
       x = "",
       y = "",
       color = "Country")


ggsave("country_shares_top5.pdf", plot = country_shares_top_5, device = "pdf", path = directories$dir_herfindahal, units = "in",
       width = 10, height = 6)

ggsave("country_shares_top5.jpeg", plot = country_shares_top_5, device = "jpeg", path = directories$dir_herfindahal, units = "in",
       width = 10, height = 6)

###### Journal shares ######

journal_shares <- 
  authors_journals_clean %>% 
  group_by(PY, SN) %>% 
  summarise(journal_year = n())  %>% 
  add_count(PY, wt = journal_year) %>% 
  mutate(journal_share = journal_year / n * 100) %>% 
  ungroup() %>% 
  #expand(PY, SN) %>% 
  complete(PY, SN, fill = list(journal_year = 0, journal_share = 0)) %>%
  group_by(PY) %>% 
  fill(n, .direction = "updown") %>% 
  ungroup()

write.xlsx(journal_shares, paste(directories$dir_herfindahal, "journal_shares.xlsx", sep = "/"), overwrite = T)

###### Calculates Herfindahal index - journals ######

Herfindahal_index_journals <- 
  journal_shares %>% 
  pivot_wider(-c(journal_year, n), names_from = PY, values_from = journal_share) %>% 
  summarise(across(where(is.numeric), ~ sum(.x^2))) %>% 
  pivot_longer(everything()) %>% 
  rename("Year" = name, "Herfindahal_index" = value) %>% 
  mutate(Year = as.numeric(Year))

write.xlsx(Herfindahal_index_journals, paste(directories$dir_herfindahal, "Herfindahal_index_journals.xlsx", sep = "/"), overwrite = T)

print("Calculates Herfindahal index - journals: done")

###### Plots herfindahal index - journals ######

herfindahal_index_journals <- 
  Herfindahal_index_journals %>% 
  ggplot(aes(x = Year, y = round(Herfindahal_index,0), group = 1)) +
  geom_line(color = "grey", lwd = 1) +
  geom_point(shape=20, size=20) +
  geom_text(aes(label=round(Herfindahal_index,0)),color = "white", size = 4) + 
  #scale_x_discrete(limits = seq(min(M$PY), max(M$PY), 1)) +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_blank(), legend.text = element_text(size = 11),
        axis.text.x = element_text(size = 10), panel.border = element_blank()) +
  labs(title = "Herfindahal index for journals participating in 4G related articles",
       x = "",
       y = "") +
  expand_limits(y = 0)


ggsave("herfindahal_index_journals.pdf", plot = herfindahal_index_journals, device = "pdf", path = directories$dir_herfindahal, units = "in",
       width = 10, height = 6)

ggsave("herfindahal_index_journals.jpeg", plot = herfindahal_index_journals, device = "jpeg", path = directories$dir_herfindahal, units = "in",
       width = 10, height = 6)


##### Authors #####

###### Authors per continent ######

authors_continent <- 
  left_join(
    API_info$author_info,
    API_info$affil_info %>% 
      select(-DI),
    by = c("article_id", "affiliation_id")) %>% 
  mutate(region = countrycode(affiliation_country, origin = 'country.name', destination = 'region'),
         continent = countrycode(affiliation_country, origin = 'country.name', destination = 'continent'),
         continent = ifelse(affiliation_country == "KOSOVO", "Europe", continent)) %>% 
  filter(!is.na(continent))

write.xlsx(authors_continent, paste(directories$dir_authors, "authors_article_info_granular.xlsx", sep = "/"), overwrite = T)

Unique_Authors_Continent <- 
  authors_continent %>% 
  distinct(article_id, Aut_ID, affiliation_id, .keep_all = T) %>% 
  group_by(continent) %>% 
  summarise(Unique_Authors = n_distinct(Aut_ID),
            Unique_countries = n_distinct(affiliation_country))

write.xlsx(Unique_Authors_Continent, paste(directories$dir_authors, "Unique_Authors_Continent.xlsx", sep = "/"), overwrite = T)

print("Authors per continent: done")

###### Top 20 authors V2 ######

top_20_authors <-
  API_info$author_info %>% 
  distinct(article_id, Aut_ID, .keep_all = T) %>% 
  count(Aut_ID, sort = T, name = "value") %>% 
  slice_max(value, n = 20, with_ties = F)

###### Dominant affiliation ######

dominant_affil_aut <-
  authors_continent %>% 
  distinct(article_id, Aut_ID, affiliation_id, .keep_all = T) %>%
  count(Aut_ID, affiliation_id, sort = T) %>% 
  group_by(Aut_ID) %>% 
  slice_max(n, n = 1) %>% 
  ungroup()

###### Most recent affiliation ######

most_recent_affil_untie_dominant_aut <-
  authors_continent %>% 
  mutate(article_id = as.numeric(article_id)) %>% 
  left_join(M_clean %>% select(article_id, PY),
            by = "article_id") %>% 
  distinct(article_id, Aut_ID, .keep_all = T) %>%
  distinct(Aut_ID, affiliation_id, PY) %>% 
  group_by(Aut_ID) %>% 
  slice_max(PY, n = 1) %>% 
  ungroup() %>%
  left_join(API_info$author_info %>% 
              distinct(article_id, Aut_ID, .keep_all = T) %>%
              count(Aut_ID, affiliation_id, sort = T),
            by = c("Aut_ID", "affiliation_id")) %>% 
  group_by(Aut_ID) %>% 
  slice_max(n, n = 1) %>% ungroup()

###### Top 20 by dominant affiliation - number articles ######

top_20_authors_by_dom_affil <-
  top_20_authors %>% 
  left_join(dominant_affil_aut %>% 
              filter(Aut_ID %in% top_20_authors$Aut_ID),
            by = "Aut_ID") %>% 
  left_join(authors_continent %>% 
              count(Aut_ID, Index_name, sort = T, name = "name_count") %>% 
              filter(Aut_ID %in% top_20_authors$Aut_ID) %>% 
              group_by(Aut_ID) %>% 
              slice_max(name_count, n = 1) %>% 
              ungroup(),
            by = "Aut_ID") %>% 
  left_join(authors_continent %>%
              distinct(affiliation_id, affiliation_name, affiliation_country),
            by = "affiliation_id") %>% 
  mutate(country_codes  = countrycode(affiliation_country, origin = "country.name", destination = "iso2c"),
         country_codes = str_replace_all(country_codes, "GB", "UK")) %>% 
  arrange(desc(value)) %>% 
  mutate(metric = "By number of articles")



###### Top 20 by most recent affiliation - number articles ######

top_20_authors_by_most_recent_affil <-
  top_20_authors %>% 
  left_join(most_recent_affil_untie_dominant_aut %>% 
              filter(Aut_ID %in% top_20_authors$Aut_ID),
            by = "Aut_ID") %>% 
  left_join(API_info$author_info %>% 
              count(Aut_ID, Index_name, sort = T, name = "name_count") %>% 
              filter(Aut_ID %in% top_20_authors$Aut_ID) %>% 
              group_by(Aut_ID) %>% 
              slice_max(name_count, n = 1) %>% 
              ungroup(),
            by = "Aut_ID") %>% 
  left_join(authors_continent %>%
              distinct(affiliation_id, affiliation_name, affiliation_country),
            by = "affiliation_id") %>% 
  mutate(country_codes  = countrycode(affiliation_country, origin = "country.name", destination = "iso2c"),
         country_codes = str_replace_all(country_codes, "GB", "UK")) %>% 
  arrange(desc(value)) %>% 
  mutate(metric = "By number of articles")

write.xlsx(top_20_authors_by_most_recent_affil, paste(directories$dir_authors, "top_20_authors_by_most_recent_affil.xlsx", sep = "/"), overwrite = T)

###### Top 20 by dominant affiliation - citations ######

top_20_authors_citations_dominant_affil <-
  API_info$author_info %>% 
  distinct(article_id, Aut_ID, .keep_all = T) %>%
  mutate(article_id = as.numeric(article_id)) %>% 
  left_join(M_clean %>% select(article_id, TC),
            by = "article_id") %>% 
  group_by(Aut_ID) %>% 
  summarize(value = sum(TC),
            articles = n_distinct(article_id)) %>% 
  slice_max(value, n = 20, with_ties = F) %>% 
  left_join(API_info$author_info %>% 
              count(Aut_ID, Index_name, sort = T, name = "name_count") %>%
              group_by(Aut_ID) %>% 
              slice_max(name_count, n = 1) %>% 
              ungroup(),
            by = "Aut_ID") %>% 
  left_join(dominant_affil_aut,
            by = "Aut_ID") %>% 
  left_join(authors_continent %>%
              distinct(affiliation_id, affiliation_name, affiliation_country),
            by = "affiliation_id") %>% 
  mutate(country_codes  = countrycode(affiliation_country, origin = "country.name", destination = "iso2c"),
         country_codes = str_replace_all(country_codes, "GB", "UK")) %>% 
  arrange(desc(value))%>% 
  mutate(metric = "By total citations")

###### Top 20 by most recent affiliation - citations ######

top_20_authors_citations_recent_affil <-
  API_info$author_info %>% 
  distinct(article_id, Aut_ID, .keep_all = T) %>%
  mutate(article_id = as.numeric(article_id)) %>% 
  left_join(M_clean %>% select(article_id, TC),
            by = "article_id") %>% 
  group_by(Aut_ID) %>% 
  summarize(value = sum(TC),
            articles = n_distinct(article_id)) %>% 
  slice_max(value, n = 20) %>% 
  left_join(API_info$author_info %>% 
              count(Aut_ID, Index_name, sort = T, name = "name_count") %>%
              group_by(Aut_ID) %>% 
              slice_max(name_count, n = 1) %>% 
              ungroup(),
            by = "Aut_ID") %>% 
  left_join(most_recent_affil_untie_dominant_aut,
            by = "Aut_ID") %>% 
  left_join(authors_continent %>%
              distinct(affiliation_id, affiliation_name, affiliation_country),
            by = "affiliation_id") %>% 
  mutate(country_codes  = countrycode(affiliation_country, origin = "country.name", destination = "iso2c"),
         country_codes = str_replace_all(country_codes, "GB", "UK")) %>% 
  arrange(desc(value))%>% 
  mutate(metric = "By total citations")

###### Author report by dominant affiliation ######

author_info_report_dominant_affil <- bind_rows(top_20_authors_by_dom_affil %>% 
                                                 select(Index_name, value, affiliation_name, country_codes, metric),
                                               top_20_authors_citations_dominant_affil %>% 
                                                 select(Index_name, value, affiliation_name, country_codes, metric))

write.xlsx(author_info_report_dominant_affil, paste(directories$dir_authors, "top_20_authors_by_dom_affil.xlsx", sep = "/"), overwrite = T)

print("Author report by dominant affiliation: done")

###### Plots author report by dominant affiliation ######

author_dominant_affil <-
  author_info_report_dominant_affil %>% 
  mutate(compact_names = paste(Index_name, affiliation_name, country_codes, sep = "; "),
         compact_names = reorder_within(compact_names, value, metric))  %>% 
  ggplot(aes(x = compact_names, y = value)) +
  geom_col() +
  coord_flip() +
  facet_wrap( ~ metric, scales = "free", ncol = 2) +
  scale_x_reordered() +
  labs(title = NULL,
       x = "",
       y = "") +
  geom_text(aes(label = value, hjust = 1.2), colour = "white", size = 15) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 32), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        strip.text = element_text(size = 40),
        legend.title = element_text(size = 15))


ggsave("author_dominant_affil.pdf", plot = author_dominant_affil, device = "pdf", path = directories$dir_authors, units = "in",
       width = 60, height = 20, limitsize = FALSE)

ggsave("author_dominant_affil.jpeg", plot = author_dominant_affil, device = "jpeg", path = directories$dir_authors, units = "in",
       width = 60, height = 20, limitsize = FALSE)


###### Author report by most recent affiliation ######

author_info_report_recent_affil <- bind_rows(top_20_authors_by_most_recent_affil %>% 
                                               select(Index_name, value, affiliation_name, country_codes, metric),
                                             top_20_authors_citations_recent_affil %>% 
                                               select(Index_name, value, affiliation_name, country_codes, metric))

write.xlsx(author_info_report_recent_affil, paste(directories$dir_authors, "top_20_authors_by_recent_affil.xlsx", sep = "/"), overwrite = T)

print("Author report by most recent affiliation: done")

###### Plots author report by most recent affiliation ######

author_recent_affil <- 
  author_info_report_recent_affil %>% 
  mutate(compact_names = paste(Index_name, affiliation_name, country_codes, sep = "; "),
         compact_names = reorder_within(compact_names, value, metric))  %>% 
  ggplot(aes(x = compact_names, y = value)) +
  geom_col() +
  coord_flip() +
  facet_wrap( ~ metric, scales = "free", ncol = 2) +
  scale_x_reordered() +
  labs(title = NULL,
       x = "",
       y = "") +
  geom_text(aes(label = value, hjust = 1.2), colour = "white", size = 15) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 32), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        strip.text = element_text(size = 40),
        legend.title = element_text(size = 15))


ggsave("author_recent_affil.pdf", plot = author_recent_affil, device = "pdf", path = directories$dir_authors, units = "in",
       width = 60, height = 20, limitsize = FALSE)

ggsave("author_recent_affil.jpeg", plot = author_recent_affil, device = "jpeg", path = directories$dir_authors, units = "in",
       width = 60, height = 20, limitsize = FALSE)

##### SJR Info #####

###### Evolution top 20 countries ######

SJR_info <-
  M_clean %>% 
  as_tibble() %>% 
  select(article_id, PY, SO, SN, AU_CO) %>% 
  mutate(SN = str_replace_all(SN, "-", ""),
         PY = as.factor(PY)) %>% 
  left_join(Journal_metrics_info %>% 
              separate_rows(Issn, sep = ",") %>% 
              mutate(Issn = trimws(Issn, which = "both")), by = c("SN" = "Issn", "PY" = "year")) %>% 
  arrange(SO, PY)

sjr_processed <- 
  SJR_info %>% 
  mutate(PY = as.numeric(as.character(PY)),
         AU_CO = str_to_title(AU_CO)) %>% 
  filter(!is.na(SJR.Best.Quartile),
         PY >= 2001) %>%
  mutate(Quartile_numeric = as.numeric(str_extract(SJR.Best.Quartile, "\\d"))) %>% 
  separate_rows(AU_CO, sep = ";") %>% 
  mutate(AU_CO = trimws(AU_CO, which = "both")) %>% 
  filter(str_detect(AU_CO, "")) %>% 
  distinct(article_id, AU_CO, .keep_all = TRUE) %>% 
  add_count(AU_CO, name = "Number_of_articles_CO") %>% 
  filter(dense_rank(desc(Number_of_articles_CO)) %in% 1:20)  %>%
  select(PY, Quartile_numeric, AU_CO) %>%
  group_by(AU_CO, PY) %>% 
  summarise(average_quartile = mean(Quartile_numeric, na.rm  =T),
            sem = var(Quartile_numeric) / sqrt(n())) %>%
  ungroup() %>% 
  mutate(AU_CO = fct_reorder(AU_CO, -average_quartile, last))

evolution_sjr_rank <- 
  sjr_processed %>% 
  ggplot(aes(x = PY, y = AU_CO, fill = average_quartile)) +
  geom_tile(colour = "white") +
  scale_fill_continuous(high = "#56B1F7", low = "#132B43") +
  geom_text(aes(label=round(average_quartile,2)), colour = "white", size = 12) +
  labs(title = "Average publication journal quartile for top 20 countries with more publications (descending by year 2019)",
       y = "",
       x = "",
       fill = "Average Journal Quartile") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.text.y = element_text(size = 32), legend.title = element_text(size = 15),
        axis.text.x = element_text(size = 32), plot.title = element_text(size = 40)) +
  scale_x_continuous(breaks = 2001:2020)

ggsave("sjr_evolution_top20.pdf", plot = evolution_sjr_rank, device = "pdf", path = directories$dir_sjr, units = "in",
       width = 60, height = 20, limitsize = FALSE)

ggsave("sjr_evolution_top20.jpeg", plot = evolution_sjr_rank, device = "jpeg", path = directories$dir_sjr, units = "in",
       width = 60, height = 20, limitsize = FALSE)

write.xlsx(sjr_processed, paste(directories$dir_sjr, "evolution_sjr_top20.xlsx", sep = "/"), overwrite = T)

###### Temporal evolution of average SJR quartile ######

average_sjr_quartile <- 
  SJR_info %>% 
  mutate(PY = as.numeric(as.character(PY))) %>% 
  filter(PY >= 2014) %>% 
  filter(!is.na(SJR.Best.Quartile)) %>%
  mutate(Quartile_numeric = as.numeric(str_extract(SJR.Best.Quartile, "\\d"))) %>% 
  select(PY, Quartile_numeric) %>%
  group_by(PY) %>% 
  summarise(average_quartile = mean(Quartile_numeric, na.rm = T),
            sem = var(Quartile_numeric) / sqrt(n()))

average_sjr_quartile_chart <-
  average_sjr_quartile %>% 
  ggplot(aes(x = PY, y = average_quartile, group = 1)) +
  geom_ribbon(aes(ymin = average_quartile - sem, ymax = average_quartile + sem), fill = "grey70") +
  geom_line() +
  geom_point() + 
  scale_y_continuous(trans = "reverse") +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.text.x = element_text(size = 22)) + 
  expand_limits(y = 1) +
  labs(title = NULL,
       subtitle = NULL,
       x = "",
       y = "")

ggsave("average_sjr_quartile_evolution.pdf", plot = average_sjr_quartile_chart, device = "pdf", path = directories$dir_sjr, units = "in",
       width = 60, height = 20, limitsize = FALSE)

ggsave("average_sjr_quartile_evolution.jpeg", plot = average_sjr_quartile_chart, device = "jpeg", path = directories$dir_sjr, units = "in",
       width = 60, height = 20, limitsize = FALSE)

write.xlsx(average_sjr_quartile, paste(directories$dir_sjr, "average_sjr_quartile_evolution.xlsx", sep = "/"), overwrite = T)

##### Countries #####

###### First author countries ######

# Adding countrycode's regions and continents.
# countrycode returns NA for continent for Kosovo, but we will fill it with Europe.

author1_country_papers <-
  M_clean %>% 
  group_by(AU1_CO) %>% 
  count() %>% 
  filter(!is.na(AU1_CO)) %>% ungroup() %>% 
  mutate(region = countrycode(AU1_CO, origin = 'country.name', destination = 'region'),
         continent = countrycode(AU1_CO, origin = 'country.name', destination = 'continent'),
         continent = ifelse(AU1_CO == "KOSOVO", "Europe", continent))

# Exports country distributions.

author1_country_papers %>% 
  #select(AU1_CO, n) %>% 
  arrange(desc(n)) %>% 
  write.xlsx(paste(directories$dir_authors, "first_Author_Distribution_country.xlsx", sep = "/"), overwrite = T)

data("pop")

UN_wpp <- 
  left_join(author1_country_papers %>% 
              select(AU1_CO, n) %>% 
              arrange(desc(n)),
            pop %>% as_tibble %>% 
              mutate(name = str_replace_all(name, c("United States of America" = "USA",
                                                    "United Kingdom" = "UK",
                                                    "Republic of Korea" = "SOUTH KOREA",
                                                    "Iran \\(Islamic Republic of\\)" = "IRAN",
                                                    "Russian Federation" = "RUSSIA",
                                                    "Viet Nam" = "VIETNAM",
                                                    "Czechia" = "CZECH REPUBLIC",
                                                    "North Macedonia" = "MACEDONIA",
                                                    "Brunei Darussalam" = "BRUNEI",
                                                    "United Republic of Tanzania" = "TANZANIA",
                                                    "Venezuela \\(Bolivarian Republic of\\)" = "VENEZUELA")),
                     name = str_to_upper(name)),
            by = c("AU1_CO" = "name")) %>% 
  mutate(region = countrycode(AU1_CO, origin = 'country.name', destination = 'region'),
         continent = countrycode(AU1_CO, origin = 'country.name', destination = 'continent'),
         continent = ifelse(AU1_CO == "KOSOVO", "Europe", continent)) %>% 
  select(AU1_CO, n, `2015`, `2020`, continent, region)

country_articles1M_habitants <- UN_wpp %>% mutate(raw_pop_2015 = `2015` * 1000,
                                                  raw_pop_2020 = `2020` * 1000,
                                                  avg_pop = (raw_pop_2015 + raw_pop_2020) / 2,
                                                  articles_1M = round(n * 1000000 / avg_pop, 2)) %>% arrange(desc(articles_1M)) 

country_top30_articles_country_1Mhab <- 
  country_articles1M_habitants %>% 
  filter(AU1_CO %in% as.character(author1_country_papers %>% slice_max(n, n = 30) %>% pull(AU1_CO))) %>% 
  select(AU1_CO, n, articles_1M, continent) %>% 
  rename(`Number of Articles` = "n",
         `Articles per 1M habitants` = "articles_1M") %>% 
  pivot_longer(-c(AU1_CO, continent), names_to = "metric", values_to = "value")

write.xlsx(country_top30_articles_country_1Mhab, paste(directories$dir_authors, "country_top30_articles_country_1Mhab.xlsx", sep = "/"), overwrite = T)

author1_country_papers %>% 
  count(continent, wt = n, sort = T) %>% 
  write.xlsx(paste(directories$dir_authors, "Author1_Distribution_per_continent.xlsx", sep = "/"), overwrite = T)

author1_country_papers %>% 
  count(continent, region, wt = n, sort = T) %>% 
  write.xlsx(paste(directories$dir_authors, "Author1_Distribution_per_continent_per_region.xlsx", sep = "/"), overwrite = T)

print("First author country analysis: done")

###### ALL authors countries ######

ALL_authors_country_papers <-
  M_clean %>% 
  unite("ALL_authors", c(AU1_CO, AU_CO), sep = ";", na.rm = T, remove = F) %>% 
  separate_rows(ALL_authors, sep = ";") %>% 
  mutate(ALL_authors = trimws(ALL_authors, which = "both")) %>% 
  filter(str_detect(ALL_authors, "")) %>% 
  distinct(article_id, ALL_authors, .keep_all = T) %>% 
  group_by(ALL_authors) %>% 
  summarise(n = n(),
             total_citations = sum(TC)) %>% 
  filter(!is.na(ALL_authors)) %>% 
  ungroup() %>% 
  mutate(region = countrycode(ALL_authors, origin = 'country.name', destination = 'region'),
         continent = countrycode(ALL_authors, origin = 'country.name', destination = 'continent'),
         continent = ifelse(ALL_authors == "KOSOVO", "Europe", continent))

ALL_authors_country_papers %>% 
  #select(ALL_authors, n) %>% 
  arrange(desc(n)) %>% 
  write.xlsx(paste(directories$dir_authors, "ALL_authors_Distribution_country.xlsx", sep = "/"), overwrite = T)

ALL_authors_country_papers %>% 
  count(continent, wt = n, sort = T) %>% 
  write.xlsx(paste(directories$dir_authors, "ALL_authors_Distribution_per_continent.xlsx", sep = "/"), overwrite = T)

ALL_authors_country_papers %>% 
  count(continent, region, wt = n, sort = T) %>% 
  write.xlsx(paste(directories$dir_authors, "ALL_authors_Distribution_per_continent_per_region.xlsx", sep = "/"), overwrite = T)

UN_wpp <- 
  left_join(ALL_authors_country_papers %>% 
              select(ALL_authors, n) %>% 
              arrange(desc(n)),
            pop %>% as_tibble %>% 
              mutate(name = str_replace_all(name, c("United States of America" = "USA",
                                                    "United Kingdom" = "UK",
                                                    "Republic of Korea" = "SOUTH KOREA",
                                                    "Iran \\(Islamic Republic of\\)" = "IRAN",
                                                    "Russian Federation" = "RUSSIA",
                                                    "Viet Nam" = "VIETNAM",
                                                    "Czechia" = "CZECH REPUBLIC",
                                                    "North Macedonia" = "MACEDONIA",
                                                    "Brunei Darussalam" = "BRUNEI",
                                                    "United Republic of Tanzania" = "TANZANIA",
                                                    "Venezuela \\(Bolivarian Republic of\\)" = "VENEZUELA")),
                     name = str_to_upper(name)),
            by = c("ALL_authors" = "name")) %>% 
  mutate(region = countrycode(ALL_authors, origin = 'country.name', destination = 'region'),
         continent = countrycode(ALL_authors, origin = 'country.name', destination = 'continent'),
         continent = ifelse(ALL_authors == "KOSOVO", "Europe", continent)) %>% 
  select(ALL_authors, n, `2015`, `2020`, continent, region)

country_all_articles1M_habitants <- UN_wpp %>% mutate(raw_pop_2015 = `2015` * 1000,
                                                      raw_pop_2020 = `2020` * 1000,
                                                      avg_pop = (raw_pop_2015 + raw_pop_2020) / 2,
                                                      articles_1M = round(n * 1000000 / avg_pop, 2)) %>% arrange(desc(articles_1M)) 


country_all_top30_articles_country_1Mhab <- 
  country_all_articles1M_habitants %>% 
  filter(ALL_authors %in% as.character(ALL_authors_country_papers %>% slice_max(n, n = 30) %>% pull(ALL_authors))) %>% 
  left_join(country_articles1M_habitants %>% 
              select(AU1_CO, n, articles_1M) %>% 
              rename(`First author articles` = "n", `First author articles per 1M habitants` = "articles_1M"),
            by = c("ALL_authors" = "AU1_CO")) %>% 
  select(ALL_authors, n, `First author articles per 1M habitants`, continent) %>% 
  rename(`Number of Articles` = "n") %>% 
  pivot_longer(-c(ALL_authors, continent), names_to = "metric", values_to = "value")

write.xlsx(country_all_top30_articles_country_1Mhab, paste(directories$dir_authors, "country_all_top30_articles_country_1Mhab", sep = "/"), overwrite = T)

print("All authors countries analysis: done")

###### plots the quantification of all author countries. ######

ALL_authors_country_papers %>% 
  slice_max(n, n = 30) %>%
  mutate(ALL_authors = str_to_title(ALL_authors),
         ALL_authors = fct_reorder(ALL_authors, n)) %>% 
  ggplot(aes(x = ALL_authors, y = n, fill = continent)) +
  geom_col() +
  coord_flip() +
  labs(title = "Number of articles per Country (Top 30)",
       x = "",
       y = "",
       fill = "Continent") +
  geom_text(aes(label = n, hjust = -0.2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 12), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

###### plots the quantifications of all citations accruing from authors in a given country ######

top_30_countries_with_more_citations <-
  ALL_authors_country_papers %>% 
  slice_max(total_citations, n = 30) %>%
  mutate(ALL_authors = str_to_title(ALL_authors),
         ALL_authors = fct_reorder(ALL_authors, n),
         ALL_authors = fct_reorder(ALL_authors, total_citations)) %>% 
  ggplot(aes(x = ALL_authors, y = total_citations, fill = continent)) +
  geom_col() +
  coord_flip() +
  labs(title = "Total number of citations accruing from authors of a given country (Top 30)",
       subtitle = "Numbers accrue from authors belonging to a given country",
       x = "",
       y = "",
       fill = "Continent") +
  geom_text(aes(label = total_citations, hjust = -0.2), size = 5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 15), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 14),
        title = element_text(size = 17))

top_30_countries_with_more_citations

ggsave("total_number_of_citations_top30_country.pdf", plot = top_30_countries_with_more_citations, device = "pdf",
       path = directories$dir_authors, units = "in",
       width = 20, height = 10, limitsize = FALSE)

ggsave("total_number_of_citations_top30_country.jpeg", plot = top_30_countries_with_more_citations,
       device = "jpeg", path = directories$dir_authors, units = "in",
       width = 20, height = 10, limitsize = FALSE)

###### Plots top of articles/1M habitants ######

country_articles1M_habitants %>% 
  slice_max(articles_1M, n = 30) %>% 
  mutate(AU1_CO = str_to_title(AU1_CO),
         AU1_CO = fct_reorder(AU1_CO, articles_1M)) %>% 
  ggplot(aes(x = AU1_CO, y = articles_1M, fill = continent)) +
  geom_col() +
  coord_flip() +
  labs(title = "Number of articles per Country per 1M habitants (Top 30)",
       subtitle = "By 1st author affiliation country",
       x = "",
       y = "",
       fill = "Continent") +
  geom_text(aes(label = articles_1M, hjust = -0.2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 12), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

###### Plots facet wrap absolute articles (ALL Authors Countries) and per 1M habitants (Articles 1st Author). ######

top30_articles_country_1MHab <-
  country_all_top30_articles_country_1Mhab %>% 
  mutate(ALL_authors = str_to_title(ALL_authors),
         ALL_authors = str_replace_all(ALL_authors, c("Usa" = "US", "Uk" = "UK")),
         ALL_authors = reorder_within(ALL_authors, value, metric),
         metric = fct_relevel(metric, "Number of Articles", "First author articles per 1M habitants")) %>% 
  ggplot(aes(x = ALL_authors, y = value, fill = continent)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ metric, scales = "free", ncol = 2, labeller = label_wrap_gen()) +
  labs(title = NULL,
       x = "",
       y = "",
       fill = "Continent") +
  geom_text(aes(label = value, hjust = 0.3), size = 15) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 46), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.text = element_text(size = 55),
        legend.title = element_text(size = 60),
        strip.text = element_text(size = 55)) +
  scale_x_reordered()


ggsave("top30_articles_country_1MHab.pdf", plot = top30_articles_country_1MHab, device = "pdf", path = directories$dir_authors, units = "in",
       width = 60, height = 20, limitsize = FALSE)

ggsave("top30_articles_country_1MHab.jpeg", plot = top30_articles_country_1MHab, device = "jpeg", path = directories$dir_authors, units = "in",
       width = 60, height = 20, limitsize = FALSE)

# Removes Antarctica from World_map dataset
world_map <- map_data("world") %>% filter(region != "Antarctica") %>% mutate(region = str_to_upper(region))

# Checks which countries don't match between our dataset and world map (KOREA, U ARAB EMIRATES, UNITED KINGDOM)
# In world map they have the following names, respectively: SOUTH KOREA, UNITED ARAB EMIRATES, UK

ALL_authors_country_papers %>% 
  mutate(Is_in_world_map = ALL_authors %in% world_map$region) %>% 
  filter(!Is_in_world_map)

# Changes names to allow matching

# country_papers_processed <- 
#   country_papers %>% 
#   mutate(AU_CO = str_replace_all(AU_CO, c("U ARAB EMIRATES" = "UNITED ARAB EMIRATES", "KOREA" = "SOUTH KOREA", "UNITED KINGDOM" = "UK")))

# Joins datasets, we want all regions in world_map, so left_join.

map_countries_papers <- 
  left_join(world_map, ALL_authors_country_papers, by = c("region" = "ALL_authors")) %>% 
  replace_na(list(n = 0))

map_number_articles_country <-
  ggplot(map_countries_papers, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = n), colour = "white") + 
  theme_map() +
  coord_map(xlim=c(-180,180)) +
  scale_fill_gradient(trans = "log", breaks = c(1,8,60,500,4000), labels = c(1,8,60,500,4000),
                      high = "#132B43", low = "#56B1F7") +
  labs(title = "Number of articles per country",
       subtitle = "Colour scale in logarithmic scale\nGrey countries have 0 articles",
       fill = "Number of articles")


ggsave("map_number_articles_country.pdf", plot = map_number_articles_country, device = "pdf", path = directories$dir_general_analysis, units = "in",
       width = 10, height = 6)

ggsave("map_number_articles_country.jpeg", plot = map_number_articles_country, device = "jpeg", path = directories$dir_general_analysis, units = "in",
       width = 10, height = 6)

##### Country Network #####

country_network <- 
  M_clean %>% 
  unite("ALL_authors", c(AU1_CO, AU_CO), sep = ";", na.rm = T, remove = F) %>% 
  separate_rows(ALL_authors, sep = ";") %>% 
  mutate(ALL_authors = trimws(ALL_authors, which = "both")) %>% 
  filter(str_detect(ALL_authors, "")) %>% 
  distinct(article_id, ALL_authors, .keep_all = T) %>% 
  as_tibble() %>% 
  select(article_id, ALL_authors) %>% 
  rename(AU_CO = ALL_authors) %>% 
  group_by(article_id) %>% 
  summarise(AU_CO = paste(AU_CO, collapse = ";")) %>% as.data.frame()

teste_2 <- country_network %>% select(article_id, AU_CO)

net_country_collaboration <- biblioNetwork(teste_2, analysis = "collaboration", network = "countries", sep = ";")

# We export the network for a folder in the output (C:\Users\luis\OneDrive\5G\Output\Affiliation Collaboration), to visualize it in VOSVIEWER.
# We exported with n = 200, this n = 50 is for alternative plot with bibliometrix.

###### Collaboration ######

net_country_collab_plot <- networkPlot(net_country_collaboration, labelsize=1, weighted = T, n  = 200) 
#net2VOSviewer(net_country_collab_plot, vos.path = directories$dir_country_net)

###### Country Collaboration Network statistics ######
# With all data, we find disconnected components. (Isolated countries, might be noise, or name problems.)
# We can get only the main component, we just need to filter to min size of 10, in this version.

net_country_collab_plot <- networkPlot(net_country_collaboration, type = "auto", verbose = F, remove.isolates = T)

# Getting all component info.

country_network_components <- components(net_country_collab_plot$graph)
membership <- data.frame(affiliation = names(country_network_components$membership),
                         member = as.numeric(country_network_components$membership)) %>% 
  as_tibble()
sub_network_size <- country_network_components$csize
number_subnetworks <- country_network_components$no 

# Filtering out sub networks.

main_network_country <- decompose(net_country_collab_plot$graph, min.vertices = 10)

entire_network_stats <- networkStat(main_network_country[[1]])
Centrality_betweenness <- networkStat(main_network_country[[1]], type = "betweenness")$network$networkCentrbetweenness
Centrality_closeness <- networkStat(main_network_country[[1]], type = "closeness")$network$networkCentrCloseness

Entire_network_stats <- 
  data.frame(Size = entire_network_stats$network$networkSize,
             Density = entire_network_stats$network$networkDensity,
             Transitivity = entire_network_stats$network$networkTransitivity,
             Diameter = entire_network_stats$network$networkDiameter,
             Average_path_length = entire_network_stats$network$NetworkAverPathLeng,
             Centrality_Degree = entire_network_stats$network$networkCentrDegree,
             Centrality_Betweenness = Centrality_betweenness,
             Centrality_Closenness = Centrality_closeness) %>% as_tibble() %>% 
  pivot_longer(everything(), names_to = "Metric", values_to = "Value")

write.xlsx(Entire_network_stats, paste(directories$dir_country_net , "Country_main_Network_statistics.xlsx", sep = "/"), overwrite = T)

# Degree

z_raw_degree <- degree(main_network_country[[1]], mode = "total")
z_raw_degree <- data.frame(country = names(z_raw_degree), degree = as.numeric(z_raw_degree)) %>% as_tibble()

z_normalized_degree <- degree(main_network_country[[1]], mode = "total", normalized = TRUE)
z_normalized_degree <- data.frame(country = names(z_normalized_degree), degree = as.numeric(z_normalized_degree)) %>% as_tibble();z_normalized_degree

top_20_degree <- z_normalized_degree %>% slice_max(degree, n = 20, with_ties = F)

# Betweenness

z_raw_betweenness <- betweenness(main_network_country[[1]], directed = F)
z_raw_betweenness <- data.frame(country = names(z_raw_betweenness), degree = as.numeric(z_raw_betweenness)) %>% as_tibble()

z_normalized_betweenness <- betweenness(main_network_country[[1]], directed = F, normalized = T)
z_normalized_betweenness <- data.frame(country = names(z_normalized_betweenness), degree = as.numeric(z_normalized_betweenness)) %>% 
  as_tibble() %>% rename(betweenness = "degree");z_normalized_betweenness %>% arrange(desc(betweenness))

top_20_betweenness <- z_normalized_betweenness %>% slice_max(betweenness, n = 20, with_ties = F) 


# Closeness

z_raw_closeness <- closeness(main_network_country[[1]])
z_raw_closeness <- data.frame(country = names(z_raw_closeness), degree = as.numeric(z_raw_closeness)) %>% as_tibble()

z_normalized_closeness <- closeness(main_network_country[[1]], normalized = T)
z_normalized_closeness <- data.frame(country = names(z_normalized_closeness), degree = as.numeric(z_normalized_closeness)) %>% 
  as_tibble() %>% rename(closeness = "degree");z_normalized_closeness

top_20_closeness <- z_normalized_closeness %>% slice_max(closeness, n = 20, with_ties = F) 


node_stats_per_country <- 
  full_join(z_normalized_degree, z_normalized_betweenness, by = "country") %>% 
  full_join(z_normalized_closeness, by = "country") %>% 
  mutate(degree_norm = (degree - min(degree)) / (max(degree) - min(degree)),
         between_norm = (betweenness - min(betweenness)) / (max(betweenness) - min(betweenness)),
         closeness_norm  = (closeness - min(closeness)) / (max(closeness) - min(closeness))) %>% 
  mutate(indicator_dominance = (degree_norm + between_norm + closeness_norm) / 3,
         country = str_to_title(country))

cbind(top_20_degree, top_20_betweenness, top_20_closeness) %>%
  write.xlsx(paste(directories$dir_country_net, "Node_centrality_stats_top20.xlsx", sep = "/"), overwrite = T)

###### Checking top 30 collabs ######

top30_country_collabs <-
  country_network %>% 
  as_tibble() %>% 
  separate_rows(AU_CO, sep = ";") %>% 
  pairwise_count(AU_CO, article_id, sort = T, upper = FALSE) %>% 
  slice_max(n, n = 30) 

write.xlsx(top30_country_collabs, paste(directories$dir_country_net, "tpo30_country_collabs.xlsx", sep = "/"), overwrite = T)


print("Country networks: done")

##### Affiliations #####

affil_ID_names_countries <-
  API_info$affil_info %>% 
  separate_rows(affiliation_id, affiliation_name, affiliation_country, sep = ";") %>% 
  distinct(affiliation_id, affiliation_name, affiliation_country)


affil_byacademic_articles <- 
  affil_table_clean_V2 %>% 
  filter(str_detect(affiliation_name, "")) %>% 
  group_by(Academic) %>% 
  count(affiliation_id, affiliation_name, sort = T, name = "metric") %>% 
  slice_max(metric, n = 20) %>% 
  mutate(affiliation_name = fct_reorder(affiliation_name, metric)) %>% 
  ungroup() %>% 
  mutate(type = "By number of articles")

affil_byacademic_citations <- 
  affil_table_clean_V2 %>% 
  filter(str_detect(affiliation_name, ""),
         !is.na(affiliation_id)) %>% 
  group_by(Academic, affiliation_id, affiliation_name) %>% 
  summarise(metric = sum(TC)) %>%
  group_by(Academic) %>% 
  slice_max(metric, n = 20) %>% 
  mutate(affiliation_name = fct_reorder(affiliation_name, metric)) %>% 
  ungroup() %>% 
  mutate(type = "By total citations")

academic_info_artic_cit <-
  bind_rows(affil_byacademic_articles, affil_byacademic_citations) %>% 
  filter(Academic == "Academic")

non_academic_info_artic_cit <-
  bind_rows(affil_byacademic_articles, affil_byacademic_citations) %>% 
  filter(Academic == "Non Academic")

###### non-academic stats ######

affil_table_clean_V2 %>% 
  filter(Academic == "Non Academic") %>% 
  group_by(PY) %>% 
  summarise(non_academic = n_distinct(article_id)) %>% 
  right_join(papers_countries_per_year, by = "PY") %>% 
  arrange(PY) %>% 
  replace_na(list(non_academic = 0)) %>% 
  select(-countries_year) %>% 
  mutate(pct = non_academic / papers_year,
         cumulative_non_academic = cumsum(non_academic),
         cumulative_papers = cumsum(papers_year),
         pct_total_articles_non_academic = cumulative_non_academic / cumulative_papers) %>% 
  write.xlsx(paste(directories$dir_affiliations, "non_academic_descriptive_stats.xlsx", sep = "/"), overwrite = T)


# Exports last attempt of Academic and Non Academic differentiation, and quantifications of articles and citations by these affiliations.

write.xlsx(affil_table_clean_V2 %>% filter(Academic == "Non Academic"),
           paste(directories$dir_affiliations, "NonAcademic_Affiliations.xlsx", sep = "/"), overwrite = T)
write.xlsx(affil_table_clean_V2 %>% filter(Academic == "Academic"),
           paste(directories$dir_affiliations, "Academic_Affiliations.xlsx", sep = "/"), overwrite = T)

write.xlsx(affil_byacademic_citations, paste(directories$dir_affiliations, "Citations_Affiliations.xlsx", sep = "/"), overwrite = T)
write.xlsx(affil_byacademic_articles, paste(directories$dir_affiliations, "Articles_Affiliations.xlsx", sep = "/"), overwrite = T)


####### Plots NON-Academic. ######

affil_table_clean_V2 %>% 
  filter(Academic == "Non Academic",
         str_detect(affiliation_name, "")) %>% 
  count(affiliation_id, affiliation_name, sort = T) %>% 
  slice_max(n, n = 20) %>% 
  mutate(affiliation_name = fct_reorder(affiliation_name, n)) %>% 
  ggplot(aes(x = affiliation_name, y = n)) + 
  geom_col() +
  coord_flip() +
  labs(title = "Productive non-academic Affiliations",
       subtitle = "Non academic = Name without Univ, School, Instit, Coll")

###### Plots Academic and non-Academic, facet wrap. ######

fill_academic_non_academic_top20_art <- 
  affil_byacademic_articles %>% 
  ggplot(aes(x = affiliation_name, y = metric, fill = Academic)) + 
  geom_col() +
  coord_flip() +
  facet_wrap(~ Academic, scales = "free") +
  labs(title = "Number of articles by productive affiliations (Top 20)",
       y = "",
       x = "") +
  geom_text(aes(label = metric, hjust = 1), size = 6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text = element_text(size = 15),
        axis.text.y = element_text(size = 14), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.title = element_text(size = 15), legend.position = "none")


ggsave("fill_academic_non_academic_top20_art.pdf", plot = fill_academic_non_academic_top20_art, device = "pdf", path = directories$dir_affiliations, units = "in",
       width = 15, height = 6)

ggsave("fill_academic_non_academic_top20_art.jpeg", plot = fill_academic_non_academic_top20_art, device = "jpeg", path = directories$dir_affiliations, units = "in",
       width = 15, height = 6)


###### Plots top 20 affiliations, filled by academic. ######

affil_table_clean_V2 %>% 
  filter(str_detect(affiliation_name, "")) %>% 
  add_count(affiliation_name, name = "Articles_affiliation") %>% 
  distinct(affiliation_name, Articles_affiliation, .keep_all = T) %>% 
  slice_max(Articles_affiliation, n = 20) %>% 
  mutate(affiliation_name = fct_reorder(affiliation_name, Articles_affiliation)) %>% 
  ggplot(aes(x = affiliation_name, y = Articles_affiliation, fill = Academic)) + 
  geom_col() +
  coord_flip() +
  labs(title = "Productive Affiliations",
       y = "",
       x = "")

###### Plots information about number of articles and citation counts for academic ######

fill_academic_top20_art_cit<- 
  academic_info_artic_cit %>% 
  mutate(affiliation_name = reorder_within(affiliation_name, metric, within = type)) %>% 
  ggplot(aes(x = affiliation_name, y = metric, fill = type)) + 
  geom_col() +
  coord_flip() +
  facet_wrap(~ type, scales = "free") +
  labs(title = "Productive academic affiliations (Top 20)",
       y = "",
       x = "") +
  geom_text(aes(label = metric, hjust = 1), size = 6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text = element_text(size = 15),
        axis.text.y = element_text(size = 14), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.title = element_text(size = 15), legend.position = "none") +
  scale_x_reordered()


ggsave("fill_academic_top20_art_cit.pdf", plot = fill_academic_top20_art_cit, device = "pdf", path = directories$dir_affiliations, units = "in",
       width = 15, height = 6)

ggsave("fill_academic_top20_art_cit.jpeg", plot = fill_academic_top20_art_cit, device = "jpeg", path = directories$dir_affiliations, units = "in",
       width = 15, height = 6)

###### Plots information about number of articles and citation counts for non academic ######

fill_non_academic_top20_art_cit <- 
  non_academic_info_artic_cit %>% 
  mutate(affiliation_name = reorder_within(affiliation_name, metric, within = type)) %>% 
  ggplot(aes(x = affiliation_name, y = metric, fill = type)) + 
  geom_col() +
  coord_flip() +
  facet_wrap(~ type, scales = "free") +
  labs(title = "Productive non-academic affiliations (Top 20)",
       y = "",
       x = "") +
  geom_text(aes(label = metric, hjust = 0.75), size = 5.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text = element_text(size = 15),
        axis.text.y = element_text(size = 14), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.title = element_text(size = 15), legend.position = "none") +
  scale_x_reordered()


ggsave("fill_non_academic_top20_art_cit.pdf", plot = fill_non_academic_top20_art_cit, device = "pdf", path = directories$dir_affiliations, units = "in",
       width = 17, height = 6)

ggsave("fill_non_academic_top20_art_cit.jpeg", plot = fill_non_academic_top20_art_cit, device = "jpeg", path = directories$dir_affiliations, units = "in",
       width = 17, height = 6)

print("Affiliation analysis: done")

##### Affiliation Network Statistics) #####

#  Min degree = 5

affil_net <-
  affil_table_clean_V2 %>% 
  select(article_id, affiliation_name) %>% 
  group_by(article_id) %>% 
  summarise(AU_UN = paste(affiliation_name, collapse = ";")) %>%
  mutate(AU_UN = str_replace_all(AU_UN, '\\"', "")) %>% 
  as.data.frame() %>% 
  biblioNetwork(analysis = "collaboration", network = "universities", sep = ";")

# We export the network for a folder in the output (C:\Users\luis\OneDrive\5G\Output\Affiliation Collaboration), to visualize it in VOSVIEWER.

net_affil_plot_5 <- networkPlot(affil_net,labelsize=1, weighted = T, degree = 5, remove.isolates = T, verbose = F) 
#net2VOSviewer(net_affil_plot_5, vos.path = directories$dir_affil_net)

# Getting all component info.


affil_network_components <- components(net_affil_plot_5$graph)
membership <- data.frame(affiliation = names(affil_network_components$membership),
                         member = as.numeric(affil_network_components$membership)) %>% 
  as_tibble()
sub_network_size <- affil_network_components$csize
number_subnetworks <- affil_network_components$no 

# Filtering out sub networks.

main_network_affil <- decompose(net_affil_plot_5$graph, min.vertices = 10)

entire_network_stats <- networkStat(main_network_affil[[1]])
Centrality_betweenness <- networkStat(main_network_affil[[1]], type = "betweenness")$network$networkCentrbetweenness
Centrality_closeness <- networkStat(main_network_affil[[1]], type = "closeness")$network$networkCentrCloseness

Entire_network_stats <- 
  data.frame(Size = entire_network_stats$network$networkSize,
             Density = entire_network_stats$network$networkDensity,
             Transitivity = entire_network_stats$network$networkTransitivity,
             Diameter = entire_network_stats$network$networkDiameter,
             Average_path_length = entire_network_stats$network$NetworkAverPathLeng,
             Centrality_Degree = entire_network_stats$network$networkCentrDegree,
             Centrality_Betweenness = Centrality_betweenness,
             Centrality_Closenness = Centrality_closeness) %>% as_tibble() %>% 
  pivot_longer(everything(), names_to = "Metric", values_to = "Value")

write.xlsx(Entire_network_stats, paste(directories$dir_affil_net,
                                       "Affiliation_main_Network_statistics_mindeg_5.xlsx", sep = "/"), overwrite = T)

# Degree

z_raw_degree <- degree(main_network_affil[[1]], mode = "total")
z_raw_degree <- data.frame(affilaition = names(z_raw_degree), degree = as.numeric(z_raw_degree)) %>% as_tibble()

z_normalized_degree <- degree(main_network_affil[[1]], mode = "total", normalized = TRUE)
z_normalized_degree <- data.frame(affilaition = names(z_normalized_degree), degree = as.numeric(z_normalized_degree)) %>% as_tibble();z_normalized_degree

top_20_degree <- z_normalized_degree %>% slice_max(degree, n = 20, with_ties = FALSE)

# Betweenness

z_raw_betweenness <- betweenness(main_network_affil[[1]], directed = F)
z_raw_betweenness <- data.frame(affilaition = names(z_raw_betweenness), degree = as.numeric(z_raw_betweenness)) %>% as_tibble()

z_normalized_betweenness <- betweenness(main_network_affil[[1]], directed = F, normalized = T)
z_normalized_betweenness <- data.frame(affilaition = names(z_normalized_betweenness), degree = as.numeric(z_normalized_betweenness)) %>% 
  as_tibble() %>% rename(betweenness = "degree");z_normalized_betweenness

top_20_betweenness <- z_normalized_betweenness %>% slice_max(betweenness, n = 20, with_ties = FALSE)


# Closeness

z_raw_closeness <- closeness(main_network_affil[[1]])
z_raw_closeness <- data.frame(affilaition = names(z_raw_closeness), degree = as.numeric(z_raw_closeness)) %>% as_tibble()

z_normalized_closeness <- closeness(main_network_affil[[1]])
z_normalized_closeness <- data.frame(affilaition = names(z_normalized_closeness), degree = as.numeric(z_normalized_closeness)) %>% 
  as_tibble() %>% rename(closeness = "degree");z_normalized_closeness

top_20_closeness <- z_normalized_closeness %>% slice_max(closeness, n = 20, with_ties = FALSE)

node_stats_per_affil <- 
  full_join(z_normalized_degree, z_normalized_betweenness, by = "affilaition") %>% 
  full_join(z_normalized_closeness, by = "affilaition") %>% 
  mutate(degree_norm = (degree - min(degree)) / (max(degree) - min(degree)),
         between_norm = (betweenness - min(betweenness)) / (max(betweenness) - min(betweenness)),
         closeness_norm  = (closeness - min(closeness)) / (max(closeness) - min(closeness))) %>% 
  mutate(indicator_dominance = (degree_norm + between_norm + closeness_norm) / 3,
         affilaition = str_to_title(affilaition))

cbind(top_20_degree, top_20_betweenness, top_20_closeness) %>%
  write.xlsx(paste(directories$dir_affil_net,
                   "Node_centrality_stats_affiliations_top20_mindeg_5.xlsx", sep = "/"), overwrite = T)

print("Affiliation network: done")

##### Most cited Papers #####

most_cited_papers <- 
  M_clean %>% 
  as_tibble() %>% 
  arrange(desc(TC)) %>% 
  head(20) %>% 
  select(TI, TC) %>% 
  mutate(TI = str_wrap(TI, 70),
         TI = fct_reorder(TI, TC)) %>% 
  ggplot(aes(x = TI, y = TC)) + 
  geom_col(fill = "grey") +
  coord_flip() +
  labs(title = NULL,
       x = "",
       y = "") +
  geom_text(aes(label = TC, hjust = 0.8), colour = "black", size = 15) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 32), axis.text.x = element_blank(), axis.ticks.x = element_blank())


ggsave("most_cited_papers.pdf", plot = most_cited_papers, device = "pdf", path = directories$dir_most_cited_papers, units = "in",
       width = 30, height = 20)

ggsave("most_cited_papers.jpeg", plot = most_cited_papers, device = "jpeg", path = directories$dir_most_cited_papers, units = "in",
       width = 30, height = 20)

# Exports titles, citation count, year, keywords, AU_CO.

most_cited_articles_info <- 
  M_clean %>% 
  as_tibble() %>% 
  arrange(desc(TC)) %>%
  head(20) %>% 
  select(article_id, TI, PY, TC, ID, DE, AU1_CO, DI) %>%
  unite("keywords", c(DE, ID), remove = F, na.rm = T)

most_cited_articles_info <- 
  left_join(most_cited_articles_info,
            API_info$author_info %>%
              select(-DI) %>% 
              mutate(article_id = as.numeric(article_id)),
            by = "article_id") %>% 
  left_join(affil_table_clean_V2 %>% 
              select(-DI) %>%
              distinct(affiliation_id, affiliation_name, affiliation_country, Academic) %>% 
              filter(!is.na(affiliation_id)),
            by = c("affiliation_id"))

most_cited_articles_info_noKW <- 
  most_cited_articles_info %>% 
  group_by(TI, DI, TC, PY) %>% 
  summarize(Authors = paste(unique(Index_name), collapse = "; "),
            Number_authors = n_distinct(Index_name),
            AU1_Country = unique(AU1_CO),
            Countries = paste(unique(affiliation_country), collapse = "; "),
            Unique_countries = n_distinct(affiliation_country),
            Affiliations = paste(unique(affiliation_name), collapse = "; "),
            Unique_affiliations = n_distinct(affiliation_id),
            Affiliation_type = paste(unique(Academic), collapse = "; ")) %>% 
  arrange(desc(TC)) %>% 
  mutate(TI = str_to_title(TI),
         AU1_Country = str_to_title(AU1_Country))

write.xlsx(most_cited_articles_info_noKW, paste(directories$dir_most_cited_papers,
                                                "Most cited papers_info_noKW.xlsx", sep = "/"), overwrite = T)

print("Most cited Papers: done")


##### Subject Categories #####

# Still need to find a way to efficiently show different categories.

Aggregated_categories <- 
  M_clean %>% 
  as_tibble() %>% 
  select(article_id, SO, TI, SC, PY, TC, AU1_CO, AU_CO, AB, ID, DE) %>% 
  mutate(total_papers = length(article_id)) %>% 
  separate_rows(SC, sep = ";") %>% 
  mutate(SC = trimws(SC, which = "both")) %>% 
  left_join(Aggreagted_SC, by = "SC") %>% 
  mutate(Category_group = str_to_upper(Category_group)) %>% 
  filter(!is.na(Category_group))

aggregated_SC_ts <- 
  Aggregated_categories %>% 
  distinct(article_id, Category_group,.keep_all = T) %>% 
  #filter(PY >= 2014) %>% 
  group_by(PY) %>% 
  count(Category_group, sort = T) %>% 
  ungroup() %>% 
  complete(PY, Category_group, fill = list(n = 0)) %>% 
  mutate(Category_group = fct_reorder(Category_group, -n, sum)) %>% 
  ggplot(aes(x = PY, y = n, group = Category_group, color = Category_group)) +
  geom_line(lwd = 1) + 
  facet_wrap(~ Category_group, scales = "free_y") +
  scale_y_continuous(breaks= pretty_breaks()) +
  theme(panel.grid.major = element_blank(), strip.text = element_text(size = 15),
        axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14),
        legend.title = element_text(size = 15), legend.text = element_text(size = 14),
        legend.position = "none", title = element_text(size = 17)) +
  labs(title = "Evolution of disciplinary areas per year",
       subtitle = "Absolute number of articles",
       x = "",
       y = "")

ggsave("aggregated_SC_categories_ts.pdf", plot = aggregated_SC_ts, device = "pdf", path = directories$dir_aggregated_SC,
       units = "in",
       width = 20, height = 10)

ggsave("aggregated_SC_categories_ts.jpeg", plot = aggregated_SC_ts, device = "pdf", path = directories$dir_aggregated_SC,
       units = "in",
       width = 20, height = 10)


Aggregated_categories %>% 
  mutate(Citations_year = TC / (2021 - PY)) %>% 
  group_by(Category_group) %>% 
  summarise(Articles = n(),
            Percentage = n() / total_papers,
            Average_citations_year = mean(Citations_year)) %>% distinct(Category_group, .keep_all = T)

summary_table_categories <- 
  
  left_join(
    Aggregated_categories %>%
      distinct(article_id, Category_group,.keep_all = T) %>% 
      #filter(PY >= 2014) %>% 
      mutate(Citations_year = TC / (2021 - PY)) %>% 
      group_by(Category_group) %>% 
      summarise(Articles = n(),
                Percentage = n() / unique(total_papers),
                Average_citation = mean(TC),
                Average_citation_year = mean(Citations_year),
                total_citations = sum(TC)
      ),
    Aggregated_categories %>% 
      distinct(article_id, Category_group,.keep_all = T) %>% 
      #filter(PY >= 2014) %>%
      group_by(PY) %>% 
      count(Category_group) %>%
      arrange(Category_group, PY) %>% 
      group_by(Category_group) %>% 
      mutate(n_less_1 = lag(n, 1),
             growth = (n - n_less_1) / n_less_1) %>% 
      summarise(Average_growth_YoY = mean(growth, na.rm = T)),
    
    by = "Category_group") %>% 
  arrange(desc(Articles))

write.xlsx(summary_table_categories, paste(directories$dir_aggregated_SC,
                                           "Indicators_aggregated_SC_categories.xlsx", sep = "/"), overwrite = T)

print("Subject categories: done")

##### Sources ######

top_20_journals <-
  M_clean %>% 
  as_tibble() %>% 
  select(PY, SO) %>% 
  count(SO, sort = T) %>% 
  slice_max(n, n = 20) %>% 
  mutate(SO = fct_reorder(SO, n))

top_20_journals_chart <-
  top_20_journals %>% 
  ggplot(aes(x  = SO, y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n, hjust = 0)) +
  theme(panel.grid.major = element_blank()) +
  labs(title = "Top 20 journals: number of articles",
       y = "",
       x = "")

write.xlsx(top_20_journals, paste(directories$dir_sources,
                                           "top_20_journals.xlsx", sep = "/"), overwrite = T)

ggsave("top_20_journals.jpeg", plot = top_20_journals_chart, device = "pdf", path = directories$dir_sources,
       units = "in",
       width = 20, height = 10)

ggsave("top_20_journals.pdf", plot = top_20_journals_chart, device = "pdf", path = directories$dir_sources,
       units = "in",
       width = 20, height = 10)

print("Sources: done")

##### Most cited papers, citing most cited papers #####

# Here we want to retrieve information about papers citing the most cited papers in our collection (say top 50).

top100_cited_citing_most_cited <- 
  citing_most_cited %>% 
  mutate(Is_in_collection = case_when(DI %in% M_clean$DI ~ 1,
                                      TRUE ~ 0)) %>% 
  group_by(Original_paper) %>% 
  add_count(Original_paper, name = "Citations_Original_paper") %>% 
  arrange(desc(TC)) %>% 
  slice_max(TC, n = 100) %>% 
  select(article_id, AU, TI, DE, ID, PY, AU_CO, AU_UN, SC, Is_in_collection) %>% 
  separate_rows(AU_CO, sep = ";") %>% 
  distinct(article_id, AU_CO, .keep_all = T) %>% 
  group_by(Original_paper, article_id, AU, TI, DE, ID, PY, AU_UN, SC, Is_in_collection) %>% 
  summarise(AU_CO = paste(AU_CO, collapse = "; ")) %>% 
  ungroup() %>% 
  separate_rows(AU_UN, sep = ";") %>% 
  distinct(article_id, AU_UN, .keep_all = T) %>% 
  group_by(Original_paper, article_id, AU, TI, DE, ID, PY, AU_CO, SC, Is_in_collection) %>% 
  summarise(AU_UN = paste(AU_UN, collapse = "; ")) %>% ungroup()

# Some numerical information like if the papers citing are in our collection and the median year of publication.

numerical_info_citing_most_cited <- 
  top100_cited_citing_most_cited %>% 
  group_by(Original_paper) %>% 
  summarise(Present_in_collection = mean(Is_in_collection),
            Median_year_citation = median(PY, na.rm = T))

# Find the most common keywords used by these papers. Top 5 most common.

keywords_info_citing_most_cited <-
  top100_cited_citing_most_cited %>% 
  unite("keywords", c(ID, DE), na.rm = T, remove = F) %>% 
  group_by(Original_paper) %>% 
  separate_rows(keywords, sep = ";") %>% 
  mutate(keywords = trimws(keywords, which = "both")) %>% 
  filter(str_detect(keywords, "",),
         !str_detect(keywords, "5G")) %>% 
  add_count(keywords, name = "keyword_per_original_paper") %>%
  distinct(Original_paper, keywords, .keep_all = T) %>% 
  slice_max(keyword_per_original_paper, n = 5, with_ties = F) %>% 
  mutate(keyword_per_original_paper = paste("(", keyword_per_original_paper, ")", sep = ""),
         Common_keywords = paste(keywords, keyword_per_original_paper, sep = " ")) %>% 
  summarise(Common_keywords = paste(keywords, keyword_per_original_paper, collapse = "; "))

# Find the most common countries contributing for these papers. Top 5 most common.

Country_info_citing_most_cited <-
  top100_cited_citing_most_cited %>% 
  group_by(Original_paper) %>% 
  separate_rows(AU_CO, sep = ";") %>% 
  mutate(AU_CO = trimws(AU_CO, which = "both")) %>% 
  filter(str_detect(AU_CO, "",)) %>% 
  distinct(article_id, AU_CO, .keep_all = T) %>% 
  add_count(AU_CO, name = "Countries_per_original_paper") %>%
  distinct(Original_paper, AU_CO, .keep_all = T) %>% 
  slice_max(Countries_per_original_paper, n = 5, with_ties = F) %>% 
  mutate(Countries_per_original_paper = paste("(", Countries_per_original_paper, ")", sep = ""),
         Common_countries = paste(AU_CO, Countries_per_original_paper, sep = " ")) %>% 
  summarise(Common_countries = paste(AU_CO, Countries_per_original_paper, collapse = "; "))

# Find the most common affiliations contributing for these papers. Top 5 most common.

Affil_info_citing_most_cited <-
  top100_cited_citing_most_cited %>% 
  group_by(Original_paper) %>% 
  separate_rows(AU_UN, sep = ";") %>% 
  mutate(AU_UN = trimws(AU_UN, which = "both")) %>% 
  filter(str_detect(AU_UN, "",)) %>% 
  distinct(article_id, AU_UN, .keep_all = T) %>% 
  add_count(AU_UN, name = "Affiliation_per_original_paper") %>%
  distinct(Original_paper, AU_UN, .keep_all = T) %>% 
  slice_max(Affiliation_per_original_paper, n = 5, with_ties = F) %>% 
  mutate(Affiliation_per_original_paper = paste("(", Affiliation_per_original_paper, ")", sep = ""),
         Common_affiliation = paste(AU_UN, Affiliation_per_original_paper, sep = " ")) %>% 
  summarise(Common_affiliation = paste(AU_UN, Affiliation_per_original_paper, collapse = "; "))

# Find the most common categories of journals in which these papers are published. Top 5 most common.

Category_info_citing_most_cited <-
  top100_cited_citing_most_cited %>% 
  group_by(Original_paper) %>% 
  separate_rows(SC, sep = ";") %>% 
  mutate(SC = trimws(SC, which = "both")) %>% 
  filter(str_detect(SC, "",)) %>% 
  distinct(article_id, SC, .keep_all = T) %>% 
  add_count(SC, name = "Category_per_original_paper") %>%
  distinct(Original_paper, SC, .keep_all = T) %>% 
  slice_max(Category_per_original_paper, n = 5, with_ties = F) %>% 
  mutate(Category_per_original_paper = paste("(", Category_per_original_paper, ")", sep = ""),
         Common_category = paste(SC, Category_per_original_paper, sep = " ")) %>% 
  summarise(Common_category = paste(SC, Category_per_original_paper, collapse = "; "))

# Joins everything and exports.

inner_join(numerical_info_citing_most_cited, keywords_info_citing_most_cited, by = "Original_paper") %>% 
  inner_join(Country_info_citing_most_cited, by = "Original_paper") %>% 
  inner_join(Affil_info_citing_most_cited, by = "Original_paper") %>% 
  inner_join(Category_info_citing_most_cited, by = "Original_paper") %>% 
  write.xlsx(paste(directories$dir_most_cited_citing_most_cited, "Most_cited_citing_key_features.xlsx", sep = "/"))

print("Most cited citing most cited: done")

##### Keywords #####

# There is some cleaning to be done when analysing keywords.

# Since there are keywords missing (either in ID or in DE, we should merge both in a new keyword column).
# We should also remove 5G containing keywords (but inspect them to know what are we removing). Like: 5G, FIFTH GENERATION
# Joining specific terms into one like NOMA = NON ORTHOGONAL MULTIPLE ACCESS = ETC, IOT = INTERNET OF THINGS,


###### GENERAL - Highest median ranked keywords across years ######

highest_median_ranked_kw <- 
  keywords_clean %>%
  group_by(PY) %>% 
  count(PY, keywords_new, sort = T) %>%
  #filter(n > 10) %>% 
  mutate(word_rank = order(n, decreasing = T)) %>% 
  group_by(keywords_new) %>% 
  add_count(keywords_new, wt = n, name = "total_count") %>% 
  filter(total_count > 10) %>% 
  summarise(median_rank = median(word_rank, na.rm = T)) %>%
  ungroup() %>% 
  arrange(median_rank) %>% 
  slice_max(- median_rank, n = 30) %>% 
  mutate(rank = order(median_rank, decreasing = F))

write.xlsx(highest_median_ranked_kw, paste(directories$dir_keywords, "Highest_average_ranked_keywords_allyears.xlsx", sep = "/"), overwrite = T)

print("Keywords general: done")

###### UNIGRAMS - Highest median ranked keywords across years ######

highest_uni_median_ranked_kw <- 
  keywords_clean %>%
  mutate(keywords_new = stringi::stri_trans_general(keywords_new, "ASCII"),
         length_kw = str_count(keywords_new, " "))  %>%
  filter(length_kw == 0) %>% 
  group_by(PY) %>% 
  count(PY, keywords_new, sort = T) %>%
  ungroup() %>% 
  complete(PY, keywords_new, fill = list(n = 0)) %>% 
  group_by(PY) %>% 
  mutate(word_rank = rank(-n, ties.method = "min"), 
         word_rank_norm = (word_rank - min(word_rank)) / (max(word_rank) - min(word_rank)) ) %>%
  group_by(keywords_new) %>% 
  summarise(median_rank = median(word_rank_norm, na.rm = T)) %>%
  ungroup() %>% 
  arrange(median_rank) %>% 
  slice_min(median_rank, n = 30) %>% 
  mutate(rank = order(median_rank, decreasing = F))

write.xlsx(highest_uni_median_ranked_kw, paste(directories$dir_keywords, "UNI_Highest_average_ranked_keywords_allyears.xlsx", sep = "/"), overwrite = T)

# Plots ranking evolution of highest_average_ranked_kw UNI (v0.5)

keywords_clean_uni_median_evo <- 
  keywords_clean %>%
  mutate(keywords_new = stringi::stri_trans_general(keywords_new, "ASCII"),
         length_kw = str_count(keywords_new, " "))  %>%
  filter(length_kw == 0) %>%
  group_by(PY) %>% 
  count(PY, keywords_new, sort = T) %>%
  ungroup() %>% 
  complete(PY, keywords_new, fill = list(n = 0)) %>% 
  group_by(PY) %>% 
  mutate(word_rank = rank(-n, ties.method = "min"),
         word_rank_norm = (word_rank - min(word_rank)) / (max(word_rank) - min(word_rank)) ) %>%
  group_by(keywords_new, PY) %>% 
  filter(keywords_new %in% highest_uni_median_ranked_kw$keywords_new) %>% 
  ungroup()  %>% 
  mutate(keywords_new = fct_reorder(keywords_new, -word_rank_norm, median)) %>% 
  filter(n != 0) %>% 
  ggplot(aes(x = as.factor(as.character(PY)), y = keywords_new, fill = word_rank)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = word_rank), color = "white", size = 6) +
  scale_fill_gradient(trans = "log", breaks = c(1,5,30,160), labels = c(1,5,30,160),
                      high = "#56B1F7" , low = "#132B43") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text = element_text(size = 15),
        axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14),
        legend.title = element_text(size = 15), legend.text = element_text(size = 14)) + 
  labs(title = "Ranking evolution of highest median ranked unigram keywords (Top 30, with ties)",
       x = "",
       y = "",
       fill = "Ranking of keyword\n(by # articles using)")


ggsave("keywords_clean_uni_median_evo.pdf", plot = keywords_clean_uni_median_evo, device = "pdf", path = directories$dir_keywords, units = "in",
       width = 20, height = 10)

ggsave("keywords_clean_uni_median_evo.jpeg", plot = keywords_clean_uni_median_evo, device = "jpeg", path = directories$dir_keywords, units = "in",
       width = 20, height = 10)

###### Evolution of top 30 keywords with highest median YoY growth UNI ######

#  Plots the evolution of top 30 keywords with highest YoY average growth UNI

growth_kw_uni <- 
  keywords_clean %>%  
  count(PY, keywords_new) %>% 
  mutate(keywords_new = stringi::stri_trans_general(keywords_new, "ASCII"),
         length_kw = str_count(keywords_new, " "))  %>%
  filter(length_kw == 0) %>% 
  arrange(keywords_new, PY) %>% 
  add_count(keywords_new, wt = n, name = "word_total") %>% 
  filter(word_total > 50) %>%
  complete(PY, keywords_new, fill = list(n = 0)) %>% 
  group_by(keywords_new) %>% 
  fill(word_total, .direction = "downup") %>% 
  group_by(keywords_new) %>% 
  mutate(cumsum_words = cumsum(n)) %>% 
  filter(cumsum_words >= 1) %>% 
  mutate(yoy_growth = (cumsum_words - lag(cumsum_words, 1)) / lag(cumsum_words, 1),
         median_growth = median(yoy_growth, na.rm = T)) %>% 
  ungroup() %>% 
  filter(n != 0) %>% 
  filter(dense_rank(desc(median_growth)) %in% 1:30)  %>% 
  mutate(keywords_new = fct_reorder(keywords_new, median_growth)) %>% 
  arrange(desc(median_growth), desc(PY))

write.xlsx(growth_kw_uni, paste(directories$dir_keywords, "growth_kw_uni.xlsx", sep = "/"), overwrite = T)

growth_kw_uni_evo <-
  growth_kw_uni %>% 
  ggplot(aes(x = as.factor(as.character(PY)), y = keywords_new, fill = cumsum_words)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = cumsum_words), colour = "white", size = 5) +
  scale_fill_gradient(trans = "log", breaks = c(1,6,40,250), labels = c(1,6,40,250),
                      high = "#132B43", low = "#56B1F7") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text = element_text(size = 15),
        axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14),
        legend.title = element_text(size = 15), legend.text = element_text(size = 14)) +
  labs(title = "Unigram keyword YoY growth evolution",
       subtitle = "Top 30 ranked (with ties) keywords with highest median growth YoY (descending)",
       y = "",
       x = "",
       fill = "Cumulative articles using")


ggsave("growth_kw_uni_evo.pdf", plot = growth_kw_uni_evo, device = "pdf", path = directories$dir_keywords, units = "in",
       width = 20, height = 10)

ggsave("growth_kw_uni_evo.jpeg", plot = growth_kw_uni_evo, device = "jpeg", path = directories$dir_keywords, units = "in",
       width = 20, height = 10)

print("Keywords unigrams: done")

###### BIGRAMS - Highest median ranked keywords across years ######

highest_bi_median_ranked_kw <- 
  keywords_clean %>%
  mutate(keywords_new = stringi::stri_trans_general(keywords_new, "ASCII"),
         length_kw = str_count(keywords_new, " "))  %>%
  filter(length_kw == 1) %>% 
  group_by(PY) %>% 
  count(PY, keywords_new, sort = T) %>%
  ungroup() %>% 
  complete(PY, keywords_new, fill = list(n = 0)) %>% 
  group_by(PY) %>% 
  mutate(word_rank = rank(-n, ties.method = "min"), 
         word_rank_norm = (word_rank - min(word_rank)) / (max(word_rank) - min(word_rank)) ) %>%
  group_by(keywords_new) %>% 
  summarise(median_rank = median(word_rank_norm, na.rm = T)) %>%
  ungroup() %>% 
  arrange(median_rank) %>% 
  slice_min(median_rank, n = 30) %>% 
  mutate(rank = order(median_rank, decreasing = F))

write.xlsx(highest_bi_median_ranked_kw, paste(directories$dir_keywords, "BI_Highest_average_ranked_keywords_allyears.xlsx", sep = "/"), overwrite = T)

# Plots ranking evolution of highest_average_ranked_kw UNI (v0.5)

keywords_clean_bi_median_evo <- 
  keywords_clean %>%
  mutate(keywords_new = stringi::stri_trans_general(keywords_new, "ASCII"),
         length_kw = str_count(keywords_new, " "))  %>%
  filter(length_kw == 1) %>%
  group_by(PY) %>% 
  count(PY, keywords_new, sort = T) %>%
  ungroup() %>% 
  complete(PY, keywords_new, fill = list(n = 0)) %>% 
  group_by(PY) %>% 
  mutate(word_rank = rank(-n, ties.method = "min"),
         word_rank_norm = (word_rank - min(word_rank)) / (max(word_rank) - min(word_rank)) ) %>%
  group_by(keywords_new, PY) %>% 
  filter(keywords_new %in% highest_bi_median_ranked_kw$keywords_new) %>% 
  ungroup()  %>% 
  mutate(keywords_new = fct_reorder(keywords_new, -word_rank_norm, median)) %>% 
  filter(n != 0) %>% 
  ggplot(aes(x = as.factor(as.character(PY)), y = keywords_new, fill = word_rank)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = word_rank), color = "white", size = 6) +
  scale_fill_gradient(trans = "log", breaks = c(1,5,30,160), labels = c(1,5,30,160),
                      high = "#56B1F7" , low = "#132B43") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text = element_text(size = 15),
        axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14),
        legend.title = element_text(size = 15)) + 
  labs(title = "Ranking evolution of highest median ranked bigram keywords (Top 30, with ties)",
       x = "",
       y = "",
       fill = "Ranking of keyword\n(by # articles using)")


ggsave("keywords_clean_bi_median_evo.pdf", plot = keywords_clean_bi_median_evo, device = "pdf", path = directories$dir_keywords, units = "in",
       width = 20, height = 10)

ggsave("keywords_clean_bi_median_evo.jpeg", plot = keywords_clean_bi_median_evo, device = "jpeg", path = directories$dir_keywords, units = "in",
       width = 20, height = 10)

###### Evolution of top 30 keywords with highest median YoY growth BI ######

#  Plots the evolution of top 30 keywords with highest YoY average growth BI

growth_kw_bi <-
  keywords_clean %>%  
  count(PY, keywords_new) %>% 
  mutate(keywords_new = stringi::stri_trans_general(keywords_new, "ASCII"),
         length_kw = str_count(keywords_new, " "))  %>%
  filter(length_kw == 1) %>% 
  arrange(keywords_new, PY) %>% 
  add_count(keywords_new, wt = n, name = "word_total") %>% 
  filter(word_total > 30) %>%
  complete(PY, keywords_new, fill = list(n = 0)) %>% 
  group_by(keywords_new) %>% 
  fill(word_total, .direction = "downup") %>% 
  group_by(keywords_new) %>% 
  mutate(cumsum_words = cumsum(n)) %>% 
  filter(cumsum_words >= 1) %>% 
  mutate(yoy_growth = (cumsum_words - lag(cumsum_words, 1)) / lag(cumsum_words, 1),
         median_growth = median(yoy_growth, na.rm = T)) %>% 
  ungroup() %>% 
  filter(n != 0) %>% 
  filter(dense_rank(desc(median_growth)) %in% 1:30)  %>% 
  mutate(keywords_new = fct_reorder(keywords_new, median_growth)) %>% 
  arrange(desc(median_growth), desc(PY))

write.xlsx(growth_kw_bi, paste(directories$dir_keywords, "growth_kw_bi.xlsx", sep = "/"), overwrite = T)

growth_kw_bi_evo <- 
  growth_kw_bi %>% 
  ggplot(aes(x = as.factor(as.character(PY)), y = keywords_new, fill = cumsum_words)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = cumsum_words), colour = "white", size = 5.5) +
  scale_fill_gradient(trans = "log", breaks = c(1,6,40,250), labels = c(1,6,40,250),
                      high = "#132B43", low = "#56B1F7") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text = element_text(size = 15),
        axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14),
        legend.title = element_text(size = 15)) +
  labs(title = "Bigram keyword YoY growth evolution",
       subtitle = "Top 30 ranked (with ties) keywords with highest median growth YoY (descending)",
       y = "",
       x = "",
       fill = "Cumulative articles using")


ggsave("growth_kw_bi_evo.pdf", plot = growth_kw_bi_evo, device = "pdf", path = directories$dir_keywords, units = "in",
       width = 20, height = 10)

ggsave("growth_kw_bi_evo.jpeg", plot = growth_kw_bi_evo, device = "jpeg", path = directories$dir_keywords, units = "in",
       width = 20, height = 10)

print("Keywords bigrams: done")

###### > Bigrams - Highest median ranked keywords across years ######

highest_tri_median_ranked_kw <- 
  keywords_clean %>%
  mutate(keywords_new = stringi::stri_trans_general(keywords_new, "ASCII"),
         length_kw = str_count(keywords_new, " "))  %>%
  filter(length_kw >= 2) %>% 
  group_by(PY) %>% 
  count(PY, keywords_new, sort = T) %>%
  ungroup() %>% 
  complete(PY, keywords_new, fill = list(n = 0)) %>% 
  group_by(PY) %>% 
  mutate(word_rank = rank(-n, ties.method = "min"), 
         word_rank_norm = (word_rank - min(word_rank)) / (max(word_rank) - min(word_rank)) ) %>%
  group_by(keywords_new) %>% 
  summarise(median_rank = mean(word_rank_norm, na.rm = T)) %>%
  ungroup() %>% 
  arrange(median_rank) %>% 
  slice_min(median_rank, n = 30) %>% 
  mutate(rank = order(median_rank, decreasing = F))

write.xlsx(highest_tri_median_ranked_kw, paste(directories$dir_keywords, "TRI_Highest_average_ranked_keywords_allyears.xlsx", sep = "/"), overwrite = T)

# Plots ranking evolution of highest_average_ranked_kw UNI (v0.5)

keywords_clean_tri_median_evo <-
  keywords_clean %>%
  mutate(keywords_new = stringi::stri_trans_general(keywords_new, "ASCII"),
         length_kw = str_count(keywords_new, " "))  %>%
  filter(length_kw >= 2) %>%
  group_by(PY) %>% 
  count(PY, keywords_new, sort = T) %>%
  ungroup() %>% 
  complete(PY, keywords_new, fill = list(n = 0)) %>% 
  group_by(PY) %>% 
  mutate(word_rank = rank(-n, ties.method = "min"),
         word_rank_norm = (word_rank - min(word_rank)) / (max(word_rank) - min(word_rank)) ) %>%
  group_by(keywords_new, PY) %>% 
  filter(keywords_new %in% highest_tri_median_ranked_kw$keywords_new) %>% 
  ungroup()  %>% 
  mutate(keywords_new = fct_reorder(keywords_new, -word_rank_norm, median)) %>% 
  filter(n != 0) %>% 
  ggplot(aes(x = as.factor(as.character(PY)), y = keywords_new, fill = word_rank)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = word_rank), color = "white", size = 6) +
  scale_fill_gradient(trans = "log", breaks = c(1,5,30,160), labels = c(1,5,30,160),
                      high = "#56B1F7" , low = "#132B43") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text = element_text(size = 15),
        axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14),
        legend.title = element_text(size = 15)) + 
  labs(title = "Ranking evolution of highest median ranked >= Trigram keywords (Top 30, with ties)",
       x = "",
       y = "",
       fill = "Ranking of keyword\n(by # articles using)")


ggsave("keywords_clean_tri_median_evo.pdf", plot = keywords_clean_tri_median_evo, device = "pdf", path = directories$dir_keywords, units = "in",
       width = 20, height = 10)

ggsave("keywords_clean_tri_median_evo.jpeg", plot = keywords_clean_tri_median_evo, device = "jpeg", path = directories$dir_keywords, units = "in",
       width = 20, height = 10)

###### Evolution of top 30 keywords with highest median YoY growth > BI ######

#  Plots the evolution of top 30 keywords with highest YoY average growth > BI

growth_kw_tri <- 
  keywords_clean %>%  
  count(PY, keywords_new) %>% 
  mutate(keywords_new = stringi::stri_trans_general(keywords_new, "ASCII"),
         length_kw = str_count(keywords_new, " "))  %>%
  filter(length_kw >= 2) %>% 
  arrange(keywords_new, PY) %>% 
  add_count(keywords_new, wt = n, name = "word_total") %>% 
  filter(word_total > 10) %>%
  complete(PY, keywords_new, fill = list(n = 0)) %>% 
  group_by(keywords_new) %>% 
  fill(word_total, .direction = "downup") %>% 
  group_by(keywords_new) %>% 
  mutate(cumsum_words = cumsum(n)) %>% 
  filter(cumsum_words >= 1) %>% 
  mutate(yoy_growth = (cumsum_words - lag(cumsum_words, 1)) / lag(cumsum_words, 1),
         median_growth = median(yoy_growth, na.rm = T)) %>% 
  ungroup() %>% 
  filter(dense_rank(desc(median_growth)) %in% 1:30)  %>% 
  mutate(keywords_new = fct_reorder(keywords_new, median_growth)) %>% 
  filter(n != 0) %>% 
  arrange(desc(median_growth), desc(PY))

write.xlsx(growth_kw_tri, paste(directories$dir_keywords, "growth_kw_tri.xlsx", sep = "/"), overwrite = T)

growth_kw_tri_evo <- 
  growth_kw_tri %>% 
  ggplot(aes(x = as.factor(as.character(PY)), y = keywords_new, fill = cumsum_words)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = cumsum_words), colour = "white", size = 5.5) +
  scale_fill_gradient(trans = "log", breaks = c(1,6,40,250), labels = c(1,6,40,250),
                      high = "#132B43", low = "#56B1F7") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.text = element_text(size = 15),
        axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14),
        legend.title = element_text(size = 15)) +
  labs(title = ">= Trigram keyword YoY growth evolution",
       subtitle = "Top 30 ranked (with ties) keywords with highest median growth YoY (descending)",
       y = "",
       x = "",
       fill = "Cumulative articles using")


ggsave("growth_kw_tri_evo.pdf", plot = growth_kw_tri_evo, device = "pdf", path = directories$dir_keywords, units = "in",
       width = 20, height = 10)

ggsave("growth_kw_tri_evo.jpeg", plot = growth_kw_tri_evo, device = "jpeg", path = directories$dir_keywords, units = "in",
       width = 20, height = 10)

print("Keywords > bigrams: done")


