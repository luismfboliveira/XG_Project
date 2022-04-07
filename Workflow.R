##### Necessary functions #####

source("Functions.R")

##### Installs/Loads Packages #####

if (!require("pacman")) install.packages("pacman")

required_packages <- c("readxl", "bibliometrix", "tidyverse", "ggrepel", "scales",
                       "maps", "tidytext", "topicmodels", "stm", "rscopus", "openxlsx", "igraph", "countrycode",
                       "widyr", "wpp2019", "textstem", "glue", "ggthemes")

pacman::p_load(char = required_packages)

checks_loaded_packages(required_packages)

##### Setting Scopus API key #####

api_key <- "a37e8a7500807b7438eb438c1324c85a" #please insert your Scopus API key here.

checks_api_access(your_api_key = api_key)

##### Checks input files #####

data_file_check <- checks_input_data_files()

##### Sets theme for plots #####

theme_set(theme_light())

##### Creates output directories #####

directories <- creates_output_directories()

##### Load Data #####

if (data_file_check$has_initial_dataset){
  
  M_all <- read.xlsx(paste(directories$dir_input_data, "initial_dataset.xlsx", sep = "/"))
  print(glue("Initial dataset loaded"))
  
  M_clean <- 
    M_all %>% 
    as_tibble() %>%
    rowid_to_column(var ="article_id") %>% 
    filter(!is.na(PY), PY < 2021) %>% 
    metaTagExtraction(Field = "AU1_CO") %>% #1st author country
    metaTagExtraction(Field = "AU_CO", sep = ";") %>% 
    metaTagExtraction(Field = "AU_UN", sep = ";", aff.disamb = T ) %>% # Affiliation of author.
    mutate(AU1_CO = str_replace_all(AU1_CO, c("U ARAB EMIRATES" = "UNITED ARAB EMIRATES", "KOREA" = "SOUTH KOREA", "UNITED KINGDOM" = "UK")),
           AU_CO = str_replace_all(AU_CO, c("U ARAB EMIRATES" = "UNITED ARAB EMIRATES", "KOREA" = "SOUTH KOREA", "UNITED KINGDOM" = "UK")))
  
} 

if (data_file_check$has_API_data) {
  
  load(paste(directories$dir_input_data, "API_info.RData", sep = "/"))
  print(glue("priviously retrieved API data loaded"))
  
} else if (!data_file_check$has_API_data) {
  
  DI_not_na <- 
    M_clean %>% 
    as_tibble() %>% 
    select(article_id, DI) %>% 
    filter(!is.na(DI))
  
  API_info <- get_api_info(DI_not_na)
  
  save(API_info, file = paste(directories$dir_input_data, "API_info.RData", sep = "/"))
  
}

##### Pre - Process #####

###### Affiliations ######

affil_table_clean_V2 <-
  API_info$affil_info %>%
  mutate(affiliation_name = str_replace_all(affiliation_name, "&amp;", "&"),
         affiliation_name = str_replace_all(affiliation_name, c("Mongkut&x92;s" = "Mongkut's", "IRT b&lt;&gt;com" = "IRT B-Com")),
         affiliation_name = stringi::stri_trans_general(affiliation_name, "ASCII"),
         affiliation_name = str_replace_all(affiliation_name, "\\.|\\(|\\)|\\-|\\\\", " "),
         affiliation_name = trimws(affiliation_name, which = "both"),
         article_id = as.numeric(article_id)) %>% 
  filter(str_detect(affiliation_name, "")) %>%
  distinct(article_id, affiliation_id, .keep_all = T) %>%
  mutate(affiliation_name = case_when(affiliation_name == "The Electrical And Computer Engineering Department" ~ "Jacobs School Of Engineering",
                                      TRUE ~ affiliation_name),
         Academic = case_when(str_detect(str_to_upper(affiliation_name),
                                         paste("UNIV","SCHOOL","INSTI","COLL","ECO","ACADEM",
                                               "YLIOPISTO", "ESC", "POLYTEC", "POLITEC", "FACUL",
                                               "CENTRE FOR WIRELESS COMMUNICATIONS FINLAND",
                                               "KU LEUVEN", "UNSW SYDNEY", "CONSIGLIO NAZIONALE DELLE RICERCHE",
                                               "CEA LETI", "COMMONWEALTH SCIENTIFIC AND INDUSTRIAL RESEARCH ORGANIZATION",
                                               "PANEPISTIMION PATRON", "LABORATOIRE DES SIGNAUX ET SYSTEMES",
                                               "SUPELEC CAMPUS DE GIF", "PANEPISTIMIO", "ISTITUTO", "MIT", sep = "|")) ~ "Academic",
                              TRUE ~ "Non Academic")) %>% 
  left_join(M_clean %>% 
              select(article_id, TC, PY),
            by = "article_id")

write.xlsx(affil_table_clean_V2, paste(directories$dir_affiliations, "affiliations_enriched.xlsx", sep = "/"), overwrite = T)

print("Affiliations pre-processed")

###### Keywords ######

keywords_clean <- 
  M_clean %>% 
  as_tibble() %>% 
  select(article_id, DI, PY, DE, ID) %>% 
  unite("keywords_new", c(DE, ID), remove = F, na.rm = T) %>%
  select(article_id, DI, PY, keywords_new) %>%
  mutate(keywords_new = str_replace_all(keywords_new, c("INTERNET; OF; THINGS" = "INTERNET OF THINGS",
                                                        "INTERNET; OF; THING" = "INTERNET OF THINGS",
                                                        "INTERNET; THING" = "INTERNET OF THINGS",
                                                        "INTERNET; THINGS" = "INTERNET OF THINGS",
                                                        "; THINGS; " = " THINGS; ",
                                                        ";;" = ";"))) %>% 
  separate_rows(keywords_new, sep = "; ") %>% 
  mutate(keywords_new = trimws(keywords_new, which = "both"),
         keywords_new = str_replace_all(keywords_new, "_", " "),
         keywords_new = str_replace_all(keywords_new, "-", " "),
         keywords_new = str_remove_all(keywords_new, "\\(.*"),
         keywords_new = str_replace_all(keywords_new, c("NON ORTHOGONAL MULTIPLE ACCESS" = "NOMA",
                                                        "NONORTHOGONAL MULTIPLE ACCESS" = "NOMA",
                                                        "INTERNET OF THINGS" = "IOT",
                                                        "MASSIVE MIMO" = "MIMO",
                                                        "MM WAVE" = "MILLIMETER WAVE",
                                                        "MMWAVE" = "MILLIMETER WAVE",
                                                        "ORTHOGONAL FREQUENCY DIVISION MULTIPLEXING" = "OFDM",
                                                        "WIRELESS NETWORKS" = "WIRELESS NETWORK",
                                                        "NETWORKS" = "NETWORK",
                                                        "SYSTEMS" = "SYSTEM",
                                                        "LONG TERM EVOLUTION" = "LTE",
                                                        "MULTIPLE INPUT MULTIPLE OUTPUT" = "MIMO",
                                                        "NEW RADIO" = "NR",
                                                        "EDGE COMPUTING" = "EDGE",
                                                        "NETWORK FUNCTION VIRTUALIZATION" = "NFV",
                                                        "C RAN" = "CLOUD RAN",
                                                        "COMMUNICATIONS" = "COMMUNICATION",
                                                        "VEHICLES" = "VEHICLE",
                                                        "PATTERNS" = "PATTERN",
                                                        "MODELS" = "MODEL",
                                                        "\\bLTE A\\b" = "LTE ADVANCED")),
         keywords_new = trimws(keywords_new, which = "both"),
         keywords_new = str_squish(keywords_new),
         keywords_new = str_to_upper(lemmatize_words(keywords_new))) %>% 
  filter(str_detect(keywords_new, ""),
         !str_detect(keywords_new, "4G|FOURTH GENERATION"))

write.xlsx(keywords_clean, paste(directories$dir_keywords, "keywords_clean.xlsx", sep = "/"), overwrite = T)

print("Keywords pre-processed")

##### Analysis #####

source("Analysis.R")

net2VOSviewer(net_country_collab_plot, vos.path = directories$dir_country_net)
net2VOSviewer(net_affil_plot_5, vos.path = directories$dir_affil_net)
