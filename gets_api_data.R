##### Necessary functions #####

source("Functions.R")

##### Setting Scopus API key #####

api_key <- "a37e8a7500807b7438eb438c1324c85a" #please insert your Scopus API key here.

checks_api_access(your_api_key = api_key)

##### Installs/Loads Packages #####

if (!require("pacman")) install.packages("pacman")

required_packages <- c("readxl", "bibliometrix", "tidyverse", "ggrepel", "scales",
                       "maps", "tidytext", "topicmodels", "stm", "rscopus", "openxlsx", "igraph", "countrycode",
                       "widyr", "wpp2019", "textstem", "glue", "ggthemes")

pacman::p_load(char = required_packages)

checks_loaded_packages(required_packages)

data_file_check <- checks_input_data_files()

directories <- creates_output_directories()

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

DI_na <- 
  M_clean %>% 
  as_tibble() %>% 
  select(article_id, DI, TI) %>% 
  filter(is.na(DI)) %>% 
  mutate(TI_search = str_c("{",TI, "}"))

DI_na_scopus_search <- search_for_missing_dois_scopus(DI_na)

DI_not_na_enrich <- bind_rows(DI_not_na, DI_na_scopus_search$articles_with_doi)

API_info <- get_api_info(DI_not_na_enrich)

save(API_info, file = paste(directories$dir_input_data, "API_info.RData", sep = "/"))