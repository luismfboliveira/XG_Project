library(tidyverse)
library(scales)

path <- paste(getwd(), "/input_data/SJR info", sep = "")

files <- list.files(path)

all_files <- vector(mode = "list", length = length(files))

for (i in 1:length(files)){
  
  file <- files[i]
  year <- as.numeric(str_extract(file, "[0-9]+"))
  all_files[[i]] <- 
    read.csv(paste(path, "/", file, sep = ""), sep = ";") %>% 
    as_tibble() %>% 
    mutate(year = as.factor(year),
           Rank = as.numeric(Rank),
           Sourceid = as.numeric(Sourceid),
           H.index = as.numeric(H.index)) %>% 
    mutate(across(contains("Total"), as.numeric),
           across(contains("Citable"), as.numeric))
  
}

Journal_metrics_info <- all_files %>% bind_rows()
save(Journal_metrics_info, file = paste(getwd(), "/input_data/Journal_metrics_info.RData", sep = ""))




