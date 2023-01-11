dir_cited_most_citing <- 'C:\\Users\\luis\\OneDrive\\5G\\Output\\Highest cited papers citations'

folders <- list.files(dir_cited_most_citing)

all_papers <- vector(mode = "list", length = length(folders))
names(all_papers) <- folders

for (i in 1:length(folders)){
  original_paper <- folders[[i]]
  print(original_paper)
  dir_paper_cited <- paste(dir_cited_most_citing, original_paper, sep = "\\")
  input_files <- list.files(dir_paper_cited, pattern = ".txt")
  print(input_files)
  setwd(dir_paper_cited)
  data <- convert2df(file = input_files, format = "plaintext", dbsource = "wos")
  data_compiled <- 
    data %>% 
    as_tibble() %>% 
    separate(EA, c("Month", "Year"), sep = "-", remove = F) %>% # separate EA in month and year, by "-"
      mutate(Year = as.numeric(Year) + 2000, # Convert year, PY to numeric and coalesce PY with year.
             PY = as.numeric(PY),
             PY = coalesce(PY, Year)) %>% 
      metaTagExtraction(Field = "AU1_CO") %>% 
      metaTagExtraction(Field = "AU_CO", sep = ";") %>% 
      metaTagExtraction(Field = "AU_UN", sep = ";", aff.disamb = T ) %>% 
      rowid_to_column(var = "article_id") %>% 
    mutate(Original_paper = original_paper)
  all_papers[[i]] <- data_compiled
}

citing_most_cited <- bind_rows(all_papers)

save(citing_most_cited, file = paste(dir_input_data, "Citing_most_cited.RData", sep = "/"))



