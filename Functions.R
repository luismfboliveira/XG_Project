checks_loaded_packages <- function(required_packages){
  
  all_loaded_packages <- (.packages())
  necessary_loaded_packages <- required_packages[required_packages %in% all_loaded_packages]
  necessary_unloaded_packages <- required_packages[!required_packages %in% all_loaded_packages]
  
  if (length(necessary_unloaded_packages) > 0) {
    
    return(stop(paste("The following package(s) is(are) missing: ", paste(necessary_unloaded_packages, collapse = ", "),
                "\nPlease install/load them to proceed", sep = "")))
    
  } else {
    
    return(print("All necesssary packages loaded successfully"))
    
  }
  
}

creates_output_directories <- function() {
  
  wd <- getwd()
  
  dir_input_data <- paste(wd, "input_data", sep = "/")
  dir_output_data <- paste(wd, "output_data", sep = "/")
  
  if (dir.exists(dir_output_data)) {
    
    print("Directory for output already created")
    
  } else {
    
    dir.create(dir_output_data)
    print("Directory for output created")
    
  }
  
  ###### General Analysis ######
  
  if (dir.exists(paste(dir_output_data, "API_data_enrich", sep = "/"))) {
    
    print("Directory for API_data_enrich already created")
    
  } else {
    
    dir.create(paste(dir_output_data, "API_data_enrich", sep = "/"))
    print("Directory for API_data_enrich created")
    
  }
  
  if (dir.exists(paste(dir_output_data, "general_analysis", sep = "/"))) {
    
    print("Directory for general_analysis already created")
    
  } else {
    
    dir.create(paste(dir_output_data, "general_analysis", sep = "/"))
    print("Directory for general_analysis created")
    
  }
  
  dir_general_analysis <- paste(wd, "output_data", "general_analysis",  sep = "/")
  
  ###### Herfindahal analysis ######
  
  if (dir.exists(paste(dir_output_data, "Herfindahal", sep = "/"))) {
    
    print("Directory for Herfindahal already created")
    
  } else {
    
    dir.create(paste(dir_output_data, "Herfindahal", sep = "/"))
    print("Directory for Herfindahal created")
    
  }
  
  dir_herfindahal <- paste(wd, "output_data", "Herfindahal",  sep = "/")
  
  ###### Authors analysis ######
  
  if (dir.exists(paste(dir_output_data, "Authors", sep = "/"))) {
    
    print("Directory for Authors already created")
    
  } else {
    
    dir.create(paste(dir_output_data, "Authors", sep = "/"))
    print("Directory for Authors created")
    
  }
  
  dir_authors <- paste(wd, "output_data", "Authors",  sep = "/")
  
  ###### Keywords ######
  
  if (dir.exists(paste(dir_output_data, "keywords", sep = "/"))) {
    
    print("Directory for keywords already created")
    
  } else {
    
    dir.create(paste(dir_output_data, "keywords", sep = "/"))
    print("Directory for keywords created")
    
  }
  
  dir_keywords <- paste(wd, "output_data", "keywords",  sep = "/")
  
  ###### SJR data ######
  
  if (dir.exists(paste(dir_output_data, "SJR_data", sep = "/"))) {
    
    print("Directory for SJR data already created")
    
  } else {
    
    dir.create(paste(dir_output_data, "SJR_data", sep = "/"))
    print("Directory for SJR_data created")
    
  }
  
  dir_sjr_data <- paste(wd, "output_data", "SJR_data",  sep = "/")
  
  ###### Aggregated SC categories data ######
  
  if (dir.exists(paste(dir_output_data, "Aggregated SC categories", sep = "/"))) {
    
    print("Directory for Aggregated SC categories already created")
    
  } else {
    
    dir.create(paste(dir_output_data, "Aggregated SC categories", sep = "/"))
    print("Directory for Aggregated SC categories created")
    
  }
  
  dir_aggregated_SC <- paste(wd, "output_data", "Aggregated SC categories",  sep = "/")
  
  ###### Affiliations ######
  
  if (dir.exists(paste(dir_output_data, "affiliations", sep = "/"))) {
    
    print("Directory for affiliations already created")
    
  } else {
    
    dir.create(paste(dir_output_data, "affiliations", sep = "/"))
    print("Directory for affiliations created")
    
  }
  
  dir_affiliations <- paste(wd, "output_data", "affiliations",  sep = "/")
  
  ###### Country networks ######
  
  if (dir.exists(paste(dir_output_data, "country_network", sep = "/"))) {
    
    print("Directory for country network already created")
    
  } else {
    
    dir.create(paste(dir_output_data, "country_network", sep = "/"))
    print("Directory for country network created")
    
  }
  
  dir_country_net <- paste(wd, "output_data", "country_network",  sep = "/")
  
  ###### Affiliation networks ######
  
  if (dir.exists(paste(dir_output_data, "affiliation_network", sep = "/"))) {
    
    print("Directory for affiliation network already created")
    
  } else {
    
    dir.create(paste(dir_output_data, "affiliation_network", sep = "/"))
    print("Directory for affiliation network created")
    
  }
  
  dir_affil_net <- paste(wd, "output_data", "affiliation_network", sep = "/")
  
  ###### Most cited papers ######
  
  if (dir.exists(paste(dir_output_data, "most_cited_papers", sep = "/"))) {
    
    print("Directory for most_cited_papers already created")
    
  } else {
    
    dir.create(paste(dir_output_data, "most_cited_papers", sep = "/"))
    print(glue("Directory for most_cited_papers created"))
    
  }
  
  dir_most_cited_papers <- paste(wd, "output_data", "most_cited_papers", sep = "/")
  
  
 return(list(dir_input_data = dir_input_data, dir_output_data = dir_output_data,
             dir_affil_net = dir_affil_net, dir_keywords = dir_keywords, dir_affiliations = dir_affiliations,
             dir_authors = dir_authors, dir_most_cited_papers = dir_most_cited_papers, dir_country_net = dir_country_net,
             dir_general_analysis = dir_general_analysis, dir_herfindahal = dir_herfindahal, dir_sjr = dir_sjr_data,
             dir_aggregated_SC = dir_aggregated_SC))
}


checks_api_access <- function(your_api_key) {
  
  set_api_key(your_api_key)
  
  if (have_api_key()) {
    
    print("User has Scopus API key")
    
    #authentication <- elsevier_authenticate(your_api_key)
    
    # to be completed for furhter check.
    
  } else {
    
    warning("User does not have valid API key. API data retrieval is not possible.")
    
  }
  
}


checks_input_data_files <- function() {
  
  files_input_data <- list.files(paste(getwd(), "input_data", sep = "/"))
  
  has_initial_dataset <- "initial_dataset.xlsx" %in% files_input_data
  has_API_data <- "API_info.RData" %in% files_input_data
  has_previous_analysis <- "Data_analysis.RData" %in% files_input_data
  has_sjr_data <- "Journal_metrics_info.RData" %in% files_input_data
  has_subject_category_aggregation <- "Aggregated Category Groups_SC.xlsx" %in% files_input_data
  has_jar_file <- "VOSviewer.jar" %in% files_input_data
  
  print(glue("Files present in input_data folder:\nInitial dataset: {has_initial_dataset}\n",
             "API data: {has_API_data}\n",
             "Previous analysis: {has_previous_analysis}\n",
             "SJR data: {has_sjr_data}\n",
             "Aggregated SC categories: {has_subject_category_aggregation}\n",
             "Jar file for VOSviewer: {has_jar_file}\n\n"))
  
  if (!has_initial_dataset) {
    
    stop(glue("User does not have file 'initial_data.xlsx' in folder 'input_data'"))
    
  } else if (has_initial_dataset & !has_API_data) {
    
    warning(glue("User has initial data but does not have API data (API_data.RData) in 'input_data' folder.\n",
                 "Some analysis will not be possible."))
    
  } else if (has_initial_dataset & has_API_data) {
    
    print(glue("User has initial data and API data in 'input_data' folder.\n",
               "All analysis will be performed."))
    
  } 
  
  if (!has_jar_file) {
    
    warning(glue("User needs file 'VOSviewer.jar' in input_data for network plots in VOSviewer."))
    
  } else if (has_jar_file) {
    
    file.copy(paste(paste(getwd(), "input_data", sep = "/"),"VOSviewer.jar", sep = "/"),
              paste(paste(getwd(), "output_data","country_network", sep = "/")))
    file.copy(paste(paste(getwd(), "input_data", sep = "/"),"VOSviewer.jar", sep = "/"),
              paste(paste(getwd(), "output_data","affiliation_network", sep = "/")))
    
    print(glue("Jar file was copied to output network directories"))
    
  }
  
  return(list(has_initial_dataset = has_initial_dataset, has_API_data = has_API_data,
              has_previous_analysis = has_previous_analysis))
  
}

search_for_missing_dois_scopus <- function(tbl){
  
  articles_not_found <- 0
  articles_doi_found <- 0
  articles_doi_missing <- 0
  articles_more_than_one_found <- 0
  n_rows <- nrow(tbl)
  
  for (i in 1:n_rows){
    
    article_to_search <- tbl[["TI_search"]][[i]]
    print(article_to_search)
    api_response <- generic_elsevier_api(type = "search", api_key = api_key, query = as.character(glue('title({article_to_search})')), search_type = "scopus")
    
    found_articles <- as.integer(api_response$content$`search-results`$`opensearch:totalResults`)
    
    NULL_doi <- is.null(api_response$content$`search-results`$entry[[1]]$`prism:doi`)
    
    if (found_articles == 0){
      
      tbl[["DI"]][[i]] <- "Article not found"
      articles_not_found <- articles_not_found + 1
      
    } else if (found_articles == 1 & !NULL_doi) {
      
      tbl[["DI"]][[i]] <- api_response$content$`search-results`$entry[[1]]$`prism:doi`
      articles_doi_found <- articles_doi_found + 1
      
    } else if (found_articles == 1 & NULL_doi) {
      
      tbl[["DI"]][[i]] <- "No DOI"
      articles_doi_missing <- articles_doi_missing + 1
      
    } else if (found_articles > 1) {
      
      tbl[["DI"]][[i]] <- "Several articles found"
      articles_more_than_one_found <- articles_more_than_one_found + 1
      
    }
    
  }
  
  print("Tentative: Enriching missing DOI articles from WoS with SCOPUS data...\n")
  
  print(glue("Number of articles missing from SCOPUS: {articles_not_found}/{n_rows}"))
  print(glue("Number of articles DOI found: {articles_doi_found}/{n_rows}"))
  print(glue("Number of articles with DOI missing: {articles_doi_missing}/{n_rows}"))
  print(glue("Number of articles with multiple results: {articles_more_than_one_found}/{n_rows}"))
  
  articles_with_doi <- 
    tbl %>% 
    filter(DI != "Several articles found" & DI != "No DOI" & DI != "Article not found") %>% 
    select(article_id, DI) %>% 
    arrange(as.numeric(article_id))
  
  articles_no_doi <- 
    tbl %>% 
    filter(DI == "Several articles found" | DI == "No DOI" | DI == "Article not found") %>% 
    arrange(as.numeric(article_id))
  
  return(list(articles_with_doi = articles_with_doi, articles_no_doi = articles_no_doi))
  
}

get_api_info <- function(tbl){
  
  author_info_table <- data.frame(article_id = NA, DI = NA, Index_name = NA, Given_name = NA,
                                  Initials = NA, Surname = NA, affiliation_id = NA, Aut_ID = NA)
  
  affil_info_table <- data.frame(article_id = NA, DI = NA, affiliation_id = NA,  affiliation_name = NA,
                                 affiliation_city = NA, affiliation_country = NA)
  
  dois <- tbl %>% 
    pull(DI)
  
  Article_ids <- tbl %>% 
    pull(article_id)
  
  for (i in 1:nrow(tbl)){
    
    print(i)
    print(dois[i])
    if (!is.na(dois[i])) {
      api_res <- embase_retrieval(dois[i], identifier = "doi") 
      if (!api_res$get_statement$status_code %in% c(404, 400)){
        aut_temp <- api_res$content$`abstracts-retrieval-response`$authors$author
        affil_temp <- api_res$content$`abstracts-retrieval-response`$affiliation
        for (p in 1:length(aut_temp)){
          aut_ind_name <- aut_temp[[p]][["preferred-name"]][["ce:indexed-name"]]
          aut_given_name <- aut_temp[[p]][["preferred-name"]][["ce:given-name"]]
          aut_intials <- aut_temp[[p]][["preferred-name"]][["ce:initials"]]
          aut_surname <- aut_temp[[p]][["preferred-name"]][["ce:surname"]]
          aut_affil_id <- str_c(unname(unlist(aut_temp[[p]]$affiliation)[names(unlist(aut_temp[[p]]$affiliation)) == "@id"]), collapse = ";")
          aut_id <- aut_temp[[p]][["@auid"]]
          author_info_table <- rbind(author_info_table, c(Article_ids[i], dois[i], aut_ind_name, aut_given_name, aut_intials, aut_surname, aut_affil_id, aut_id))
        }
        if (is.null(names(affil_temp))) {
          for (p in 1:length(affil_temp)) {
            
            affil_id <- affil_temp[[p]][["@id"]]
            affil_name <- affil_temp[[p]][["affilname"]]
            affil_city <- affil_temp[[p]][["affiliation-city"]]
            affil_country <- affil_temp[[p]][["affiliation-country"]]
            
            affil_info_table <- rbind(affil_info_table, c(Article_ids[i], dois[i], affil_id, affil_name, affil_city, affil_country))
            
          }
        } else {
          
          affil_id <- affil_temp[["@id"]]
          affil_name <- affil_temp[["affilname"]]
          affil_city <- affil_temp[["affiliation-city"]]
          affil_country <- affil_temp[["affiliation-country"]]
          
          affil_info_table <- rbind(affil_info_table, c(Article_ids[i], dois[i], affil_id, affil_name, affil_city, affil_country))
          
        }
      }
    } else {
      author_info_table <- rbind(author_info_table, c(Article_ids[i], dois[i], NA, NA, NA, NA, NA, NA))
      affil_info_table <- rbind(affil_info_table, c(Article_ids[i], dois[i], NA, NA, NA, NA))
    }
    
  }
  
  author_table <- author_info_table %>% as_tibble() %>% filter(!is.na(article_id)) %>% separate_rows(affiliation_id, sep = ";")
  affil_table <- affil_info_table %>% as_tibble() %>% filter(!is.na(article_id))
  
  return(list(author_info = author_table, affil_info = affil_table))
  
}





