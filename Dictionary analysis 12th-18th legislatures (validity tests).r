setwd("C:/Users/jacop/Tesi/")

suppressWarnings(suppressPackageStartupMessages(library(dtplyr)))
suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))
suppressWarnings(suppressPackageStartupMessages(library(data.table)))
suppressWarnings(suppressPackageStartupMessages(library(quanteda)))
suppressWarnings(suppressPackageStartupMessages(library(manifestoR)))

sessionInfo()

# remove.packages('quanteda')
# devtools::install_version("quanteda", version = "2.1.2", repos = "http://cran.us.r-project.org")

load("data/parliamentary_groups2.rds")

texts <- lazy_dt(Texts)

texts <- texts %>% mutate(legislatura = as.integer(legislatura)) %>% as_tibble()

texts <- texts %>% filter(legislatura >= 12) %>% as_tibble()

mp_setapikey("data/manifesto_apikey.txt")

party_codes <- c(32061, 32230, 32440, 32460, 32530, 32610, 32630, 32720, 32956, 32450)

ita_manifestoes <- mp_corpus(countryname == "Italy" & party %in% party_codes)

ches <- read_csv("data/1999-2019_CHES_dataset_means(v2).csv", show_col_types = FALSE)

populist <- readxl::read_xlsx("data/populist-version-2-20200626.xlsx")

db_additional_stopwords  <- suppressMessages(read_csv("data/it_stopwords_new_list.csv")) %>% 
                            pull(stopwords)

procedural_stopwords <- suppressMessages(read_csv("data/it_stopwords_procedural.csv")) %>% 
                        pull(it_stopwords_procedural)

anti_elitism <- c("elit*", "consens*", "antidemocratic*", "referend*", "corrot*", "propagand*", 
                  "politici*","ingann*", "tradi*", "vergogn*", "scandal*", "verita", "disonest*", 
                  "partitocrazia", "menzogn*", "mentir*")

rp_dictionary <- dictionary(list(anti_elitism = anti_elitism))

anti_elitism <- c("antidemocratic*", "casta", "consens*", "corrot*", "disonest*", "elit*", 
                  "establishment", "ingann*", "mentir*", "menzogn*", "partitocrazia", "propagand*", 
                  "scandal*", "tradim*", "tradir*", "tradit*", "vergogn*", "verita")

people_centrism  <- c("abitant*", "cittadin*", "consumator*", "contribuent*", "elettor*", "gente", "popol*")

db_dictionary <- dictionary(list(anti_elitism = anti_elitism, 
                                 people_centrism = people_centrism))

grundl <- readxl::read_xlsx("data/gruendl_terms_Fedra_Silvia.xlsx", sheet = 2)  %>% 
filter(!is.na(terms))  %>% # Removing null values 
mutate(terms = str_split(terms, ', ')) %>% # Some cells contain more than one value: let's split and unnest everything
unnest(cols = c(terms)) %>% 
distinct(terms) %>% # Removing duplicate terms
pull(terms) # Extracting the 'terms' vector

g_dictionary <- dictionary(list(populism = grundl))

grundl_2 <- readxl::read_xlsx("data/gruendl_terms_Fedra_Silvia.xlsx", sheet = 3)  %>% 
filter(!is.na(terms))  %>%  
mutate(terms = str_split(terms, ', ')) %>% 
unnest(cols = c(terms)) %>% 
distinct(terms) %>% 
pull(terms)

grundl_2

dbg <- sort(unique(unlist(c(anti_elitism, people_centrism, grundl_2))))
dbg_dictionary <- dictionary(list(populism = dbg))
dbg

speeches_corpus <- corpus(texts, text_field = "textclean")

speeches_toks <- speeches_corpus %>% 
                 tokens(., remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_separators = TRUE)  %>% 
                 tokens_remove(., pattern = stopwords("it"), padding = TRUE) %>% 
                 tokens_remove(., pattern = db_additional_stopwords) %>% 
                 tokens_remove(., pattern = procedural_stopwords) %>% 
                 quanteda:::tokens_group(x = ., groups = c('year', 'gruppoP'))

manifesto_corpus <- corpus(ita_manifestoes)

manifesto_toks <- manifesto_corpus %>% 
                  tokens(., remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_separators = TRUE)  %>% 
                  tokens_remove(., pattern = stopwords("it"), padding = TRUE) %>% 
                  tokens_remove(., pattern = db_additional_stopwords) %>% 
                  tokens_remove(., pattern = procedural_stopwords) %>% 
                  quanteda:::tokens_group(x = ., groups = 'party')

dict_analysis <- function(tokens, data, dictionary) {
        
  # Applying Rooduijn and Pauwels' dictionary to the speeches dataset
  
  if (data == "speeches" & dictionary == "Rooduijn_Pauwels") {
    
  my_dfm <- tokens_lookup(x = tokens, dictionary = rp_dictionary) %>% 
            dfm(.)  %>% 
            convert(., to = "data.frame") %>% 
            mutate(year = docvars(tokens)$year,
                   party = docvars(tokens)$gruppoP,
                   cluster = docvars(tokens)$group_cluster,
                   total_toks = ntoken(tokens),
                   perc_of_populist_toks = anti_elitism / total_toks,
                   standardized_perc_of_populist_toks = as.double(scale(perc_of_populist_toks))) %>% 
            relocate(doc_id, year, party, cluster, anti_elitism, total_toks, perc_of_populist_toks, 
                     standardized_perc_of_populist_toks) %>% 
            as_tibble()

  }
    
  # Applying Decadri and Boussalis' dictionary to the speeches dataset
  
  if (data == 'speeches' & dictionary == "Decadri_Boussalis") {
    
    my_dict_lookup <- 
    
    my_dfm <- tokens_lookup(x = tokens, dictionary = db_dictionary) %>% 
              dfm(.) %>% 
              convert(., to = "data.frame") %>% 
              mutate(year = docvars(tokens)$year,
                     party = docvars(tokens)$gruppoP,
                     cluster = docvars(tokens)$group_cluster,
                     populist_toks = anti_elitism + people_centrism,
                     total_toks = ntoken(tokens),
                     perc_of_populist_toks = populist_toks / total_toks,
                     standardized_perc_of_populist_toks = as.double(scale(perc_of_populist_toks))) %>% 
              relocate(doc_id, year, party, cluster, anti_elitism, people_centrism, populist_toks,
                       total_toks, perc_of_populist_toks, standardized_perc_of_populist_toks) %>% 
              as_tibble()
    
  }
    
  # Applying Grundl's dictionary
    
  if (data == "manifesto" & dictionary == "Grundl") {
      
      my_dfm <- tokens_lookup(x = tokens, dictionary = g_dictionary) %>% 
                dfm(.)  %>% 
                convert(., to = "data.frame")  %>% 
                rename(party = doc_id) %>% 
                mutate(party = case_when(
                                           party == '32630' ~ 'FDI-CDN', 
                                           party == '32610' ~ 'FI',
                                           party == '32720' ~ 'LN',
                                           party == '32956' ~ 'M5S',
                                           party == '32061' ~ 'PdL',
                                           party == '32460' ~ 'SC',
                                           party == '32450' ~ 'CD',
                                           party == '32530' ~ 'UDC',
                                           party == '32230' ~ 'SEL',
                                           party == '32440' ~ 'PD'),
                       total_toks = ntoken(tokens),
                       perc_of_populist_toks = populism / total_toks,
                       standardized_perc_of_populist_toks = as.double(scale(perc_of_populist_toks))) %>% 
               arrange(desc(perc_of_populist_toks)) %>% 
               as_tibble()
  }
    
  if (data == "manifesto" & dictionary == "Decadri_Boussalis") {
      
      my_dfm <- tokens_lookup(x = tokens, dictionary = db_dictionary) %>% 
                dfm(.) %>% 
                convert(., to = "data.frame") %>% 
                rename(party = doc_id) %>% 
                mutate(party = case_when(
                                           party == '32630' ~ 'FDI-CDN', 
                                           party == '32610' ~ 'FI',
                                           party == '32720' ~ 'LN',
                                           party == '32956' ~ 'M5S',
                                           party == '32061' ~ 'PdL',
                                           party == '32460' ~ 'SC',
                                           party == '32450' ~ 'CD',
                                           party == '32530' ~ 'UDC',
                                           party == '32230' ~ 'SEL',
                                           party == '32440' ~ 'PD'),
                       total_toks = ntoken(tokens),
                       populist_toks = anti_elitism + people_centrism,
                       perc_of_populist_toks = populist_toks / total_toks,
                       standardized_perc_of_populist_toks = as.double(scale(perc_of_populist_toks))) %>% 
                arrange(desc(perc_of_populist_toks)) %>% 
                as_tibble()
  }
    
    
  if (data == "manifesto" & dictionary == "Decadri_Boussalis_Grundl") {
      
      my_dfm <- tokens_lookup(x = tokens, dictionary = dbg_dictionary) %>% 
                dfm(.) %>% 
                convert(., to = "data.frame") %>% 
                rename(party = doc_id) %>% 
                mutate(party = case_when(
                                           party == '32630' ~ 'FDI-CDN', 
                                           party == '32610' ~ 'FI',
                                           party == '32720' ~ 'LN',
                                           party == '32956' ~ 'M5S',
                                           party == '32061' ~ 'PdL',
                                           party == '32460' ~ 'SC',
                                           party == '32450' ~ 'CD',
                                           party == '32530' ~ 'UDC',
                                           party == '32230' ~ 'SEL',
                                           party == '32440' ~ 'PD'),
                       total_toks = ntoken(tokens),
                       perc_of_populist_toks = populism / total_toks,
                       standardized_perc_of_populist_toks = as.double(scale(perc_of_populist_toks))) %>% 
                arrange(desc(perc_of_populist_toks)) %>% 
                as_tibble()
  }  
  
  return(my_dfm)
    
  
}


df_rp <- dict_analysis(tokens = speeches_toks, data = "speeches", dictionary = "Rooduijn_Pauwels")

head(df_rp)

df_rp %>% 
arrange(desc(standardized_perc_of_populist_toks)) %>% 
head(20)

df_rp %>% 
arrange(desc(standardized_perc_of_populist_toks)) %>% 
tail(20) %>% 
arrange(standardized_perc_of_populist_toks)

ches %>% filter(country == 8 & year >= 2014 & year <= 2019) %>% distinct(party)

df_rp %>% filter(year >= 2014 & year <= 2019) %>% distinct(party)

df_rp %>% 
filter((year == 2014 | year == 2019) & party != "MISTO" & party != "IV") %>% 
arrange(desc(standardized_perc_of_populist_toks))

to_drop <- c('VdA', 'SVP', 'RI')

ches %>% 
filter(country == 8 & year >= 2014 & year <= 2019 & (!party %in% to_drop))  %>% 
group_by(party, year) %>% 
summarize(mean_anti_elite_salience = mean(antielite_salience), .groups = "keep") %>% 
arrange(desc(mean_anti_elite_salience))

populist %>% filter(country_name == "Italy") %>% distinct(party_name)

to_keep <- c("F-ITA", "FI", "PDL", "FI-PDL", "FDI-AN", "FDI", "LEGA-N", "LEGA-NORD-P", "LNA", "LEGA", "LNP", "M5S", 
             "RC-PROGR", "COMUNISTA", "RC", "COM/IT/", "RC-SE", "SI-SEL-POS-LU")

df_rp %>% 
filter(party %in% to_keep) %>% 
arrange(desc(perc_of_populist_toks)) %>% 
head(20)

to_drop <- c("Fiamma Tricolore", "Lega d'Azione Meridionale", "Movimento Sociale Italiano")

populist %>% 
filter(country_name == "Italy" & (!party_name %in% to_drop)) %>% 
select(party_name, populist) %>% 
arrange(desc(populist))

df_db <- dict_analysis(tokens = speeches_toks, data = "speeches", dictionary = "Decadri_Boussalis")

head(df_db)

df_db %>% 
arrange(desc(standardized_perc_of_populist_toks)) %>% 
head(20)

df_db %>% 
arrange(desc(standardized_perc_of_populist_toks)) %>% 
tail(20) %>% 
arrange(standardized_perc_of_populist_toks)

ches %>% filter(country == 8 & year == 2019) %>% select(party, antielite_salience, people_vs_elite)

df_db %>% filter(year == 2019) %>% distinct(party)

to_drop <- c("RI", "SVP")

ches %>% 
filter(country == 8 & year == 2019 & (!party %in% to_drop)) %>% 
group_by(party) %>% 
summarize(mean_populism = mean(people_vs_elite + antielite_salience)) %>% 
arrange(desc(mean_populism))

to_drop <- c("IV", "MISTO")

df_db %>% 
filter(year == 2019 & (! party %in% to_drop)) %>% 
arrange(desc(perc_of_populist_toks))

populist %>% 
filter(country_name == "Italy") %>%
select(party_name, populist) %>% 
arrange(desc(populist))

to_keep <- c("F-ITA", "FI", "PDL", "FI-PDL", "FDI-AN", "FDI", "LEGA-N", "LEGA-NORD-P", "LNA", "LEGA", "LNP", "M5S", 
             "RC-PROGR", "COMUNISTA", "RC", "COM/IT/", "RC-SE", "SI-SEL-POS-LU")

df_db %>% 
filter(party %in% to_keep) %>% 
arrange(desc(perc_of_populist_toks)) %>% 
head(20)

dict_analysis(tokens = manifesto_toks, data = "manifesto", dictionary = "Grundl")

dict_analysis(tokens = manifesto_toks, data = "manifesto", dictionary = "Decadri_Boussalis")

dict_analysis(tokens = manifesto_toks, data = "manifesto", dictionary = "Decadri_Boussalis_Grundl")
