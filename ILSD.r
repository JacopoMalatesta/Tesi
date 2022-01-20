suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))
suppressWarnings(suppressPackageStartupMessages(library(haven)))
suppressWarnings(suppressPackageStartupMessages(library(bbplot)))

ilsd <- read_dta("data/ilsd jun2018 (con scale).dta")

ilsd <- ilsd %>% 
mutate(right = per101 + per104 + per110 + per107 + per401 + per402 + per406 + per410 + per414 + per416 + per601 + per603 + per605 + per609 + per608 + per202,
       left = per102 + per103 + per105 + per106 + per108 + per109 + per403 + per404 + per405 + per408 + per409 + per411 + per412 + per413 + per415 + per417 + per602 + per604 + per606 + per607 + per610 + per611 + per201,
       logit_left_right = log(right + .5) - log(left + .5),
       economic_right = per401 + per402 + per406 + per410 + per414 + per416,
       economic_left = per403 + per404 + per405 + per407 + per408 + per409 + per411 + per412 + per413 + per415 + per417,
       classic_economic = economic_right - economic_left,
       ratio_economic = classic_economic / (economic_right + economic_left),
       logit_economic = log(economic_right + .5) - log(economic_left + .5),
       classic_gal_tan = ((per108 + per608) - (per107 + per607)),
       ratio_gal_tan = classic_gal_tan / (per107 + per108 + per607 + per608),
       logit_gal_tan = log((per108 + per608) + .5) - log((per107 + per607) + .5),
       economic_gal_tan_left = per107 + per405 + per607,
       economic_gal_tan_right = per108 + per406 + per608,
       classic_economic_gal_tan = (economic_gal_tan_right - economic_gal_tan_left),
       ratio_economic_gal_tan = classic_economic_gal_tan / (economic_gal_tan_left + economic_gal_tan_right),
       logit_economic_gal_tan = log(economic_gal_tan_right + .5) - log(economic_gal_tan_left + .5),
       gal_tan_controllo = if_else((per107 == 0 & per108 == 0 & per405 == 0 & per406 == 0 & per607 == 0 & per608 == 0), 1, 0))

ilsd %>% 
filter(is.na(ratio_gal_tan)) %>% 
select(per107, per108, per607, per608, classic_gal_tan, ratio_gal_tan, logit_gal_tan)

sum(is.na(ilsd$ratio_gal_tan))

sum(is.na(ilsd$ratio_economic_gal_tan))

ilsd %>% 
ggplot(aes(logit_left_right)) +
geom_histogram(fill = "dark blue") +
bbc_style() +
labs(title = "Frequency distribution for the logit left-right scale") +
theme(plot.title = element_text(size = 20))

ilsd %>% 
group_by(PARTY) %>% 
summarize(avg_logit_left_right = mean(logit_left_right)) %>% 
arrange(desc(avg_logit_left_right)) %>% 
ggplot(aes(x = fct_reorder(PARTY, avg_logit_left_right), y = avg_logit_left_right, fill = avg_logit_left_right)) +
geom_col() +
coord_flip() +
bbc_style() +
scale_fill_gradient(low = "light blue", high = "dark blue") +
labs(title = "Parties by logit left-right scale") +
theme(plot.title = element_text(size = 20),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 9),
      axis.text.y = element_text(size = 7),
      legend.position = "none")

ilsd %>% 
ggplot(aes(logit_left_right)) +
geom_histogram(fill = "dark green") +
bbc_style() +
labs(title = "Frequency distribution for the logit \neconomic left-right scale") +
theme(plot.title = element_text(size = 20))

ilsd %>% 
group_by(PARTY) %>% 
summarize(avg_logit_economic = mean(logit_economic)) %>% 
arrange(desc(avg_logit_economic)) %>% 
ggplot(aes(x = fct_reorder(PARTY, avg_logit_economic), y = avg_logit_economic, fill = avg_logit_economic)) +
geom_col() +
coord_flip() +
bbc_style() +
scale_fill_gradient(low = "light green", high = "dark green") +
labs(title = "Parties by logit economic scale") +
theme(plot.title = element_text(size = 20),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 9),
      axis.text.y = element_text(size = 7),
      legend.position = "none") 

ilsd %>% 
ggplot(aes(logit_left_right)) +
geom_histogram(fill = "dark red") +
bbc_style() +
labs(title = "Frequency distribution for the logit GAL-TAN scale") +
theme(plot.title = element_text(size = 20))

ilsd %>% 
group_by(PARTY) %>% 
summarize(avg_logit_gal_tan = mean(logit_gal_tan)) %>% 
arrange(desc(avg_logit_gal_tan)) %>% 
ggplot(aes(x = fct_reorder(PARTY, avg_logit_gal_tan), y = avg_logit_gal_tan, fill = avg_logit_gal_tan)) +
geom_col() +
coord_flip() +
bbc_style() +
scale_fill_gradient(low = "pink", high = "dark red") +
labs(title = "Parties by logit GAL/TAN scale") +
theme(plot.title = element_text(size = 20),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 9),
      axis.text.y = element_text(size = 7),
      legend.position = "none") 

ilsd %>% 
ggplot(aes(logit_left_right)) +
geom_histogram(fill = "dark grey") +
bbc_style() +
labs(title = "Frequency distribution for the logit economic GAL-TAN scale") +
theme(plot.title = element_text(size = 18))

ilsd %>% 
group_by(PARTY) %>% 
summarize(avg_logit_gal_tan = mean(logit_gal_tan)) %>% 
arrange(desc(avg_logit_gal_tan)) %>% 
ggplot(aes(x = fct_reorder(PARTY, avg_logit_gal_tan), y = avg_logit_gal_tan, fill = avg_logit_gal_tan)) +
geom_col() +
coord_flip() +
bbc_style() +
scale_fill_gradient(low = "light gray", high = "black") +
labs(title = "Parties by logit economic GAL/TAN scale") +
theme(plot.title = element_text(size = 20),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 9),
      axis.text.y = element_text(size = 7),
      legend.position = "none") 

load("data/parliamentary_groups2.rds")
texts <- Texts %>% as_tibble()

ilsd %>% distinct(PARTY) %>% arrange(PARTY) %>% pull()

texts %>% distinct(gruppoP) %>% arrange(gruppoP) %>% pull()

texts <- texts %>% 
mutate(gruppoP = case_when(
gruppoP %in% c('CCD', 'CCD-CDU', 'UNIONE DEI DEMOCRATICI CRISTIANI E DEI DEMOCRATICI DI CENTRO', 'UDCPTP')  ~ 'UDC',
gruppoP == 'COMUNISTA' ~ 'RC',
gruppoP == 'DC-PPI' ~ 'DC',
gruppoP == 'DP-COM' ~ 'DP',
gruppoP == 'FDI-AN' ~ 'FDI',
gruppoP %in% c('F-ITA', 'FI', 'PDL') ~ 'FI-PDL',
gruppoP == 'GC-PDS' ~ "PDS",
gruppoP %in% c('LEGA-N', 'LEGA-NORD-P', 'LNA', 'LNFP', 'LNP') ~ 'LEGA',
gruppoP == 'MSI-DN' ~ "MSI",
gruppoP %in% c('DS-U', 'PD-U', 'PD-ULIVO', 'PD', 'DS-ULIVO') ~ 'PD-ULIVO',
gruppoP == 'PDUP-DP' ~ 'DP',
gruppoP == 'POP-UDEUR' ~ 'UDEUR',
gruppoP == 'PSI-PSDI' ~ "PSI",
gruppoP %in% c('RC-PROGR', 'RC-SE') ~ 'RC',
gruppoP == 'SI' ~ 'SI-SEL-POS-LU',
gruppoP == 'VERDE' ~ 'VERDI',
TRUE ~ gruppoP))

texts %>% distinct(gruppoP) %>% arrange(gruppoP) %>% pull()

parties <- tribble(
~party, ~full_name, ~corresponding_party_in_the_speeches_dataset, ~legislatures, ~notes,
"AD", "Alleanza Democratica", "PROGR-F", "12", "",
"ALA", "Alleanza Liberalpopolare - Autonomie", NA, "17", "Part of the Mixed Group",
"ALP", "Alternativa Libera-Possibile", "SI-SEL-POS-LU", "17", "",
"AN", "Alleanza Nazionale", "AN", "12-15", "",
"API", "Alleanza per l'Italia", "UDCPTP", "16", "",
"AUTSVP", "Sudtiroler Volkspartei", NA, "18", "Part of Per le autonomie",
"BNL", "Blocco Nazionale della Libertà", NA , "0", "Constituent Assembly is not in our dataset",
"CCD", "Centro Cristiano Democratico", "UDC", "12-14", "",
"CD", "Centro Democratico", "DES-CD", "17", "",
"CDU","Cristiani Democratici Uniti", "UDC" , "13", "",
"CGM5SLN", "SintesiContrattoGovM5SLN", NA, "18", "",
"CR", "Conservatori e Riformisti", NA, "17", "Part of FI, then it exited it and it was later disbanded",
"DC", "Democrazia Cristiana", "DC", "0-11", "",
"DEM", "I Democratici", "DEM-U", "13", "",
"DL", "Democrazia e Libertà/La Margherita", "MARGH-U" , "14", "",
"DP", "Democrazia Proletaria", "DP", "7-10", "",
"DS", "Democratici di Sinistra", "PD-ULIVO", "13-14", "",
"FDI", "Fratelli d'Italia", "FDI", "17-18", "",
"FELD", "Federalisti", "FLD", "12", "",
"FI", "Forza Italia", "FI-PDL", "12-18", "",
"FLI", "Futuro e Libertà per l’Italia", "FLPTP", "16", "",
"FUQ", "Fronte dell'Uomo Qualunque", NA, "0", "Constituent Assembly not in our dataset",
"GAL", "Grandi Autonomie e Libertà", NA, "17", "In 2017 it entered the UDC",
"IDV", "Italia dei Valori", "IDV", "15-16", "",
"LEGA", "LEGA", "LEGA", "16", "",
"LEU", "Liberi e Uguali" , "LEU" , "18", "",
"LN", "Lega Nord", "LEGA", "10-18", "",
"LV", "Liga Veneta", NA, "9", "Today it's part of Lega, but it was absent in texts in the 9th legislature",
"M5S", "Movimento 5 stelle", "M5S", "17-18", "",
"MPA", "Movimento per le Autonomie", NA, "16", "Could be part of NOI SUD-LIBERTA\' ED AUTONOMIA/POPOLARI D\'ITALIA DOMANI-PID/MOVIMENTO DI RESPONSABILITA\' NAZIONALE-MRN/AZIONE POPOLARE/ALLEANZA DI CENTRO-ADC/INTESA POPOLARE",
"MSI", "Movimento Sociale Italiano", "MSI", "1-11", "",
"NCD", "Nuovo Centro Destra", "AP-CPE-NCD-NCI", "17", "",
"NPSI", "Nuovo Partito Socialista Italiano", NA, "14", "texts includes DCA-NPSI. However, there's no match for the 14th legislature",
"NS", "Noi Sud", "NOI SUD-LIBERTA\' ED AUTONOMIA/POPOLARI D\'ITALIA DOMANI-PID/MOVIMENTO DI RESPONSABILITA\' NAZIONALE-MRN/AZIONE POPOLARE/ALLEANZA DI CENTRO-ADC/INTESA POPOLARE", "16", "",
"PAT", "Patto Segni", NA, "12", "Absent in texts",
"PCI", "Partito Comunista Italiano", "PCI", "0-10", "",
"PD", "Partito Democratico", "PD-ULIVO", "15-18", "",
"PDCI", "Partito dei Comunisti Italiani", "COM/IT/", "13-15", "",
"PDIUM", "Partito Democratico Italiano di Unità Monarchica", "PDIUM", "3-5", "",
"PDL", "Popolo Delle Libertà", "FI-PDL", "16-17", "",
"PDS", "Partito Democratico della Sinistra", "PDS", "10-13", "",
"PI", "Per l'Italia", "DES-CD", "17", "",
"PLI", "Partito Liberale Italiano", "PLI", "0-11", "",
"PMP", "Partito Monarchico Popolare", "PMP", "2-3", "",
"PNM", "Partito Nazionale Monarchico", "PNM", "1-3", "",
"PPI", "Partito Popolare Italiano", "PPI", "12-13", "",
"PR", "Partito Radicale", "RADICALE", "7-11", "",
"PRI", "Partito Repubblicano Italiano", "PRI", "0-11", "",
"PSDI", "Partito Socialista Democratico Italiano", "PSDI", "0-11", "",
"PSI", "Partito Socialista Italiano", "PSI", "0-12", "",
"PSIUP", "Partito Socialista di Unità Proletaria", "PSIUP", "4-5", "",
"PSU", "Partito Socialista Unitario", "PSU", "5", "",
"PT", "Popolo e Territorio", NA, "16", "Absent in texts",
"RC", "Rifondazione Comunista", "RC", "11-15", "",
"RETE", "La Rete", "RETE", "11-12", "",
"RI", "Rinnovamento Italiano", "RINN/IT", "13", "", 
"RNP", "Rosa Nel Pugno", "SOCRAD-RNP", "15", "",
"SC", "Scelta Civica", "NCI-SCPI-MAIE", "17", "", 
"SD", "Sinistra Democratica", NA, "15", "Absent in texts",
"SDI", "Socialisti Democratici Italiani", NA, "13-14", "",
"SEL", "Sinistra Ecologia e Libertà", "SI-SEL-POS-LU", "17", "",
"UDC", "Unione dei Democratici Cristiani", "UDC", "14-17", "",
"UDEUR", "Unione Democratici per l'Europa", "UDEUR", "13-15", "", 
"UDR", "Unione Democratica per la Repubblica", "UDR", "13", "",
"ULIVO", "Ulivo", "PD-ULIVO", "15", "",
"VER", "Verdi", "VERDI", "10-15", "")

writexl::write_xlsx(x = parties, "data/ilsd_parties.xlsx")

parties %>% 
filter(is.na(corresponding_party_in_the_speeches_dataset))

ilsd <- ilsd %>% 
mutate(PARTY_RECODED = case_when(
PARTY == "AD" ~ "PROGR-F",
PARTY == "ALP" ~ 'SI-SEL-POS-LU',
PARTY == "API" ~ "UDCPTP",
PARTY == "CCD" ~ 'UDC',
PARTY == "CD" ~ "DES-CD",
PARTY == "CDU" ~ "UDC",
PARTY == "DEM" ~ "DEM-U",
PARTY == "DL" ~ "MARGH-U",
PARTY == "DS" ~ "PD-ULIVO",
PARTY == "FELD" ~ "FLD",
PARTY == "FI" ~ "FI-PDL",
PARTY == "FLI" ~ "FLPTP",
PARTY == "LN" ~ "LEGA",
PARTY == "NCD" ~ "AP-CPE-NCD-NCI",
PARTY == "NS" ~ "NOI SUD-LIBERTA' ED AUTONOMIA/POPOLARI D'ITALIA DOMANI-PID/MOVIMENTO DI RESPONSABILITA' NAZIONALE-MRN/AZIONE POPOLARE/ALLEANZA DI CENTRO-ADC/INTESA POPOLARE",
PARTY == "PD" ~ "PD-ULIVO",
PARTY == "PDCI" ~ "COM/IT/",
PARTY == "PDL" ~ "FI-PDL",
PARTY == "PI" ~ "DES-CD",
PARTY == "PR" ~ "RADICALE",
PARTY == "RI" ~ "RINN/IT",
PARTY == "RNP" ~ "SOCRAD-RNP",
PARTY == "SC" ~ "NCI-SCPI-MAIE",
PARTY == "SEL" ~ "SI-SEL-POS-LU",
PARTY == "ULIVO" ~ "PD-ULIVO",
PARTY == "VER" ~ "VERDI",
TRUE ~ PARTY
)) 

ilsd <- ilsd %>% 
mutate(Legislature_RECODED = Legislature - 1)

ilsd <- ilsd %>% 
mutate(Year = as.integer(str_extract(string = Edate, pattern = "\\d{4}")))

ilsd <- ilsd %>% 
group_by(PARTY_RECODED, Legislature_RECODED, Year) %>% 
mutate(
    across(.cols = c("left_right", "ratio_leftright", "logit_left_right", "classic_economic", "ratio_economic", 
                     "logit_economic", "classic_gal_tan", "ratio_gal_tan", "logit_gal_tan", "classic_economic_gal_tan", 
                     "ratio_economic_gal_tan", "logit_economic_gal_tan", "gal_tan_controllo"),
           .fns = ~ mean(., na.rm = TRUE)))

texts <- texts %>% 
left_join(ilsd, by = c("gruppoP" = "PARTY_RECODED", "legislature" = "Legislature_RECODED", "year" = "Year"))

map_dfr(texts, ~ sum(is.na(.)) / length(.)) %>% 
select(left_right, ratio_leftright, logit_left_right, classic_economic, ratio_economic, logit_economic, classic_gal_tan,
      ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, ratio_economic_gal_tan, logit_economic_gal_tan, gal_tan_controllo)

texts <- texts %>% 
arrange(year) %>% 
group_by(gruppoP, legislature) %>% 
fill_(fill_cols = c("left_right", "ratio_leftright", "logit_left_right", "classic_economic", "ratio_economic", 
                    "logit_economic", "classic_gal_tan", "ratio_gal_tan", "logit_gal_tan", "classic_economic_gal_tan", 
                    "ratio_economic_gal_tan", "logit_economic_gal_tan", "gal_tan_controllo"),
       .direction = "down")

map_dfr(texts, ~ sum(is.na(.)) / length(.)) %>% 
select(left_right, ratio_leftright, logit_left_right, classic_economic, ratio_economic, logit_economic, classic_gal_tan,
      ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, ratio_economic_gal_tan, logit_economic_gal_tan, gal_tan_controllo)
