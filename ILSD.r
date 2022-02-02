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

ilsd <- ilsd %>% 
mutate(Legislature_RECODED = Legislature - 1)

ilsd <- ilsd %>% 
mutate(Year = as.integer(str_extract(string = Edate, pattern = "\\d{4}")))

texts %>% arrange(gruppoP) %>% distinct(gruppoP) %>% pull(gruppoP)

texts <- texts %>% 
mutate(gruppoP_recoded = case_when(
gruppoP %in% c('CCD', 'CCD-CDU', 'UNIONE DEI DEMOCRATICI CRISTIANI E DEI DEMOCRATICI DI CENTRO', 'UDCPTP')  ~ 'UDC-CCD-CDU',
gruppoP %in% c('COMUNISTA', 'COM/IT/') ~ 'PDCI',
gruppoP == 'DC-PPI' ~ 'DC',
gruppoP %in% c('DP-COM', 'PDUP-DP') ~ 'DP',
gruppoP == "FED/EUR/" ~ 'RADICALE',
gruppoP == 'FDI-AN' ~ 'FDI',
gruppoP %in% c('F-ITA', 'FI', 'PDL') ~ 'FI-PDL',
gruppoP == 'GC-PDS' ~ "PDS",
gruppoP %in% c('LEGA-N', 'LEGA-NORD-P', 'LNA', 'LNFP', 'LNP') ~ 'LEGA',
gruppoP == 'MSI-DN' ~ "MSI",
gruppoP %in% c('DS-U', 'PD-U', 'PD-ULIVO', 'PD', 'DS-ULIVO') ~ 'PD-ULIVO-DS',
gruppoP == 'PDI' ~ 'PDIUM',
gruppoP == 'POP-UDEUR' ~ 'UDEUR',
gruppoP == 'PSI-PSDI' ~ "PSI",
gruppoP %in% c('RC-PROGR', 'RC-SE') ~ 'RC',
gruppoP == 'VERDE' ~ 'VERDI',
TRUE ~ gruppoP))

ilsd %>% arrange(PARTY) %>% distinct(PARTY) %>% pull(PARTY)

ilsd <- ilsd %>% 
mutate(PARTY_RECODED = case_when(
PARTY == "ALP" ~ 'SI-SEL-POS-LU',
PARTY == "API" ~ "UDCPTP", 
PARTY %in% c("UDC", "CCD", "CDU") ~ "UDC-CCD-CDU",
PARTY %in% c("CD", "PI") ~ "DES-CD", 
PARTY == "DEM" ~ "DEM-U",
PARTY == "DL" ~ "MARGH-U",
PARTY %in% c('PD', 'ULIVO', 'DS') ~ "PD-ULIVO-DS",
PARTY == "FELD" ~ "FLD",
PARTY %in% c("FI", "PDL") ~ "FI-PDL",
PARTY == "FLI" ~ "FLPTP",
PARTY == "LN" ~ "LEGA",
PARTY == "NCD" ~ "AP-CPE-NCD-NCI",
PARTY == "NS" ~ "NOI SUD-LIBERTA' ED AUTONOMIA/POPOLARI D'ITALIA DOMANI-PID/MOVIMENTO DI RESPONSABILITA' NAZIONALE-MRN/AZIONE POPOLARE/ALLEANZA DI CENTRO-ADC/INTESA POPOLARE",
PARTY == "PR" ~ "RADICALE",
PARTY == "RI" ~ "RINN/IT",
PARTY == "RNP" ~ "SOCRAD-RNP",
PARTY == "SC" ~ "NCI-SCPI-MAIE",
PARTY == "SD" ~ "SDPSE",
PARTY == "SEL" ~ "SI-SEL-POS-LU",
PARTY == "VER" ~ "VERDI",
TRUE ~ PARTY
)) 

ilsd <- ilsd %>% 
group_by(PARTY_RECODED, Legislature_RECODED, Year) %>% 
mutate(
    across(.cols = c("left_right", "ratio_leftright", "logit_left_right", "classic_economic", "ratio_economic", 
                     "logit_economic", "classic_gal_tan", "ratio_gal_tan", "logit_gal_tan", "classic_economic_gal_tan", 
                     "ratio_economic_gal_tan", "logit_economic_gal_tan", "gal_tan_controllo"),
           .fns = ~ mean(., na.rm = TRUE)))

nrow(ilsd)

ilsd <- distinct(ilsd, PARTY_RECODED, Legislature_RECODED, Year, .keep_all = TRUE)

nrow(ilsd)

joined_texts <- texts %>% 
left_join(ilsd, by = c("gruppoP_recoded" = "PARTY_RECODED", "legislature" = "Legislature_RECODED", "year" = "Year"))

nrow(texts) == nrow(joined_texts)

map_dfr(joined_texts, ~ sum(is.na(.)) / length(.)) %>% 
select(left_right, ratio_leftright, logit_left_right, classic_economic, ratio_economic, logit_economic, classic_gal_tan,
      ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, ratio_economic_gal_tan, logit_economic_gal_tan, gal_tan_controllo)

joined_texts <- joined_texts %>% 
arrange(year) %>% 
group_by(gruppoP_recoded, legislature) %>% 
fill_(fill_cols = c("left_right", "ratio_leftright", "logit_left_right", "classic_economic", "ratio_economic", 
                    "logit_economic", "classic_gal_tan", "ratio_gal_tan", "logit_gal_tan", "classic_economic_gal_tan", 
                    "ratio_economic_gal_tan", "logit_economic_gal_tan", "gal_tan_controllo"),
       .direction = "down")

map_dfr(joined_texts, ~ sum(is.na(.)) / length(.)) %>% 
select(left_right, ratio_leftright, logit_left_right, classic_economic, ratio_economic, logit_economic, classic_gal_tan,
      ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, ratio_economic_gal_tan, logit_economic_gal_tan, gal_tan_controllo)

joined_texts <- joined_texts %>% 
mutate(manual_scales = case_when(
!is.na(left_right) ~ 0,
is.na(left_right) & gruppoP_recoded != "MISTO" ~ 1,
is.na(left_right) & gruppoP_recoded == "MISTO" ~ NA_real_))

joined_texts %>% 
filter(is.na(left_right)) %>% 
distinct(gruppoP_recoded)

parties <- tribble(~legislatura, ~missing_party, ~replacement_party, ~nota,
        1, "US", "Mean of PSDI and PSI values", 'Unità Socialista" was born out of an agreement between PSDI (then "Partito Socialista dei Lavoratori Italiani") and "Unione dei Socialisti". The latter splintered from PSI.',
        1, "PSU", "Mean of PSDI and PSI values", '"Partito Socialista Unitario" was founded in 1949 as a result of the merging of three parties: Movimento Socialista Autonomista, Unione dei Socialisti and PSDI (then "Partito Socialista dei Lavoratori Italiani"). Movimento Socialista Autonomista is not available in ILSD.',
        2, "PMP", "PNM", 'It was founded in 1954 by a split from Partito Nazionale Monarchico',
        3, "PDIUM", "Mean of PNM and PMP values", "It was founded in 1959 as a result of the merging between Partito Nazionale Monarchico (PNM) and Partito Monarchico Popolare (PMP)",
        4, "PSIUP", "PSI", "It was founded in 1964 by a split from Partito Socialista Italiano.",
        5, "PSI", "PSU", "In 1968 PSI was part of Partito Socialista Unificato",
        5, "PSDI", "PSU", "In 1968 PSI was part of Partito Socialista Unificato",
        7, "CD-DN", "MSI", "It was a spin-off of Movimento Sociale Italiano",
        8, "PDUP", "DP", 'It was founded in 1974 with the merge between "Partito di Unità Proletaria" and "Il Manifesto" group. Neither of those two parties are in ILSD in the 8th legislature, so I will fill the null values with the values pertaining to Democrazia Proletaria (DP), which PdUP was in coalition with.',
        9, "SI", "PCI", "It was created by the PCI",
        10, "SIN/IND/", "PCI", "It was created by the PCI",
        10, "PDS", "PCI", "It was founded in February 1991 as the post-communist evolution of PCI",
        11, "UDC-CCD-CDU", "DC", "It was founded in January 1994 by members of Democrazia Cristiana",
        12, "DEMO", "Mean of PSI, PAT, AD values", 'Il Patto dei Democratici consisted of Socialisti Italiani (PSI), Patto Segni (PAT) and Alleanza Democratica (AD)',
        12, "PROGR-F", "PDS", "",           
        12, "FLD", "Mean of LEGA, PAT, FI-PDL values", "Members of FLD previously belonged to Lega Nord, Patto Segni, Forza Italia and Lega d'Azione Meridionale",
        12, "LIFED", "LEGA", "It was founded in 1995 by a split from Lega Nord.",
        13, "PD-ULIVO-DS", "PDS", "",
        13, "UDR", "UDC-CCD-CDU", "UDR was founded as a federation that included Cristiani Democratici Uniti, Cristiano Democratici per la Repubblica and Socialdemocrazia Liberale Europea. Both CDU and CDR were part of CCD. SOLE is not included in ILSD. The null values for UDR will thus be filled with values for CCD.",
        13, "UDEUR", "CCD and UDEUR", "UDEUR was founded in 1999 after UDR was disbanded. The null values in 1996 and 1997 will be replaced with values from CCD (the same values used to replace the null in UDR), while the null values in 1998 will be replaced with values from UDR.",
        13, "PDCI", "RC", "We have null values for PDCI in 1996 and 1997. This party was founded by a split from Rifondazione Comunista",
        13, "DEM-U", "PDS and PD-ULIVO-DS", "It was founded in 1999 by members of the following five parties: ULIVO, Italia dei Valori, la Rete, Movimento per l'Ulivo and Unione Democratica. None of these parties are present in ILSD with the exception of L'Ulivo/PDS",
        15, "SDPSE", "PD-ULIVO-DS", "SDPSE was founded in 2007 by splinters from DS/ULIVO.",
        15, "DCA-NPSI", "UDC-CCD-CDU", "Originally affiliated with UDC.",
        16, "FLPTP", "FI-PDL", "It was founded in 2010 by a split from Popolo delle Libertà",
        16, "NOI SUD-LIBERTA' ED AUTONOMIA/POPOLARI D'ITALIA DOMANI-PID/MOVIMENTO DI RESPONSABILITA' NAZIONALE-MRN/AZIONE POPOLARE/ALLEANZA DI CENTRO-ADC/INTESA POPOLARE", "MPA", "It was founded in 2010 by a split from Movimento per le auotonomie",
        17, "MDP-LU", "Mean of PD-ULIVO-DS and SI-SEL-POS-LU values", "It was founded in 2017 by a split from PD and a split from SI-SEL-POS-LU",
        17, "CI", "NCI-SCPI-MAIE", "It was originally named Scelta Civica per l'Italia. In 2016 it changed its name to Civici e Innovatori",
        18, "IV", "PD-ULIVO-DS", "It was founded by former PD secretary Matteo Renzi. It mostly consists of former PD members."
        
       )

writexl::write_xlsx(parties, "data/parties.xlsx")

parties

ilsd %>% 
filter(Legislature_RECODED == 1 & (PARTY_RECODED == "PSI" | PARTY_RECODED == "PSDI")) %>% 
group_by(Year) %>% 
summarize(left_right = mean(left_right),
          ratio_leftright = mean(logit_left_right),
          logit_left_right = mean(logit_left_right),
         classic_economic = mean(classic_economic),
         ratio_economic = mean(ratio_economic),
         logit_economic = mean(logit_economic),
         classic_gal_tan = mean(classic_gal_tan),
         ratio_gal_tan = mean(ratio_gal_tan),
         logit_gal_tan = mean(logit_gal_tan),
         classic_economic_gal_tan = mean(classic_economic_gal_tan),
         ratio_economic_gal_tan = mean(ratio_economic_gal_tan),
         logit_economic_gal_tan = mean(logit_economic_gal_tan))

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1948 ~ -31.98393,
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1950 ~ -24.45500,
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1951 ~ -25.60750,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1948 ~ -2.899208,
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1950 ~ -2.585112,
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1951 ~ -2.243195,
TRUE ~ ratio_leftright),
      logit_left_right = case_when(
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1948 ~ -2.899208,
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1950 ~ -2.585112,
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1951 ~ -2.243195,
TRUE ~ logit_left_right),
      classic_economic = case_when(
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1948 ~ -14.01744,
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1950 ~ -13.48500,
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1951 ~ -15.92500,
TRUE ~ classic_economic),
      ratio_economic = case_when(
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1948 ~ -0.9098242,
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1950 ~ -0.8469818,
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1951 ~ -0.9035449,
TRUE ~ ratio_economic),
      logit_economic = case_when(
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1948 ~ -2.577971,
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1950 ~ -2.413050,
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1951 ~ -2.740863,
TRUE ~ logit_economic),
      classic_gal_tan = case_when(
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1948 ~ 0.48207,
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1950 ~ -0.07500,
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1951 ~ -0.30000,
TRUE ~ classic_gal_tan),
      ratio_gal_tan = case_when(
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1948 ~ 0,
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1951 ~ -1,
TRUE ~ ratio_gal_tan),
      logit_gal_tan = case_when(
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1948 ~ 0.3934124,
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1950 ~ -0.1311821,
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1951 ~ -0.4237901,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1948 ~ 0.48207,
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1950 ~ -0.07500,
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1951 ~ -0.30000,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1948 ~ 0,
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1951 ~ -1,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1948 ~ 0.3934124,
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1950 ~ -0.1311821,
gruppoP_recoded %in% c('US', 'PSU') & legislature == 1 & year == 1951 ~ -0.4237901,
TRUE ~ logit_economic_gal_tan)) 

ilsd %>% 
filter(Legislature_RECODED == 2 & PARTY_RECODED == "PNM" & Year %in% c(1953, 1954)) %>% 
select(left_right, ratio_leftright, logit_left_right, classic_economic, ratio_economic, logit_economic, 
      classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, ratio_economic_gal_tan, logit_economic_gal_tan)

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == "PMP" & legislature == 2 & year == 1953 ~ 6.178276,
gruppoP_recoded == "PMP" & legislature == 2 & year == 1954 ~ 12.906653,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == "PMP" & legislature == 2 & year == 1953 ~ 0.1603617,
gruppoP_recoded == "PMP" & legislature == 2 & year == 1954 ~ 0.3385695,
TRUE ~ ratio_leftright),
       logit_left_right = case_when(
gruppoP_recoded == "PMP" & legislature == 2 & year == 1953 ~ 0.3151961,
gruppoP_recoded == "PMP" & legislature == 2 & year == 1954 ~ 0.7013311,
TRUE ~ logit_left_right),
       classic_economic = case_when(
gruppoP_recoded == "PMP" & legislature == 2 & year == 1953 ~ -0.09371656,
gruppoP_recoded == "PMP" & legislature == 2 & year == 1954 ~ -5.97460102,
TRUE ~ classic_economic),
       ratio_economic = case_when(
gruppoP_recoded == "PMP" & legislature == 2 & year == 1953 ~ -0.02436869,
gruppoP_recoded == "PMP" & legislature == 2 & year == 1954 ~ -0.57692307,
TRUE ~ ratio_economic),
       logit_economic = case_when(
gruppoP_recoded == "PMP" & legislature == 2 & year == 1953 ~ -0.04303556, 
gruppoP_recoded == "PMP" & legislature == 2 & year == 1954 ~ -1.65142326,
TRUE ~ logit_economic),
       classic_gal_tan = case_when(
gruppoP_recoded == "PMP" & legislature == 2 & year == 1953 ~ -0.9362835,
gruppoP_recoded == "PMP" & legislature == 2 & year == 1954 ~ -0.4273504,
TRUE ~ classic_gal_tan),
       ratio_gal_tan = case_when(
gruppoP_recoded == "PMP" & legislature == 2 & year == 1953 ~ -0.6,
gruppoP_recoded == "PMP" & legislature == 2 & year == 1954 ~ -1.0,
TRUE ~ ratio_gal_tan),
       logit_gal_tan = case_when(
gruppoP_recoded == "PMP" & legislature == 2 & year == 1953 ~ -0.8278213,
gruppoP_recoded == "PMP" & legislature == 2 & year == 1954 ~ -0.4983639,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == "PMP" & legislature == 2 & year == 1953 ~ -0.9362835,
gruppoP_recoded == "PMP" & legislature == 2 & year == 1954 ~ 0.1424501,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == "PMP" & legislature == 2 & year == 1953 ~ -0.6000000,
gruppoP_recoded == "PMP" & legislature == 2 & year == 1954 ~ 0.1428571,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == "PMP" & legislature == 2 & year == 1953 ~ -0.82782131,
gruppoP_recoded == "PMP" & legislature == 2 & year == 1954 ~ 0.09543616,
TRUE ~ logit_economic_gal_tan))

ilsd %>% 
filter(Legislature_RECODED == 3 & (PARTY_RECODED == "PNM" | PARTY_RECODED == "PMP")) %>% 
group_by(Year) %>% 
summarize(left_right = mean(left_right),
          ratio_leftright = mean(logit_left_right),
          logit_left_right = mean(logit_left_right),
         classic_economic = mean(classic_economic),
         ratio_economic = mean(ratio_economic),
         logit_economic = mean(logit_economic),
         classic_gal_tan = mean(classic_gal_tan),
         ratio_gal_tan = mean(ratio_gal_tan),
         logit_gal_tan = mean(logit_gal_tan),
         classic_economic_gal_tan = mean(classic_economic_gal_tan),
         ratio_economic_gal_tan = mean(ratio_economic_gal_tan),
         logit_economic_gal_tan = mean(logit_economic_gal_tan))

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1958 ~ 16.075050,
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1959 ~ 2.450408,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1958 ~ 0.8142263,
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1959 ~ 0.1127485,
TRUE ~ ratio_leftright),
       logit_left_right = case_when(
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1958 ~ 0.8142263,
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1959 ~ 0.1127485,
TRUE ~ logit_left_right),
       classic_economic = case_when(
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1958 ~ 18.0120900,
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1959 ~ -0.7208499,
TRUE ~ classic_economic),
       ratio_economic = case_when(
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1958 ~ 0.55125044,
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1959 ~ 0.02636363,
TRUE ~ ratio_economic),
       logit_economic = case_when(
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1958 ~ 1.35203540, 
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1959 ~ 0.04431227,
TRUE ~ logit_economic),
       classic_gal_tan = case_when(
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1958 ~ -3.488119,
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1959 ~ -0.735303,
TRUE ~ classic_gal_tan),
       ratio_gal_tan = case_when(
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1958 ~ -1,
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1959 ~ -1,
TRUE ~ ratio_gal_tan),
       logit_gal_tan = case_when(
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1958 ~ -1.9676145,
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1959 ~ -0.8847821,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1958 ~ -3.488119,
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1959 ~ -0.735303,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1958 ~ -1,
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1959 ~ -1,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1958 ~ -1.9676145,
gruppoP_recoded == "PDIUM" & legislature == 3 & year == 1959 ~ -0.8847821,
TRUE ~ logit_economic_gal_tan))

ilsd %>% 
filter(Legislature_RECODED == 4 & Year == 1963 & PARTY_RECODED == "PSI") %>% 
select(left_right, ratio_leftright, logit_left_right, classic_economic, ratio_economic, logit_economic, 
      classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, ratio_economic_gal_tan, logit_economic_gal_tan)

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == "PSIUP" & legislature == 4 & year == 1963 ~ -35.595,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == "PSIUP" & legislature == 4 & year == 1963 ~ -0.7446428,
TRUE ~ ratio_leftright),
       logit_left_right = case_when(
gruppoP_recoded == "PSIUP" & legislature == 4 & year == 1963 ~ -1.87759,
TRUE ~ logit_left_right),
       classic_economic = case_when(
gruppoP_recoded == "PSIUP" & legislature == 4 & year == 1963 ~ -24.17,
TRUE ~ classic_economic),
       ratio_economic = case_when(
gruppoP_recoded == "PSIUP" & legislature == 4 & year == 1963 ~ -0.9420962,
TRUE ~ ratio_economic),
       logit_economic = case_when(
gruppoP_recoded == "PSIUP" & legislature == 4 & year == 1963 ~ -3.059421, 
TRUE ~ logit_economic),
       classic_gal_tan = case_when(
gruppoP_recoded == "PSIUP" & legislature == 4 & year == 1963 ~ -0.28,
TRUE ~ classic_gal_tan),
       ratio_gal_tan = case_when(
gruppoP_recoded == "PSIUP" & legislature == 4 & year == 1963 ~ -0.5090909,
TRUE ~ ratio_gal_tan),
       logit_gal_tan = case_when(
gruppoP_recoded == "PSIUP" & legislature == 4 & year == 1963 ~ -0.2732718,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == "PSIUP" & legislature == 4 & year == 1963 ~ -0.28,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == "PSIUP" & legislature == 4 & year == 1963 ~ -0.5090909,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == "PSIUP" & legislature == 4 & year == 1963 ~ -0.2732718,
TRUE ~ logit_economic_gal_tan))

ilsd %>% 
filter(Legislature_RECODED == 5 & PARTY_RECODED == 'PSU' & Year == 1968) %>% 
select(left_right, ratio_leftright, logit_left_right, classic_economic, ratio_economic, logit_economic, 
      classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, ratio_economic_gal_tan, logit_economic_gal_tan)

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded %in% c('PSI', 'PSDI') & legislature == 5 & year == 1968 ~ -21.075,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded %in% c('PSI', 'PSDI') & legislature == 5 & year == 1968 ~ -0.430095,
TRUE ~ ratio_leftright),
       logit_left_right = case_when(
gruppoP_recoded %in% c('PSI', 'PSDI') & legislature == 5 & year == 1968 ~ -0.9760826,
TRUE ~ logit_left_right),
       classic_economic = case_when(
gruppoP_recoded %in% c('PSI', 'PSDI') & legislature == 5 & year == 1968 ~ -19.955,
TRUE ~ classic_economic),
       ratio_economic = case_when(
gruppoP_recoded %in% c('PSI', 'PSDI') & legislature == 5 & year == 1968 ~ -0.9149279,
TRUE ~ ratio_economic),
       logit_economic = case_when(
gruppoP_recoded %in% c('PSI', 'PSDI') & legislature == 5 & year == 1968 ~ -2.710567, 
TRUE ~ logit_economic),
       classic_gal_tan = case_when(
gruppoP_recoded %in% c('PSI', 'PSDI') & legislature == 5 & year == 1968 ~ -4.25,
TRUE ~ classic_gal_tan),
       ratio_gal_tan = case_when(
gruppoP_recoded %in% c('PSI', 'PSDI') & legislature == 5 & year == 1968 ~ -1,
TRUE ~ ratio_gal_tan),
       logit_gal_tan = case_when(
gruppoP_recoded %in% c('PSI', 'PSDI') & legislature == 5 & year == 1968 ~ -2.095158,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded %in% c('PSI', 'PSDI') & legislature == 5 & year == 1968 ~ -4.25,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded %in% c('PSI', 'PSDI') & legislature == 5 & year == 1968 ~ -1,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded %in% c('PSI', 'PSDI') & legislature == 5 & year == 1968 ~ -2.095158,
TRUE ~ logit_economic_gal_tan))

ilsd %>% 
filter(Legislature_RECODED == 7 & PARTY_RECODED == 'MSI') %>% 
select(Legislature_RECODED, PARTY_RECODED, Year, left_right, ratio_leftright, logit_left_right, classic_economic, 
       ratio_economic, logit_economic, classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, 
       ratio_economic_gal_tan, logit_economic_gal_tan)


joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1976 ~ 10.73986,
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1978 ~ 55.87906,
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1979 ~ 23.28817,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1976 ~ 0.2586207,
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1978 ~ 0.9896504,
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1979 ~ 0.8315191,
TRUE ~ ratio_leftright),
      logit_left_right = case_when(
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1976 ~ 0.5162467,
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1978 ~ 4.2702242,
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1979 ~ 2.2131744,
TRUE ~ logit_left_right),
      classic_economic = case_when(
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1976 ~ 0.3579952,
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1978 ~ 1.1687500,
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1979 ~ 4.0537131,
TRUE ~ classic_economic),
      ratio_economic = case_when(
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1976 ~ 0.4285714,
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1978 ~ 1.0000000,
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1979 ~ 0.5693994,
TRUE ~ ratio_economic),
      logit_economic = case_when(
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1976 ~ 0.3951808,
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1978 ~ 1.2052221,
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1979 ~ 1.0966668,
TRUE ~ logit_economic),
      classic_gal_tan = case_when(
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1976 ~ -0.8353222,
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1978 ~ 0.0000000,
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1979 ~ -0.3324468,
TRUE ~ classic_gal_tan),
      ratio_gal_tan = case_when(
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1976 ~ -1,
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1979 ~ -1,
TRUE ~ ratio_gal_tan),
      logit_gal_tan = case_when(
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1976 ~ -0.9823198,
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1978 ~ 0.0000000,
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1979 ~ -0.5097612,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1976 ~ -0.8353222,
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1978 ~ 0.0000000,
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1979 ~ -0.3324468,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1976 ~ -1,
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1979 ~ -1,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1976 ~ -0.9823198,
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1978 ~ 0.0000000,
gruppoP_recoded == "CD-DN" & legislature == 7 & year == 1979 ~ -0.5097612,
TRUE ~ logit_economic_gal_tan)) 

ilsd %>% 
filter(Legislature_RECODED == 8 & PARTY_RECODED == "DP") %>% 
select(Legislature_RECODED, PARTY_RECODED, Year, left_right, ratio_leftright, logit_left_right, classic_economic, 
       ratio_economic, logit_economic, classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, 
       ratio_economic_gal_tan, logit_economic_gal_tan)

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1979 ~ -41.420,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1980 ~ -29.035,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1981 ~ -20.260,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1982 ~ -22.310,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1979 ~ -0.8716330,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1980 ~ -0.7395584,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1981 ~ -0.4877227,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1982 ~ -0.5803005,
TRUE ~ ratio_leftright),
      logit_left_right = case_when(
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1979 ~ -2.539048,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1980 ~ -2.095848,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1981 ~ -1.036266,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1982 ~ -1.349249,
TRUE ~ logit_left_right),
      classic_economic = case_when(
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1979 ~ -29.79,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1980 ~ -23.14,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1981 ~ -15.24,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1982 ~ -18.91,
TRUE ~ classic_economic),
      ratio_economic = case_when(
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1979 ~ -1.0000000,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1980 ~ -0.8963370,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1981 ~ -0.7912773,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1982 ~ -0.6588462,
TRUE ~ ratio_economic),
      logit_economic = case_when(
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1979 ~ -4.103965,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1980 ~ -2.647445,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1981 ~ -1.956103,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1982 ~ -1.860461,
TRUE ~ logit_economic),
      classic_gal_tan = case_when(
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1979 ~ 0.000,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1980 ~ -0.315,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1981 ~ -5.420,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1982 ~ -0.770,
TRUE ~ classic_gal_tan),
      ratio_gal_tan = case_when(
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1980 ~ -1,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1981 ~ -1,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1982 ~ -1,
TRUE ~ ratio_gal_tan),
      logit_gal_tan = case_when(
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1979 ~ 0.0000000,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1980 ~ -0.4076824,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1981 ~ -2.4714836,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1982 ~ -0.8771326,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1979 ~ 0.000,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1980 ~ -0.315,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1981 ~ -5.420,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1982 ~ -1.485,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1979 ~ -1.0000000,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1981 ~ -1.0000000,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1982 ~ -0.8565737,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1979 ~ 0.0000000,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1980 ~ -0.4076824,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1981 ~ -2.4714836,
gruppoP_recoded == "PDUP" & legislature == 8 & year == 1982 ~ -1.1686617,
TRUE ~ logit_economic_gal_tan)) 

ilsd %>% 
filter(Legislature_RECODED == 9 & PARTY_RECODED == "PCI") %>% 
select(Legislature_RECODED, PARTY_RECODED, Year, left_right, ratio_leftright, logit_left_right, classic_economic, 
       ratio_economic, logit_economic, classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, 
       ratio_economic_gal_tan, logit_economic_gal_tan)

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == "SI" & legislature == 9 & year == 1983 ~ -26.17944,
gruppoP_recoded == "SI" & legislature == 9 & year == 1986 ~ -17.53310,
gruppoP_recoded == "SI" & legislature == 9 & year == 1987 ~ -17.83554,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == "SI" & legislature == 9 & year == 1983 ~ -0.6308939,
gruppoP_recoded == "SI" & legislature == 9 & year == 1986 ~ -0.5499507,
gruppoP_recoded == "SI" & legislature == 9 & year == 1987 ~ -0.6542756,
TRUE ~ ratio_leftright),
      logit_left_right = case_when(
gruppoP_recoded == "SI" & legislature == 9 & year == 1983 ~ -1.437221,
gruppoP_recoded == "SI" & legislature == 9 & year == 1986 ~ -1.189282,
gruppoP_recoded == "SI" & legislature == 9 & year == 1987 ~ -1.486563,
TRUE ~ logit_left_right),
      classic_economic = case_when(
gruppoP_recoded == "SI" & legislature == 9 & year == 1983 ~ -16.44706,
gruppoP_recoded == "SI" & legislature == 9 & year == 1986 ~ -11.80513,
gruppoP_recoded == "SI" & legislature == 9 & year == 1987 ~ -14.19994,
TRUE ~ classic_economic),
      ratio_economic = case_when(
gruppoP_recoded == "SI" & legislature == 9 & year == 1983 ~ -0.7288902,
gruppoP_recoded == "SI" & legislature == 9 & year == 1986 ~ -0.6850729,
gruppoP_recoded == "SI" & legislature == 9 & year == 1987 ~ -1.0000000,
TRUE ~ ratio_economic),
      logit_economic = case_when(
gruppoP_recoded == "SI" & legislature == 9 & year == 1983 ~ -1.726618,
gruppoP_recoded == "SI" & legislature == 9 & year == 1986 ~ -1.541955,
gruppoP_recoded == "SI" & legislature == 9 & year == 1987 ~ -3.380991,
TRUE ~ logit_economic),
      classic_gal_tan = case_when(
gruppoP_recoded == "SI" & legislature == 9 & year == 1983 ~ -2.911066,
gruppoP_recoded == "SI" & legislature == 9 & year == 1986 ~ -0.197665,
gruppoP_recoded == "SI" & legislature == 9 & year == 1987 ~ -2.736114,
TRUE ~ classic_gal_tan),
      ratio_gal_tan = case_when(
gruppoP_recoded == "SI" & legislature == 9 & year == 1983 ~ -1,
gruppoP_recoded == "SI" & legislature == 9 & year == 1986 ~ -1,
gruppoP_recoded == "SI" & legislature == 9 & year == 1987 ~ -1,
TRUE ~ ratio_gal_tan),
      logit_gal_tan = case_when(
gruppoP_recoded == "SI" & legislature == 9 & year == 1983 ~ -1.9201721,
gruppoP_recoded == "SI" & legislature == 9 & year == 1986 ~ -0.3331309,
gruppoP_recoded == "SI" & legislature == 9 & year == 1987 ~ -1.8675205,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == "SI" & legislature == 9 & year == 1983 ~ -2.911066,
gruppoP_recoded == "SI" & legislature == 9 & year == 1986 ~ -0.197665,
gruppoP_recoded == "SI" & legislature == 9 & year == 1987 ~ -2.736114,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == "SI" & legislature == 9 & year == 1983 ~ -1,
gruppoP_recoded == "SI" & legislature == 9 & year == 1986 ~ -1,
gruppoP_recoded == "SI" & legislature == 9 & year == 1987 ~ -1,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == "SI" & legislature == 9 & year == 1983 ~ -1.9201721,
gruppoP_recoded == "SI" & legislature == 9 & year == 1986 ~ -0.3331309,
gruppoP_recoded == "SI" & legislature == 9 & year == 1987 ~ -1.8675205,
TRUE ~ logit_economic_gal_tan)) 

ilsd %>% 
filter(Legislature_RECODED == 10 & PARTY_RECODED == "PCI") %>% 
select(Legislature_RECODED, PARTY_RECODED, Year, left_right, ratio_leftright, logit_left_right, classic_economic, 
       ratio_economic, logit_economic, classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, 
       ratio_economic_gal_tan, logit_economic_gal_tan)

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1987 ~ -32.973785,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1988 ~ -7.032656,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1989 ~ -3.702825,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1987 ~ -0.7027305,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1988 ~ -0.2811525,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1989 ~ -0.1540813,
TRUE ~ ratio_leftright),
      logit_left_right = case_when(
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1987 ~ -1.6885495,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1988 ~ -0.5544711,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1989 ~ -0.2980383,
TRUE ~ logit_left_right),
      classic_economic = case_when(
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1987 ~ -18.069944,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1988 ~ -3.094237,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1989 ~ -7.299153,
TRUE ~ classic_economic),
      ratio_economic = case_when(
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1987 ~ -0.8357102,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1988 ~ -0.9084395,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1989 ~ -0.8076959,
TRUE ~ ratio_economic),
      logit_economic = case_when(
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1987 ~ -2.190400,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1988 ~ -1.743499,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1989 ~ -1.845620,
TRUE ~ logit_economic),
      classic_gal_tan = case_when(
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1987 ~ -0.1333333,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1988 ~ -0.1932203,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1989 ~ -2.0819774,
TRUE ~ classic_gal_tan),
      ratio_gal_tan = case_when(
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1987 ~ -1,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1988 ~ -1,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1989 ~ -1,
TRUE ~ ratio_gal_tan),
      logit_gal_tan = case_when(
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1987 ~ -0.2363888,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1988 ~ -0.3267398,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1989 ~ -1.6417027,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1987 ~ -0.1333333,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1988 ~ -0.1932203,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1989 ~ -2.0469491,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1987 ~ -1.0000000,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1988 ~ -1.0000000,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1989 ~ -0.9669077,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1987 ~ -0.2363888,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1988 ~ -0.3267398,
gruppoP_recoded == "SIN/IND/" & legislature == 10 & year == 1989 ~ -1.5739913,
TRUE ~ logit_economic_gal_tan)) 

ilsd %>% 
filter(Legislature_RECODED == 10 & PARTY_RECODED == "PCI") %>% 
select(Legislature_RECODED, PARTY_RECODED, Year, left_right, ratio_leftright, logit_left_right, classic_economic, 
       ratio_economic, logit_economic, classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, 
       ratio_economic_gal_tan, logit_economic_gal_tan)

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == "PDS" & legislature == 10 & year == 1987 ~ -32.973785,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1988 ~ -7.032656,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1989 ~ -3.702825,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == "PDS" & legislature == 10 & year == 1987 ~ -0.7027305,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1988 ~ -0.2811525,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1989 ~ -0.1540813,
TRUE ~ ratio_leftright),
      logit_left_right = case_when(
gruppoP_recoded == "PDS" & legislature == 10 & year == 1987 ~ -1.6885495,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1988 ~ -0.5544711,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1989 ~ -0.2980383,
TRUE ~ logit_left_right),
      classic_economic = case_when(
gruppoP_recoded == "PDS" & legislature == 10 & year == 1987 ~ -18.069944,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1988 ~ -3.094237,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1989 ~ -7.299153,
TRUE ~ classic_economic),
      ratio_economic = case_when(
gruppoP_recoded == "PDS" & legislature == 10 & year == 1987 ~ -0.8357102,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1988 ~ -0.9084395,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1989 ~ -0.8076959,
TRUE ~ ratio_economic),
      logit_economic = case_when(
gruppoP_recoded == "PDS" & legislature == 10 & year == 1987 ~ -2.190400,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1988 ~ -1.743499,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1989 ~ -1.845620,
TRUE ~ logit_economic),
      classic_gal_tan = case_when(
gruppoP_recoded == "PDS" & legislature == 10 & year == 1987 ~ -0.1333333,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1988 ~ -0.1932203,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1989 ~ -2.0819774,
TRUE ~ classic_gal_tan),
      ratio_gal_tan = case_when(
gruppoP_recoded == "PDS" & legislature == 10 & year == 1987 ~ -1,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1988 ~ -1,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1989 ~ -1,
TRUE ~ ratio_gal_tan),
      logit_gal_tan = case_when(
gruppoP_recoded == "PDS" & legislature == 10 & year == 1987 ~ -0.2363888,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1988 ~ -0.3267398,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1989 ~ -1.6417027,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == "PDS" & legislature == 10 & year == 1987 ~ -0.1333333,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1988 ~ -0.1932203,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1989 ~ -2.0469491,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == "PDS" & legislature == 10 & year == 1987 ~ -1.0000000,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1988 ~ -1.0000000,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1989 ~ -0.9669077,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == "PDS" & legislature == 10 & year == 1987 ~ -0.2363888,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1988 ~ -0.3267398,
gruppoP_recoded == "PDS" & legislature == 10 & year == 1989 ~ -1.5739913,
TRUE ~ logit_economic_gal_tan)) 

ilsd %>% 
filter(Legislature_RECODED == 11 & PARTY_RECODED == "DC") %>% 
select(Legislature_RECODED, PARTY_RECODED, Year, left_right, ratio_leftright, logit_left_right, classic_economic, 
       ratio_economic, logit_economic, classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, 
       ratio_economic_gal_tan, logit_economic_gal_tan)


joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1992 ~ 11.36,
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1993 ~ 3.98,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1992 ~ 0.4344168,
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1993 ~ 0.1529593,
TRUE ~ ratio_leftright),
       logit_left_right = case_when(
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1992 ~ 0.8915411,
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1993 ~ 0.2967554,
TRUE ~ logit_left_right),
       classic_economic = case_when(
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1992 ~ -1.48,
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1993 ~ -3.50,
TRUE ~ classic_economic),
       ratio_economic = case_when(
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1992 ~ -0.3583535,
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1993 ~ -0.4768392,
TRUE ~ ratio_economic),
       logit_economic = case_when(
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1992 ~ -0.5938565, 
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1993 ~ -0.8945689,
TRUE ~ logit_economic),
       classic_gal_tan = case_when(
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1992 ~ -5.50,
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1993 ~ -0.93,
TRUE ~ classic_gal_tan),
       ratio_gal_tan = case_when(
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1992 ~ -1,
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1993 ~ -1,
TRUE ~ ratio_gal_tan),
       logit_gal_tan = case_when(
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1992 ~ -2.484907,
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1993 ~ -1.050822,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1992 ~ -5.50,
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1993 ~ -0.93,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1992 ~ -1,
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1993 ~ -1,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1992 ~ -2.484907,
gruppoP_recoded == "UDC-CCD-CDU" & legislature == 11 & year == 1993 ~ -1.050822,
TRUE ~ logit_economic_gal_tan))

ilsd %>% 
filter(Legislature_RECODED == 12 & PARTY_RECODED %in% c('PSI', 'PAT', 'AD')) %>% 
group_by(Year) %>% 
summarize(left_right = mean(left_right),
          ratio_leftright = mean(logit_left_right),
          logit_left_right = mean(logit_left_right),
         classic_economic = mean(classic_economic),
         ratio_economic = mean(ratio_economic),
         logit_economic = mean(logit_economic),
         classic_gal_tan = mean(classic_gal_tan),
         ratio_gal_tan = mean(ratio_gal_tan),
         logit_gal_tan = mean(logit_gal_tan),
         classic_economic_gal_tan = mean(classic_economic_gal_tan),
         ratio_economic_gal_tan = mean(ratio_economic_gal_tan),
         logit_economic_gal_tan = mean(logit_economic_gal_tan))


joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == "DEMO" & legislature == 12 & year == 1994 ~ -3.193333,
gruppoP_recoded == "DEMO" & legislature == 12 & year == 1995 ~ -2.426666,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == "DEMO" & legislature == 12 & year == 1994 ~ -0.2556950,
gruppoP_recoded == "DEMO" & legislature == 12 & year == 1995 ~ -0.3549939,
TRUE ~ ratio_leftright),
       logit_left_right = case_when(
gruppoP_recoded == "DEMO" & legislature == 12 & year == 1994 ~ -0.2556950,
gruppoP_recoded == "DEMO" & legislature == 12 & year == 1995 ~ -0.3549939,
TRUE ~ logit_left_right),
       classic_economic = case_when(
gruppoP_recoded == "DEMO" & legislature == 12 & year == 1994 ~ -4.750000,
gruppoP_recoded == "DEMO" & legislature == 12 & year == 1995 ~ -6.943333,
TRUE ~ classic_economic),
       ratio_economic = case_when(
gruppoP_recoded == "DEMO" & legislature == 12 & year == 1994 ~ -0.2473399,
gruppoP_recoded == "DEMO" & legislature == 12 & year == 1995 ~ -0.3174291,
TRUE ~ ratio_economic),
       logit_economic = case_when(
gruppoP_recoded == "DEMO" & legislature == 12 & year == 1994 ~ -0.5081133, 
gruppoP_recoded == "DEMO" & legislature == 12 & year == 1995 ~ -0.8556488,
TRUE ~ logit_economic),
       classic_gal_tan = case_when(
gruppoP_recoded == "DEMO" & legislature == 12 & year == 1994 ~ -2.356667,
gruppoP_recoded == "DEMO" & legislature == 12 & year == 1995 ~ -1.953333,
TRUE ~ classic_gal_tan),
       ratio_gal_tan = case_when(
gruppoP_recoded == "DEMO" & legislature == 12 & year == 1994 ~ -1,
TRUE ~ ratio_gal_tan),
       logit_gal_tan = case_when(
gruppoP_recoded == "DEMO" & legislature == 12 & year == 1994 ~ -1.362913,
gruppoP_recoded == "DEMO" & legislature == 12 & year == 1995 ~ -1.283666,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == "DEMO" & legislature == 12 & year == 1994 ~ -2.356667,
gruppoP_recoded == "DEMO" & legislature == 12 & year == 1995 ~ -1.953333,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == "DEMO" & legislature == 12 & year == 1994 ~ -1,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == "DEMO" & legislature == 12 & year == 1994 ~ -1.362913,
gruppoP_recoded == "DEMO" & legislature == 12 & year == 1995 ~ -1.283666,
TRUE ~ logit_economic_gal_tan))

ilsd %>% 
filter(Legislature_RECODED == 12 & Year == 1994 & PARTY_RECODED %in% c("PAT", "LEGA", "FI-PDL")) %>% 
group_by(Year) %>% 
summarize(left_right = mean(left_right),
         ratio_leftright = mean(logit_left_right),
         logit_left_right = mean(logit_left_right),
         classic_economic = mean(classic_economic),
         ratio_economic = mean(ratio_economic),
         logit_economic = mean(logit_economic),
         classic_gal_tan = mean(classic_gal_tan),
         ratio_gal_tan = mean(ratio_gal_tan),
         logit_gal_tan = mean(logit_gal_tan),
         classic_economic_gal_tan = mean(classic_economic_gal_tan),
         ratio_economic_gal_tan = mean(ratio_economic_gal_tan),
         logit_economic_gal_tan = mean(logit_economic_gal_tan))

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == "FLD" & legislature == 12 & year == 1994 ~ 15.97,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == "FLD" & legislature == 12 & year == 1994 ~ 0.8792168,
TRUE ~ ratio_leftright),
       logit_left_right = case_when(
gruppoP_recoded == "FLD" & legislature == 12 & year == 1994 ~ 0.8792168,
TRUE ~ logit_left_right),
       classic_economic = case_when(
gruppoP_recoded == "FLD" & legislature == 12 & year == 1994 ~ 4.32,
TRUE ~ classic_economic),
       ratio_economic = case_when(
gruppoP_recoded == "FLD" & legislature == 12 & year == 1994 ~ 0.1793768,
TRUE ~ ratio_economic),
       logit_economic = case_when(
gruppoP_recoded == "FLD" & legislature == 12 & year == 1994 ~ 0.4837237, 
TRUE ~ logit_economic),
       classic_gal_tan = case_when(
gruppoP_recoded == "FLD" & legislature == 12 & year == 1994 ~ -6.663333,
TRUE ~ classic_gal_tan),
       ratio_gal_tan = case_when(
gruppoP_recoded == "FLD" & legislature == 12 & year == 1994 ~ -0.8787879,
TRUE ~ ratio_gal_tan),
       logit_gal_tan = case_when(
gruppoP_recoded == "FLD" & legislature == 12 & year == 1994 ~ -2.284952,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == "FLD" & legislature == 12 & year == 1994 ~ -6.663333,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == "FLD" & legislature == 12 & year == 1994 ~ -0.8787879,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == "FLD" & legislature == 12 & year == 1994 ~ -2.284952,
TRUE ~ logit_economic_gal_tan))

ilsd %>% 
filter(Legislature_RECODED == 12 & PARTY_RECODED == "PDS") %>% 
select(Legislature_RECODED, PARTY_RECODED, Year, left_right, ratio_leftright, logit_left_right, classic_economic, 
       ratio_economic, logit_economic, classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, 
       ratio_economic_gal_tan, logit_economic_gal_tan)

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == "PROGR-F" & legislature == 12 & year == 1994 ~ -12.430000,
gruppoP_recoded == "PROGR-F" & legislature == 12 & year == 1995 ~ -1.890001,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == "PROGR-F" & legislature == 12 & year == 1994 ~ -0.4564818,
gruppoP_recoded == "PROGR-F" & legislature == 12 & year == 1995 ~ -0.0551503,
TRUE ~ ratio_leftright),
       logit_left_right = case_when(
gruppoP_recoded == "PROGR-F" & legislature == 12 & year == 1994 ~ -0.9452349,
gruppoP_recoded == "PROGR-F" & legislature == 12 & year == 1995 ~ -0.1072760,
TRUE ~ logit_left_right),
       classic_economic = case_when(
gruppoP_recoded == "PROGR-F" & legislature == 12 & year == 1994 ~ -15.68,
gruppoP_recoded == "PROGR-F" & legislature == 12 & year == 1995 ~ -8.57,
TRUE ~ classic_economic),
       ratio_economic = case_when(
gruppoP_recoded == "PROGR-F" & legislature == 12 & year == 1994 ~ -0.7678746,
gruppoP_recoded == "PROGR-F" & legislature == 12 & year == 1995 ~ -0.6005606,
TRUE ~ ratio_economic),
       logit_economic = case_when(
gruppoP_recoded == "PROGR-F" & legislature == 12 & year == 1994 ~ -1.866158, 
gruppoP_recoded == "PROGR-F" & legislature == 12 & year == 1995 ~ -1.269257,
TRUE ~ logit_economic),
       classic_gal_tan = case_when(
gruppoP_recoded == "PROGR-F" & legislature == 12 & year == 1994 ~ -0.89,
gruppoP_recoded == "PROGR-F" & legislature == 12 & year == 1995 ~ 0.00,
TRUE ~ classic_gal_tan),
       ratio_gal_tan = case_when(
gruppoP_recoded == "PROGR-F" & legislature == 12 & year == 1994 ~ -1,
TRUE ~ ratio_gal_tan),
       logit_gal_tan = case_when(
gruppoP_recoded == "PROGR-F" & legislature == 12 & year == 1994 ~ -1.022451,
gruppoP_recoded == "PROGR-F" & legislature == 12 & year == 1995 ~ 0.000000,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == "PROGR-F" & legislature == 12 & year == 1994 ~ -0.89,
gruppoP_recoded == "PROGR-F" & legislature == 12 & year == 1995 ~ 0.00,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == "PROGR-F" & legislature == 12 & year == 1994 ~ -1,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == "PROGR-F" & legislature == 12 & year == 1994 ~ -1.022451,
gruppoP_recoded == "PROGR-F" & legislature == 12 & year == 1995 ~ 0.000000,
TRUE ~ logit_economic_gal_tan))

ilsd %>% 
filter(Legislature_RECODED == 12 & PARTY_RECODED == "LEGA") %>% 
select(Legislature_RECODED, PARTY_RECODED, Year, left_right, ratio_leftright, logit_left_right, classic_economic, 
       ratio_economic, logit_economic, classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, 
       ratio_economic_gal_tan, logit_economic_gal_tan)

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == "LIFED" & legislature == 12 & year == 1994 ~ 13.380000,
gruppoP_recoded == "LIFED" & legislature == 12 & year == 1995 ~ 4.779999,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == "LIFED" & legislature == 12 & year == 1994 ~ 0.2820405,
gruppoP_recoded == "LIFED" & legislature == 12 & year == 1995 ~ 0.1294691,
TRUE ~ ratio_leftright),
       logit_left_right = case_when(
gruppoP_recoded == "LIFED" & legislature == 12 & year == 1994 ~ 0.5671661,
gruppoP_recoded == "LIFED" & legislature == 12 & year == 1995 ~ 0.2534579,
TRUE ~ logit_left_right),
       classic_economic = case_when(
gruppoP_recoded == "LIFED" & legislature == 12 & year == 1994 ~ -0.79,
gruppoP_recoded == "LIFED" & legislature == 12 & year == 1995 ~ 5.37,
TRUE ~ classic_economic),
       ratio_economic = case_when(
gruppoP_recoded == "LIFED" & legislature == 12 & year == 1994 ~ -0.03605659,
gruppoP_recoded == "LIFED" & legislature == 12 & year == 1995 ~ 0.20044791,
TRUE ~ ratio_economic),
       logit_economic = case_when(
gruppoP_recoded == "LIFED" & legislature == 12 & year == 1994 ~ -0.06899287, 
gruppoP_recoded == "LIFED" & legislature == 12 & year == 1995 ~ 0.39139087,
TRUE ~ logit_economic),
       classic_gal_tan = case_when(
gruppoP_recoded == "LIFED" & legislature == 12 & year == 1994 ~ -11.34,
gruppoP_recoded == "LIFED" & legislature == 12 & year == 1995 ~ 0.00,
TRUE ~ classic_gal_tan),
       ratio_gal_tan = case_when(
gruppoP_recoded == "LIFED" & legislature == 12 & year == 1994 ~ -1,
TRUE ~ ratio_gal_tan),
       logit_gal_tan = case_when(
gruppoP_recoded == "LIFED" & legislature == 12 & year == 1994 ~ -3.164631,
gruppoP_recoded == "LIFED" & legislature == 12 & year == 1995 ~ 0.000000,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == "LIFED" & legislature == 12 & year == 1994 ~ -11.34,
gruppoP_recoded == "LIFED" & legislature == 12 & year == 1995 ~ 0.00,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == "LIFED" & legislature == 12 & year == 1994 ~ -1,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == "LIFED" & legislature == 12 & year == 1994 ~ -3.164631,
gruppoP_recoded == "LIFED" & legislature == 12 & year == 1995 ~ 0.000000,
TRUE ~ logit_economic_gal_tan))

ilsd %>% 
filter(Legislature_RECODED == 13 & Year == 1996 & PARTY_RECODED == "PDS") %>% 
select(Legislature_RECODED, PARTY_RECODED, Year, left_right, ratio_leftright, logit_left_right, classic_economic, 
       ratio_economic, logit_economic, classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, 
       ratio_economic_gal_tan, logit_economic_gal_tan)

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == "PD-ULIVO-DS" & legislature == 13 & year == 1996 ~ -15.82,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == "PD-ULIVO-DS" & legislature == 13 & year == 1996 ~ -0.3947106,
TRUE ~ ratio_leftright),
       logit_left_right = case_when(
gruppoP_recoded == "PD-ULIVO-DS" & legislature == 13 & year == 1996 ~ -0.8120733,
TRUE ~ logit_left_right),
       classic_economic = case_when(
gruppoP_recoded == "PD-ULIVO-DS" & legislature == 13 & year == 1996 ~ -16.16,
TRUE ~ classic_economic),
       ratio_economic = case_when(
gruppoP_recoded == "PD-ULIVO-DS" & legislature == 13 & year == 1996 ~ -0.5998515,
TRUE ~ ratio_economic),
       logit_economic = case_when(
gruppoP_recoded == "PD-ULIVO-DS" & legislature == 13 & year == 1996 ~ -1.320057, 
TRUE ~ logit_economic),
       classic_gal_tan = case_when(
gruppoP_recoded == "PD-ULIVO-DS" & legislature == 13 & year == 1996 ~ -3.71,
TRUE ~ classic_gal_tan),
       ratio_gal_tan = case_when(
gruppoP_recoded == "PD-ULIVO-DS" & legislature == 13 & year == 1996 ~ -0.9160494,
TRUE ~ ratio_gal_tan),
       logit_gal_tan = case_when(
gruppoP_recoded == "PD-ULIVO-DS" & legislature == 13 & year == 1996 ~ -1.877526,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == "PD-ULIVO-DS" & legislature == 13 & year == 1996 ~ -3.71,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == "PD-ULIVO-DS" & legislature == 13 & year == 1996 ~ -0.9160494,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == "PD-ULIVO-DS" & legislature == 13 & year == 1996 ~ -1.877526,
TRUE ~ logit_economic_gal_tan))

ilsd %>% 
filter(Legislature_RECODED == 13 & Year == 1996 & PARTY_RECODED == "UDC-CCD-CDU") %>% 
select(Legislature_RECODED, PARTY_RECODED, Year, left_right, ratio_leftright, logit_left_right, classic_economic, 
       ratio_economic, logit_economic, classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, 
       ratio_economic_gal_tan, logit_economic_gal_tan)

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded %in% c('UDR', 'UDEUR') & legislature == 13 & year == 1996 ~ 13.29,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded %in% c('UDR', 'UDEUR') & legislature == 13 & year == 1996 ~ 0.4282952,
TRUE ~ ratio_leftright),
       logit_left_right = case_when(
gruppoP_recoded %in% c('UDR', 'UDEUR') & legislature == 13 & year == 1996 ~ 0.8830882,
TRUE ~ logit_left_right),
       classic_economic = case_when(
gruppoP_recoded %in% c('UDR', 'UDEUR') & legislature == 13 & year == 1996 ~ 4.1,
TRUE ~ classic_economic),
       ratio_economic = case_when(
gruppoP_recoded %in% c('UDR', 'UDEUR') & legislature == 13 & year == 1996 ~ 0.3010279,
TRUE ~ ratio_economic),
       logit_economic = case_when(
gruppoP_recoded %in% c('UDR', 'UDEUR') & legislature == 13 & year == 1996 ~ 0.5763143, 
TRUE ~ logit_economic),
       classic_gal_tan = case_when(
gruppoP_recoded %in% c('UDR', 'UDEUR') & legislature == 13 & year == 1996 ~ -2.85,
TRUE ~ classic_gal_tan),
       ratio_gal_tan = case_when(
gruppoP_recoded %in% c('UDR', 'UDEUR') & legislature == 13 & year == 1996 ~ -1,
TRUE ~ ratio_gal_tan),
       logit_gal_tan = case_when(
gruppoP_recoded %in% c('UDR', 'UDEUR') & legislature == 13 & year == 1996 ~ -1.902108,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded %in% c('UDR', 'UDEUR') & legislature == 13 & year == 1996 ~ -2.85,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded %in% c('UDR', 'UDEUR') & legislature == 13 & year == 1996 ~ -1,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded %in% c('UDR', 'UDEUR') & legislature == 13 & year == 1996 ~ -1.902108,
TRUE ~ logit_economic_gal_tan))

ilsd %>% 
filter(Legislature_RECODED == 13 & Year == 1998 & PARTY_RECODED == "UDR") %>% 
select(Legislature_RECODED, PARTY_RECODED, Year, left_right, ratio_leftright, logit_left_right, classic_economic, 
       ratio_economic, logit_economic, classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, 
       ratio_economic_gal_tan, logit_economic_gal_tan)

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == 'UDEUR' & legislature == 13 & year == 1998 ~ 14.76,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == 'UDEUR' & legislature == 13 & year == 1998 ~ 0.5303502,
TRUE ~ ratio_leftright),
       logit_left_right = case_when(
gruppoP_recoded == 'UDEUR' & legislature == 13 & year == 1998 ~ 1.131542,
TRUE ~ logit_left_right),
       classic_economic = case_when(
gruppoP_recoded == 'UDEUR' & legislature == 13 & year == 1998 ~ 0.07499996,
TRUE ~ classic_economic),
       ratio_economic = case_when(
gruppoP_recoded == 'UDEUR' & legislature == 13 & year == 1998 ~ -0.0009718222,
TRUE ~ ratio_economic),
       logit_economic = case_when(
gruppoP_recoded == 'UDEUR' & legislature == 13 & year == 1998 ~ 0.006135036, 
TRUE ~ logit_economic),
       classic_gal_tan = case_when(
gruppoP_recoded == 'UDEUR' & legislature == 13 & year == 1998 ~ -8.005,
TRUE ~ classic_gal_tan),
       ratio_gal_tan = case_when(
gruppoP_recoded == 'UDEUR' & legislature == 13 & year == 1998 ~ -1,
TRUE ~ ratio_gal_tan),
       logit_gal_tan = case_when(
gruppoP_recoded == 'UDEUR' & legislature == 13 & year == 1998 ~ -2.438876,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == 'UDEUR' & legislature == 13 & year == 1998 ~ -8.005,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == 'UDEUR' & legislature == 13 & year == 1998 ~ -1,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == 'UDEUR' & legislature == 13 & year == 1998 ~ -2.438876,
TRUE ~ logit_economic_gal_tan))

ilsd %>% 
filter(Legislature_RECODED == 13 & Year == 1996 & PARTY_RECODED == "RC") %>% 
select(Legislature_RECODED, PARTY_RECODED, Year, left_right, ratio_leftright, logit_left_right, classic_economic, 
       ratio_economic, logit_economic, classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, 
       ratio_economic_gal_tan, logit_economic_gal_tan)

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == 'PDCI' & legislature == 13 & year == 1996 ~ -47.2,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == 'PDCI' & legislature == 13 & year == 1996 ~ -0.9007633,
TRUE ~ ratio_leftright),
       logit_left_right = case_when(
gruppoP_recoded == 'PDCI' & legislature == 13 & year == 1996 ~ -2.786603,
TRUE ~ logit_left_right),
       classic_economic = case_when(
gruppoP_recoded == 'PDCI' & legislature == 13 & year == 1996 ~ -47.6,
TRUE ~ classic_economic),
       ratio_economic = case_when(
gruppoP_recoded == 'PDCI' & legislature == 13 & year == 1996 ~ -0.9370079,
TRUE ~ ratio_economic),
       logit_economic = case_when(
gruppoP_recoded == 'PDCI' & legislature == 13 & year == 1996 ~ -3.164068, 
TRUE ~ logit_economic),
       classic_gal_tan = case_when(
gruppoP_recoded == 'PDCI' & legislature == 13 & year == 1996 ~ -0.6,
TRUE ~ classic_gal_tan),
       ratio_gal_tan = case_when(
gruppoP_recoded == 'PDCI' & legislature == 13 & year == 1996 ~ -0.2727273,
TRUE ~ ratio_gal_tan),
       logit_gal_tan = case_when(
gruppoP_recoded == 'PDCI' & legislature == 13 & year == 1996 ~ -0.3794896,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == 'PDCI' & legislature == 13 & year == 1996 ~ -0.6,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == 'PDCI' & legislature == 13 & year == 1996 ~ -0.2727273,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == 'PDCI' & legislature == 13 & year == 1996 ~ -0.3794896,
TRUE ~ logit_economic_gal_tan))

ilsd %>% 
filter(Legislature_RECODED == 13 & Year == 1996 & PARTY_RECODED == 'PDS') %>% 
select(Legislature_RECODED, PARTY_RECODED, Year, left_right, ratio_leftright, logit_left_right, classic_economic, 
       ratio_economic, logit_economic, classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, 
       ratio_economic_gal_tan, logit_economic_gal_tan)

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1996 ~ -15.82,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1996 ~ -0.3947106,
TRUE ~ ratio_leftright),
       logit_left_right = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1996 ~ -0.8120733,
TRUE ~ logit_left_right),
       classic_economic = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1996 ~ -16.16,
TRUE ~ classic_economic),
       ratio_economic = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1996 ~ -0.5998515,
TRUE ~ ratio_economic),
       logit_economic = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1996 ~ -1.320057, 
TRUE ~ logit_economic),
       classic_gal_tan = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1996 ~ -3.71,
TRUE ~ classic_gal_tan),
       ratio_gal_tan = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1996 ~ -0.9160494,
TRUE ~ ratio_gal_tan),
       logit_gal_tan = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1996 ~ -1.877526,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1996 ~ -3.71,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1996 ~ -0.9160494,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1996 ~ -1.877526,
TRUE ~ logit_economic_gal_tan))

ilsd %>% 
filter(Legislature_RECODED == 13 & Year == 1998 & PARTY_RECODED == 'PD-ULIVO-DS') %>% 
select(Legislature_RECODED, PARTY_RECODED, Year, left_right, ratio_leftright, logit_left_right, classic_economic, 
       ratio_economic, logit_economic, classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, 
       ratio_economic_gal_tan, logit_economic_gal_tan)

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1998 ~ 3.16,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1998 ~ 0.01986045,
TRUE ~ ratio_leftright),
       logit_left_right = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1998 ~ 0.04337,
TRUE ~ logit_left_right),
       classic_economic = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1998 ~ -10.215,
TRUE ~ classic_economic),
       ratio_economic = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1998 ~ -0.6535243,
TRUE ~ ratio_economic),
       logit_economic = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1998 ~ -1.54235, 
TRUE ~ logit_economic),
       classic_gal_tan = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1998 ~ -5.505,
TRUE ~ classic_gal_tan),
       ratio_gal_tan = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1998 ~ -1,
TRUE ~ ratio_gal_tan),
       logit_gal_tan = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1998 ~ -2.401466,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1998 ~ -5.505,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1998 ~ -1,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == 'DEM-U' & legislature == 13 & year == 1998 ~ -2.401466,
TRUE ~ logit_economic_gal_tan))

ilsd %>% 
filter(Legislature_RECODED == 15 & Year == 2006 & PARTY_RECODED == "PD-ULIVO-DS") %>% 
select(Legislature_RECODED, PARTY_RECODED, Year, left_right, ratio_leftright, logit_left_right, classic_economic, 
       ratio_economic, logit_economic, classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, 
       ratio_economic_gal_tan, logit_economic_gal_tan)

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == 'SDPSE' & legislature == 15 & year == 2006 ~ -0.1900002,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == 'SDPSE' & legislature == 15 & year == 2006 ~ -0.005338584,
TRUE ~ ratio_leftright),
       logit_left_right = case_when(
gruppoP_recoded == 'SDPSE' & legislature == 15 & year == 2006 ~ -0.01038545,
TRUE ~ logit_left_right),
       classic_economic = case_when(
gruppoP_recoded == 'SDPSE' & legislature == 15 & year == 2006 ~ -7.3,
TRUE ~ classic_economic),
       ratio_economic = case_when(
gruppoP_recoded == 'SDPSE' & legislature == 15 & year == 2006 ~ -0.584,
TRUE ~ ratio_economic),
       logit_economic = case_when(
gruppoP_recoded == 'SDPSE' & legislature == 15 & year == 2006 ~ -1.210404, 
TRUE ~ logit_economic),
       classic_gal_tan = case_when(
gruppoP_recoded == 'SDPSE' & legislature == 15 & year == 2006 ~ -11.81,
TRUE ~ classic_gal_tan),
       ratio_gal_tan = case_when(
gruppoP_recoded == 'SDPSE' & legislature == 15 & year == 2006 ~ -1,
TRUE ~ ratio_gal_tan),
       logit_gal_tan = case_when(
gruppoP_recoded == 'SDPSE' & legislature == 15 & year == 2006 ~ -3.203559,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == 'SDPSE' & legislature == 15 & year == 2006 ~ -11.81,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == 'SDPSE' & legislature == 15 & year == 2006 ~ -1,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == 'SDPSE' & legislature == 15 & year == 2006 ~ -3.203559,
TRUE ~ logit_economic_gal_tan))

ilsd %>% 
filter(Legislature_RECODED == 15 & PARTY_RECODED == "UDC-CCD-CDU") %>% 
select(Legislature_RECODED, PARTY_RECODED, Year, left_right, ratio_leftright, logit_left_right, classic_economic, 
       ratio_economic, logit_economic, classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, 
       ratio_economic_gal_tan, logit_economic_gal_tan)

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == "DCA-NPSI" & legislature == 15 & year == 2006 ~ 16.48,
gruppoP_recoded == "DCA-NPSI" & legislature == 15 & year == 2008 ~ 13.78,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == "DCA-NPSI" & legislature == 15 & year == 2006 ~ 0.4993939,
gruppoP_recoded == "DCA-NPSI" & legislature == 15 & year == 2008 ~ 1.0000000,
TRUE ~ ratio_leftright),
       logit_left_right = case_when(
gruppoP_recoded == "DCA-NPSI" & legislature == 15 & year == 2006 ~ 1.058234,
gruppoP_recoded == "DCA-NPSI" & legislature == 15 & year == 2008 ~ 3.352007,
TRUE ~ logit_left_right),
       classic_economic = case_when(
gruppoP_recoded == "DCA-NPSI" & legislature == 15 & year == 2006 ~ 0.3099999,
gruppoP_recoded == "DCA-NPSI" & legislature == 15 & year == 2008 ~ 1.7200000,
TRUE ~ classic_economic),
       ratio_economic = case_when(
gruppoP_recoded == "DCA-NPSI" & legislature == 15 & year == 2006 ~ 0.01993569,
gruppoP_recoded == "DCA-NPSI" & legislature == 15 & year == 2008 ~ 1.00000000,
TRUE ~ ratio_economic),
       logit_economic = case_when(
gruppoP_recoded == "DCA-NPSI" & legislature == 15 & year == 2006 ~ 0.03746661, 
gruppoP_recoded == "DCA-NPSI" & legislature == 15 & year == 2008 ~ 1.49065439,
TRUE ~ logit_economic),
       classic_gal_tan = case_when(
gruppoP_recoded == "DCA-NPSI" & legislature == 15 & year == 2006 ~ 4.45,
gruppoP_recoded == "DCA-NPSI" & legislature == 15 & year == 2008 ~ 0.00,
TRUE ~ classic_gal_tan),
       ratio_gal_tan = case_when(
gruppoP_recoded == "DCA-NPSI" & legislature == 15 & year == 2006 ~ 0.7793345,
TRUE ~ ratio_gal_tan),
       logit_gal_tan = case_when(
gruppoP_recoded == "DCA-NPSI" & legislature == 15 & year == 2006 ~ 1.596971,
gruppoP_recoded == "DCA-NPSI" & legislature == 15 & year == 2008 ~ 0.000000,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == "DCA-NPSI" & legislature == 15 & year == 2006 ~ 4.45,
gruppoP_recoded == "DCA-NPSI" & legislature == 15 & year == 2008 ~ 0.00,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == "DCA-NPSI" & legislature == 15 & year == 2006 ~ 0.7793345,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == "DCA-NPSI" & legislature == 15 & year == 2006 ~ 1.596971,
gruppoP_recoded == "DCA-NPSI" & legislature == 15 & year == 2008 ~ 0.000000,
TRUE ~ logit_economic_gal_tan))

ilsd %>% 
filter(Legislature_RECODED == 16 & Year == 2008 & PARTY_RECODED == "FI-PDL") %>% 
select(Legislature_RECODED, PARTY_RECODED, Year, left_right, ratio_leftright, logit_left_right, classic_economic, 
       ratio_economic, logit_economic, classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, 
       ratio_economic_gal_tan, logit_economic_gal_tan)

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == 'FLPTP' & legislature == 16 & year == 2008 ~ 21.74,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == 'FLPTP' & legislature == 16 & year == 2008 ~ 0.5872501,
TRUE ~ ratio_leftright),
       logit_left_right = case_when(
gruppoP_recoded == 'FLPTP' & legislature == 16 & year == 2008 ~ 1.300399,
TRUE ~ logit_left_right),
       classic_economic = case_when(
gruppoP_recoded == 'FLPTP' & legislature == 16 & year == 2008 ~ 8.46,
TRUE ~ classic_economic),
       ratio_economic = case_when(
gruppoP_recoded == 'FLPTP' & legislature == 16 & year == 2008 ~ 0.7513321,
TRUE ~ ratio_economic),
       logit_economic = case_when(
gruppoP_recoded == 'FLPTP' & legislature == 16 & year == 2008 ~ 1.696098, 
TRUE ~ logit_economic),
       classic_gal_tan = case_when(
gruppoP_recoded == 'FLPTP' & legislature == 16 & year == 2008 ~ -1.82,
TRUE ~ classic_gal_tan),
       ratio_gal_tan = case_when(
gruppoP_recoded == 'FLPTP' & legislature == 16 & year == 2008 ~ -0.5321637,
TRUE ~ ratio_gal_tan),
       logit_gal_tan = case_when(
gruppoP_recoded == 'FLPTP' & legislature == 16 & year == 2008 ~ -0.8754687,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == 'FLPTP' & legislature == 16 & year == 2008 ~ -1.82,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == 'FLPTP' & legislature == 16 & year == 2008 ~ -0.5321637,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == 'FLPTP' & legislature == 16 & year == 2008 ~ -0.8754687,
TRUE ~ logit_economic_gal_tan))

ilsd %>% 
filter(Legislature_RECODED == 16 & Year == 2008 & PARTY_RECODED == "MPA") %>% 
select(Legislature_RECODED, PARTY_RECODED, Year, left_right, ratio_leftright, logit_left_right, classic_economic, 
       ratio_economic, logit_economic, classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, 
       ratio_economic_gal_tan, logit_economic_gal_tan)

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == "NOI SUD-LIBERTA' ED AUTONOMIA/POPOLARI D'ITALIA DOMANI-PID/MOVIMENTO DI RESPONSABILITA' NAZIONALE-MRN/AZIONE POPOLARE/ALLEANZA DI CENTRO-ADC/INTESA POPOLARE" & legislature == 16 & year == 2008 ~ 7.76,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == "NOI SUD-LIBERTA' ED AUTONOMIA/POPOLARI D'ITALIA DOMANI-PID/MOVIMENTO DI RESPONSABILITA' NAZIONALE-MRN/AZIONE POPOLARE/ALLEANZA DI CENTRO-ADC/INTESA POPOLARE" & legislature == 16 & year == 2008 ~ 0.2827988,
TRUE ~ ratio_leftright),
       logit_left_right = case_when(
gruppoP_recoded == "NOI SUD-LIBERTA' ED AUTONOMIA/POPOLARI D'ITALIA DOMANI-PID/MOVIMENTO DI RESPONSABILITA' NAZIONALE-MRN/AZIONE POPOLARE/ALLEANZA DI CENTRO-ADC/INTESA POPOLARE" & legislature == 16 & year == 2008 ~ 0.5598921,
TRUE ~ logit_left_right),
       classic_economic = case_when(
gruppoP_recoded == "NOI SUD-LIBERTA' ED AUTONOMIA/POPOLARI D'ITALIA DOMANI-PID/MOVIMENTO DI RESPONSABILITA' NAZIONALE-MRN/AZIONE POPOLARE/ALLEANZA DI CENTRO-ADC/INTESA POPOLARE" & legislature == 16 & year == 2008 ~ 1.42,
TRUE ~ classic_economic),
       ratio_economic = case_when(
gruppoP_recoded == "NOI SUD-LIBERTA' ED AUTONOMIA/POPOLARI D'ITALIA DOMANI-PID/MOVIMENTO DI RESPONSABILITA' NAZIONALE-MRN/AZIONE POPOLARE/ALLEANZA DI CENTRO-ADC/INTESA POPOLARE" & legislature == 16 & year == 2008 ~ 0.07759564,
TRUE ~ ratio_economic),
       logit_economic = case_when(
gruppoP_recoded == "NOI SUD-LIBERTA' ED AUTONOMIA/POPOLARI D'ITALIA DOMANI-PID/MOVIMENTO DI RESPONSABILITA' NAZIONALE-MRN/AZIONE POPOLARE/ALLEANZA DI CENTRO-ADC/INTESA POPOLARE" & legislature == 16 & year == 2008 ~ 0.1474167, 
TRUE ~ logit_economic),
       classic_gal_tan = case_when(
gruppoP_recoded == "NOI SUD-LIBERTA' ED AUTONOMIA/POPOLARI D'ITALIA DOMANI-PID/MOVIMENTO DI RESPONSABILITA' NAZIONALE-MRN/AZIONE POPOLARE/ALLEANZA DI CENTRO-ADC/INTESA POPOLARE" & legislature == 16 & year == 2008 ~ -2.81,
TRUE ~ classic_gal_tan),
       ratio_gal_tan = case_when(
gruppoP_recoded == "NOI SUD-LIBERTA' ED AUTONOMIA/POPOLARI D'ITALIA DOMANI-PID/MOVIMENTO DI RESPONSABILITA' NAZIONALE-MRN/AZIONE POPOLARE/ALLEANZA DI CENTRO-ADC/INTESA POPOLARE" & legislature == 16 & year == 2008 ~ -1,
TRUE ~ ratio_gal_tan),
       logit_gal_tan = case_when(
gruppoP_recoded == "NOI SUD-LIBERTA' ED AUTONOMIA/POPOLARI D'ITALIA DOMANI-PID/MOVIMENTO DI RESPONSABILITA' NAZIONALE-MRN/AZIONE POPOLARE/ALLEANZA DI CENTRO-ADC/INTESA POPOLARE" & legislature == 16 & year == 2008 ~ -1.890095,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == "NOI SUD-LIBERTA' ED AUTONOMIA/POPOLARI D'ITALIA DOMANI-PID/MOVIMENTO DI RESPONSABILITA' NAZIONALE-MRN/AZIONE POPOLARE/ALLEANZA DI CENTRO-ADC/INTESA POPOLARE" & legislature == 16 & year == 2008 ~ -2.81,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == "NOI SUD-LIBERTA' ED AUTONOMIA/POPOLARI D'ITALIA DOMANI-PID/MOVIMENTO DI RESPONSABILITA' NAZIONALE-MRN/AZIONE POPOLARE/ALLEANZA DI CENTRO-ADC/INTESA POPOLARE" & legislature == 16 & year == 2008 ~ -1,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == "NOI SUD-LIBERTA' ED AUTONOMIA/POPOLARI D'ITALIA DOMANI-PID/MOVIMENTO DI RESPONSABILITA' NAZIONALE-MRN/AZIONE POPOLARE/ALLEANZA DI CENTRO-ADC/INTESA POPOLARE" & legislature == 16 & year == 2008 ~ -1.890095,
TRUE ~ logit_economic_gal_tan))

ilsd %>% 
filter(Legislature_RECODED == 17 & PARTY_RECODED %in% c('PD-ULIVO-DS', 'SI-SEL-POS-LU')) %>% 
group_by(Year) %>% 
summarize(left_right = mean(left_right),
          ratio_leftright = mean(logit_left_right),
          logit_left_right = mean(logit_left_right),
         classic_economic = mean(classic_economic),
         ratio_economic = mean(ratio_economic),
         logit_economic = mean(logit_economic),
         classic_gal_tan = mean(classic_gal_tan),
         ratio_gal_tan = mean(ratio_gal_tan),
         logit_gal_tan = mean(logit_gal_tan),
         classic_economic_gal_tan = mean(classic_economic_gal_tan),
         ratio_economic_gal_tan = mean(ratio_economic_gal_tan),
         logit_economic_gal_tan = mean(logit_economic_gal_tan))

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2013 ~ -21.64439,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2014 ~ -16.20470,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2016 ~ -14.95555,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2013 ~ -1.104227,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2014 ~ -1.152655,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2016 ~ -1.415719,
TRUE ~ ratio_leftright),
      logit_left_right = case_when(
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2013 ~ -1.104227,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2014 ~ -1.152655,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2016 ~ -1.415719,
TRUE ~ logit_left_right),
      classic_economic = case_when(
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2013 ~ -19.056212,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2014 ~ -14.590125,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2016 ~ -8.301418,
TRUE ~ classic_economic),
      ratio_economic = case_when(
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2013 ~ -0.5358288,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2014 ~ -0.6785539,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2016 ~ -0.5106952,
TRUE ~ ratio_economic),
      logit_economic = case_when(
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2013 ~ -1.902942,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2014 ~ -1.869514,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2016 ~ -1.360002,
TRUE ~ logit_economic),
      classic_gal_tan = case_when(
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2013 ~ -1.9383180,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2014 ~ -1.8041480,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2016 ~ -0.1288377,
TRUE ~ classic_gal_tan),
      ratio_gal_tan = case_when(
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2013 ~ -0.5352941,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2014 ~ -0.7093023,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2016 ~ 0.2500000,
TRUE ~ ratio_gal_tan),
      logit_gal_tan = case_when(
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2013 ~ -0.82120503,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2014 ~ -0.63946857,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2016 ~ -0.08108178,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2013 ~ -1.6093706,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2014 ~ -1.7585276,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2016 ~ -0.1288377,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2013 ~ -0.4686275,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2014 ~ -0.6881720,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2016 ~ 0.2500000,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2013 ~ -0.55472906,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2014 ~ -0.59988311,
gruppoP_recoded == 'MDP-LU' & legislature == 17 & year == 2016 ~ -0.08108178,
TRUE ~ logit_economic_gal_tan)) 

ilsd %>% 
filter(Legislature_RECODED == 17 & PARTY_RECODED == "NCI-SCPI-MAIE") %>% 
select(Legislature_RECODED, PARTY_RECODED, Year, left_right, ratio_leftright, logit_left_right, classic_economic, 
       ratio_economic, logit_economic, classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, 
       ratio_economic_gal_tan, logit_economic_gal_tan)


joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == 'CI' & legislature == 17 & year == 2013 ~ 25.336577,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2014 ~ 25.271738,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2016 ~ 4.242424,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == 'CI' & legislature == 17 & year == 2013 ~ 0.61775710,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2014 ~ 0.68888891,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2016 ~ 0.09589041,
TRUE ~ ratio_leftright),
      logit_left_right = case_when(
gruppoP_recoded == 'CI' & legislature == 17 & year == 2013 ~ 1.5171773,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2014 ~ 1.6236965,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2016 ~ 0.1880945,
TRUE ~ logit_left_right),
      classic_economic = case_when(
gruppoP_recoded == 'CI' & legislature == 17 & year == 2013 ~ 6.233084,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2014 ~ 21.195652,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2016 ~ 6.060606,
TRUE ~ classic_economic),
      ratio_economic = case_when(
gruppoP_recoded == 'CI' & legislature == 17 & year == 2013 ~ 0.3267974,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2014 ~ 0.7222222,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2016 ~ 0.2272727,
TRUE ~ ratio_economic),
      logit_economic = case_when(
gruppoP_recoded == 'CI' & legislature == 17 & year == 2013 ~ 0.7681459,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2014 ~ 1.7284342,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2016 ~ 0.4453329,
TRUE ~ logit_economic),
      classic_gal_tan = case_when(
gruppoP_recoded == 'CI' & legislature == 17 & year == 2013 ~ -5.233575,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2014 ~ -2.445652,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2016 ~ 4.848485,
TRUE ~ classic_gal_tan),
      ratio_gal_tan = case_when(
gruppoP_recoded == 'CI' & legislature == 17 & year == 2013 ~ -0.9047619,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2014 ~ -1.0000000,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2016 ~ 0.8000000,
TRUE ~ ratio_gal_tan),
      logit_gal_tan = case_when(
gruppoP_recoded == 'CI' & legislature == 17 & year == 2013 ~ -1.899488,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2014 ~ -1.773477,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2016 ~ 1.683350,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == 'CI' & legislature == 17 & year == 2013 ~ -5.233575,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2014 ~ -1.902174,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2016 ~ 4.848485,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == 'CI' & legislature == 17 & year == 2013 ~ -0.9047619,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2014 ~ -0.6363637,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2016 ~ 0.8000000,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == 'CI' & legislature == 17 & year == 2013 ~ -1.899488,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2014 ~ -1.037771,
gruppoP_recoded == 'CI' & legislature == 17 & year == 2016 ~ 1.683350,
TRUE ~ logit_economic_gal_tan)) 

ilsd %>% 
filter(Legislature_RECODED == 18 & PARTY_RECODED == "PD-ULIVO-DS") %>% 
select(Legislature_RECODED, PARTY_RECODED, Year, left_right, ratio_leftright, logit_left_right, classic_economic, 
       ratio_economic, logit_economic, classic_gal_tan, ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, 
       ratio_economic_gal_tan, logit_economic_gal_tan)

joined_texts <- joined_texts %>% 
mutate(left_right = case_when(
gruppoP_recoded == 'IV' & legislature == 18 & year == 2018 ~ -9.815951,
TRUE ~ left_right),
      ratio_leftright = case_when(
gruppoP_recoded == 'IV' & legislature == 18 & year == 2018 ~ -0.2580645,
TRUE ~ ratio_leftright),
       logit_left_right = case_when(
gruppoP_recoded == 'IV' & legislature == 18 & year == 2018 ~ -0.5139281,
TRUE ~ logit_left_right),
       classic_economic = case_when(
gruppoP_recoded == 'IV' & legislature == 18 & year == 2018 ~ 1.840491,
TRUE ~ classic_economic),
       ratio_economic = case_when(
gruppoP_recoded == 'IV' & legislature == 18 & year == 2018 ~ 0.2,
TRUE ~ ratio_economic),
       logit_economic = case_when(
gruppoP_recoded == 'IV' & legislature == 18 & year == 2018 ~ 0.3647858, 
TRUE ~ logit_economic),
       classic_gal_tan = case_when(
gruppoP_recoded == 'IV' & legislature == 18 & year == 2018 ~ -7.361963,
TRUE ~ classic_gal_tan),
       ratio_gal_tan = case_when(
gruppoP_recoded == 'IV' & legislature == 18 & year == 2018 ~ -0.8571429,
TRUE ~ ratio_gal_tan),
       logit_gal_tan = case_when(
gruppoP_recoded == 'IV' & legislature == 18 & year == 2018 ~ -2.029669,
TRUE ~ logit_gal_tan),
       classic_economic_gal_tan = case_when(
gruppoP_recoded == 'IV' & legislature == 18 & year == 2018 ~ -7.361963,
TRUE ~ classic_economic_gal_tan),
       ratio_economic_gal_tan = case_when(
gruppoP_recoded == 'IV' & legislature == 18 & year == 2018 ~ -0.8571429,
TRUE ~ ratio_economic_gal_tan),
       logit_economic_gal_tan = case_when(
gruppoP_recoded == 'IV' & legislature == 18 & year == 2018 ~ -2.029669,
TRUE ~ logit_economic_gal_tan))

joined_texts <- joined_texts %>% 
arrange(year) %>% 
group_by(gruppoP_recoded, legislature) %>% 
fill_(fill_cols = c("left_right", "ratio_leftright", "logit_left_right", "classic_economic", "ratio_economic", 
                    "logit_economic", "classic_gal_tan", "ratio_gal_tan", "logit_gal_tan", "classic_economic_gal_tan", 
                    "ratio_economic_gal_tan", "logit_economic_gal_tan"),
       .direction = "down")

map_dfr(joined_texts, ~ sum(is.na(.)) / length(.)) %>% 
select(left_right, ratio_leftright, logit_left_right, classic_economic, ratio_economic, logit_economic, classic_gal_tan,
      ratio_gal_tan, logit_gal_tan, classic_economic_gal_tan, ratio_economic_gal_tan, logit_economic_gal_tan, gal_tan_controllo)

joined_texts %>% 
filter(is.na(left_right)) %>% 
distinct(legislature)

saveRDS(joined_texts, file = "data/joined_texts.rds")
