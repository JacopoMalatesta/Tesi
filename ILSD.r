suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))
suppressWarnings(suppressPackageStartupMessages(library(haven)))

ilsd <- read_dta("data/ilsd jun2018 (con scale).dta")

ilsd <- ilsd %>% 
mutate(jacopo_right = per101 + per104 + per110 + per107 + per401 + per402 + per406 + per410 + per414 + per416 + per601 + per603 + per605 + per609 + per608 + per202,
       jacopo_left = per102 + per103 + per105 + per106 + per108 + per109 + per403 + per404 + per405 + per408 + per409 + per411 + per412 + per413 + per415 + per417 + per602 + per604 + per606 + per607 + per610 + per611 + per201,
       jacopo_left_right = jacopo_right - jacopo_left,
       jacopo_ratio_leftright = (jacopo_right - jacopo_left) / (jacopo_right + jacopo_left),
       logit_left_right = log(jacopo_right + .5) - log(jacopo_left + .5),
       gal_tan = ((per108 + per608) - (per107 + per607)),
       ratio_gal_tan = gal_tan / (per107 + per108 + per607 + per608),
       logit_gal_tan = log((per108 + per608) + .5) - log((per107 + per607) + .5))

ilsd %>% 
select(left_right, jacopo_left_right, ratio_leftright, jacopo_ratio_leftright, logit_left_right) %>% 
arrange(desc(logit_left_right))

ilsd %>% 
select(per107, per108, per607, per608, gal_tan, ratio_gal_tan, logit_gal_tan)

ilsd %>% 
filter(is.na(ratio_gal_tan)) %>% 
select(per107, per108, per607, per608, gal_tan, ratio_gal_tan, logit_gal_tan)

load("data/parliamentary_groups2.rds")
texts <- Texts %>% as_tibble()

texts <- texts %>% mutate(legislatura = as.double(legislatura))

ilsd$Legislature

ilsd %>% mutate(Legislature = Legislature - 1)



colnames(texts)


