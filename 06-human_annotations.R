# Extracting human annotated impacts on ecosystem services

library(tidyverse)

rsdat <- read_csv("~/Documents/Projects_old/Cascading Effects/data/161025_RegimeShiftsDatabase.CSV") |> 
    janitor::clean_names()

rsdat <- rsdat |> 
    select(
        rsname = regime_shift_name,
        ecoprocess = impacts_key_ecosystem_processes ,
        provisioning = impacts_provisioning_services,
        regulating = impacts_regulating_services,
        cultural = impacts_cultural_services ,
        hwb = impacts_human_well_being
    )

rsdat <- rsdat |> 
    mutate(across(.cols = ecoprocess:hwb, .fns = ~str_split(.x, pattern =", "))) 

rsdat <- rsdat |> 
    pivot_longer(cols = ecoprocess:hwb, names_to = "es_type", values_to = "es_cat") |> 
    unnest(es_cat) |> 
    mutate(detect = 1)

#save(rsdat, file = "data/rsdb_ES_annotations.RData")

load("data/rsdb_ES_annotations.RData")
rsdat |> 
    ggplot(aes(es_cat, rsname)) +
    geom_tile(aes(fill = detect)) +
    #facet_wrap(~es_type, scales = "free", nrow = 4)
    theme(axis.text.x = element_text(angle =90))
