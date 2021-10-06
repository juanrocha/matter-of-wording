## Recover missing abstracts
## This script only works from an IP based on campus
## Juan Rocha
## 211006

library(tidyverse)
library(fs)
library(here)
library(rscopus)


load("data/missing_abstracts.RData")


res <- list() # response list

res <- df_lack |> 
    slice(1:10000) |> # API allows 10k per week, change index every week
    pull(doi) |> 
    abstract_retrieval(identifier = "doi") 

abstracts <- res |> 
    purrr::map_chr(function(x){ 
        x$content$`abstracts-retrieval-response`$item$bibrecord$head$abstracts 
    })





