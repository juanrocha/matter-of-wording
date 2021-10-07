## Recover missing abstracts
## This script only works from an IP based on campus
## Juan Rocha
## 211006

library(tidyverse)
library(fs)
library(here)
library(rscopus)
library(tictoc)


load("data/missing_abstracts.RData")


res <- list() # response list

dois <- df_lack |> 
    slice(1:10000) |> # API allows 10k per week, change index every week
    pull(doi)

tic()
for (i in seq_along(dois)){
    res[[i]] <- abstract_retrieval(id = dois[i], identifier = "doi") 
    Sys.sleep(0.2)
}
toc()

## The res (response) object is over 8.7GB, not point of saving it all, only the
## abstracts which are already >20MB for 1000 records. It does however return
## full reference information. Good to know for future work.

## abs_safe extract the abstract safely, if there was an error in the call, it returns null.
abs_safe <- safely(
    function(x){ 
        x$content$`abstracts-retrieval-response`$item$bibrecord$head$abstracts 
    }
)


abs_safe(res[[597]])

abstracts <- res |> 
    map(abs_safe)
    
abstracts[[597]] # an example of an error, it should be stored as NULL

abstracts <- transpose(abstracts)

df_1_10k <- tibble(
    doi = dois,
    abstract = abstracts$result) |> 
    unnest(abstract, keep_empty = TRUE)

save(df_1_10k, file = "data/abstracts_1_10k.RData")
